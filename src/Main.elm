port module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


main : Program UnsafeFlags Model Msg
main =
    Html.programWithFlags
        { init = initSafely
        , view = view
        , update = updateWithSave
        , subscriptions = \_ -> Sub.none
        }


initSafely : UnsafeFlags -> ( Model, Cmd Msg )
initSafely =
    decodeFlags >> init


{-| Save model on every update.

From <https://github.com/evancz/elm-todomvc/blob/master/Todo.elm#L39>.

-}
updateWithSave : Msg -> Model -> ( Model, Cmd Msg )
updateWithSave msg model =
    let
        ( newModel, cmds ) =
            update msg model

        encodedNewModel =
            newModel
                |> encodeModel
    in
        ( newModel
        , Cmd.batch [ saveStandUp encodedNewModel, cmds ]
        )



-- Model


type alias Model =
    { standUpEntries : Dict String StandUpEntry
    , error : Error
    , addTeamMemberError : AddTeamMemberError
    , addTeamMemberInput : String
    }


type Error
    = NoError
    | InitError String


type AddTeamMemberValidation
    = ValidTeamMember TeamMember
    | TeamMemberNameAlreadyExists String


type AddTeamMemberError
    = NoAddTeamMemberError
    | NameAlreadyExists String


type alias StandUpEntry =
    { teamMember : TeamMember
    , completed : Bool
    }


type alias TeamMember =
    { name : String }


teamMembersToDict : List TeamMember -> Dict String TeamMember
teamMembersToDict teamMembers =
    let
        teamMembersKeyedTuple =
            List.map (\teamMember -> ( teamMember.name, teamMember ))
                teamMembers
    in
        Dict.fromList teamMembersKeyedTuple



-- Init


type alias UnsafeFlags =
    Encode.Value


type alias Flags =
    { teamMembers : List TeamMember }


init : Result String Flags -> ( Model, Cmd Msg )
init flagsResult =
    case flagsResult of
        Ok flags ->
            initModelFromFlags flags
                ! []

        Err err ->
            initModelFromError err
                ! []


initModelFromFlags : Flags -> Model
initModelFromFlags flags =
    let
        teamMembersDict =
            teamMembersToDict flags.teamMembers
    in
        { standUpEntries =
            Dict.map initStandUpFromTeamMemberDict teamMembersDict
        , error = NoError
        , addTeamMemberError = NoAddTeamMemberError
        , addTeamMemberInput = ""
        }


initModelFromError : String -> Model
initModelFromError error =
    { standUpEntries = Dict.empty
    , error = InitError error
    , addTeamMemberError = NoAddTeamMemberError
    , addTeamMemberInput = ""
    }


initStandUpFromTeamMemberDict : String -> TeamMember -> StandUpEntry
initStandUpFromTeamMemberDict name teamMember =
    initStandUpEntryFromTeamMember teamMember


initStandUpEntryFromTeamMember : TeamMember -> StandUpEntry
initStandUpEntryFromTeamMember teamMember =
    { teamMember = teamMember
    , completed = False
    }



-- View


view : Model -> Html Msg
view model =
    case model.error of
        NoError ->
            viewStandUp model

        InitError error ->
            viewError model


viewStandUp : Model -> Html Msg
viewStandUp model =
    div [ class "stand-up-meeting" ]
        [ h2 [] [ text "Stand-up meeting" ]
        , Keyed.ul [ class "stand-up-entries" ]
            ((viewStandUpEntries model.standUpEntries)
                -- TODO: Improve the keying/clarity here.
                |> List.append [ ( "", viewAddNewStandupEntryInput model ) ]
            )
        ]


viewStandUpEntries : Dict String StandUpEntry -> List ( String, Html Msg )
viewStandUpEntries standUpEntries =
    standUpEntries
        |> Dict.values
        |> List.sortWith completedComparison
        |> List.map viewKeyedEntry


completedComparison : StandUpEntry -> StandUpEntry -> Order
completedComparison entryA entryB =
    case ( entryA.completed, entryB.completed ) of
        ( True, True ) ->
            EQ

        ( False, False ) ->
            EQ

        ( True, False ) ->
            GT

        ( False, True ) ->
            LT


viewKeyedEntry : StandUpEntry -> ( String, Html Msg )
viewKeyedEntry standUpEntry =
    ( standUpEntry.teamMember.name, viewStandUpEntry standUpEntry )


viewStandUpEntry : StandUpEntry -> Html Msg
viewStandUpEntry standUpEntry =
    let
        completedClass =
            if standUpEntry.completed then
                "completed"
            else
                ""
    in
        li [ class completedClass ]
            [ input
                [ type_ "checkbox"
                , checked standUpEntry.completed
                , onClick (ToggleEntryCompleted standUpEntry.teamMember.name)
                ]
                []
            , text standUpEntry.teamMember.name
            ]


viewAddNewStandupEntryInput : Model -> Html Msg
viewAddNewStandupEntryInput model =
    li []
        [ input
            [ type_ "checkbox"
            , checked False
            ]
            []
        , input
            [ type_ "text"
            , placeholder "John Doe"
            , onInput UpdateTeamMemberInput
            , onEnter AddTeamMember
            , value model.addTeamMemberInput
            ]
            []

        -- TODO: Add error message when NameAlreadyExists.
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        Html.Events.on "keydown"
            (Html.Events.keyCode |> Decode.andThen isEnter)


viewError : Model -> Html Msg
viewError model =
    case model.error of
        NoError ->
            text ""

        InitError error ->
            div [ class "init-error" ]
                [ h2 [] [ text "Initialization error" ]
                , p []
                    [ text "Error:"
                    , pre [] [ text error ]
                    ]
                , p []
                    [ text "Ensure you are initializing the data, something like this:"
                    , pre []
                        [ code [] [ text viewErrorHint ]
                        ]
                    ]
                ]


viewErrorHint : String
viewErrorHint =
    """var storedState = localStorage.getItem('elm-stand-up');
var startingState = storedState ? JSON.parse(storedState) : [];
var elmStandUp = Elm.Main.fullscreen(startingState);
"""



-- Update


type Msg
    = ToggleEntryCompleted String
    | AddTeamMember
    | UpdateTeamMemberInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleEntryCompleted name ->
            toggleEntryCompleted model name ! []

        AddTeamMember ->
            (model |> addTeamMember model.addTeamMemberInput) ! []

        UpdateTeamMemberInput name ->
            { model
                | addTeamMemberInput = name
            }
                ! []


toggleEntryCompleted : Model -> String -> Model
toggleEntryCompleted model name =
    let
        updatedStandUpEntries =
            Dict.update name toggleCompleted model.standUpEntries
    in
        { model | standUpEntries = updatedStandUpEntries }


toggleCompleted : Maybe StandUpEntry -> Maybe StandUpEntry
toggleCompleted maybeStandUpEntry =
    case maybeStandUpEntry of
        Nothing ->
            -- Could show an error in the future.
            Nothing

        Just standUpEntry ->
            Just
                { standUpEntry
                    | completed = not standUpEntry.completed
                }


validateTeamMember : TeamMember -> Model -> AddTeamMemberValidation
validateTeamMember teamMember model =
    let
        nameInUse =
            model.standUpEntries
                |> Dict.get teamMember.name
    in
        case nameInUse of
            Just standUpEntry ->
                TeamMemberNameAlreadyExists standUpEntry.teamMember.name

            Nothing ->
                ValidTeamMember teamMember


addTeamMember : String -> Model -> Model
addTeamMember name model =
    let
        addTeamMemberValidation =
            model
                |> validateTeamMember (TeamMember name)
    in
        case addTeamMemberValidation of
            ValidTeamMember teamMember ->
                { model
                    | standUpEntries = Dict.insert teamMember.name (initStandUpEntryFromTeamMember teamMember) model.standUpEntries
                    , addTeamMemberInput = ""
                }

            TeamMemberNameAlreadyExists name ->
                { model
                    | addTeamMemberError = NameAlreadyExists name
                }



-- Ports


port saveStandUp : Encode.Value -> Cmd msg



-- Serialization


encodeModel : Model -> Encode.Value
encodeModel model =
    let
        standUpEntryList =
            model.standUpEntries
                |> Dict.values

        encodedStandUpEntries =
            standUpEntryList
                |> List.map encodeStandUpEntry

        encodedTeamMembers =
            standUpEntryList
                |> List.map .teamMember
                |> List.map encodeTeamMember
    in
        Encode.object
            [ ( "standUpEntries", Encode.list encodedStandUpEntries )
            , ( "teamMembers", Encode.list encodedTeamMembers )
            ]


encodeStandUpEntry : StandUpEntry -> Encode.Value
encodeStandUpEntry standUpEntry =
    Encode.object
        [ ( "teamMember", encodeTeamMember standUpEntry.teamMember )
        , ( "completed", Encode.bool standUpEntry.completed )
        ]


encodeTeamMember : TeamMember -> Encode.Value
encodeTeamMember teamMember =
    Encode.object
        [ ( "name", Encode.string teamMember.name )
        ]


decodeFlags : UnsafeFlags -> Result String Flags
decodeFlags unsafeFlags =
    Decode.decodeValue flagsDecoder unsafeFlags


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map Flags
        (Decode.list teamMemberDecoder)


teamMemberDecoder : Decoder TeamMember
teamMemberDecoder =
    Decode.map TeamMember
        (Decode.field "name" Decode.string)
