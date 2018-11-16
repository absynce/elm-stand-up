port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import Time exposing (Posix)


main : Program UnsafeFlags Model Msg
main =
    Browser.element
        { init = initSafely
        , view = view
        , update = updateWithSave
        , subscriptions = \_ -> Sub.none
        }



-- Aspects (AOP)


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

    -- TODO: Move into StandUpEntryStatus (NotHere?).
    , checkHipChat : Bool
    , status : StandUpEntryStatus
    }


type StandUpEntryStatus
    = Completed Posix
    | ToDo
    | NotWorking NotWorkingReason


type NotWorkingReason
    = PaidTimeOff


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


isComplete : StandUpEntry -> Bool
isComplete standUpEntry =
    case standUpEntry.status of
        Completed _ ->
            True

        ToDo ->
            False

        NotWorking notWorkingReason ->
            True


isNotWorking : StandUpEntry -> Bool
isNotWorking standUpEntry =
    case standUpEntry.status of
        Completed _ ->
            False

        ToDo ->
            False

        NotWorking notWorkingReason ->
            True



-- Init


type alias UnsafeFlags =
    Encode.Value


type alias Flags =
    { teamMembers : List TeamMember }


init : Result Decode.Error Flags -> ( Model, Cmd Msg )
init flagsResult =
    case flagsResult of
        Ok flags ->
            ( initModelFromFlags flags
            , Cmd.none
            )

        Err err ->
            ( initModelFromError err
            , Cmd.none
            )


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


initModelFromError : Decode.Error -> Model
initModelFromError error =
    let
        errorString =
            error |> Decode.errorToString
    in
    { standUpEntries = Dict.empty
    , error = InitError errorString
    , addTeamMemberError = NoAddTeamMemberError
    , addTeamMemberInput = ""
    }


initStandUpFromTeamMemberDict : String -> TeamMember -> StandUpEntry
initStandUpFromTeamMemberDict name teamMember =
    initStandUpEntryFromTeamMember teamMember


initStandUpEntryFromTeamMember : TeamMember -> StandUpEntry
initStandUpEntryFromTeamMember teamMember =
    { teamMember = teamMember
    , checkHipChat = False
    , status = ToDo
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
            -- TODO: Improve the keying/clarity here.
            (( "", viewAddNewStandupEntryInput model )
                :: viewStandUpEntries model.standUpEntries
             -- |> List.append [ ( "", viewAddNewStandupEntryInput model ) ]
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
    case ( entryA |> isComplete, entryB |> isComplete ) of
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
        -- TODO: Use classList instead.
        completedClass =
            if standUpEntry |> isComplete then
                "completed"

            else
                ""
    in
    li
        [ class completedClass
        , class "stand-up-entry"
        ]
        [ i
            [ classList
                [ ( "far fa-square", standUpEntry.status == ToDo )
                , ( "far fa-check-square", standUpEntry |> isComplete )
                , ( "fas fa-times", standUpEntry |> isNotWorking )
                ]
            , onClick (StartToggleEntryCompleted standUpEntry.teamMember.name)
            ]
            []
        , span []
            [ text standUpEntry.teamMember.name
            ]
        , i
            [ classList
                [ ( "far fa-comment", True )
                , ( "disabled", not standUpEntry.checkHipChat )
                ]
            , onClick (ToggleCheckHipChat standUpEntry.teamMember.name)
            ]
            []
        , i
            [ classList
                [ ( "fas fa-ban", True )
                , ( "disabled", standUpEntry |> isNotWorking |> not )
                ]
            , onClick (ToggleEntryNotWorking standUpEntry.teamMember.name PaidTimeOff)
            ]
            []
        ]


viewAddNewStandupEntryInput : Model -> Html Msg
viewAddNewStandupEntryInput model =
    li [ class "stand-up-entry" ]
        [ i
            [ class "far fa-square"
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
    = StartToggleEntryCompleted String
    | ToggleEntryCompleted String Posix
    | ToggleEntryNotWorking String NotWorkingReason
    | AddTeamMember
    | UpdateTeamMemberInput String
    | ToggleCheckHipChat String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartToggleEntryCompleted name ->
            ( model
            , Task.perform (ToggleEntryCompleted name) Time.now
            )

        ToggleEntryCompleted name time ->
            ( toggleEntryCompleted model name time
            , Cmd.none
            )

        ToggleEntryNotWorking name notWorkingReason ->
            ( toggleEntryNotWorking model name notWorkingReason
            , Cmd.none
            )

        AddTeamMember ->
            ( model |> addTeamMember model.addTeamMemberInput
            , Cmd.none
            )

        UpdateTeamMemberInput name ->
            ( { model
                | addTeamMemberInput = name
              }
            , Cmd.none
            )

        ToggleCheckHipChat name ->
            ( toggleEntryCheckHipChat model name
            , Cmd.none
            )


toggleEntryCompleted : Model -> String -> Posix -> Model
toggleEntryCompleted model name time =
    let
        updatedStandUpEntries =
            Dict.update name maybeToggleCompleted model.standUpEntries

        maybeToggleCompleted =
            Maybe.map (toggleCompleted time)
    in
    { model | standUpEntries = updatedStandUpEntries }


toggleCompleted : Posix -> StandUpEntry -> StandUpEntry
toggleCompleted time standUpEntry =
    { standUpEntry
        | status = toggleStatusCompleted time standUpEntry.status
    }


toggleStatusCompleted : Posix -> StandUpEntryStatus -> StandUpEntryStatus
toggleStatusCompleted time standUpEntryStatus =
    case standUpEntryStatus of
        Completed _ ->
            ToDo

        ToDo ->
            Completed time

        NotWorking notWorkingReason ->
            ToDo


toggleEntryNotWorking : Model -> String -> NotWorkingReason -> Model
toggleEntryNotWorking model name notWorkingReason =
    let
        maybeToggleNotWorking =
            Maybe.map (toggleNotWorking notWorkingReason)

        updatedStandUpEntries =
            Dict.update name maybeToggleNotWorking model.standUpEntries
    in
    { model | standUpEntries = updatedStandUpEntries }


toggleNotWorking : NotWorkingReason -> StandUpEntry -> StandUpEntry
toggleNotWorking notWorkingReason standUpEntry =
    { standUpEntry
        | status = toggleStatusNotWorking notWorkingReason standUpEntry.status
    }


toggleStatusNotWorking : NotWorkingReason -> StandUpEntryStatus -> StandUpEntryStatus
toggleStatusNotWorking notWorkingReason standUpEntryStatus =
    case standUpEntryStatus of
        Completed _ ->
            ToDo

        ToDo ->
            NotWorking notWorkingReason

        NotWorking _ ->
            ToDo


{-| This is a clear duplication of toggleEntryCompleted. Not sure yet whether to generalize.
-}
toggleEntryCheckHipChat : Model -> String -> Model
toggleEntryCheckHipChat model name =
    let
        updatedStandUpEntries =
            Dict.update name (Maybe.map toggleCheckHipChat) model.standUpEntries
    in
    { model | standUpEntries = updatedStandUpEntries }


toggleCheckHipChat : StandUpEntry -> StandUpEntry
toggleCheckHipChat standUpEntry =
    { standUpEntry
        | checkHipChat = not standUpEntry.checkHipChat
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

        TeamMemberNameAlreadyExists existingName ->
            { model
                | addTeamMemberError = NameAlreadyExists existingName
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

        teamMembers =
            standUpEntryList
                |> List.map .teamMember
    in
    Encode.object
        [ ( "standUpEntries", Encode.list encodeStandUpEntry standUpEntryList )
        , ( "teamMembers", Encode.list encodeTeamMember teamMembers )
        ]


encodeStandUpEntry : StandUpEntry -> Encode.Value
encodeStandUpEntry standUpEntry =
    Encode.object
        [ ( "teamMember", encodeTeamMember standUpEntry.teamMember )
        ]


encodeTeamMember : TeamMember -> Encode.Value
encodeTeamMember teamMember =
    Encode.object
        [ ( "name", Encode.string teamMember.name )
        ]


decodeFlags : UnsafeFlags -> Result Decode.Error Flags
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
