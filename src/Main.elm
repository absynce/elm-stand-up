module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Json.Encode


main : Program UnsafeFlags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { standUpEntries : Dict String StandUpEntry }


type alias UnsafeFlags =
    Json.Encode.Value


type alias StandUpEntry =
    { name : String
    , completed : Bool
    }


type alias TeamMember =
    { name : String }


{-| `teamMembers` might come from an HTTP request in the real world.
-}
teamMembers : List TeamMember
teamMembers =
    [ TeamMember "Jesse"
    , TeamMember "Skelly"
    , TeamMember "Nanette"
    , TeamMember "Michael"
    , TeamMember "Alex"
    , TeamMember "Carrie"
    , TeamMember "Jared"
    , TeamMember "Tanner"
    , TeamMember "Matth"
    , TeamMember "Heather"
    ]


init : UnsafeFlags -> ( Model, Cmd Msg )
init unsafeFlags =
    let
        teamMembersKeyedTuple =
            List.map (\teamMember -> ( teamMember.name, teamMember ))
                teamMembers

        teamMembersDict =
            Dict.fromList teamMembersKeyedTuple
    in
        { standUpEntries =
            Dict.map initStandUp teamMembersDict
        }
            ! []


initStandUp : String -> TeamMember -> StandUpEntry
initStandUp name teamMember =
    { name = teamMember.name
    , completed = False
    }



-- View


view : Model -> Html Msg
view model =
    div [ class "stand-up-meeting" ]
        [ h2 [] [ text "Stand-up meeting" ]
        , Keyed.ul [ class "stand-up-entries" ]
            (viewStandUpEntries model.standUpEntries)
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
    ( standUpEntry.name, viewStandUpEntry standUpEntry )


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
                , onClick (ToggleEntryCompleted standUpEntry.name)
                ]
                []
            , text standUpEntry.name
            ]



-- Update


type Msg
    = ToggleEntryCompleted String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleEntryCompleted name ->
            toggleEntryCompleted model name ! []


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
