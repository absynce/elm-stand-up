module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onClick)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- Model


type alias Model =
    { standUpEntries : Dict String StandUpEntry }


type alias StandUpEntry =
    { name : String
    , completed : Bool
    }


type alias TeamMember =
    { name : String }


{-| `teamMembers` might come from an HTTP request in the real world.
-}
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


model =
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


initStandUp : String -> TeamMember -> StandUpEntry
initStandUp name teamMember =
    { name = teamMember.name
    , completed = False
    }



-- View


view model =
    div [ class "stand-up-meeting" ]
        [ h2 [] [ text "Stand-up meeting" ]
        , ul [ class "stand-up-entries" ]
            (viewStandUpEntries model.standUpEntries)
        ]


viewStandUpEntries standUpEntries =
    standUpEntries
        |> Dict.values
        |> List.sortWith completedComparison
        |> List.map viewStandUpEntry


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleEntryCompleted name ->
            toggleEntryCompleted model name


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
