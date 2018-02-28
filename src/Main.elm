module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes


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
            (model.standUpEntries
                |> Dict.values
                |> List.map
                    (\standUpEntry -> li [] [ text standUpEntry.name ])
            )
        ]



-- Update


update msg model =
    model
