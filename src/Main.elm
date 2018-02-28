module Main exposing (..)

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
    { standUpEntries : List StandUpEntry }


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
    { standUpEntries =
        List.map initStandUp teamMembers
    }


initStandUp teamMember =
    { name = teamMember.name
    , completed = False
    }



-- View


view model =
    div [ class "stand-up-meeting" ]
        [ h2 [] [ text "Stand-up meeting" ]
        , ul [ class "stand-up-entries" ]
            (List.map
                (\standUpEntry -> li [] [ text standUpEntry.name ])
                model.standUpEntries
            )
        ]



-- Update


update msg model =
    model
