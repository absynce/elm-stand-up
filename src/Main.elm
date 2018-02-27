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
