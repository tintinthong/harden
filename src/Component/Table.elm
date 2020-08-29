module Component.Table exposing (..)

import Element exposing (..)
import Element.Border as Border


tableUI =
    table
        [ width (fill |> maximum 300)
        , centerX
        ]
        { data = persons
        , columns =
            [ { header = text "First Name"
              , width = fill
              , view =
                    \person ->
                        text person.firstName
              }
            , { header = text "Last Name"
              , width = fill
              , view =
                    \person ->
                        text person.lastName
              }
            , { header = text "Points"
              , width = fill
              , view =
                    \person ->
                        text person.lastName
              }
            ]
        }


type alias Person =
    { firstName : String
    , lastName : String
    , ppg : Int
    }


persons =
    [ { firstName = "David"
      , lastName = "Bowie"
      , ppg = 10
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      , ppg = 10
      }
    ]
