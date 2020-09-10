module Data.Font exposing (..)

import Element exposing (..)
import Element.Font as Font


titleAttrs =
    [ Font.size 25
    , Font.extraBold
    , Font.family
        [ Font.external
            { name = "Roboto"
            , url = "https://fonts.googleapis.com/css?family=Roboto"
            }
        , Font.sansSerif
        ]
    ]


paragraphAttrs =
    [ Font.size 12
    , Font.extraLight
    , Font.family
        [ Font.external
            { name = "Roboto"
            , url = "https://fonts.googleapis.com/css?family=Roboto"
            }
        , Font.sansSerif
        ]
    , Font.alignLeft
    ]



-- paragraphAttributes =
