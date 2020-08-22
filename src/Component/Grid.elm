module Component.Grid exposing (..)

import Component.Button as Button
import Component.Card as Card
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , el
        , fill
        , fillPortion
        , height
        , image
        , mouseOver
        , padding
        , paddingXY
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Tuple


type Model
    = LocalModel
    | CardModel Card.Model


init : ( Model, Cmd Msg )
init =
    ( CardModel (Tuple.first Card.init), Cmd.none )


type Msg
    = LocalMsg
    | CardMsg Card.Msg


card =
    Element.map CardMsg (Card.cardUI { title = "Card Name", subtitle = "Subtitle Name", imageUrl = "https://bit.ly/2VS0QBW" })


gridUI : Element Msg
gridUI =
    let
        colAttrs =
            [ Border.width 2
            , Element.padding 10
            ]

        rowAttrs =
            [ Border.width 2
            , Element.padding 10
            , Element.spacing 10
            ]

        elAttrs =
            [ Border.width 2
            , Element.padding 5
            , Font.center
            ]

        makeCardCol c =
            Element.column colAttrs [ c ]
        exampleCard = makeCardCol card
    in
    Element.wrappedRow
        rowAttrs
        (List.repeat 12 exampleCard)
