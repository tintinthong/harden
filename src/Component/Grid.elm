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


-- List of Cards


gridUI : Element msg
gridUI =
    let
        colAttrs =
            [ Border.width 2
            , Element.padding 10
            , Element.alignRight
            ]

        rowAttrs =
            [ Border.width 2
            , Element.padding 10
            , Element.width Element.fill
            , Element.spacing 10
            ]

        elAttrs =
            [ Border.width 2
            , Element.padding 5
            , Element.width Element.fill
            , Element.alignLeft
            , Font.center
            ]

        makePage pageName =
            Element.el elAttrs (Element.text <| pageName)

        makeCol colOpts pageName =
            Element.column colOpts [ makePage pageName ]

        makeCardCol card =
            Element.column colAttrs card

        -- leftCol =
        --     makeCol [ Element.alignLeft ] "Logo"
        -- rightCols =
        --     List.map (\name -> makeCol colAttrs name)
        --         pageNames
    in
    Element.row
        rowAttrs
        [ makeCol colAttrs "jakodsf" ]
