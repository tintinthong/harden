module Component.Button exposing (..)

import Browser as Browser
import Element exposing (Element, Option, centerX, focusStyle, layoutWith, mouseOver, padding, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    { text : String, options : ButtonOptions }


type alias ButtonOptions =
    { color : Element.Color }


type Msg
    = Clicked


init : Model
init =
    { text = "", options = { color = Element.rgb255 255 0 0 } }


update msg model =
    case msg of
        Clicked ->
            { model | text = "Button Clicked" }



-- buttonOption : Option
-- buttonOption =
--     focusStyle
--         { borderColor = Just (rgb255 255 0 0)
--         , backgroundColor = Just (rgb255 0 0 255)
--         , shadow = Nothing
--         }


buttonUI : Element Msg
buttonUI =
    Input.button
        [ mouseOver
            [ Background.color (Element.rgb255 205 235 249)
            ]
        , Font.size 16
        , padding 10
        , Border.width 2
        , Background.color (Element.rgb255 255 255 255)
        , Element.focused
            [ Background.color (Element.rgb255 69 179 231) ]
        ]
        { onPress = Just Clicked, label = text "Default Button Text" }
