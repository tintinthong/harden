module Page.Login exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Encode


type alias Model =
    { form : Form
    }


type alias Form =
    { email : String
    , password : String
    }


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String


blue =
    rgb255 238 238 238


purple =
    rgb255 220 238 238


button =
    Input.button
        [ Background.color blue
        , Element.focused
            [ Background.color purple ]
        ]
        { onPress = Just SubmittedForm
        , label = text "My Button"
        }

viewLogin : Model -> Html Msg
viewLogin model =
    layout [ width fill, height fill ] <|
        column [ width fill, centerX ]
            [ button
            ]
