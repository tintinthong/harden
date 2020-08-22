module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Page.Home as Home
import Tuple

---- MODEL ----
-- Should separate by pages

type Model
    = Home Home.Model



-- | CardModel Card.Model
-- { buttonModel : Button.Model }


init : ( Model, Cmd Msg )
init =
    ( Home (Tuple.first Home.init), Cmd.none )



---- UPDATE ----


type Msg
    = HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Home x ->
            Html.map HomeMsg (Home.view x)



-- case model of
--             Button _ -> Button.view model
-- div []
--     [ img [ src "/logo.svg" ] []
--     , h1 [] [ text "Your Elm App is working!" ]
--     ]
-- ---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
