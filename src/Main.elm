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
    ( Home (Tuple.first Home.init), Cmd.map HomeMsg (Tuple.second Home.init) )


---- UPDATE ----


type Msg
    = HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( HomeMsg submsg, Home submodel ) ->
            Home.update submsg submodel |> updateWith Home HomeMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Home submodel ->
            Html.map HomeMsg (Home.view submodel)



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
