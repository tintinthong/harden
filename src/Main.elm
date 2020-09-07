module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Page.Home as Home
import Tuple
import Url



---- MODEL ----
-- Should separate by pages


type Model
    = Home Home.Model



-- | CardModel Card.Model
-- { buttonModel : Button.Model }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Home (Tuple.first Home.init), Cmd.map HomeMsg (Tuple.second Home.init) )



---- UPDATE ----


type Msg
    = HomeMsg Home.Msg
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( HomeMsg submsg, Home submodel ) ->
            Home.update submsg submodel |> updateWith Home HomeMsg model
        ( UrlChanged _ , Home submodel ) ->
            (model, Cmd.none)
        ( LinkClicked _ , Home submodel ) ->
            (model, Cmd.none)


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



---- VIEW ----
-- view : Model -> Html Msg


view : Model -> Browser.Document Msg
view model =
    case model of
        Home submodel ->
            { title = "Home"
            , body =
                [Html.map HomeMsg (Home.view submodel)]
            }



-- view : (subMsg-> Msg )->Model -> Html Msg
-- view toMsg model =
--     case model of
--         Home submodel ->  Html.map toMsg (Home.view submodel)
-- Html.map HomeMsg (Home.view submodel)
-- case model of
--             Button _ -> Button.view model
-- div []
--     [ img [ src "/logo.svg" ] []
--     , h1 [] [ text "Your Elm App is working!" ]
--     ]
-- ---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- , onUrlRequest=
-- , onUrlChange =
-- ,subscriptions =
-- Browser.element
--     { view = view
--     , init = \_ -> init
--     , update = update
--     , subscriptions = always Sub.none
--     }
