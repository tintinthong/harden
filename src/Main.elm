module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json.Decode as Decode exposing (Value)
import Page.Home as Home
import Page.Login as Login
import Route exposing (Route)
import Session exposing (Session)
import Tuple
import Url exposing (Url)
import Viewer exposing (Viewer)



---- MODEL ----
-- Should separate by pages


type Model
    = Home Home.Model
    | Login Login.Model
    | Redirect Session
    | NotFound Session



-- { key : Nav.Key
-- , url : Url.Url
-- , page : PageModel
-- }
-- type PageModel
--     = Home Home.Model
-- | CardModel Card.Model
-- { buttonModel : Button.Model }
-- init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
-- init flags url key =
--     ( Home (Tuple.first Home.init), Cmd.map HomeMsg (Tuple.second Home.init) )
-- init flags url key


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.fromViewer navKey maybeViewer))


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
        _ = Debug.log "changeRouteTo: session" session
    in
    case maybeRoute of
        -- Just Route.Logout ->
        --     ( model, Api.logout )
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home HomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login LoginMsg model



---- UPDATE ----


type Msg
    = HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- case Debug.log "msg, model" (msg,model) of
    case ( msg, model ) of
        ( HomeMsg submsg, Home submodel ) ->
            Home.update submsg submodel |> updateWith Home HomeMsg model

        ( LoginMsg submsg, Login submodel ) ->
            Login.update submsg submodel |> updateWith Login LoginMsg model

        ( UrlChanged _, Home submodel ) ->
            ( model, Cmd.none )

        ( LinkClicked _, Home submodel ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- don't exactly like this but its for msges from the wrong page
-- Just Route.Register ->
--     Register.init session
--         |> updateWith Register GotRegisterMsg model
-- Just (Route.Profile username) ->
--     Profile.init session username
--         |> updateWith (Profile username) GotProfileMsg model


toSession : Model -> Session
toSession page =
    case page of
        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        NotFound session ->
            session

        Redirect session ->
            session



-- Redirect session ->
--     session
-- NotFound session ->
--     session
-- Settings settings ->
--     Settings.toSession settings
-- Register register ->
--     Register.toSession register
-- Profile _ profile ->
--     Profile.toSession profile
-- Article article ->
--     Article.toSession article
-- Editor _ editor ->
--     Editor.toSession editor
-- case urlRequest of
--     Browser.Internal url -> (model, Nav.pushUrl model.key (Url.toString url))
--     Browser.External url -> (model, Nav.pushUrl model.key (Url.toString url))
-- updateWith2 : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
-- updateWith2 toModel toMsg model ( subModel, subCmd ) =
--     ( toModel subModel
--     , Cmd.map toMsg subCmd
--     )


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
                [ Html.map HomeMsg (Home.view submodel) ]
            }

        Login submodel ->
            { title = "Login"
            , body =
                [ Html.map LoginMsg (Login.view submodel) ]
            }

        Redirect _ ->
            { title = "Redirect"
            , body =
                [ Html.text "redirecting" ]
            }

        NotFound _ ->
            { title = "NotFound"
            , body =
                [ Html.text "notfounding" ]
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



-- main : Program () Model Msg
-- main =
--     Browser.application
--         { init = init
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         , onUrlRequest = LinkClicked
--         , onUrlChange = UrlChanged
--         }


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
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
