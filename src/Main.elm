module Main exposing (main)

import Api
import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Html exposing (Html, div, h1, img, text)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Content as Content
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Route exposing (Route)
import Session exposing (Session)
import Tuple
import Url exposing (Url)
import Viewer exposing (Viewer)
 -- import Http exposing (Https)


type Model
    = Home Home.Model
    | Login Login.Model
    | Register Register.Model
    | Content Content.Model
    | Redirect Session
    | NotFound Session


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let
        _ =
            Debug.log "Main:url" url

        _ =
            Debug.log "Main:navKey" navKey

        _ =
            Debug.log "Main:navKey" maybeViewer
    in
    changeRouteTo (Route.fromUrl url) (Redirect (Session.fromViewer navKey maybeViewer))


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model

        _ =
            Debug.log "changeRouteTo: session" session
    in
    case Debug.log "changeRouteZTo: maybeRoute" maybeRoute of
        -- Just Route.Logout ->
        --     ( model, Api.logout )
        Just Route.Register ->
            Register.init session
                |> updateWith Register RegisterMsg model

        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home HomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login LoginMsg model

        Just Route.Content ->
            Content.init session
                |> updateWith Content ContentMsg model


type Msg
    = HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | ContentMsg Content.Msg
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotSession Session


-- contentUrl = { fragment = Just "/content", host = "localhost", path = "/", port_ = Just 3000, protocol = Http, query = Nothing }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg, model" ( msg, model ) of
        -- case ( msg, model ) of
        ( HomeMsg submsg, Home submodel ) ->
            -- let
            --     pair =
                    Home.update submsg submodel |> updateWith Home HomeMsg model
            
                -- _ ->
                    -- pair

        ( RegisterMsg submsg, Register submodel ) ->
            Register.update submsg submodel |> updateWith Register RegisterMsg model

        ( LoginMsg submsg, Login submodel ) ->
            Login.update submsg submodel |> updateWith Login LoginMsg model

        ( UrlChanged url, _ ) ->
            let
                _ = Debug.log "hi" url
            in
            changeRouteTo (Route.fromUrl url) model

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( GotSession session, Redirect _ ) ->
            let
                _ =
                    Debug.log "update: Got Session " session
            in
            ( Redirect session, Route.replaceUrl (Session.navKey session) Route.Home )

        ( _, _ ) ->
            -- this may just be a mistake compiler doesn't tell you anything
            ( model, Cmd.none )


toSession : Model -> Session
toSession page =
    case page of
        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        Register register ->
            Register.toSession register

        Content content ->
            Content.toSession content

        NotFound session ->
            session

        Redirect session ->
            session


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


triggerParentMsg : Cmd Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
triggerParentMsg msg ( model, oldmsg ) =
    ( model, Cmd.batch [ msg, oldmsg ] )


view : Model -> Browser.Document Msg
view model =
    let
        viewer : Maybe Viewer
        viewer =
            Session.viewer <| toSession model

        -- viewPage : Page-> (subMsg -> Msg) -> Element Msg -> Browser.Document Msg
        viewPage page toMsg contentView =
            { title = Page.pageToString page
            , body =
                Page.view viewer page contentView
                    |> Element.map toMsg
                    |> Element.layout []
                    |> List.singleton
            }
    in
    case model of
        Home submodel ->
            viewPage Page.Home HomeMsg (Element.html (Home.view submodel))

        Login submodel ->
            viewPage Page.Login LoginMsg (Element.html (Login.view submodel))

        Register submodel ->
            viewPage Page.Register RegisterMsg (Element.html (Register.view submodel))

        Content submodel ->
            viewPage Page.Content ContentMsg (Element.html (Content.view submodel))

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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            let
                _ =
                    Debug.log "subscriptions: redirect"
            in
            Session.changes GotSession (Session.navKey (toSession model))

        Home home ->
            Sub.map HomeMsg (Home.subscriptions home)

        Login loginModel ->
            Sub.map LoginMsg (Login.subscriptions loginModel)

        Register registerModel ->
            Sub.map RegisterMsg (Register.subscriptions registerModel)

        Content contentModel ->
            Sub.map ContentMsg (Content.subscriptions contentModel)


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
