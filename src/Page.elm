module Page exposing (Page(..), viewNew, pageToString, view, viewErrors)

import Api exposing (Cred)
import Avatar
import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, a, button, div, footer, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Username exposing (Username)
import Viewer exposing (Viewer)


type Page
    = Home
    | Login


viewNew : Maybe Viewer -> Page -> Element msg
viewNew maybeViewer page =
    Element.column []
        [ header
        , menu maybeViewer [ Home, Login ]
        , footerNew
        ]


view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
view maybeViewer page { title, content } =
    { title = title ++ " - Conduit"
    , body = viewHeader page maybeViewer :: content :: [ viewFooter ]
    }


viewHeader : Page -> Maybe Viewer -> Html msg
viewHeader page maybeViewer =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ Html.text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                navbarLink page Route.Home [ Html.text "Home" ]
                    :: viewMenu page maybeViewer
            ]
        ]


pageToString : Page -> String
pageToString page =
    case page of
        Home ->
            "Home"

        Login ->
            "Home"



-- Why are there page and route


pageToRoute : Page -> Route
pageToRoute page =
    case page of
        Home ->
            Route.Home

        Login ->
            Route.Login


header : Element msg
header =
    Element.column [] [ Element.text "Header" ]


menu : Maybe Viewer -> List Page -> Element msg
menu maybeViewer pages =
    let
        makeLink pageName =
            Element.el
                [ padding 5
                , width fill
                , alignLeft
                , Font.center
                ]
                (Element.link [] { label = Element.text pageName, url = Route.routeToString Route.Home })

        makeColumn colAttrs link =
            Element.column colAttrs [ link ]

        pageButtons =
            pages
                |> List.map pageToString
                |> List.map makeLink
                |> List.map (makeColumn [])
    in
    Element.row
        [ Border.width 2
        , padding 10
        , width fill
        , spacing 10
        ]
        []


footerNew : Element msg
footerNew =
    row
        [ width fill
        , padding 10
        , Background.color <| rgb255 0xFF 0xFC 0xF6
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        ]
        [ column [ alignRight, spacing 10 ]
            [ el [ alignRight ] <| Element.text "Services"
            , el [ alignRight ] <| Element.text "About"
            , el [ alignRight ] <| Element.text "Contact"
            ]
        ]


viewMenu : Page -> Maybe Viewer -> List (Html msg)
viewMenu page maybeViewer =
    let
        linkTo =
            navbarLink page
    in
    case maybeViewer of
        Just viewer ->
            let
                username =
                    Viewer.username viewer

                avatar =
                    Viewer.avatar viewer
            in
            [ -- linkTo Route.NewArticle [ i [ class "ion-compose" ] [], text "\u{00A0}New Post" ]
              -- , linkTo Route.Settings [ i [ class "ion-gear-a" ] [], text "\u{00A0}Settings" ]
              -- , linkTo
              --     (Route.Profile username)
              --     [ img [ class "user-pic", Avatar.src avatar ] []
              --     , Username.toHtml username
              --     ]
              linkTo Route.Home [ Html.text "Go Home" ]
            , linkTo Route.Login [ Html.text "Login" ]
            ]

        Nothing ->
            [ linkTo Route.Login [ Html.text "Sign in" ]

            -- , linkTo Route.Register [ text "Sign up" ]
            ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ Html.text "conduit" ]
            , span [ class "attribution" ]
                [ Html.text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ Html.text "Thinkster" ]
                , Html.text ". Code & design licensed under MIT."
                ]
            ]
        ]


navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        -- ( Register, Route.Register ) ->
        --     True
        -- ( Settings, Route.Settings ) ->
        --     True
        -- ( Profile pageUsername, Route.Profile routeUsername ) ->
        --     pageUsername == routeUsername
        -- ( NewArticle, Route.NewArticle ) ->
        --     True
        _ ->
            False


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ Html.text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ Html.text "Ok" ] ]
