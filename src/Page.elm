module Page exposing (Page(..), pageToString, view, viewErrors)

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
    | Register



-- Util


pageToString : Page -> String
pageToString page =
    case page of
        Home ->
            "Home"

        Login ->
            "Login"

        Register ->
            "Register"


pageToRoute : Page -> Route
pageToRoute page =
    case page of
        Home ->
            Route.Home

        Login ->
            Route.Login

        Register ->
            Route.Register


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        ( Register, Route.Register ) ->
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



-- View


view : Maybe Viewer -> Page -> Element msg -> Element msg
view maybeViewer page pageContent =
    let
        pagesIfLoggedIn =
            [ Home, Login ]

        pagesIfNotLoggedIn =
            [ Home, Login, Register ]
    in
    case maybeViewer of
        Just viewer ->
            let
                username =
                    Viewer.username viewer
            in
            Element.column
                [ height fill
                , width fill
                ]
                [ header
                , menu maybeViewer pagesIfLoggedIn
                , pageContent
                , footer pagesIfLoggedIn
                ]

        Nothing ->
            Element.column
                [ height fill
                , width fill
                ]
                [ header
                , menu maybeViewer pagesIfNotLoggedIn
                , pageContent
                , footer pagesIfNotLoggedIn
                ]


header : Element msg
header =
    Element.column [] [ Element.text "" ]


makeLink : Page -> Element msg
makeLink page =
    Element.el
        [ padding 5
        , width fill
        , alignLeft
        , Font.center
        ]
        (Element.link [] { label = Element.text <| pageToString page, url = Route.routeToString <| pageToRoute page })


menu : Maybe Viewer -> List Page -> Element msg
menu maybeViewer pages =
    let
        pagesIfLoggedIn =
            [ Home, Login ]

        pagesIfNotLoggedIn =
            [ Home, Login, Register ]

        makeColumn colAttrs link =
            column colAttrs [ link ]

        pageLinks =
            pages
                |> List.map makeLink
                |> List.map (makeColumn [alignLeft])
    in
    case maybeViewer of
        Just viewer ->
            let
                username =
                    Viewer.username viewer

                welcomeMessage =
                    "Hi," ++ Username.toString username 

                welcomeSection =
                    column [alignRight] [ Element.text <| welcomeMessage ]
            in
            row
                [ Border.width 2
                , padding 10
                , width fill
                , spacing 10
                ]
                (pageLinks ++ [welcomeSection])

        Nothing ->
            row
                [ Border.width 2
                , padding 10
                , width fill
                , spacing 10
                ]
                (pageLinks )


footer : List Page -> Element msg
footer pages =
    let
        makeColumn colAttrs link =
            Element.column colAttrs [ link ]

        footerPageButtons =
            pages
                |> List.map makeLink
                |> List.map (makeColumn [ centerX ])

        -- maybe put centerX here
    in
    row
        [ width fill
        , padding 10
        , Border.width 2
        , alignBottom
        ]
        footerPageButtons


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
