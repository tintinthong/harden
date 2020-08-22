module Page.Home exposing (..)

import Component.Grid as Grid
import Component.Navbar as Navbar
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


type alias Placeholder =
    { name : String }


type Model
    = LocalModel Placeholder
    | NavbarModel Navbar.Model
    | GridModel Grid.Model


type Msg
    = LocalMsg
    | NavbarMsg Navbar.Msg
    | GridMsg Grid.Msg


init : ( Model, Cmd Msg )
init =
    -- ( NavbarModel Navbar.init, Cmd.none )
    ( GridModel (Tuple.first Grid.init), Cmd.none )


logo : Element msg
logo =
    el
        [ width <| px 80
        , height <| px 40
        , Border.width 2
        , Border.rounded 6
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        ]
        none


header : Element msg
header =
    row [ width fill, padding 20, spacing 20 ]
        [ logo
        , el [ alignRight ] <| text "Services"
        , el [ alignRight ] <| text "About"
        , el [ alignRight ] <| text "Contact"
        ]


content : Element msg
content =
    List.range 2 16
        |> List.map
            (\i ->
                el [ centerX, Font.size 64, Font.color <| rgb255 (i * 20) (i * 20) (i * 20) ] <|
                    text "Scrollable site content"
            )
        |> column [ width fill, padding 50 ]


footer : Element msg
footer =
    row
        [ width fill
        , padding 10
        , Background.color <| rgb255 0xFF 0xFC 0xF6
        , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        ]
        [ logo
        , column [ alignRight, spacing 10 ]
            [ el [ alignRight ] <| text "Services"
            , el [ alignRight ] <| text "About"
            , el [ alignRight ] <| text "Contact"
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        NavbarModel x ->
            Html.map NavbarMsg
                (layout [ width fill, height fill ] <|
                    column [ width fill ]
                        [ Navbar.navbarUI x
                        , header
                        , content
                        , footer
                        ]
                )

        LocalModel x ->
            layout [ width fill, height fill ] <|
                column [ width fill ]
                    [ header
                    , content
                    , footer
                    ]

        GridModel x ->
            Html.map GridMsg
                (layout [] <|
                    column []
                        [ header
                        , Grid.gridUI
                        , content
                        , footer
                        ]
                )
