module Page.Home exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    { something : String
    }


type Msg
    = Default


init : ( Model, Cmd Msg )
init =
    ( { something = "hi" }, Cmd.none )


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


navbar : Element msg
navbar =
    let
        colAttrs =
            [ Border.width 2
            , Element.padding 10
            , Element.alignRight
            ]

        rowAttrs =
            [ Border.width 2
            , Element.padding 10
            , Element.width Element.fill
            , Element.spacing 10
            ]

        elAttrs =
            [ Element.padding 5
            , Element.width Element.fill
            , Element.alignLeft
            , Font.center
            ]

        makePage pageName =
            Element.el elAttrs (Element.text <| pageName)

        makeCol colOpts pageName =
            Element.column colOpts [ makePage pageName ]

        leftCol =
            makeCol [ Element.alignLeft ] "Logo"

        rightCols =
            List.map (\name -> makeCol colAttrs name)
                [ "Home", "Stats", "Login" ]
    in
    Element.row
        rowAttrs
        (leftCol :: rightCols)


grid : Element msg
grid =
    let
        colAttrs =
            [ Element.padding 10
            ]

        rowAttrs =
            [ Element.padding 10
            , Element.spacing 10
            ]

        elAttrs =
            [ Element.padding 5
            , Font.center
            ]

        makeCardCol c =
            Element.column colAttrs [ c ]
    in
    Element.wrappedRow
        rowAttrs
        (List.repeat 12 (makeCardCol exampleCardData))


type alias Card =
    { title : String, subtitle : String, imageUrl : String }


exampleCardData =
    card { title = "Card Name", subtitle = "Subtitle Name", imageUrl = "https://bit.ly/2VS0QBW" }


card : Card -> Element msg
card cardData =
    Element.el
        [ Background.color (rgb255 255 255 255)
        , Font.color (rgb255 0 0 0)
        , Border.color (rgb255 0 0 0)
        , Border.width 2
        , padding 10
        ]
        (Element.column []
            [ cardHeader { title = cardData.title, subtitle = cardData.subtitle }
            , cardImage cardData.imageUrl
            , cardDescription loremipsum
            , button
            ]
        )


cardHeader : { title : String, subtitle : String } -> Element msg
cardHeader titleObj =
    Element.column
        [ Font.alignRight
        , padding 10
        , spacing 5
        ]
        [ Element.el
            [ Font.size 25
            , Font.extraBold
            , Font.family
                [ Font.external
                    { name = "Roboto"
                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                    }
                , Font.sansSerif
                ]
            ]
            (Element.text <| titleObj.title ++ " - Title")
        , Element.el
            [ Font.size 15
            , Font.extraLight
            ]
            (Element.text <| titleObj.subtitle)
        ]


cardDescription : String -> Element msg
cardDescription content =
    Element.textColumn
        [ Font.size 12
        , Font.extraLight
        , Font.family
            [ Font.external
                { name = "Roboto"
                , url = "https://fonts.googleapis.com/css?family=Roboto"
                }
            , Font.sansSerif
            ]
        , Font.alignLeft
        , spacing 10
        , padding 10
        , width (fill |> maximum 300)
        ]
        [ Element.paragraph
            []
            [ Element.text <| content ]
        ]


cardImage : String -> Element msg
cardImage url =
    image [ Element.width (Element.px 300) ]
        { src = url, description = "Some image" }


button : Element msg
button =
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
        { onPress = Nothing, label = text "Click Here" }


loremipsum =
    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged."


view model =
    layout [ width fill, height fill ] <|
        column [ width fill ]
            [
              navbar
            , grid

            ]
