module Component.Card exposing (..)

import Component.Button as Button
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , el
        , fill
        , fillPortion
        , height
        , image
        , mouseOver
        , padding
        , paddingXY
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Tuple


type Model
    = LocalModel Schema
    | ButtonModel Button.Model


init : ( Model, Cmd Msg )
init =
    ( LocalModel { title = "Card Title", subtitle = "Card SubTitle", imageUrl = Tuple.first imageURLs }, Cmd.none )


imageURLs =
    ( "https://bit.ly/2VS0QBW", "https://bit.ly/3eMl8FA" )


type Msg
    = LocalMsg
    | ButtonMsg Button.Msg


update msg model =
    case msg of
        ButtonMsg _ ->
            model

        LocalMsg ->
            model
 

type alias Schema =
    { title : String, subtitle : String, imageUrl : String }


card : Schema -> Element Msg
card cardData =
    Element.el
        [ Background.color (rgb255 255 255 255)
        , Font.color (rgb255 0 0 0)
        , Border.color (rgb255 0 0 0)
        , Border.width 2
        , padding 10
        , width fill
        ]
        (Element.column [ spacing 10 ]
            [ headerUI { title = cardData.title, subtitle = cardData.subtitle }
            , imageUI cardData.imageUrl
            , descriptionUI loremipsum
            , Element.map ButtonMsg Button.buttonUI
            ]
        )


loremipsum =
    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged."


headerUI : { title : String, subtitle : String } -> Element msg
headerUI titleObj =
    Element.column
        [ width fill
        , Font.alignRight
        , Border.width 2
        , padding 10
        , spacing 5
        ]
        [ Element.el
            [ Font.size 25
            , Border.width 2
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
            , Border.width 2
            , width fill
            ]
            (Element.text <| titleObj.subtitle)
        ]


descriptionUI : String -> Element msg
descriptionUI content =
    Element.textColumn
        [ width fill
        , Border.width 2
        , Font.size 12
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
        ]
        [ Element.paragraph
            []
            [ Element.text <| content ]
        ]


imageUI : String -> Element msg
imageUI url =
    image [ Border.width 2, width fill ]
        { src = url, description = "Some image" }
