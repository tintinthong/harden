module Page.Home exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, string)



-- type alias Model =
--     { something : String
--     }


type Model
    = Success String
    | Loading
    | Failure
    | Default


type Msg
    = GotCards (Result Http.Error String)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCards result ->
            case result of
                Ok title ->
                    ( Success title, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( Loading
    , getMovie
    )


movieDecoder : Decoder String
movieDecoder =
    field "Title" string


getMovie : Cmd Msg
getMovie =
    Http.get
        { url = "http://www.omdbapi.com/?apikey=564562be&t=Guardian"
        , expect = Http.expectJson GotCards movieDecoder
        }


 -- { url = "http://www.omdbapi.com/?apikey=564562be&s=Rick and Morty"
-- { url = "https://elm-lang.org/assets/public-opinion.txt"
-- field "Search" (field "Title_url" string)


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
        rowAttrs =
            [ Border.width 2
            , padding 10
            , width <| fill
            , spacing 10
            ]

        elAttrs =
            [ padding 5
            , width fill
            , alignLeft
            , Font.center
            ]

        makePage pageName =
            Element.el elAttrs (text <| pageName)

        makeCol colOpts pageName =
            Element.column colOpts [ makePage pageName ]

        leftCol =
            makeCol [ alignLeft ] "Logo"

        rightCols =
            List.map (\name -> makeCol [ Border.width 2, padding 10, alignRight ] name)
                [ "Home", "Stats", "Login" ]
    in
    Element.row
        rowAttrs
        (leftCol :: rightCols)


grid : Model -> Element msg
grid model =
    let
        colAttrs =
            [ padding 10
            ]

        rowAttrs =
            [ padding 10
            , spacing 10
            ]

        elAttrs =
            [ padding 5
            , Font.center
            ]

        makeCardCol c =
            Element.column colAttrs [ c ]
    in
    Element.wrappedRow
        rowAttrs
        (List.repeat 12 (makeCardCol <| viewCard model))


type alias Card =
    { title : String, subtitle : String, imageUrl : String }


exampleViewCard : Element msg
exampleViewCard =
    card { title = "Card Name", subtitle = "Subtitle Name", imageUrl = "https://bit.ly/2VS0QBW" }


viewCard : Model -> Element msg
viewCard model =
    case model of
        Failure ->
            card2 { title = "not working", subtitle = "shit", imageUrl = "https://bit.ly/2VS0QBW" }

        Loading ->
            card2 { title = "not working", subtitle = "shit", imageUrl = "https://bit.ly/2VS0QBW" }

        Default ->
            card2 { title = "not working", subtitle = "shit", imageUrl = "https://bit.ly/2VS0QBW" }

        Success x ->
            card2 { title = x, subtitle = "shit", imageUrl = "https://bit.ly/2VS0QBW" }


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


card2 : Card -> Element msg
card2 cardData =
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
        column [ width fill, centerX ]
            [ navbar
            , grid model
            ]
