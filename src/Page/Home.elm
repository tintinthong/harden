module Page.Home exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, at, field, float, list, map3, string)
import String as String


type Request
    = Success (List Card)
    | Loading
    | Failure
    | Default


type alias Model =
    { searchString : String
    , request : Request
    }


type Msg
    = SendHttpRequest
    | GotCards (Result Http.Error (List Card))
    | SearchChanged String


movieDecoder : Decoder Card
movieDecoder =
    map3 Card
        (at [ "Title" ] string)
        --title
        (at [ "Type" ] string)
        --subtitle
        (at [ "Poster" ] string)



-- type alias Search =
--     { title : String
--     , description : String
--     , imageUrl : String
--     }


moviesDecoder : Decoder (List Card)
moviesDecoder =
    field "Search" <| list movieDecoder


type alias Card =
    { title : String, subtitle : String, imageUrl : String }



-- getMovie : Cmd Msg
-- getMovie =
--     Http.get
--         { url = "https://www.omdbapi.com/?apikey=564562be&t=Guardian"
--         , expect = Http.expectJson GotCards movieDecoder
--         }
-- { url = "http://www.omdbapi.com/?apikey=564562be&s=Rick and Morty"
-- { url = "https://elm-lang.org/assets/public-opinion.txt"
-- field "Search" (field "Title_url" string)


getMovies : String -> Cmd Msg
getMovies searchString =
    Http.get
        { url = "https://www.omdbapi.com/?apikey=564562be&s=" ++ searchString
        , expect = Http.expectJson GotCards moviesDecoder
        }


init : ( Model, Cmd Msg )
init =
    ( { searchString = ""
      , request = Loading
      }
    , getMovies "Rick"
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | request = Loading }, getMovies model.searchString )

        SearchChanged a ->
            ( { model | searchString = a }, Cmd.none )

        GotCards result ->
            case result of
                Ok cards ->
                    ( { model | request = Success cards }, Cmd.none )

                Err _ ->
                    ( { model | request = Failure }, Cmd.none )


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


navbar : Model -> Element Msg
navbar model =
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

        -- Element.msg
        makePage pageName =
            Element.el elAttrs (text <| pageName)

        makeCol colOpts pageName =
            Element.column colOpts [ makePage pageName ]

        leftCol =
            makeCol [ alignLeft ] "Logo"

        handler newSearchString =
            SearchChanged newSearchString

        { searchString, request } =
            model

        errorMessage theRequest =
            case theRequest of
                Default ->
                    Element.text ""

                Loading ->
                    Element.text ""

                Failure ->
                    Element.text "You suck"

                Success cards ->
                    Element.text ""

        centerCol =
            Element.row []
                [ Input.text []
                    { label = Input.labelAbove [] (Element.el [] (text <| "Type in name of movie"))
                    , onChange = handler
                    , text = "Rick"
                    , placeholder = Nothing
                    }
                , buttonSearch
                , errorMessage request
                ]

        rightCols =
            List.map (\name -> makeCol [ Border.width 2, padding 10, alignRight ] name)
                [ "Home", "Stats", "Login" ]
    in
    Element.row
        rowAttrs
        (leftCol :: centerCol :: rightCols)


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

        { searchString, request } =
            model
    in
    case request of
        Default ->
            Element.wrappedRow rowAttrs []

        Failure ->
            Element.wrappedRow rowAttrs []

        Loading ->
            Element.wrappedRow rowAttrs []

        Success cards ->
            Element.wrappedRow rowAttrs (List.map (makeCardCol << card) cards)


card : Card -> Element msg
card cardData =
    Element.el
        [ Background.color (rgb255 255 255 255)
        , Font.color (rgb255 0 0 0)
        , Border.color (rgb255 0 0 0)
        , Border.width 2
        , padding 10
        ]
        (Element.column
            [ width (fill |> maximum 200)
            ]
            [ cardHeader { title = cardData.title, subtitle = cardData.subtitle }
            , cardImage cardData.imageUrl
            , cardDescription loremipsum
            , Element.row
                []
                [ buttonSeeMore
                , buttonDelete
                ]
            ]
        )


cardHeader : { title : String, subtitle : String } -> Element msg
cardHeader titleObj =
    Element.column
        [ Font.alignRight
        , padding 10
        , spacing 5

        -- , width (fill |> maximum 300)
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
            (Element.text <| titleObj.title)
        , Element.el
            [ Font.size 15
            , Font.extraLight
            ]
            (Element.text <| String.right 100 <| titleObj.subtitle)
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

        -- , width (fill |> maximum 300)
        ]
        [ Element.paragraph
            []
            [ Element.text <| content ]
        ]


cardImage : String -> Element msg
cardImage url =
    image
        [ width (fill |> maximum 200)
        ]
        { src = url, description = "Some image" }


buttonSearch : Element Msg
buttonSearch =
    Input.button
        [ mouseOver
            [ Background.color hoverBlue
            ]
        , Font.size 16
        , padding 10
        , Border.width 2
        , Background.color white
        , Element.focused
            [ Background.color focusedBlue ]
        ]
        { onPress = Just SendHttpRequest, label = text "Search" }


buttonSeeMore : Element msg
buttonSeeMore =
    Input.button
        [ mouseOver
            [ Background.color hoverBlue
            ]
        , Font.size 16
        , padding 10
        , Border.width 2
        , Background.color white
        , Element.focused
            [ Background.color focusedBlue ]
        ]
        { onPress = Nothing, label = text "See More" }


white =
    Element.rgb255 255 255 255


red =
    Element.rgb255 255 0 0


deleteRed =
    Element.rgb255 223 71 89


focusedBlue =
    Element.rgb255 69 179 231


hoverBlue =
    Element.rgb255 205 235 249


dangerRed =
    Element.rgb255 223 71 89


buttonDelete : Element msg
buttonDelete =
    Input.button
        [ Font.color white
        , Font.size 16
        , padding 10
        , Border.width 2
        , Background.color dangerRed
        , mouseOver
            [ Background.color hoverBlue
            ]
        , Element.focused
            [ Background.color (Element.rgb255 69 179 231) ]
        ]
        { onPress = Nothing, label = text "Delete" }


loremipsum =
    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged."


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ] <|
        column [ width fill, centerX ]
            [ navbar model
            , grid model
            ]
