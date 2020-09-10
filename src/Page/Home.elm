module Page.Home exposing (..)

import Data.Color exposing (dangerRed, deleteRed, focusedBlue, hoverBlue, red, white)
import Data.Font exposing (paragraphAttrs, titleAttrs)
import Data.Size exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, Error, at, bool, decodeString, field, float, list, map2, map3, string)
import Paginate
import Session exposing (Session)
import String


type Request
    = Success (List Card)
    | Loading
    | Failure
    | Default


type alias Model =
    { session : Session
    , searchString : String
    , request : Request
    , errorMessage : Maybe String
    , paginatedList : Paginate.PaginatedList Card
    }


toSession : Model -> Session
toSession model =
    model.session


type Msg
    = SendHttpRequest
    | GotCards (Result Http.Error (List Card))
    | SearchChanged String
    | UsePaginate PaginateAction


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


init : Session -> ( Model, Cmd Msg )
init session =
    let
        _ =
            Debug.log "Session" session
    in
    ( { session = session
      , searchString = ""
      , request = Loading
      , errorMessage = Nothing
      , paginatedList = Paginate.fromList 10 [] --initialise with empty cards
      }
    , getMovies "Rick"
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


paginateSize : Int
paginateSize =
    10


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        SendHttpRequest ->
            ( { model | request = Loading }, getMovies model.searchString )

        SearchChanged a ->
            ( { model | searchString = a }, Cmd.none )

        GotCards result ->
            case result of
                Ok cards ->
                    ( { model | paginatedList = Paginate.fromList paginateSize cards, request = Success cards }, Cmd.none )

                Err error ->
                    ( { model | request = Failure, errorMessage = Just (errorToString error) }, Cmd.none )

        UsePaginate paginateAction ->
            case paginateAction of
                Next ->
                    ( { model | paginatedList = Paginate.next model.paginatedList }, Cmd.none )

                Prev ->
                    ( { model | paginatedList = Paginate.prev model.paginatedList }, Cmd.none )



-- NOt to be confused with the one in JSON.decode


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown Error"

        Http.BadBody errorMessage ->
            errorMessage



-- Result.withDefault "There was an error but we had problems parsing" (decodeString apiErrorDecoder errorMessage)
-- Result.withDefault "There was an error but we had problems parsing" (decodeString errorDecoder errorMessage)
--JSONString -> Single Field String


errorDecoder : Decoder String
errorDecoder =
    field "Error" string


apiErrorDecoder : Decoder ApiError
apiErrorDecoder =
    map2 ApiError
        (field "Response" bool)
        (field "Error" string)


type alias ApiError =
    { response : Bool
    , error : String
    }


logo : Element msg
logo =
    el
        [ Border.width 2
        , Border.rounded 6
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        ]
        none


searchbar : Element Msg
searchbar =
    let
        rowAttrs =
            [ Border.width 2
            , padding 10
            , width fill
            , spacing 10
            ]

        elAttrs =
            [ padding 5
            , width fill
            , Font.center
            ]

        handler newSearchString =
            SearchChanged newSearchString

        centerCol =
            Element.row [ centerX, spacing 10 , Border.width 2]
                [ Element.el [centerX]
                    (Input.text [
                         ]
                        { label = Input.labelAbove [] (Element.el [] (text <| "Enter a show"))
                        , onChange = handler
                        , text = "Rick"
                        , placeholder = Nothing
                        }
                    )
                , Element.el [ Border.width 2, alignBottom ] buttonSearch
                ]
    in
    centerCol


buttonSearch : Element Msg
buttonSearch =
    Input.button
        [ mouseOver
            [ Background.color hoverBlue
            ]
        , Font.size 16
        , padding 10
        , Background.color white
        , Element.focused
            [ Background.color focusedBlue ]
        , centerY
        ]
        { onPress = Just SendHttpRequest, label = text "Find" }



--         Success cards ->
--             Element.text ""


grid : Model -> Element Msg
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
    case model.request of
        Default ->
            Element.wrappedRow rowAttrs []

        Failure ->
            Element.wrappedRow rowAttrs []

        Loading ->
            Element.wrappedRow rowAttrs []

        Success cards ->
            Element.column
                []
                [ Element.row [ alignRight ]
                    [ paginateButton Next
                    , paginateButton Prev
                    ]
                , Element.wrappedRow rowAttrs (List.map (makeCardCol << cardView) <| Paginate.page model.paginatedList)
                ]



-- grid : Request -> Element msg
-- grid request =
--     let
--         colAttrs =
--             [ padding 10
--             ]
--         rowAttrs =
--             [ padding 10
--             , spacing 10
--             ]
--         elAttrs =
--             [ padding 5
--             , Font.center
--             ]
--         makeCardCol c =
--             Element.column colAttrs [ c ]
--     in
--     case request of
--         Default ->
--             Element.wrappedRow rowAttrs []
--         Failure ->
--             Element.wrappedRow rowAttrs []
--         Loading ->
--             Element.wrappedRow rowAttrs []
--         Success cards ->
--             Element.wrappedRow rowAttrs (List.map (makeCardCol << cardView) cards)


cardView : Card -> Element msg
cardView cardData =
    Element.column
        [ -- width (fill |> maximum 300)
          -- maxSizeOfCard
              spacing 10
        ,someHeight
        , someWidth
        , Background.color (rgb255 255 255 255)
        , Font.color (rgb255 0 0 0)
        , Border.color (rgb255 0 0 0)
        , Border.width 2
        ]
        [ cardHeader { title = cardData.title, subtitle = cardData.subtitle }
        , Element.textColumn
            [ Border.width 2
            , width fill
                , height <| fillPortion 8
            ]
            [ cardImage cardData.imageUrl
            , cardDescription loremipsum
            ]

        -- Row of buttons
        , Element.row
            [ alignRight, alignBottom ,height <| fillPortion 3 ]
            [ buttonSeeMore

            -- , buttonDelete
            ]
        ]


cutText : Int -> String -> String
cutText maxChar textToCut =
    String.left maxChar textToCut ++ ".."


cardHeader : { title : String, subtitle : String } -> Element msg
cardHeader titleObj =
    Element.column
        [ centerX, Border.width 2 , cardTitleHeight, height <| fillPortion 2]
        [ Element.paragraph
            (titleAttrs ++ [ cardTitleMaxWidth])
            [ Element.text <| titleObj.title ]
        , Element.el
            [ Font.size 15
            , Font.extraLight
            ]
            (Element.text <| String.right 100 <| titleObj.subtitle)
        ]


cardDescription : String -> Element msg
cardDescription textContent =
    let
        parsedText =
            cutText 300 textContent
    in
    Element.paragraph
        (paragraphAttrs
            ++ [ Border.width 2
               , padding 10
               , cardDescriptionMaxWidth
               ]
        )
        [ Element.text <| parsedText ]


cardImage : String -> Element msg
cardImage url =
    image
        [ -- width (fill |> maximum 100)
          -- height (fill |> maximum 10)
          cardImgMaxWidth
        , Border.width 2
        , alignLeft
        ]
        { src = url, description = "Some image" }


type PaginateAction
    = Next
    | Prev


getPaginateRecord : PaginateAction -> { text : String, action : PaginateAction }
getPaginateRecord paginateAction =
    case paginateAction of
        Next ->
            { text = "next", action = Next }

        Prev ->
            { text = "prev", action = Prev }


paginateButton : PaginateAction -> Element Msg
paginateButton paginateAction =
    let
        paginateRecord =
            getPaginateRecord paginateAction
    in
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
        { onPress = Just (UsePaginate paginateRecord.action), label = text paginateRecord.text }


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



-- white =
--     Element.rgb255 255 255 255
-- red =
--     Element.rgb255 255 0 0
-- deleteRed =
--     Element.rgb255 223 71 89
-- focusedBlue =
--     Element.rgb255 69 179 231
-- hoverBlue =
--     Element.rgb255 205 235 249
-- dangerRed =
--     Element.rgb255 223 71 89


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
    case Debug.log "Home:view" model.request of
        Loading ->
            layout [ width fill, height fill ] <|
                column [ width fill, centerX ]
                    [ searchbar

                    -- , grid model
                    ]

        Default ->
            layout [ width fill, height fill ] <|
                column [ width fill, centerX ]
                    [ searchbar

                    -- , searchbar
                    -- , grid model
                    ]

        Failure ->
            layout [ width fill, height fill ] <|
                column [ width fill, centerX ]
                    [ searchbar

                    -- , grid model
                    , Element.text (Maybe.withDefault "" model.errorMessage)
                    ]

        Success cards ->
            layout [ width fill, height fill ] <|
                column [ width fill, centerX ]
                    [ searchbar
                    , grid model
                    ]
