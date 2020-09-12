module Page.Content exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode exposing (Decoder, at, field, float, int, list, map, map2, map7, string)
import Session exposing (Session)


type alias Model =
    { session : Session
    , content : Content
    }

type Msg = Default

toSession : Model -> Session
toSession model =
    model.session


type alias Content =
    { title : String
    , released : String
    , writer : String
    , actors : String
    , plot : String
    , genre : String
    , ratings : List RatingsObj
    }


view : Model -> Html msg
view model =
    Element.layout [ width fill ] <|
        Element.column [ width fill ]
            [ contentHeader model.content.title
            ]


contentHeader : String -> Element msg
contentHeader title =
    Element.column []
        [ text <| title 
        ]


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , content = { title = "", released = "", writer = "", actors = "" , plot ="", genre = "", ratings = [{source = "", value = ""}]} 
      }
    , Cmd.none
    )

-- {
--     "Title": "Rick and Morty",
--     "Year": "2013–",
--     "Rated": "TV-14",
--     "Released": "02 Dec 2013",
--     "Runtime": "23 min",
--     "Genre": "Animation, Adventure, Comedy, Sci-Fi",
--     "Director": "N/A",
--     "Writer": "Dan Harmon, Justin Roiland",
--     "Actors": "Justin Roiland, Chris Parnell, Spencer Grammer, Sarah Chalke",
--     "Plot": "An animated series that follows the exploits of a super scientist and his not-so-bright grandson.",
--     "Language": "English",
--     "Country": "USA",
--     "Awards": "Won 1 Primetime Emmy. Another 15 wins & 18 nominations.",
--     "Poster": "https://m.media-amazon.com/images/M/MV5BZjRjOTFkOTktZWUzMi00YzMyLThkMmYtMjEwNmQyNzliYTNmXkEyXkFqcGdeQXVyNzQ1ODk3MTQ@._V1_SX300.jpg",
--     "Ratings": [
--         {
--             "Source": "Internet Movie Database",
--             "Value": "9.2/10"
--         }
--     ],
--     "Metascore": "N/A",
--     "imdbRating": "9.2",
--     "imdbVotes": "352,741",
--     "imdbID": "tt2861424",
--     "Type": "series",
--     "totalSeasons": "4",
--     "Response": "True"
-- }


movieDecoder : Decoder Content
movieDecoder =
    map7 Content
        (field "Title" string)
        (field "Released" string)
        (field "Writer" string)
        (field "Actors" string)
        (field "Plot" string)
        (field "Genre" string)
        (field "Ratings" <| ratingsListDecoder)


type alias RatingsObj =
    { source : String, value : String }


ratingsListDecoder : Decoder (List RatingsObj)
ratingsListDecoder =
    list ratingsObjDecoder


ratingsObjDecoder : Decoder RatingsObj
ratingsObjDecoder =
    map2 RatingsObj
        (field "Source" string)
        (field "Value" string)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
