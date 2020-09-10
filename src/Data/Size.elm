module Data.Size exposing (..)

import Element exposing (..)
import Element.Font as Font


-- Try to only expose data rather than functions
--  The rest of the functions should be internal here

-- All values are in px


cardWidth =
    300


cardHeight =
    500


cardMaxWidth =
    10


cardMinWidth =
    10


cardMaxHeight =
    10


cardMinHeight =
    10


cardImgMaxWidth =
    partOf cardWidth 0.5 |> toMaxWidth


cardDescriptionMaxWidth =
    partOf cardWidth 0.8 |> toMaxWidth


cardTitleMaxWidth = partOf cardWidth 0.8 |> toMaxWidth
cardTitleMaxHeight = partOf cardHeight 0.8 |> toMaxHeight
cardTitleMinHeight = partOf cardHeight 0.5 |> toMinHeight
cardTitleHeight = partOf cardHeight 0.2 |> toHeight


gridMaxWidth =
    10


gridMaxHeight =
    10


screenMaxWidth =
    10


screenMaxHeight =
    10


someWidth =
    cardWidth
        |> px
        |> width


someHeight =
    cardHeight
        |> px
        |> height



-- Utils


partOf : Int -> Float -> Int
partOf val ratio =
    val |> toFloat |> (*) ratio |> round



-- Parse to Elm UI

toMinHeight : Int -> Attribute msg
toMinHeight widthPx =
    fill
        |> minimum widthPx
        |> width

toMaxWidth : Int -> Attribute msg
toMaxWidth widthPx =
    fill
        |> maximum widthPx
        |> width


toMaxHeight : Int -> Attribute msg
toMaxHeight heightPx =
    fill
        |> maximum heightPx
        |> height


toWidth widthPx =
    widthPx
        |> px
        |> width


toHeight heightPx =
    heightPx
        |> px
        |> height
