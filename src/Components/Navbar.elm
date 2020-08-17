module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Border  as Border
import Element.Font as Font
import Html exposing (..) 

type alias Model =
    { selectedOption : Maybe String 
    , activeElement : Maybe String
    }

init : Model
init =   
    { 
     selectedOption = Nothing 
    , activeElement = Nothing
    }

type Msg
    = OptionSelect String
    | SetActiveElement String

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetActiveElement uiElement -> { model | activeElement = Just uiElement }
        OptionSelect selectedOption ->
            { model
            | selectedOption = Just selectedOption
            , activeElement = Nothing
            }

view : Model -> Html Msg
view model = 
    let 
        layoutAttrs = 
            [
                Element.width Element.fill
            ]
        colAttrs = 
            [
                Border.width 2
                ,Element.padding 10
                ,Element.width Element.fill
            ]
        rowAttrs = 
            [
                Border.width 2
                ,Element.padding 10
                ,Element.width Element.fill
                ,Element.spacing 10
            ]
        pageAttrs = 
            [
                Border.width 2
                ,Element.padding 5
                ,Element.width Element.fill
                ,Font.center 
            ]
            
    in
    Element.layout 
        layoutAttrs 
        <| Element.column 
            colAttrs
            [
                Element.row 
                    rowAttrs
                    [ 
                    Element.el pageAttrs ( Element.text <| "Home" ) 
                    ,Element.el pageAttrs ( Element.text <| "About" ) 
                    ,Element.el pageAttrs ( Element.text <| "Projects" ) 
                    ,Element.el pageAttrs ( Element.text <| "Contact" ) 
                    ]
            ]
                 
     
-- Helper 

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
