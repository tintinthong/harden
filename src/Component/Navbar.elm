module Component.Navbar exposing (..)

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Html exposing (..)
import List


type alias Model =
    { selectedOption : Maybe String
    , activeElement : Maybe String
    , pageNames : List String
    }


init : Model
init =
    { selectedOption = Nothing
    , activeElement = Nothing
    , pageNames = [ "Home", "Login", "Stats" ]
    }


type Msg
    = OptionSelect String
    | SetActiveElement String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetActiveElement uiElement ->
            { model | activeElement = Just uiElement }

        OptionSelect selectedOption ->
            { model
                | selectedOption = Just selectedOption
                , activeElement = Nothing
            }


navbarUI : Model -> Element msg
navbarUI { pageNames } =
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
            [ Border.width 2
            , Element.padding 5
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
                pageNames
    in
    Element.row
        rowAttrs
        (leftCol :: rightCols)



-- List.map
-- [ Element.column
--     colAttrs
--     [ Element.el elAttrs (Element.text <| "hi") ]
-- ]
-- ,Element.column
--    colAttrs
--    (List.map (\name -> Element.el elAttrs (Element.text <| name)) pageNames)
