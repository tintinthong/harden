module Page.Login exposing (..)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import Json.Decode exposing (string)
import Json.Encode as Encode
import Route
import Session exposing (Session)
import Viewer exposing (Viewer)


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }


toSession : Model -> Session
toSession model =
    model.session


type alias Form =
    { email : String
    , password : String
    }


type ValidatedField
    = Email
    | Password


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error Viewer)
    | GotSession Session


type TrimmedForm
    = Trimmed Form


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }


blue =
    rgb255 0 0 238


purple =
    rgb255 220 238 238


button =
    Input.button
        [ Background.color blue
        , Element.focused
            [ Background.color purple ]
        ]
        { onPress = Just SubmittedForm
        , label = text "Login"
        }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , form =
            { email = ""
            , password = ""
            }
      , problems = []
      }
    , Cmd.none
    )


update msg model =
    case Debug.log "Login:msg" msg of
        SubmittedForm ->
            case validate model.form of
                Ok (Trimmed validForm) ->
                    let
                        json =
                            Encode.object [ ( "email", Encode.string validForm.email ), ( "password", Encode.string validForm.password ) ]
                        _ = Debug.log "Login:json" json
                    in
                    ( { model | problems = [] }
                    , Http.post
                        { url = "http://localhost:3001/rest/v1/login/" 
                        , body = Http.jsonBody json
                        , expect = Http.expectJson CompletedLogin (Api.decoderFromCred Viewer.decoder)
                        }
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            ( { model | problems = List.append model.problems serverErrors }
            , Cmd.none
            )

        CompletedLogin (Ok viewer) ->
            ( model
            , Viewer.store viewer
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 20
        ]
    <|
        Element.column
            [ width (px 800)
            , height shrink
            , centerY
            , centerX
            , spacing 36
            , padding 10
            ]
            [ el
                [ Region.heading 1
                , alignLeft
                , Font.size 36
                ]
                (text "Login Form")
            , Input.username
                [ spacing 12
                , below
                    (el
                        [ Font.color red
                        , Font.size 14
                        , alignRight
                        , moveDown 6
                        ]
                        (text "This one is wrong")
                    )
                ]
                { text = model.form.email
                , placeholder = Just (Input.placeholder [] (text "Write Email here fool"))
                , onChange = \newEmail -> EnteredEmail newEmail
                , label = Input.labelAbove [ Font.size 14 ] (text "Email")
                }
            , Input.currentPassword [ spacing 12 ]
                { text = model.form.password
                , placeholder = Nothing
                , onChange = \newPassword -> EnteredPassword newPassword
                , label = Input.labelAbove [ Font.size 14 ] (text "Password")
                , show = False
                }
            , Input.button
                [ Background.color blue
                , Font.color white
                , Border.color darkBlue
                , paddingXY 32 16
                , Border.rounded 3

                -- , width fill
                ]
                { onPress = Just SubmittedForm
                , label = Element.text "Login"
                }
            ]
