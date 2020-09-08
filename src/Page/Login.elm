module Page.Login exposing (..)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (string)
import Json.Encode
import Route
import Session exposing (Session)
import Viewer exposing (Viewer)


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }


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
    rgb255 238 238 238


purple =
    rgb255 220 238 238


button =
    Input.button
        [ Background.color blue
        , Element.focused
            [ Background.color purple ]
        ]
        { onPress = Just SubmittedForm
        , label = text "My Button"
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
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Http.post
                        { url = "/login"
                        , body = Http.emptyBody --validForm
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


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        layout [ width fill, height fill ] <|
            column [ width fill, centerX ]
                [ button
                ]
    }
