port module Api exposing (Cred, decodeErrors, storeCredWith, username, decoderFromCred)

import Avatar exposing (Avatar)
import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, keyValuePairs, list)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)
import Username exposing (Username)


type Cred
    = Cred Username String


username : Cred -> Username
username (Cred val _) =
    val


decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        Http.BadStatus statusCode ->
            [ fromInt statusCode ]

        -- |> decodeString (Decode.field "errors" errorsDecoder)
        -- |> Result.withDefault [ "Server error" ]
        err ->
            [ "Server error" ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors


storeCredWith : Cred -> Avatar -> Cmd msg
storeCredWith (Cred uname token) avatar =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Username.encode uname )
                        , ( "token", Encode.string token )
                        , ( "image", Avatar.encode avatar )
                        ]
                  )
                ]
    in
    storeCache (Just json)


port storeCache : Maybe Value -> Cmd msg

-- login : Http.Body -> Decoder (Cred -> a ) -> 

credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "token" Decode.string

decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder

