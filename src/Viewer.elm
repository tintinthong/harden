module Viewer exposing (Viewer, avatar, cred, decoder, minPasswordChars, store, username)


import Api exposing (Cred, storeCredWith)
import Avatar exposing (Avatar)
import Email exposing (Email)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Username exposing (Username)



-- TYPES


type Viewer
    = Viewer Avatar Cred



-- INFO


cred : Viewer -> Cred
cred (Viewer _ val) =
    val


username : Viewer -> Username
username (Viewer _ val) =
    Api.username val


avatar : Viewer -> Avatar
avatar (Viewer val _) =
    val


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6



-- SERIALIZATION


decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "image" Avatar.decoder)


store : Viewer -> Cmd msg
store (Viewer avatarVal credVal) =
    Api.storeCredWith
        credVal
        avatarVal
