module Api exposing (Cred, username)

import Username exposing (Username)

type Cred
    = Cred Username String

username : Cred -> Username
username (Cred val _) =
    val
