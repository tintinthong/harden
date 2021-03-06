module Route exposing (Route(..), fromUrl, href, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Profile exposing (Profile)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Username exposing (Username)



-- ROUTING


type Route
    = Home
    | Login
    | Register
    | Content



parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Register (s "register")
        , Parser.map Content (s "content")

        -- , Parser.map Logout (s "logout")
        -- , Parser.map Settings (s "settings")
        -- , Parser.map Profile (s "profile" </> Username.urlParser)
        -- , Parser.map Register (s "register")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    let
        _ = Debug.log "Route:replaceUrljb" key
        _ = Debug.log "hRoute:replaceUrlj" route
    in
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    "#/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Login ->
            [ "login" ]

        Register ->
            [ "register" ]

        Content ->
            [ "content" ]



-- Logout ->
--     [ "logout" ]
-- Register ->
--     [ "register" ]
-- Settings ->
--     [ "settings" ]
-- Profile username ->
--     [ "profile", Username.toString username ]
