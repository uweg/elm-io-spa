module Route exposing
    ( Route(..)
    , fromUrl
    , matchCounter
    , matchFlags
    , matchHome
    , matchShared
    , matchSubscriptions
    , toUrl
    )

import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>))


type Route
    = Home
    | Counter Int
    | Subscriptions
    | Flags Int
    | Shared
    | NotFound


fromUrl : Url -> Route
fromUrl =
    Debug.log "url"
        >> UP.parse
            (UP.oneOf
                [ UP.top |> UP.map Home
                , UP.s "counter" </> UP.int |> UP.map Counter
                , UP.s "subscriptions" |> UP.map Subscriptions
                , UP.s "flags" </> UP.int |> UP.map Flags
                , UP.s "shared" |> UP.map Shared
                ]
            )
        >> Maybe.withDefault NotFound
        >> Debug.log "parsed"


toUrl : Route -> String
toUrl route =
    case route of
        Home ->
            UB.absolute [] []

        Counter amount ->
            UB.absolute
                [ "counter"
                , String.fromInt amount
                ]
                []

        Subscriptions ->
            UB.absolute [ "subscriptions" ] []

        Flags flags ->
            UB.absolute [ "flags", String.fromInt flags ] []

        Shared ->
            UB.absolute [ "shared" ] []

        NotFound ->
            ""


matchHome : Route -> Maybe ()
matchHome route =
    case route of
        Home ->
            Just ()

        _ ->
            Nothing


matchCounter : Route -> Maybe Int
matchCounter route =
    case route of
        Counter amount ->
            Just amount

        _ ->
            Nothing


matchSubscriptions : Route -> Maybe ()
matchSubscriptions route =
    case route of
        Subscriptions ->
            Just ()

        _ ->
            Nothing


matchFlags : Route -> Maybe Int
matchFlags route =
    case route of
        Flags flags ->
            Just flags

        _ ->
            Nothing


matchShared : Route -> Maybe ()
matchShared route =
    case route of
        Shared ->
            Just ()

        _ ->
            Nothing
