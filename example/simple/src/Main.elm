module Main exposing (main)

import IO exposing (IO)
import Msg exposing (Msg)
import Page.Counter
import Page.Home
import Page.Shared
import Page.Subscriptions
import Route exposing (Route)
import Shared exposing (Shared)
import Spa
import View


mappers =
    ( View.map, View.map )


onUrlChange : Route -> IO Shared Msg
onUrlChange route =
    IO.none


main =
    Spa.setup View.default
        |> Spa.addPage mappers Page.Home.page Route.matchHome
        |> Spa.addPage mappers Page.Counter.page Route.matchCounter
        |> Spa.addPage mappers Page.Subscriptions.page Route.matchSubscriptions
        |> Spa.addPage mappers Page.Shared.page Route.matchShared
        |> Spa.toApplication
            Shared.init
            Msg.update
            Route.fromUrl
            onUrlChange
            View.toDocument
