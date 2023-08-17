module Main exposing (main)

import IO exposing (IO)
import Msg exposing (Msg)
import Page.Counter
import Page.Flags
import Page.Home
import Page.Multi
import Page.Shared
import Page.Subscriptions
import Route exposing (Route)
import Shared exposing (Shared)
import Spa
import Spa.Page
import View


mappers =
    ( View.map, View.map )


onUrlChange : Route -> IO Shared Msg
onUrlChange route =
    IO.none


main =
    Spa.setup Shared.init Msg.update View.default Route.fromUrl
        |> Spa.onUrlChange onUrlChange
        |> Spa.Page.add mappers Page.Home.page Route.matchHome
        |> Spa.Page.add mappers Page.Counter.page Route.matchCounter
        |> Spa.Page.add mappers Page.Subscriptions.page Route.matchSubscriptions
        |> Spa.Page.add mappers Page.Flags.page Route.matchFlags
        |> Spa.Page.add mappers Page.Shared.page Route.matchShared
        |> Spa.Page.add mappers Page.Multi.page Route.matchMulti
        |> Spa.toApplication View.toDocument
