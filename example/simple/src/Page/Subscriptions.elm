module Page.Subscriptions exposing (page)

import Html
import Html.Events
import IO exposing (IO)
import Msg exposing (Msg)
import Shared exposing (Shared)
import Spa
import Task
import Time exposing (Posix)
import View exposing (View)


type alias Model =
    { time : Maybe Posix
    }


page : Spa.Page Model () Msg Shared (View (IO Model Msg))
page =
    Spa.page
        (\context flags ->
            ( { time = Nothing
              }
            , Time.now
                |> Task.perform (\a -> a)
                |> IO.lift
                |> IO.andThen updateTime
            )
        )
        (\context flags model ->
            [ model.time
                |> Maybe.map (Time.posixToMillis >> String.fromInt)
                |> Maybe.withDefault "..."
                |> Html.text
            , Html.div []
                [ Html.button
                    [ Html.Events.onClick Msg.back ]
                    [ Html.text "back" ]
                ]
            ]
        )
        |> Spa.withSubscriptions
            (\model ->
                Time.every 1000
                    (\a -> a)
                    |> Sub.map updateTime
            )


updateTime : Posix -> IO Model Msg
updateTime now =
    Msg.modify
        (\model_ ->
            { model_ | time = Just now }
        )
