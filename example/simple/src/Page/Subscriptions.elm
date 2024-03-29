module Page.Subscriptions exposing (page)

import Html
import Html.Events
import IO exposing (IO)
import Msg exposing (Msg)
import Shared exposing (Shared)
import Spa.Page exposing (Page)
import Task
import Time exposing (Posix)
import View exposing (View)


type alias Model =
    { time : Maybe Posix
    }


page : Page Model () Msg Shared (View (IO Model Msg))
page =
    { init =
        \context flags ->
            ( { time = Nothing
              }
            , Time.now
                |> Task.perform (\a -> a)
                |> IO.lift
                |> IO.andThen updateTime
            )
    , view =
        \context model ->
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
    , subscriptions =
        \model ->
            Time.every 1000
                (\a -> a)
                |> Sub.map updateTime
    , onFlagsChanged = Nothing
    }


updateTime : Posix -> IO Model Msg
updateTime now =
    Msg.modify
        (\model_ ->
            { model_ | time = Just now }
        )
