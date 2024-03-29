module Page.Counter exposing (page)

import Html
import Html.Events
import IO exposing (IO)
import Msg exposing (Msg)
import Shared exposing (Shared)
import Spa.Page exposing (Page)
import View exposing (View)


type alias Model =
    { counter : Int
    }


page : Page Model Int Msg Shared (View (IO Model Msg))
page =
    { init =
        \context flags ->
            ( { counter = flags
              }
            , IO.none
            )
    , view =
        \context model ->
            [ model.counter |> String.fromInt |> Html.text
            , Html.button
                [ Html.Events.onClick decrease ]
                [ Html.text "Decrease" ]
            , Html.button
                [ Html.Events.onClick increase ]
                [ Html.text "Increase" ]
            , Html.div []
                [ Html.button
                    [ Html.Events.onClick Msg.back ]
                    [ Html.text "back" ]
                ]
            ]
    , subscriptions = \_ -> Sub.none
    , onFlagsChanged = Nothing
    }


increase : IO Model Msg
increase =
    Msg.modify (\model -> { model | counter = model.counter + 1 })


decrease : IO Model Msg
decrease =
    Msg.modify (\model -> { model | counter = model.counter - 1 })
