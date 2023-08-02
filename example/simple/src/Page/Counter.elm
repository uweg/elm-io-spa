module Page.Counter exposing (page)

import Html
import Html.Events
import IO exposing (IO)
import Msg exposing (Msg)
import Shared exposing (Shared)
import Spa
import View exposing (View)


type alias Model =
    { counter : Int
    }


page : Spa.Page Model Int Msg Shared (View (IO Model Msg))
page =
    Spa.page
        (\context flags ->
            ( { counter = flags
              }
            , IO.none
            )
        )
        (\context flags model ->
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
        )


increase : IO Model Msg
increase =
    Msg.modify (\model -> { model | counter = model.counter + 1 })


decrease : IO Model Msg
decrease =
    Msg.modify (\model -> { model | counter = model.counter - 1 })