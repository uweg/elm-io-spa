module Page.Shared exposing (page)

import Html
import Html.Events
import IO exposing (IO)
import Msg exposing (Msg)
import Shared exposing (Shared)
import Spa
import View exposing (View)


type alias Model =
    {}


page : Spa.Page Model () Msg Shared (View (IO Model Msg))
page =
    Spa.page
        (\shared flags -> ( {}, IO.none ))
        (\shared flags model ->
            [ (if shared.state then
                "on"

               else
                "off"
              )
                |> Html.text
            , Html.button
                [ Html.Events.onClick Msg.toggle ]
                [ Html.text "Toggle" ]
            , Html.div []
                [ Html.button
                    [ Html.Events.onClick Msg.back ]
                    [ Html.text "back" ]
                ]
            ]
        )
