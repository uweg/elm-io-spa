module Page.Home exposing (page)

import Html
import Html.Attributes
import IO exposing (IO)
import Msg exposing (Msg)
import Route
import Shared exposing (Shared)
import Spa.Page as Page exposing (Page)
import View exposing (View)


type alias Model =
    {}


page : Page Model () Msg Shared (View (IO Model Msg))
page =
    Page.create
        (\context flags -> ( {}, IO.none ))
        (\context flags model ->
            [ Html.text "Welcome to elm-io-spa"
            , Html.div []
                [ Html.a
                    [ Route.Counter 0 |> Route.toUrl |> Html.Attributes.href ]
                    [ Html.text "counter" ]
                ]
            , Html.div []
                [ Html.a
                    [ Route.Shared |> Route.toUrl |> Html.Attributes.href ]
                    [ Html.text "shared" ]
                ]
            , Html.div []
                [ Html.a
                    [ Route.Subscriptions |> Route.toUrl |> Html.Attributes.href ]
                    [ Html.text "subscriptions" ]
                ]
            , Html.div []
                [ Html.a
                    [ Route.Flags 1 |> Route.toUrl |> Html.Attributes.href ]
                    [ Html.text "flags" ]
                ]
            ]
        )
