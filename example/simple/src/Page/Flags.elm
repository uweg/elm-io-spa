module Page.Flags exposing (page)

import Html
import Html.Events
import IO exposing (IO)
import Msg exposing (Msg)
import Route
import Shared exposing (Shared)
import Spa.Page as Page exposing (Page)
import View exposing (View)


type alias Model =
    Int


page : Page Model Int Msg Shared (View (IO Model Msg))
page =
    { init =
        \context flags ->
            ( flags + 10
            , IO.none
            )
    , view =
        \context model ->
            [ Html.div []
                [ "Flags: " ++ String.fromInt model |> Html.text
                , Html.button
                    [ (model + 1)
                        |> Route.Flags
                        |> Msg.push
                        |> Html.Events.onClick
                    ]
                    [ Html.text "push" ]
                ]
            ]
    , onFlagsChanged = Just (\shared flags -> IO.set flags |> IO.andThen (\_ -> IO.none))
    , subscriptions = \_ -> Sub.none
    }
