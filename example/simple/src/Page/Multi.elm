module Page.Multi exposing (page)

import Html
import Html.Events
import IO
import Msg
import Page.Counter
import Page.Shared
import Page.Subscriptions
import Route
import Spa.Page as Page
import Spa.Page.Multi as Multi
import View exposing (View)


mappers =
    ( View.map, View.map )


page =
    Multi.setup View.map (\_ -> 0) Page.Counter.page
        |> Multi.add mappers (\_ -> 10) Page.Counter.page
        |> Multi.add mappers (\_ -> ()) Page.Shared.page
        |> Multi.add mappers (\_ -> ()) Page.Shared.page
        |> Multi.add mappers (\_ -> ()) Page.Subscriptions.page
        |> Multi.add mappers (\_ -> ()) Page.Subscriptions.page
        |> Multi.add mappers (\a -> a) flags
        |> Multi.toPage List.concat


flags =
    Page.page
        (\context flags_ ->
            ( flags_ + 10
            , IO.none
            )
        )
        (\context flags_ model ->
            [ Html.div []
                [ "Flags: " ++ String.fromInt model |> Html.text
                , Html.button
                    [ (model + 1)
                        |> Route.Multi
                        |> Msg.push
                        |> Html.Events.onClick
                    ]
                    [ Html.text "push" ]
                ]
            ]
        )
        |> Page.onFlagsChanged (\shared flags_ -> IO.set flags_ |> IO.andThen (\_ -> IO.none))
