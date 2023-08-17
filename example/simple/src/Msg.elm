module Msg exposing
    ( Msg
    , back
    , modify
    , push
    , toggle
    , update
    )

import Browser.Navigation
import IO exposing (IO)
import Route exposing (Route)
import Shared exposing (Shared)


type Msg
    = Toggle
    | Back
    | Push Route


toggle : IO model Msg
toggle =
    IO.pure Toggle


back : IO model Msg
back =
    IO.pure Back


push : Route -> IO model Msg
push =
    Push >> IO.pure


modify : (model -> model) -> IO model msg
modify =
    IO.modify >> IO.andThen (\_ -> IO.none)


update : Browser.Navigation.Key -> Msg -> IO Shared Msg
update key msg =
    case msg of
        Toggle ->
            modify
                (\shared ->
                    { shared | state = not shared.state }
                )

        Back ->
            Browser.Navigation.back key 1
                |> IO.lift

        Push route ->
            Route.toUrl route
                |> Browser.Navigation.pushUrl key
                |> IO.lift
