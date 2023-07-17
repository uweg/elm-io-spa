module Msg exposing
    ( Msg
    , modify
    , toggle
    , update
    , back)

import Browser.Navigation
import IO exposing (IO)
import Shared exposing (Shared)


type Msg
    = Toggle
    | Back


toggle : IO model Msg
toggle =
    IO.pure Toggle


back : IO model Msg
back =
    IO.pure Back


modify : (model -> model) -> IO model msg
modify =
    IO.modify >> IO.andThen (\_ -> IO.none)


update : Browser.Navigation.Key -> Msg -> IO Shared Msg
update key msg =
    case msg of
        Toggle ->
            modify (\shared -> { shared | state = not shared.state })

        Back ->
            Browser.Navigation.back key 1 |> IO.lift
