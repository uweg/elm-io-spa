module View exposing
    ( View
    , default
    , map
    , toDocument
    )

import Browser
import Html exposing (Html)
import IO exposing (IO)
import Msg exposing (Msg)
import Shared exposing (Shared)


type alias View msg =
    List (Html msg)


map : (a -> b) -> View a -> View b
map fn =
    List.map (Html.map fn)


default : View msg
default =
    []


toDocument : Shared -> View (IO model Msg) -> Browser.Document (IO model Msg)
toDocument shared view =
    { title = "elm-io-spa"
    , body = view
    }
