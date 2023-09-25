module Spa.Page exposing (Page)

{-|

@docs Page, withSubscriptions, onFlagsChanged

-}

import IO exposing (IO)


{-| -}
type alias Page model flags msg shared view =
    { init : shared -> flags -> ( model, IO model msg )
    , subscriptions : model -> Sub (IO model msg)
    , view : shared -> model -> view
    , onFlagsChanged : Maybe (shared -> flags -> IO model msg)
    }
