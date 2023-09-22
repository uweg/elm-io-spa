module Spa.Page exposing (Page, create, withSubscriptions, onFlagsChanged)

{-|

@docs Page, create, withSubscriptions, onFlagsChanged

-}

import IO exposing (IO)


{-| -}
type alias Page model flags msg shared view =
    { init : shared -> flags -> ( model, IO model msg )
    , subscriptions : Maybe (model -> Sub (IO model msg))
    , view : shared -> model -> view
    , flagsChanged : Maybe (shared -> flags -> IO model msg)
    }


{-| Creates a new page.
-}
create :
    (shared -> flags -> ( model, IO model msg ))
    -> (shared -> model -> view)
    -> Page model flags msg shared view
create init view =
    { init = init
    , subscriptions = Nothing
    , view = view
    , flagsChanged = Nothing
    }


{-| Adds subscriptions to page.
-}
withSubscriptions :
    (model -> Sub (IO model msg))
    -> Page model flags msg shared view
    -> Page model flags msg shared view
withSubscriptions subscriptions page =
    { page | subscriptions = Just subscriptions }


{-| Handles changed flags without initializing again.
-}
onFlagsChanged :
    (shared -> flags -> IO model msg)
    -> Page model flags msg shared view
    -> Page model flags msg shared view
onFlagsChanged flagsChanged_ page =
    { page | flagsChanged = Just flagsChanged_ }
