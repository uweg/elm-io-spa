module Shared exposing
    ( Shared
    , init
    )

import Platform exposing (Task)
import Task


type alias Shared =
    { state : Bool }


init : () -> Task Never Shared
init =
    \flags ->
        Task.succeed
            { state = False
            }
