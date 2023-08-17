module Shared exposing
    ( Shared
    , init
    )

import Platform exposing (Task)
import Route exposing (Route)
import Task


type alias Shared =
    { state : Bool }


init : () -> Route -> Task Never Shared
init flags route =
    Task.succeed
        { state = False
        }
