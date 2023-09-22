module Spa.Stack exposing (Model, Stack(..), add, setup, updateInfo)

import IO exposing (IO)
import Monocle.Prism exposing (Prism)
import Spa.Page exposing (Page)


type Stack current previous route msg shared view info
    = Stack
        { init :
            shared
            -> route
            -> Maybe (Model current previous)
            -> ( Model current previous, IO (Model current previous) msg )
        , subscriptions :
            Model current previous
            -> Sub (IO (Model current previous) msg)
        , view :
            shared
            -> route
            -> Model current previous
            -> view
        , defaultView : view
        , info : info
        }


type Model current previous
    = Current current
    | Previous previous


setup : view -> info -> Stack () () route msg shared view info
setup defaultView info_ =
    Stack
        { init = \_ _ _ -> ( Current (), IO.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ _ _ -> defaultView
        , defaultView = defaultView
        , info = info_
        }


add :
    ( (IO current msg -> IO (Model current previous) msg) -> (currentView -> view)
    , (IO previous msg -> IO (Model current previous) msg) -> (previousView -> view)
    )
    -> Page current flags msg shared currentView
    -> (route -> Maybe flags)
    -> Stack previousCurrent previousPrevious route msg shared previousView info
    -> Stack current (Model previousCurrent previousPrevious) route msg shared view info
add ( mapView, mapPreviousView ) page_ matchRoute (Stack prev) =
    let
        init :
            shared
            -> route
            -> Maybe (Model current (Model previousCurrent previousPrevious))
            ->
                ( Model current (Model previousCurrent previousPrevious)
                , IO (Model current (Model previousCurrent previousPrevious)) msg
                )
        init identity route model_ =
            case matchRoute route of
                Just flags ->
                    case ( page_.flagsChanged, model_ ) of
                        ( Just flagsChanged, Just (Current pageModel) ) ->
                            ( Current pageModel
                            , flagsChanged identity flags |> IO.prism currentPrism
                            )

                        _ ->
                            let
                                ( model, io ) =
                                    page_.init identity flags
                            in
                            ( Current model, io |> IO.prism currentPrism )

                Nothing ->
                    let
                        ( model, io ) =
                            prev.init identity route (Maybe.andThen getPrevious model_)
                    in
                    ( Previous model, io |> IO.prism previousPrism )

        subscriptions : Model current (Model previousCurrent previousPrevious) -> Sub (IO (Model current (Model previousCurrent previousPrevious)) msg)
        subscriptions model =
            case ( model, page_.subscriptions ) of
                ( Current current, Just subscriptions_ ) ->
                    subscriptions_ current |> Sub.map (IO.prism currentPrism)

                ( Current _, Nothing ) ->
                    Sub.none

                ( Previous previous, _ ) ->
                    prev.subscriptions previous |> Sub.map (IO.prism previousPrism)

        view :
            shared
            -> route
            -> Model current (Model previousCurrent previousPrevious)
            -> view
        view identity route model =
            case model of
                Current current ->
                    case matchRoute route of
                        Just _ ->
                            page_.view identity current |> mapView (IO.prism currentPrism)

                        Nothing ->
                            prev.defaultView |> mapPreviousView (IO.prism previousPrism)

                Previous previous ->
                    prev.view identity route previous |> mapPreviousView (IO.prism previousPrism)
    in
    Stack
        { init = init
        , subscriptions = subscriptions
        , view = view
        , defaultView = prev.defaultView |> mapPreviousView (IO.prism previousPrism)
        , info = prev.info
        }


currentPrism : Prism (Model current previous) current
currentPrism =
    { getOption =
        \m ->
            case m of
                Current v ->
                    Just v

                _ ->
                    Nothing
    , reverseGet = Current
    }


previousPrism : Prism (Model current previous) previous
previousPrism =
    { getOption = getPrevious
    , reverseGet = Previous
    }


getPrevious : Model current previous -> Maybe previous
getPrevious m =
    case m of
        Previous v ->
            Just v

        _ ->
            Nothing


updateInfo :
    (info -> info)
    -> Stack current previous route msg shared view info
    -> Stack current previous route msg shared view info
updateInfo update (Stack stack) =
    Stack { stack | info = update stack.info }
