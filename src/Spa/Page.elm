module Spa.Page exposing
    ( Model
    , Page
    , Stack(..)
    , add
    , info
    , onFlagsChanged
    , page
    , setup
    , withInfo
    , withSubscriptions
    )

import IO exposing (IO)
import Monocle.Prism exposing (Prism)


type alias Page model flags msg shared view =
    { init : shared -> flags -> ( model, IO model msg )
    , subscriptions : Maybe (model -> Sub (IO model msg))
    , view : shared -> flags -> model -> view
    , flagsChanged : Maybe (shared -> model -> flags -> ( model, IO model msg ))
    }


page :
    (shared -> flags -> ( model, IO model msg ))
    -> (shared -> flags -> model -> view)
    -> Page model flags msg shared view
page init view =
    { init = init
    , subscriptions = Nothing
    , view = view
    , flagsChanged = Nothing
    }


withSubscriptions :
    (model -> Sub (IO model msg))
    -> Page model flags msg shared view
    -> Page model flags msg shared view
withSubscriptions subscriptions page_ =
    { page_ | subscriptions = Just subscriptions }


onFlagsChanged :
    (shared -> model -> flags -> ( model, IO model msg ))
    -> Page model flags msg shared view
    -> Page model flags msg shared view
onFlagsChanged flagsChanged_ page_ =
    { page_ | flagsChanged = Just flagsChanged_ }


type Model current previous
    = Current current
    | Previous previous


withInfo :
    (info -> info)
    -> Stack current previous route msg shared view info
    -> Stack current previous route msg shared view info
withInfo updateInfo (Stack stack) =
    Stack
        { stack
            | info = updateInfo stack.info
        }


info : Stack current previous route msg shared view info -> info
info (Stack stack) =
    stack.info


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


setup : view -> info -> Stack () () route msg shared view info
setup defaultView info_ =
    Stack
        { init = \_ _ _ -> ( Current (), IO.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ _ _ -> defaultView
        , defaultView = defaultView
        , info = info_
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


getPrevious : Model current previous -> Maybe previous
getPrevious m =
    case m of
        Previous v ->
            Just v

        _ ->
            Nothing


previousPrism : Prism (Model current previous) previous
previousPrism =
    { getOption = getPrevious
    , reverseGet = Previous
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
                            let
                                ( model, io ) =
                                    flagsChanged identity pageModel flags
                            in
                            ( Current model, io |> IO.prism currentPrism )

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
                        Just flags ->
                            page_.view identity flags current |> mapView (IO.prism currentPrism)

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
