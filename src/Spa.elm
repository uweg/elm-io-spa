module Spa exposing (setup, addPage, withSubscriptions, withLoadingView, withErrorView, onUrlChange, toApplication, Model, StackModel)

{-|

@docs setup, addPage, withSubscriptions, withLoadingView, withErrorView, onUrlChange, toApplication, Model, StackModel

-}

import Browser
import Browser.Navigation
import Html
import IO exposing (IO)
import Monocle.Optional exposing (Optional)
import Spa.Page exposing (Page)
import Spa.Stack exposing (Stack(..))
import Task exposing (Task)
import Url exposing (Url)


{-| -}
type Model current previous shared route error
    = Loading
    | Ready
        { key : Browser.Navigation.Key
        , shared : shared
        , page : Spa.Stack.Model current previous
        , route : route
        }
    | Error error


{-| -}
type alias StackModel current previous =
    Spa.Stack.Model current previous


pageOptional :
    Optional
        (Model current previous shared route error)
        (Spa.Stack.Model current previous)
pageOptional =
    { getOption =
        \m ->
            case m of
                Ready ready ->
                    Just ready.page

                _ ->
                    Nothing
    , set =
        \v m ->
            case m of
                Ready ready ->
                    Ready { ready | page = v }

                _ ->
                    m
    }


sharedOptional : Optional (Model current previous shared route error) shared
sharedOptional =
    { getOption =
        \model ->
            case model of
                Ready data ->
                    Just data.shared

                _ ->
                    Nothing
    , set =
        \shared model ->
            case model of
                Ready data ->
                    Ready { data | shared = shared }

                _ ->
                    model
    }


type alias Info flags error shared msg route =
    { init : flags -> route -> Task error shared
    , update : Browser.Navigation.Key -> msg -> IO shared msg
    , toRoute : Url -> route
    , onUrlChange : Maybe (route -> IO shared msg)
    , subscriptions : Maybe (shared -> Sub (IO shared msg))
    , loadingView : Maybe (Browser.Document msg)
    , errorView : Maybe (error -> Browser.Document msg)
    }


{-| Sets up a new SPA.
-}
setup :
    (flags -> route -> Task error shared)
    -> (Browser.Navigation.Key -> msg -> IO shared msg)
    -> view
    -> (Url -> route)
    -> Stack () () route msg shared view (Info flags error shared msg route)
setup init_ update_ defaultView toRoute =
    Spa.Stack.setup defaultView
        { init = init_
        , update = update_
        , toRoute = toRoute
        , onUrlChange = Nothing
        , subscriptions = Nothing
        , loadingView = Nothing
        , errorView = Nothing
        }


{-| Callback when an URL is changed.
-}
onUrlChange :
    (route -> IO shared msg)
    -> Stack () () route msg shared view (Info flags error shared msg route)
    -> Stack () () route msg shared view (Info flags error shared msg route)
onUrlChange onUrlChange__ stack =
    Spa.Stack.updateInfo
        (\info ->
            { info
                | onUrlChange = Just onUrlChange__
            }
        )
        stack


{-| Adds global subscriptions.
-}
withSubscriptions :
    (shared -> Sub (IO shared msg))
    -> Stack () () route msg shared view (Info flags error shared msg route)
    -> Stack () () route msg shared view (Info flags error shared msg route)
withSubscriptions subscriptions_ stack =
    stack
        |> Spa.Stack.updateInfo
            (\info ->
                { info
                    | subscriptions = Just subscriptions_
                }
            )


{-| Defines view while SPA is loading.
-}
withLoadingView :
    Browser.Document msg
    -> Stack () () route msg shared view (Info flags error shared msg route)
    -> Stack () () route msg shared view (Info flags error shared msg route)
withLoadingView loadingView stack =
    stack
        |> Spa.Stack.updateInfo (\info -> { info | loadingView = Just loadingView })


{-| Defines view on loading error.
-}
withErrorView :
    (error -> Browser.Document msg)
    -> Stack () () route msg shared view (Info flags error shared msg route)
    -> Stack () () route msg shared view (Info flags error shared msg route)
withErrorView errorView stack =
    stack
        |> Spa.Stack.updateInfo (\info -> { info | errorView = Just errorView })


{-| Adds a page to the SPA.
-}
addPage :
    ( (IO current msg -> IO (Spa.Stack.Model current previous) msg) -> currentView -> view
    , (IO previous msg -> IO (Spa.Stack.Model current previous) msg) -> previousView -> view
    )
    -> Page current flags msg shared currentView
    -> (route -> Maybe flags)
    -> Stack previousCurrent previousPrevious route msg shared previousView info
    -> Stack current (Spa.Stack.Model previousCurrent previousPrevious) route msg shared view info
addPage =
    Spa.Stack.add


{-| Converts the SPA to an Elm application.
-}
toApplication :
    (shared -> view -> Browser.Document (IO (Spa.Stack.Model current previous) msg))
    -> Stack current previous route msg shared view (Info flags error shared msg route)
    -> IO.Program flags (Model current previous shared route error) msg
toApplication toDocument stack =
    IO.application
        { init = init stack
        , subscriptions = subscriptions stack
        , update = update stack
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange_ stack
        , view = view toDocument stack
        }


init :
    Stack current previous route msg shared view (Info flags error shared msg route)
    -> flags
    -> Url
    -> Browser.Navigation.Key
    ->
        ( Model current previous shared route error
        , IO (Model current previous shared route error) msg
        )
init (Stack stack) flags url key =
    let
        route : route
        route =
            stack.info.toRoute url
    in
    ( Loading
    , stack.info.init flags route
        |> Task.attempt (\a -> a)
        |> IO.lift
        |> IO.andThen
            (\result ->
                case result of
                    Ok shared ->
                        let
                            ( page_, io ) =
                                stack.init shared route Nothing
                        in
                        IO.set
                            (Ready
                                { key = key
                                , page = page_
                                , route = route
                                , shared = shared
                                }
                            )
                            |> IO.andThen (\_ -> io |> IO.optional pageOptional)

                    Err err ->
                        IO.modify (\_ -> Error err)
                            |> IO.andThen (\_ -> IO.none)
            )
    )


subscriptions :
    Stack current previous route msg shared view (Info flags error shared msg route)
    -> Model current previous shared route error
    -> Sub (IO (Model current previous shared route error) msg)
subscriptions (Stack stack) model =
    case model of
        Ready ready ->
            Sub.batch
                [ stack.subscriptions ready.page
                    |> Sub.map (IO.optional pageOptional)
                , stack.info.subscriptions
                    |> Maybe.map
                        (\subscriptions_ ->
                            subscriptions_ ready.shared
                                |> Sub.map (IO.optional sharedOptional)
                        )
                    |> Maybe.withDefault Sub.none
                ]

        _ ->
            Sub.none


update :
    Stack current previous route msg shared view (Info flags error shared msg route)
    -> msg
    -> IO (Model current previous shared route error) msg
update (Stack stack) msg =
    IO.get
        |> IO.andThen
            (\model ->
                case model of
                    Ready ready ->
                        stack.info.update ready.key msg |> IO.optional sharedOptional

                    _ ->
                        IO.none
            )


onUrlRequest : Browser.UrlRequest -> IO (Model current previous shared route error) msg
onUrlRequest request =
    case request of
        Browser.Internal url ->
            IO.get
                |> IO.andThen
                    (\model ->
                        case model of
                            Ready ready ->
                                Url.toString url
                                    |> Browser.Navigation.pushUrl ready.key
                                    |> IO.lift

                            _ ->
                                IO.none
                    )

        Browser.External _ ->
            IO.none


onUrlChange_ :
    Stack current previous route msg shared view (Info flags error shared msg route)
    -> Url
    -> IO (Model current previous shared route error) msg
onUrlChange_ (Stack stack) url =
    IO.get
        |> IO.andThen
            (\model ->
                case model of
                    Ready ready ->
                        let
                            route : route
                            route =
                                stack.info.toRoute url
                        in
                        if route == ready.route then
                            case stack.info.onUrlChange of
                                Just onUrlChange___ ->
                                    onUrlChange___ route |> IO.optional sharedOptional

                                Nothing ->
                                    IO.none

                        else
                            let
                                ( pageModel, io ) =
                                    stack.init ready.shared route (Just ready.page)
                            in
                            Ready { ready | page = pageModel, route = route }
                                |> IO.set
                                |> IO.andThen
                                    (\_ ->
                                        IO.batchM
                                            [ io |> IO.optional pageOptional
                                            , case stack.info.onUrlChange of
                                                Just onUrlChange___ ->
                                                    onUrlChange___ route |> IO.optional sharedOptional

                                                Nothing ->
                                                    IO.none
                                            ]
                                    )

                    _ ->
                        IO.none
            )


view :
    (shared -> view -> Browser.Document (IO (Spa.Stack.Model current previous) msg))
    -> Stack current previous route msg shared view (Info flags error shared msg route)
    -> Model current previous shared route error
    -> Browser.Document (IO (Model current previous shared route error) msg)
view toDocument (Stack stack) model =
    case model of
        Ready ready ->
            stack.view ready.shared ready.route ready.page
                |> toDocument ready.shared
                |> (\document ->
                        { title = document.title
                        , body =
                            document.body
                                |> List.map (Html.map (IO.optional pageOptional))
                        }
                   )

        Loading ->
            case stack.info.loadingView of
                Just loadingView ->
                    { title = loadingView.title
                    , body = loadingView.body |> List.map (Html.map IO.pure)
                    }

                Nothing ->
                    { title = ""
                    , body = []
                    }

        Error err ->
            case stack.info.errorView of
                Just errorView ->
                    let
                        errorView_ =
                            errorView err
                    in
                    { title = errorView_.title
                    , body = errorView_.body |> List.map (Html.map IO.pure)
                    }

                Nothing ->
                    { title = ""
                    , body = [ Html.text "Error" ]
                    }
