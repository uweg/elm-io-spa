module Spa exposing
    ( Model
    , onUrlChange
    , setup
    , toApplication
    , withSubscriptions
    )

import Browser
import Browser.Navigation
import Html
import IO exposing (IO)
import Monocle.Optional exposing (Optional)
import Spa.Page
import Task exposing (Task)
import Url exposing (Url)


type Model current previous shared route error
    = Loading
    | Ready
        { key : Browser.Navigation.Key
        , shared : shared
        , page : Spa.Page.Model current previous
        , route : route
        }
    | Error error


pageOptional :
    Optional
        (Model current previous shared route error)
        (Spa.Page.Model current previous)
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
    }


setup :
    (flags -> route -> Task error shared)
    -> (Browser.Navigation.Key -> msg -> IO shared msg)
    -> view
    -> (Url -> route)
    -> Spa.Page.Stack () () route msg shared view (Info flags error shared msg route)
setup init_ update_ defaultView toRoute =
    Spa.Page.setup defaultView
        { init = init_
        , update = update_
        , toRoute = toRoute
        , onUrlChange = Nothing
        , subscriptions = Nothing
        }


onUrlChange :
    (route -> IO shared msg)
    -> Spa.Page.Stack () () route msg shared view (Info flags error shared msg route)
    -> Spa.Page.Stack () () route msg shared view (Info flags error shared msg route)
onUrlChange onUrlChange__ stack =
    Spa.Page.withInfo
        (\info ->
            { info
                | onUrlChange = Just onUrlChange__
            }
        )
        stack


withSubscriptions :
    (shared -> Sub (IO shared msg))
    -> Spa.Page.Stack () () route msg shared view (Info flags error shared msg route)
    -> Spa.Page.Stack () () route msg shared view (Info flags error shared msg route)
withSubscriptions subscriptions_ stack =
    Spa.Page.withInfo
        (\info ->
            { info
                | subscriptions = Just subscriptions_
            }
        )
        stack


toApplication :
    (shared -> view -> Browser.Document (IO (Spa.Page.Model current previous) msg))
    -> Spa.Page.Stack current previous route msg shared view (Info flags error shared msg route)
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
    Spa.Page.Stack current previous route msg shared view (Info flags error shared msg route)
    -> flags
    -> Url
    -> Browser.Navigation.Key
    ->
        ( Model current previous shared route error
        , IO (Model current previous shared route error) msg
        )
init (Spa.Page.Stack stack) flags url key =
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
    Spa.Page.Stack current previous route msg shared view (Info flags error shared msg route)
    -> Model current previous shared route error
    -> Sub (IO (Model current previous shared route error) msg)
subscriptions (Spa.Page.Stack stack) model =
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
    Spa.Page.Stack current previous route msg shared view (Info flags error shared msg route)
    -> msg
    -> IO (Model current previous shared route error) msg
update (Spa.Page.Stack stack) msg =
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
    Spa.Page.Stack current previous route msg shared view (Info flags error shared msg route)
    -> Url
    -> IO (Model current previous shared route error) msg
onUrlChange_ (Spa.Page.Stack stack) url =
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
    (shared -> view -> Browser.Document (IO (Spa.Page.Model current previous) msg))
    -> Spa.Page.Stack current previous route msg shared view (Info flags error shared msg route)
    -> Model current previous shared route error
    -> Browser.Document (IO (Model current previous shared route error) msg)
view toDocument (Spa.Page.Stack stack) model =
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
            { title = ""
            , body = [ Html.text "Initializing..." ]
            }

        Error _ ->
            { title = ""
            , body = [ Html.text "Could not initialize page. Please try again later." ]
            }
