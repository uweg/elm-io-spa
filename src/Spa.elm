module Spa exposing (toApplication)

import Browser
import Browser.Navigation
import Html
import IO exposing (IO)
import Json.Encode as E
import Monocle.Optional exposing (Optional)
import Spa.Page
import Task exposing (Task)
import Url exposing (Url)


type Model current previous shared route
    = Loading
    | Ready
        { key : Browser.Navigation.Key
        , shared : shared
        , page : Spa.Page.Model current previous
        , route : route
        }


pageOptional :
    Optional
        (Model current previous shared route)
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


sharedOptional : Optional (Model current previous shared route) shared
sharedOptional =
    { getOption =
        \model ->
            case model of
                Loading ->
                    Nothing

                Ready data ->
                    Just data.shared
    , set =
        \shared model ->
            case model of
                Loading ->
                    model

                Ready data ->
                    Ready { data | shared = shared }
    }


toApplication :
    (E.Value -> Task Never shared)
    -> (Browser.Navigation.Key -> msg -> IO shared msg)
    -> (Url -> route)
    -> (route -> IO shared msg)
    -> (shared -> view -> Browser.Document (IO (Spa.Page.Model current previous) msg))
    -> Spa.Page.Stack current previous route msg shared view
    -> IO.Program E.Value (Model current previous shared route) msg
toApplication init_ update_ toRoute onUrlChange_ toDocument stack =
    IO.application
        { init = init init_ toRoute stack
        , subscriptions = subscriptions stack
        , update = update update_
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange toRoute onUrlChange_ stack
        , view = view toDocument stack
        }


init :
    (E.Value -> Task Never shared)
    -> (Url -> route)
    -> Spa.Page.Stack current previous route msg shared view
    -> E.Value
    -> Url
    -> Browser.Navigation.Key
    ->
        ( Model current previous shared route
        , IO (Model current previous shared route) msg
        )
init init_ fromUrl (Spa.Page.Stack stack) flags url key =
    ( Loading
    , init_ flags
        |> Task.perform (\a -> a)
        |> IO.lift
        |> IO.andThen
            (\shared ->
                let
                    route : route
                    route =
                        fromUrl url

                    ( page, io ) =
                        stack.init shared route Nothing
                in
                IO.set
                    (Ready
                        { key = key
                        , page = page
                        , route = route
                        , shared = shared
                        }
                    )
                    |> IO.andThen (\_ -> io |> IO.optional pageOptional)
            )
    )


subscriptions :
    Spa.Page.Stack current previous route msg shared view
    -> Model current previous shared route
    -> Sub (IO (Model current previous shared route) msg)
subscriptions (Spa.Page.Stack stack) model =
    case model of
        Ready ready ->
            stack.subscriptions ready.page
                |> Sub.map (IO.optional pageOptional)

        Loading ->
            Sub.none


update :
    (Browser.Navigation.Key -> msg -> IO shared msg)
    -> msg
    -> IO (Model current previous shared route) msg
update update_ msg =
    IO.get
        |> IO.andThen
            (\model ->
                case model of
                    Ready ready ->
                        update_ ready.key msg |> IO.optional sharedOptional

                    Loading ->
                        IO.none
            )


onUrlRequest : Browser.UrlRequest -> IO (Model current previous shared route) msg
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


onUrlChange :
    (Url -> route)
    -> (route -> IO shared msg)
    -> Spa.Page.Stack current previous route msg shared view
    -> Url
    -> IO (Model current previous shared route) msg
onUrlChange toRoute onUrlChange_ (Spa.Page.Stack stack) url =
    IO.get
        |> IO.andThen
            (\model ->
                case model of
                    Ready ready ->
                        let
                            route : route
                            route =
                                toRoute url
                        in
                        if route == ready.route then
                            onUrlChange_ route |> IO.optional sharedOptional

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
                                            , onUrlChange_ route |> IO.optional sharedOptional
                                            ]
                                    )

                    _ ->
                        IO.none
            )


view :
    (shared -> view -> Browser.Document (IO (Spa.Page.Model current previous) msg))
    -> Spa.Page.Stack current previous route msg shared view
    -> Model current previous shared route
    -> Browser.Document (IO (Model current previous shared route) msg)
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
            , body = []
            }
