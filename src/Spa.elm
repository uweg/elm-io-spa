module Spa exposing (Model, toApplication)

import Browser
import Browser.Navigation
import CmdM exposing (CmdM)
import IO exposing (IO)
import Json.Encode as E
import Monocle.Optional exposing (Optional)
import Spa.Page
import Url exposing (Url)


type Model current previous context route
    = Loading
    | Ready
        { key : Browser.Navigation.Key
        , context : context
        , page : Spa.Page.Model current previous
        , route : route
        }


pageOptional :
    Optional
        (Model current previous context route)
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


toApplication :
    (E.Value -> CmdM context)
    -> (msg -> IO (Model current previous context route) msg)
    -> (Url -> route)
    -> (context -> a -> Browser.Document (IO (Model current previous context route) msg))
    -> ((IO (Spa.Page.Model current previous) msg -> IO (Model current previous context route) msg) -> view -> a)
    -> Spa.Page.Stack current previous route msg context view
    -> IO.Program E.Value (Model current previous context route) msg
toApplication context update fromUrl toDocument mapView stack =
    IO.application
        { init = init context fromUrl stack
        , subscriptions = subscriptions stack
        , update = update
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange fromUrl stack
        , view = view mapView toDocument stack
        }


init :
    (E.Value -> CmdM context)
    -> (Url -> route)
    -> Spa.Page.Stack current previous route msg context view
    -> E.Value
    -> Url
    -> Browser.Navigation.Key
    ->
        ( Model current previous context route
        , IO (Model current previous context route) msg
        )
init context fromUrl (Spa.Page.Stack stack) flags url key =
    ( Loading
    , context flags
        |> IO.liftM
        |> IO.andThen
            (\context_ ->
                let
                    route =
                        fromUrl url

                    page : Spa.Page.Model current previous
                    page =
                        stack.init context_ route Nothing
                            |> Tuple.first
                in
                IO.set
                    (Ready
                        { key = key
                        , page = page
                        , route = route
                        , context = context_
                        }
                    )
                    |> IO.andThen (\_ -> IO.none)
            )
    )


subscriptions :
    Spa.Page.Stack current previous route msg context view
    -> Model current previous context route
    -> Sub (IO (Model current previous context route) msg)
subscriptions (Spa.Page.Stack stack) model =
    case model of
        Ready ready ->
            stack.subscriptions ready.page
                |> Sub.map (IO.optional pageOptional)

        Loading ->
            Sub.none


onUrlRequest : Browser.UrlRequest -> IO (Model current previous context route) msg
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
    -> Spa.Page.Stack current previous route msg context view
    -> Url
    -> IO (Model current previous context route) msg
onUrlChange fromUrl (Spa.Page.Stack stack) url =
    IO.get
        |> IO.andThen
            (\model ->
                case model of
                    Ready ready ->
                        let
                            route =
                                fromUrl url
                        in
                        if route == ready.route then
                            IO.none

                        else
                            let
                                ( pageModel, io ) =
                                    stack.init ready.context route (Just ready.page)

                                --changePage stack route model.identity (Just model.page)
                            in
                            Ready { ready | page = pageModel, route = route }
                                |> IO.set
                                |> IO.andThen (\_ -> io |> IO.optional pageOptional)

                    _ ->
                        IO.none
            )


view :
    ((IO (Spa.Page.Model current previous) msg
      -> IO (Model current previous context route) msg
     )
     -> view
     -> b
    )
    -> (context -> b -> Browser.Document (IO (Model current previous context route) msg))
    -> Spa.Page.Stack current previous route msg context view
    -> Model current previous context route
    -> Browser.Document (IO (Model current previous context route) msg)
view mapView toDocument (Spa.Page.Stack stack) model =
    case model of
        Ready ready ->
            toDocument ready.context
                (stack.view ready.context ready.route ready.page
                    |> mapView (IO.optional pageOptional)
                )

        Loading ->
            { title = ""
            , body = []
            }
