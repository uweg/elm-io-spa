module Spa exposing (Model, spa)

import Browser
import Browser.Navigation
import IO exposing (IO)
import Monocle.Optional exposing (Optional)
import Spa.Page
import Url exposing (Url)


type Model current previous context route error
    = Loading
    | Ready
        { key : Browser.Navigation.Key
        , context : context
        , page : Spa.Page.Model current previous
        , route : route
        , error : Maybe error
        }


pageOptional :
    Optional
        (Model current previous context route error)
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


spa :
    context
    -> view
    -> (msg -> IO (Model () () context route error) msg)
    -> (Url -> route)
    -> (context -> b -> Browser.Document (IO (Model () () context route error) msg))
    ->
        ((IO (Spa.Page.Model () ()) msg -> IO (Model () () context route error) msg)
         -> view
         -> b
        )
    -> IO.Program flags (Model () () context route error) msg
spa context defaultView update fromUrl toDocument mapView =
    Spa.Page.setup defaultView
        |> (\stack ->
                IO.application
                    { init = init context fromUrl stack
                    , subscriptions = subscriptions stack
                    , update = update
                    , onUrlRequest = onUrlRequest
                    , onUrlChange = onUrlChange fromUrl stack
                    , view = view mapView toDocument stack
                    }
           )


init :
    context
    -> (Url -> route)
    -> Spa.Page.Stack current previous route msg context view
    -> flags
    -> Url
    -> Browser.Navigation.Key
    -> ( Model current previous context route error, IO (Model current previous context route error) msg )
init context fromUrl (Spa.Page.Stack stack) flags url key =
    let
        route =
            fromUrl url

        page : Spa.Page.Model current previous
        page =
            stack.init context route Nothing
                |> Tuple.first
    in
    ( Ready
        { key = key
        , page = page
        , route = route
        , context = context
        , error = Nothing
        }
    , IO.none
    )


subscriptions :
    Spa.Page.Stack current previous route msg context view
    -> Model current previous context route error
    -> Sub (IO (Model current previous context route error) msg)
subscriptions (Spa.Page.Stack stack) model =
    case model of
        Ready ready ->
            stack.subscriptions ready.page
                |> Sub.map (IO.optional pageOptional)

        Loading ->
            Sub.none


onUrlRequest : Browser.UrlRequest -> IO (Model current previous context route error) msg
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
    -> IO (Model current previous context route error) msg
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
                            Ready { ready | page = pageModel, route = route, error = Nothing }
                                |> IO.set
                                |> IO.andThen (\_ -> io |> IO.optional pageOptional)

                    _ ->
                        IO.none
            )


view :
    ((IO (Spa.Page.Model current previous) msg
      -> IO (Model current previous context route error) msg
     )
     -> view
     -> b
    )
    -> (context -> b -> Browser.Document (IO (Model current previous context route error) msg))
    -> Spa.Page.Stack current previous route msg context view
    -> Model current previous context route error
    -> Browser.Document (IO (Model current previous context route error) msg)
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
