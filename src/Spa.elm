module Spa exposing (Model, toApplication)

import Browser
import Browser.Navigation
import Html
import IO exposing (IO)
import Json.Encode as E
import Monocle.Optional exposing (Optional)
import Spa.Page
import Task exposing (Task)
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


contextOptional : Optional (Model current previous context route) context
contextOptional =
    { getOption =
        \model ->
            case model of
                Loading ->
                    Nothing

                Ready data ->
                    Just data.context
    , set =
        \context model ->
            case model of
                Loading ->
                    model

                Ready data ->
                    Ready { data | context = context }
    }


toApplication :
    (E.Value -> Task Never context)
    -> (Browser.Navigation.Key -> msg -> IO context msg)
    -> (Url -> route)
    -> (context -> viewB -> Browser.Document (IO context msg))
    -> ((IO (Spa.Page.Model current previous) msg -> IO (Model current previous context route) msg) -> viewA -> viewB)
    -> Spa.Page.Stack current previous route msg context viewA
    -> IO.Program E.Value (Model current previous context route) msg
toApplication initContext update_ fromUrl toDocument mapView stack =
    IO.application
        { init = init initContext fromUrl stack
        , subscriptions = subscriptions stack
        , update = update update_
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange fromUrl stack
        , view = view mapView toDocument stack
        }


init :
    (E.Value -> Task Never context)
    -> (Url -> route)
    -> Spa.Page.Stack current previous route msg context view
    -> E.Value
    -> Url
    -> Browser.Navigation.Key
    ->
        ( Model current previous context route
        , IO (Model current previous context route) msg
        )
init initContext fromUrl (Spa.Page.Stack stack) flags url key =
    ( Loading
    , initContext flags
        |> Task.perform (\a -> a)
        |> IO.lift
        |> IO.andThen
            (\context_ ->
                let
                    route : route
                    route =
                        fromUrl url

                    ( page, io ) =
                        stack.init context_ route Nothing
                in
                IO.set
                    (Ready
                        { key = key
                        , page = page
                        , route = route
                        , context = context_
                        }
                    )
                    |> IO.andThen (\_ -> io |> IO.optional pageOptional)
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


update update_ msg =
    IO.get
        |> IO.andThen
            (\model ->
                case model of
                    Ready ready ->
                        update_ ready.key msg |> IO.optional contextOptional

                    Loading ->
                        IO.none
            )


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
     -> viewA
     -> viewB
    )
    -> (context -> viewB -> Browser.Document (IO context msg))
    -> Spa.Page.Stack current previous route msg context viewA
    -> Model current previous context route
    -> Browser.Document (IO (Model current previous context route) msg)
view mapView toDocument (Spa.Page.Stack stack) model =
    case model of
        Ready ready ->
            toDocument ready.context
                (stack.view ready.context ready.route ready.page
                    |> mapView (IO.optional pageOptional)
                )
                |> (\document ->
                        { title = document.title
                        , body =
                            document.body
                                |> List.map (Html.map (IO.optional contextOptional))
                        }
                   )

        Loading ->
            { title = ""
            , body = []
            }
