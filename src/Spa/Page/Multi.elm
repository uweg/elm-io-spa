module Spa.Page.Multi exposing
    ( Model
    , add
    , setup
    , toPage
    )

import IO exposing (IO)
import Monocle.Lens exposing (Lens)
import Spa.Page exposing (Page)


type Model current previous
    = Model current previous


type Multi current previous msg shared flags view
    = Multi
        { init : shared -> flags -> ( Model current previous, IO (Model current previous) msg )
        , subscriptions : Maybe (Model current previous -> Sub (IO (Model current previous) msg))
        , view : shared -> flags -> Model current previous -> List view
        , flagsChanged : Maybe (shared -> flags -> IO (Model current previous) msg)
        }


currentLens : Lens (Model current previous) current
currentLens =
    { get = \(Model v _) -> v
    , set = \v (Model _ prev) -> Model v prev
    }


previousLens : Lens (Model current previous) previous
previousLens =
    { get = \(Model _ v) -> v
    , set = \v (Model current _) -> Model current v
    }


setup :
    ((IO current msg -> IO (Model current ()) msg) -> (currentView -> view))
    -> (flags -> currentFlags)
    -> Page current currentFlags msg shared currentView
    -> Multi current () msg shared flags view
setup mapView mapFlags page =
    Multi
        { init =
            \shared flags ->
                let
                    ( model, io ) =
                        page.init shared (mapFlags flags)
                in
                ( Model model (), io |> IO.lens currentLens )
        , subscriptions =
            page.subscriptions
                |> Maybe.map
                    (\subscriptions ->
                        \(Model model _) ->
                            subscriptions model
                                |> Sub.map (IO.lens currentLens)
                    )
        , view =
            \shared flags (Model model _) ->
                page.view shared (mapFlags flags) model
                    |> mapView (IO.lens currentLens)
                    |> List.singleton
        , flagsChanged = Nothing
        }


add :
    ( (IO current msg -> IO (Model current previous) msg) -> (currentView -> view)
    , (IO previous msg -> IO (Model current previous) msg) -> (previousView -> view)
    )
    -> (flags -> currentFlags)
    -> Page current currentFlags msg shared currentView
    -> Multi previousCurrent previousPrevious msg shared flags previousView
    -> Multi current (Model previousCurrent previousPrevious) msg shared flags view
add ( mapView, mapPreviousView ) mapFlags page (Multi prev) =
    let
        init :
            shared
            -> flags
            ->
                ( Model current (Model previousCurrent previousPrevious)
                , IO (Model current (Model previousCurrent previousPrevious)) msg
                )
        init shared flags =
            let
                ( model, io ) =
                    page.init shared (mapFlags flags)

                ( prevModel, prevIO ) =
                    prev.init shared flags
            in
            ( Model model prevModel
            , IO.batchM
                [ io |> IO.lens currentLens
                , prevIO |> IO.lens previousLens
                ]
            )

        view :
            shared
            -> flags
            -> Model current (Model previousCurrent previousPrevious)
            -> List view
        view shared flags (Model currentModel previousModel) =
            (page.view shared (mapFlags flags) currentModel |> mapView (IO.lens currentLens))
                :: (prev.view shared flags previousModel |> List.map (mapPreviousView (IO.lens previousLens)))

        subscriptions :
            Maybe
                (Model current (Model previousCurrent previousPrevious)
                 -> Sub (IO (Model current (Model previousCurrent previousPrevious)) msg)
                )
        subscriptions =
            [ prev.subscriptions
                |> Maybe.map
                    (\subscriptions_ ->
                        \(Model _ model) -> subscriptions_ model |> Sub.map (IO.lens previousLens)
                    )
            , page.subscriptions
                |> Maybe.map
                    (\subscriptions_ ->
                        \(Model model _) -> subscriptions_ model |> Sub.map (IO.lens currentLens)
                    )
            ]
                |> List.filterMap (\a -> a)
                |> (\subscriptions_ ->
                        case subscriptions_ of
                            [] ->
                                Nothing

                            _ ->
                                Just
                                    (\model ->
                                        subscriptions_
                                            |> List.map (\subscriptions__ -> subscriptions__ model)
                                            |> Sub.batch
                                    )
                   )

        flagsChanged :
            Maybe
                (shared
                 -> flags
                 -> IO (Model current (Model previousCurrent previousPrevious)) msg
                )
        flagsChanged =
            [ prev.flagsChanged
                |> Maybe.map
                    (\flagsChanged_ ->
                        \shared flags ->
                            flagsChanged_ shared flags
                                |> IO.lens previousLens
                    )
            , page.flagsChanged
                |> Maybe.map
                    (\flagsChanged_ ->
                        \shared flags ->
                            flagsChanged_ shared (mapFlags flags)
                                |> IO.lens currentLens
                    )
            ]
                |> List.filterMap (\a -> a)
                |> (\flagsChanged_ ->
                        case flagsChanged_ of
                            [] ->
                                Nothing

                            _ ->
                                Just
                                    (\shared flags ->
                                        flagsChanged_
                                            |> List.map (\flagsChanged__ -> flagsChanged__ shared flags)
                                            |> IO.batchM
                                    )
                   )
    in
    Multi
        { init = init
        , subscriptions = subscriptions
        , view = view
        , flagsChanged = flagsChanged
        }


toPage :
    (List view -> view)
    -> Multi current previous msg shared flags view
    -> Page (Model current previous) flags msg shared view
toPage concatViews (Multi multi) =
    { init = multi.init
    , subscriptions = multi.subscriptions
    , flagsChanged = multi.flagsChanged
    , view =
        \shared flags model ->
            multi.view shared flags model
                |> List.reverse
                |> concatViews
    }
