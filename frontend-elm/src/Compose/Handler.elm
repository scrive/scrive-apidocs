module Compose.Handler exposing (Init, ListViewHandler, OptionalViewHandler, UpdateHandler, ViewHandler, genericMapViewHandler, mapInit, mapListViewHandler, mapOptionalViewHandler, mapViewHandler, outerMapUpdate, simpleUpdateHandler, toListViewHandler)

import Either exposing (Either(..))
import Html exposing (Html)


type alias Init config msg state =
    config -> ( state, Cmd msg )


type alias UpdateHandler outMsg inMsg state =
    inMsg -> state -> ( state, Cmd (Either outMsg inMsg) )


type alias ViewHandler msg state =
    state -> Html msg


type alias ListViewHandler msg state =
    state -> List (Html msg)


type alias OptionalViewHandler msg state =
    state -> Maybe (Html msg)


simpleUpdateHandler :
    (msg -> state -> ( state, Cmd msg ))
    -> UpdateHandler Never msg state
simpleUpdateHandler handler1 msg1 state1 =
    let
        ( state2, cmd1 ) =
            handler1 msg1 state1

        cmd2 =
            Cmd.map Right cmd1
    in
    ( state2, cmd2 )


outerMapUpdate :
    (msg1 -> msg2)
    -> UpdateHandler msg1 inMsg state
    -> UpdateHandler msg2 inMsg state
outerMapUpdate mapper1 handler1 =
    let
        mapper2 =
            Either.mapLeft mapper1
    in
    \msg1 state1 ->
        let
            ( state2, cmd1 ) =
                handler1 msg1 state1

            cmd2 =
                Cmd.map mapper2 cmd1
        in
        ( state2, cmd2 )


mapInit :
    (msg1 -> msg2)
    -> (config -> ( state, Cmd msg1 ))
    -> (config -> ( state, Cmd msg2 ))
mapInit mapper init config =
    let
        ( state1, cmd1 ) =
            init config

        cmd2 =
            Cmd.map mapper cmd1
    in
    ( state1, cmd2 )


genericMapViewHandler :
    ((msg1 -> msg2) -> fa -> fb)
    -> (msg1 -> msg2)
    -> (state -> fa)
    -> (state -> fb)
genericMapViewHandler mapper1 mapper2 handler1 =
    let
        mapper3 =
            mapper1 mapper2
    in
    \state1 ->
        let
            body1 =
                handler1 state1

            body2 =
                mapper3 body1
        in
        body2


mapViewHandler :
    (msg1 -> msg2)
    -> ViewHandler msg1 state
    -> ViewHandler msg2 state
mapViewHandler =
    genericMapViewHandler <|
        Html.map


toListViewHandler :
    ViewHandler msg1 state
    -> ListViewHandler msg1 state
toListViewHandler handler1 state1 =
    [ handler1 state1 ]


mapListViewHandler :
    (msg1 -> msg2)
    -> ListViewHandler msg1 state
    -> ListViewHandler msg2 state
mapListViewHandler =
    genericMapViewHandler <|
        List.map
            << Html.map


mapOptionalViewHandler :
    (msg1 -> msg2)
    -> OptionalViewHandler msg1 state
    -> OptionalViewHandler msg2 state
mapOptionalViewHandler =
    genericMapViewHandler <|
        Maybe.map
            << Html.map
