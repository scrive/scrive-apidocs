module Compose.List exposing (Config, Msg(..), State, UpdateHandler, ViewAtHandler, ViewHandler, genericLiftView, genericLiftViewAt, liftInit, liftUpdate, liftView, liftViewAt, stateAt)

import Compose.Handler as Handler
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)
import List.Extra as List
import Tuple exposing (first)


type alias Config config =
    List config


type Msg msg
    = ForMsg Int msg


type alias State state =
    List state


type alias UpdateHandler outMsg inMsg state =
    Msg inMsg
    -> State state
    -> ( State state, Cmd (Either outMsg (Msg inMsg)) )


type alias ViewHandler msg state =
    State state
    -> List (Html (Msg msg))


type alias ViewAtHandler msg state =
    Int
    -> State state
    -> Maybe (Html (Msg msg))


liftInit :
    (config -> ( state, Cmd msg ))
    -> Config config
    -> ( State state, Cmd (Msg msg) )
liftInit initElem configs =
    let
        res =
            List.map initElem configs

        states =
            List.map first res

        cmds =
            List.indexedMap
                (\i ( _, cmd2 ) -> Cmd.map (ForMsg i) cmd2)
                res

        cmd =
            Cmd.batch cmds
    in
    ( states, cmd )


liftUpdate :
    Handler.UpdateHandler outMsg inMsg state
    -> UpdateHandler outMsg inMsg state
liftUpdate handler1 =
    let
        doUpdate :
            Int
            -> Int
            -> inMsg
            -> State state
            -> ( State state, Cmd (Either outMsg (Msg inMsg)) )
        doUpdate i pos msg1 state2 =
            case List.uncons state2 of
                Just ( state3, restStates1 ) ->
                    if i == 0 then
                        let
                            ( state4, cmd1 ) =
                                handler1 msg1 state3

                            cmd2 =
                                Util.liftCmd (ForMsg pos) cmd1

                            state5 =
                                state4 :: restStates1
                        in
                        ( state5, cmd2 )

                    else
                        let
                            ( restStates2, cmd ) =
                                doUpdate (i - 1) (pos + 1) msg1 restStates1
                        in
                        ( state3 :: restStates2, cmd )

                Nothing ->
                    ( state2, Cmd.none )
    in
    \(ForMsg i msg) states ->
        doUpdate i 0 msg states


genericLiftView :
    ((msg -> Msg msg) -> fa -> fb)
    -> (state -> fa)
    -> (State state -> List fb)
genericLiftView mapper1 handler1 =
    let
        doView :
            Int
            -> State state
            -> List fb
        doView pos states1 =
            case List.uncons states1 of
                Just ( state1, restStates1 ) ->
                    let
                        body1 =
                            handler1 state1

                        body2 =
                            mapper1 (ForMsg pos) body1

                        restBody =
                            doView (pos + 1) restStates1
                    in
                    body2 :: restBody

                Nothing ->
                    []
    in
    \states ->
        doView 0 states


stateAt : Int -> State state -> Maybe state
stateAt =
    List.getAt


genericLiftViewAt :
    ((msg -> Msg msg) -> fa -> fb)
    -> (state -> fa)
    -> (Int -> State state -> Maybe fb)
genericLiftViewAt mapper1 handler1 i states =
    let
        mState =
            stateAt i states

        mBody1 =
            Maybe.map handler1 mState

        mBody2 =
            Maybe.map (mapper1 (ForMsg i)) mBody1
    in
    mBody2


liftView :
    Handler.ViewHandler msg state
    -> ViewHandler msg state
liftView =
    genericLiftView <|
        Html.map


liftViewAt :
    Handler.ViewHandler msg state
    -> ViewAtHandler msg state
liftViewAt =
    genericLiftViewAt <|
        Html.map
