module Compose.Pair exposing (Config, ConsViewHandler, Msg(..), State, UpdateHandler, ViewHandler, concatView, consView, genericLiftView, liftInit, liftUpdate, liftUpdate2, liftView)

import Compose.Handler as Handler
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)
import Tuple exposing (first, second)


type alias Config config1 config2 =
    ( config1, config2 )


type alias State state1 state2 =
    ( state1, state2 )


type Msg msg1 msg2
    = FirstMsg msg1
    | SecondMsg msg2


type alias UpdateHandler outMsg msg1 msg2 state1 state2 =
    Msg msg1 msg2
    -> State state1 state2
    -> ( State state1 state2, Cmd (Either outMsg (Msg msg1 msg2)) )


type alias ViewHandler msg1 msg2 state1 state2 =
    State state1 state2
    -> ( Html (Msg msg1 msg2), Html (Msg msg1 msg2) )


type alias ConsViewHandler msg1 msg2 state1 state2 =
    State state1 state2
    -> List (Html (Msg msg1 msg2))


liftInit :
    Handler.Init config1 msg1 state1
    -> Handler.Init config2 msg2 state2
    -> Config config1 config2
    -> ( State state1 state2, Cmd (Msg msg1 msg2) )
liftInit init1 init2 ( config1, config2 ) =
    let
        ( state1, cmd1 ) =
            init1 config1

        ( state2, cmd2 ) =
            init2 config2

        state3 =
            ( state1, state2 )

        cmd3 =
            Cmd.batch
                [ Cmd.map FirstMsg cmd1
                , Cmd.map SecondMsg cmd2
                ]
    in
    ( state3, cmd3 )


liftUpdate :
    Handler.UpdateHandler outMsg msg1 state1
    -> Handler.UpdateHandler outMsg msg2 state2
    -> UpdateHandler outMsg msg1 msg2 state1 state2
liftUpdate handler1 handler2 msg1 state1 =
    case msg1 of
        FirstMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    handler1 msg2 <| first state1

                state3 =
                    ( state2, second state1 )

                cmd2 =
                    Util.liftCmd FirstMsg cmd1
            in
            ( state3, cmd2 )

        SecondMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    handler2 msg2 <| second state1

                state3 =
                    ( first state1, state2 )

                cmd2 =
                    Util.liftCmd SecondMsg cmd1
            in
            ( state3, cmd2 )


liftUpdate2 :
    Handler.UpdateHandler outMsg1 msg1 state1
    -> Handler.UpdateHandler outMsg2 msg2 state2
    -> UpdateHandler (Msg outMsg1 outMsg2) msg1 msg2 state1 state2
liftUpdate2 handler1 handler2 msg1 state1 =
    case msg1 of
        FirstMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    handler1 msg2 <| first state1

                state3 =
                    ( state2, second state1 )

                cmd2 =
                    Cmd.map (Either.mapBoth FirstMsg FirstMsg) cmd1
            in
            ( state3, cmd2 )

        SecondMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    handler2 msg2 <| second state1

                state3 =
                    ( first state1, state2 )

                cmd2 =
                    Cmd.map (Either.mapBoth SecondMsg SecondMsg) cmd1
            in
            ( state3, cmd2 )


genericLiftView :
    ((msg1 -> Msg msg1 msg2) -> fa1 -> fa2)
    -> ((msg2 -> Msg msg1 msg2) -> fb1 -> fb2)
    -> (state1 -> fa1)
    -> (state2 -> fb1)
    -> (State state1 state2 -> ( fa2, fb2 ))
genericLiftView mapper1 mapper2 handler1 handler2 state1 =
    let
        body1 =
            handler1 <| first state1

        body2 =
            handler2 <| second state1
    in
    ( mapper1 FirstMsg body1
    , mapper2 SecondMsg body2
    )


liftView :
    Handler.ViewHandler msg1 state1
    -> Handler.ViewHandler msg2 state2
    -> ViewHandler msg1 msg2 state1 state2
liftView =
    genericLiftView
        Html.map
        Html.map


consView :
    Handler.ViewHandler msg1 state1
    -> Handler.ListViewHandler msg2 state2
    -> ConsViewHandler msg1 msg2 state1 state2
consView handler1 handler2 =
    let
        handler3 =
            genericLiftView
                Html.map
                (List.map << Html.map)
                handler1
                handler2
    in
    \state1 ->
        let
            ( body1, body2 ) =
                handler3 state1
        in
        body1 :: body2


concatView :
    Handler.ListViewHandler msg1 state1
    -> Handler.ListViewHandler msg2 state2
    -> ConsViewHandler msg1 msg2 state1 state2
concatView handler1 handler2 =
    let
        handler3 =
            genericLiftView
                (List.map << Html.map)
                (List.map << Html.map)
                handler1
                handler2
    in
    \state1 ->
        let
            ( body1, body2 ) =
                handler3 state1
        in
        body1 ++ body2
