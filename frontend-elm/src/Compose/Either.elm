module Compose.Either exposing (Config, ListViewHandler, Msg, State, UpdateHandler, ViewHandler, genericLiftView, liftInit, liftListView, liftUpdate, liftView)

import Compose.Handler as Handler
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config config1 config2 =
    Either config1 config2


type alias Msg msg1 msg2 =
    Either msg1 msg2


type alias State state1 state2 =
    Either state1 state2


type alias UpdateHandler outMsg msg1 msg2 state1 state2 =
    Msg msg1 msg2
    -> State state1 state2
    -> ( State state1 state2, Cmd (Either outMsg (Msg msg1 msg2)) )


type alias ViewHandler msg1 msg2 state1 state2 =
    State state1 state2
    -> Html (Msg msg1 msg2)


type alias ListViewHandler msg1 msg2 state1 state2 =
    State state1 state2
    -> List (Html (Msg msg1 msg2))


liftInit :
    Handler.Init config1 msg1 state1
    -> Handler.Init config2 msg2 state2
    -> Config config1 config2
    -> ( State state1 state2, Cmd (Msg msg1 msg2) )
liftInit init1 init2 config1 =
    case config1 of
        Left config2 ->
            let
                ( state1, cmd1 ) =
                    init1 config2

                state2 =
                    Left state1

                cmd2 =
                    Cmd.map Left cmd1
            in
            ( state2, cmd2 )

        Right config2 ->
            let
                ( state1, cmd1 ) =
                    init2 config2

                state2 =
                    Right state1

                cmd2 =
                    Cmd.map Right cmd1
            in
            ( state2, cmd2 )


liftUpdate :
    Handler.UpdateHandler outMsg msg1 state1
    -> Handler.UpdateHandler outMsg msg2 state2
    -> UpdateHandler outMsg msg1 msg2 state1 state2
liftUpdate handler1 handler2 msg1 state1 =
    case ( msg1, state1 ) of
        ( Left msg2, Left state2 ) ->
            let
                ( state3, cmd1 ) =
                    handler1 msg2 state2

                state4 =
                    Left state3

                cmd2 =
                    Util.liftCmd Left cmd1
            in
            ( state4, cmd2 )

        ( Right msg2, Right state2 ) ->
            let
                ( state3, cmd1 ) =
                    handler2 msg2 state2

                state4 =
                    Right state3

                cmd2 =
                    Util.liftCmd Right cmd1
            in
            ( state4, cmd2 )

        _ ->
            ( state1, Cmd.none )


genericLiftView :
    ((msg1 -> Msg msg1 msg2) -> fa -> fc)
    -> ((msg2 -> Msg msg1 msg2) -> fb -> fc)
    -> (state1 -> fa)
    -> (state2 -> fb)
    -> (State state1 state2 -> fc)
genericLiftView mapper1 mapper2 handler1 handler2 state1 =
    case state1 of
        Left state2 ->
            mapper1 Left <|
                handler1 state2

        Right state2 ->
            mapper2 Right <|
                handler2 state2


liftView :
    Handler.ViewHandler msg1 state1
    -> Handler.ViewHandler msg2 state2
    -> ViewHandler msg1 msg2 state1 state2
liftView =
    genericLiftView
        Html.map
        Html.map


liftListView :
    Handler.ListViewHandler msg1 state1
    -> Handler.ListViewHandler msg2 state2
    -> ListViewHandler msg1 msg2 state1 state2
liftListView =
    genericLiftView
        (List.map << Html.map)
        (List.map << Html.map)
