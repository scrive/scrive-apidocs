module Compose.Optional exposing (Config, ListViewHandler, Msg(..), Override, State, UpdateHandler, ViewHandler, genericLiftView, liftInit, liftListView, liftUpdate, liftView)

import Compose.Handler as Handler
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config config =
    Maybe config


type alias Override =
    Bool


type alias State state =
    Maybe state


type Msg config msg
    = InitMsg config
    | ClearMsg
    | InMsg msg


type alias UpdateHandler config outMsg inMsg state =
    Msg config inMsg
    -> State state
    -> ( State state, Cmd (Either outMsg (Msg config inMsg)) )


type alias ViewHandler config msg state =
    State state
    -> Maybe (Html (Msg config msg))


type alias ListViewHandler config msg state =
    State state
    -> Maybe (List (Html (Msg config msg)))


liftInit :
    (config -> ( state, Cmd msg ))
    -> Config config
    -> ( State state, Cmd (Msg config msg) )
liftInit init mConfig =
    case mConfig of
        Just config ->
            let
                ( state1, cmd1 ) =
                    init config

                state2 =
                    Just state1

                cmd2 =
                    Cmd.map InMsg cmd1
            in
            ( state2, cmd2 )

        Nothing ->
            ( Nothing, Cmd.none )


liftUpdate :
    Override
    -> (config -> ( state, Cmd inMsg ))
    -> Handler.UpdateHandler outMsg inMsg state
    -> UpdateHandler config outMsg inMsg state
liftUpdate override init1 handler1 =
    let
        init2 : config -> ( State state, Cmd (Either outMsg (Msg config inMsg)) )
        init2 config1 =
            let
                ( state1, cmd1 ) =
                    init1 config1

                cmd2 =
                    Cmd.map (Right << InMsg) cmd1
            in
            ( Just state1, cmd2 )
    in
    \msg1 state1 ->
        case msg1 of
            InitMsg config ->
                case state1 of
                    Just _ ->
                        if override then
                            init2 config

                        else
                            ( state1, Cmd.none )

                    Nothing ->
                        init2 config

            ClearMsg ->
                ( Nothing, Cmd.none )

            InMsg msg2 ->
                case state1 of
                    Just state2 ->
                        let
                            ( state3, cmd1 ) =
                                handler1 msg2 state2

                            cmd2 =
                                Util.liftCmd InMsg cmd1
                        in
                        ( Just state3, cmd2 )

                    Nothing ->
                        ( state1, Cmd.none )


genericLiftView :
    ((msg -> Msg config msg) -> fa -> fb)
    -> (state -> fa)
    -> (State state -> Maybe fb)
genericLiftView mapper1 handler1 =
    Maybe.andThen <|
        \state1 ->
            let
                body1 =
                    handler1 state1

                body2 =
                    mapper1 InMsg body1
            in
            Just body2


liftView :
    Handler.ViewHandler msg state
    -> ViewHandler config msg state
liftView =
    genericLiftView <|
        Html.map


liftListView :
    Handler.ListViewHandler msg state
    -> ListViewHandler config msg state
liftListView =
    genericLiftView <|
        List.map
            << Html.map
