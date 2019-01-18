module Compose.Cursor exposing (Config, Msg(..), State, UpdateHandler, ViewHandler, currentItemState, genericLiftView, liftInit, liftUpdate, liftView)

import Compose.Handler as Handler
import Compose.List as ListHandler
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)
import List.Extra as List


type alias Config config =
    { initialIndex : Maybe Int
    , itemsConfig : ListHandler.Config config
    }


type Msg msg
    = SelectMsg (Maybe Int)
    | ItemsMsg (ListHandler.Msg msg)


type alias State state =
    { currentIndex : Maybe Int
    , itemsState : ListHandler.State state
    }


type alias UpdateHandler outMsg inMsg state =
    Msg inMsg
    -> State state
    -> ( State state, Cmd (Either outMsg (Msg inMsg)) )


type alias ViewHandler msg state =
    State state
    -> Maybe (Html (Msg msg))


liftInit :
    (config -> ( state, Cmd msg ))
    -> Config config
    -> ( State state, Cmd (Msg msg) )
liftInit initElem =
    let
        init =
            ListHandler.liftInit initElem
    in
    \config ->
        let
            ( state1, cmd1 ) =
                init config.itemsConfig

            state2 =
                { currentIndex = config.initialIndex
                , itemsState = state1
                }

            cmd2 =
                Cmd.map ItemsMsg cmd1
        in
        ( state2, cmd2 )


liftUpdate :
    Handler.UpdateHandler outMsg inMsg state
    -> UpdateHandler outMsg inMsg state
liftUpdate handler1 =
    let
        handler2 =
            ListHandler.liftUpdate handler1
    in
    \msg1 state1 ->
        case msg1 of
            SelectMsg index ->
                let
                    state2 =
                        { state1 | currentIndex = index }
                in
                ( state2, Cmd.none )

            ItemsMsg msg2 ->
                let
                    ( state2, cmd2 ) =
                        handler2 msg2 state1.itemsState

                    state3 =
                        { state1 | itemsState = state2 }

                    cmd3 =
                        Util.liftCmd ItemsMsg cmd2
                in
                ( state3, cmd3 )


genericLiftView :
    ((msg -> ListHandler.Msg msg) -> fa -> fb)
    -> (state -> fa)
    -> (State state -> Maybe fb)
genericLiftView mapper1 handler1 =
    let
        handler2 =
            ListHandler.genericLiftViewAt
                mapper1
                handler1
    in
    \state ->
        let
            mIndex =
                state.currentIndex

            mBody1 =
                Maybe.andThen
                    (\i -> handler2 i state.itemsState)
                    mIndex
        in
        mBody1


liftView :
    Handler.ViewHandler msg state
    -> ViewHandler msg state
liftView handler =
    Maybe.map (Html.map ItemsMsg)
        << genericLiftView
            Html.map
            handler


currentItemState : State state -> Maybe state
currentItemState state =
    state.currentIndex
        |> Maybe.andThen
            (\index ->
                List.getAt index state.itemsState
            )
