module Compose.Loader exposing (Config, Init, Msg, State, UpdateHandler, ViewHandler, clearMsg, inLeftMsg, inMsg, inRightMsg, liftInit, liftListView, liftUpdate, liftView, loadLeftMsg, loadMsg, loadRightMsg)

import Compose.Either as Either
import Compose.Optional as Optional
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config config1 config2 =
    Optional.Config (Either.Config config1 config2)


type alias Msg config1 config2 msg1 msg2 =
    Optional.Msg (Either.Config config1 config2) (Either.Msg msg1 msg2)


type alias State state1 state2 =
    Optional.State (Either.State state1 state2)


type alias Init config1 config2 msg1 msg2 state1 state2 =
    Config config1 config2
    -> ( State state1 state2, Cmd (Msg config1 config2 msg1 msg2) )


type alias UpdateHandler config1 config2 outMsg msg1 msg2 state1 state2 =
    Msg config1 config2 msg1 msg2
    -> State state1 state2
    -> ( State state1 state2, Cmd (Either outMsg (Msg config1 config2 msg1 msg2)) )


type alias ViewHandler config1 config2 msg1 msg2 state1 state2 =
    State state1 state2
    -> Maybe (Html (Msg config1 config2 msg1 msg2))


liftInit :
    (config1 -> ( state1, Cmd msg1 ))
    -> (config2 -> ( state2, Cmd msg2 ))
    -> Init config1 config2 msg1 msg2 state1 state2
liftInit init1 init2 =
    Optional.liftInit <|
        Either.liftInit init1 init2


liftUpdate :
    (config1 -> ( state1, Cmd msg1 ))
    -> (config2 -> ( state2, Cmd msg2 ))
    -> (msg1 -> state1 -> ( state1, Cmd (Either outMsg msg1) ))
    -> (msg2 -> state2 -> ( state2, Cmd (Either outMsg msg2) ))
    -> UpdateHandler config1 config2 outMsg msg1 msg2 state1 state2
liftUpdate init1 init2 update1 update2 =
    Optional.liftUpdate True
        (Either.liftInit init1 init2)
    <|
        Either.liftUpdate update1 update2


liftView :
    (state1 -> Html msg1)
    -> (state2 -> Html msg2)
    -> State state1 state2
    -> Maybe (Html (Msg config1 config2 msg1 msg2))
liftView view1 view2 =
    Optional.liftView <|
        Either.liftView
            view1
            view2


liftListView :
    (state1 -> List (Html msg1))
    -> (state2 -> List (Html msg2))
    -> State state1 state2
    -> Maybe (List (Html (Msg config1 config2 msg1 msg2)))
liftListView view1 view2 =
    Optional.liftListView <|
        Either.liftListView
            view1
            view2


loadMsg : Either config1 config2 -> Msg config1 config2 msg1 msg2
loadMsg =
    Optional.InitMsg


loadLeftMsg : config1 -> Msg config1 config2 msg1 msg2
loadLeftMsg config =
    loadMsg <|
        Left config


loadRightMsg : config2 -> Msg config1 config2 msg1 msg2
loadRightMsg config =
    loadMsg <|
        Right config


clearMsg : Msg config1 config2 msg1 msg2
clearMsg =
    Optional.ClearMsg


inMsg : Either msg1 msg2 -> Msg config1 config2 msg1 msg2
inMsg =
    Optional.InMsg


inLeftMsg : msg1 -> Msg config1 config2 msg1 msg2
inLeftMsg =
    inMsg << Left


inRightMsg : msg2 -> Msg config1 config2 msg1 msg2
inRightMsg =
    inMsg << Right
