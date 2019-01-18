module Component.Theme.Single.ColorPair exposing (Config, Msg(..), State(..), UpdateHandler, ViewHandler, backgroundColorState, initialize, stateToColors, textColorState, unMsg, unState, update, view)

import Component.Color.Color as Color
import Component.Theme.Data exposing (ColorPair)
import Component.Theme.Single.ColorPair.Internal as Internal
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type State
    = State Internal.State


type Msg
    = Msg Internal.Msg


type alias Config =
    Internal.Config


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> Html Msg


unMsg : Msg -> Internal.Msg
unMsg (Msg msg) =
    msg


unState : State -> Internal.State
unState (State state) =
    state


backgroundColorState : State -> Color.State
backgroundColorState =
    Internal.backgroundColorState << unState


textColorState : State -> Color.State
textColorState =
    Internal.textColorState << unState


stateToColors : State -> ColorPair
stateToColors =
    Internal.stateToColors << unState


initialize : Config -> ( State, Cmd Msg )
initialize config =
    let
        ( state1, cmd1 ) =
            Internal.initialize config

        state2 =
            State state1

        cmd2 =
            Cmd.map Msg cmd1
    in
    ( state2, cmd2 )


update : UpdateHandler
update (Msg msg) (State state) =
    let
        ( state1, cmd1 ) =
            Internal.update msg state

        state2 =
            State state1

        cmd2 =
            Util.liftCmd Msg cmd1
    in
    ( state2, cmd2 )


view : ViewHandler
view (State state) =
    let
        body1 =
            Internal.view state

        body2 =
            Html.map Msg body1
    in
    body2
