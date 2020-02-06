module Component.BrandedDomain.Tabs exposing (Config, Msg(..), OutMsg, State(..), UpdateHandler, ViewHandler, brandingSavedMsg, initialize, themeSavedMsg, unMsg, unState, update, view)

import Component.BrandedDomain.Tabs.Internal as Internal
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    Internal.Config


type State
    = State Internal.State


type Msg
    = Msg Internal.Msg


type alias OutMsg =
    Internal.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> ( Html Msg, Html Msg )


unMsg : Msg -> Internal.Msg
unMsg (Msg msg) =
    msg


unState : State -> Internal.State
unState (State state) =
    state


initialize : Config -> ( State, Cmd Msg )
initialize config1 =
    let
        ( state1, cmd1 ) =
            Internal.initialize config1

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
        ( body1, body2 ) =
            Internal.view state
    in
    ( Html.map Msg body1
    , Html.map Msg body2
    )


brandingSavedMsg : Msg
brandingSavedMsg =
    Msg Internal.brandingSavedMsg


themeSavedMsg : Msg
themeSavedMsg =
    Msg Internal.themeSavedMsg
