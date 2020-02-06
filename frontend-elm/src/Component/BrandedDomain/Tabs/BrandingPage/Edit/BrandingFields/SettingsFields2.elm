module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.SettingsFields2 exposing (Config, Init, Msg, SettingsFields, State, UpdateHandler, ViewHandler, initialize, stateToSettingsFields, unMsg, unState, update, view)

import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.SettingsFields as Internal
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias SettingsFields =
    Internal.SettingsFields


type alias Config =
    Internal.Config


type State
    = State Internal.State


type Msg
    = Msg Internal.Msg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> List (Html Msg)


type alias Init =
    Config -> ( State, Cmd Msg )


unState : State -> Internal.State
unState (State state) =
    state


unMsg : Msg -> Internal.Msg
unMsg (Msg msg) =
    msg


initialize : Init
initialize config =
    let
        ( state, cmd ) =
            Internal.initialize config
    in
    ( State state, Cmd.map Msg cmd )


update : UpdateHandler
update (Msg msg) (State state) =
    let
        ( state2, cmd ) =
            Internal.update msg state
    in
    ( State state2, Util.liftCmd Msg cmd )


view : ViewHandler
view (State state) =
    List.map (Html.map Msg) <|
        Internal.view state


stateToSettingsFields : State -> SettingsFields
stateToSettingsFields (State state) =
    Internal.stateToSettingsFields state
