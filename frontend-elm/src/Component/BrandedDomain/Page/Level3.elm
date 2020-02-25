module Component.BrandedDomain.Page.Level3 exposing (Config, FailConfig, Init, Msg, OutMsg, State, SuccessConfig, UpdateHandler, ViewHandler, clearMsg, doneDeleteThemeMsg, doneCreateThemeMsg, doneSaveBrandingMsg, doneSaveThemeMsg, initEmpty, initErrorMsg, initFailure, initMsg, initSuccess, initThemesMsg, initialize, update, view)

import Component.BrandedDomain.Page.Level2 as Page
import Component.Error.Fail as Fail
import Compose.Handler as Handler
import Compose.Loader as Loader
import Either exposing (Either(..))
import Html exposing (Html)


type alias OutMsg =
    Page.OutMsg


type alias Config =
    Loader.Config Fail.Config Page.Config


type alias State =
    Loader.State Fail.State Page.State


type alias Msg =
    Loader.Msg Fail.Config Page.Config Fail.Msg Page.Msg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Maybe (Html Msg)


type alias Init =
    Config -> ( State, Cmd Msg )


type alias SuccessConfig =
    Page.Config


type alias FailConfig =
    Fail.Config


initialize : Init
initialize =
    Loader.liftInit
        Fail.initialize
        Page.initialize


update : UpdateHandler
update =
    Loader.liftUpdate
        Fail.initialize
        Page.initialize
        (Handler.outerMapUpdate never Fail.update)
        Page.update


view : ViewHandler
view =
    Loader.liftView
        Fail.view
        Page.view


clearMsg : Msg
clearMsg =
    Loader.clearMsg


initMsg : Either Fail.Config Page.Config -> Msg
initMsg =
    Loader.loadMsg


initThemesMsg : Page.Config -> Msg
initThemesMsg =
    Loader.loadRightMsg


initErrorMsg : String -> Msg
initErrorMsg =
    Loader.loadLeftMsg


doneSaveBrandingMsg : Msg
doneSaveBrandingMsg =
    Loader.inRightMsg Page.doneSaveBrandingMsg


doneSaveThemeMsg : Msg
doneSaveThemeMsg =
    Loader.inRightMsg Page.doneSaveThemeMsg


doneDeleteThemeMsg : Msg
doneDeleteThemeMsg =
    Loader.inRightMsg Page.doneDeleteThemeMsg

doneCreateThemeMsg : Msg
doneCreateThemeMsg =
    Loader.inRightMsg Page.doneCreateThemeMsg


initEmpty : ( State, Cmd Msg )
initEmpty =
    initialize Nothing


initSuccess : Page.Config -> ( State, Cmd Msg )
initSuccess =
    initialize << Just << Right


initFailure : String -> ( State, Cmd Msg )
initFailure =
    initialize << Just << Left
