module Component.BrandedDomain.Page.Four exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, brandingSavedMsg, clearMsg, initEmpty, initErrorMsg, initFailure, initMsg, initSuccess, initThemesMsg, initialize, themeSavedMsg, unMsg, unState, update, view)

import Component.BrandedDomain.Page.Three as Page
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    Page.Config


type State
    = State Page.State


type Msg
    = Msg Page.Msg


type alias OutMsg =
    Page.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Maybe (Html Msg)


type alias Init =
    Config -> ( State, Cmd Msg )


unMsg : Msg -> Page.Msg
unMsg (Msg msg) =
    msg


unState : State -> Page.State
unState (State state) =
    state


initialize : Init
initialize =
    \config ->
        let
            ( state, cmd ) =
                Page.initialize config
        in
        ( State state, Cmd.map Msg cmd )


update : UpdateHandler
update (Msg msg) (State state) =
    let
        ( state2, cmd ) =
            Page.update msg state
    in
    ( State state2, Util.liftCmd Msg cmd )


view : ViewHandler
view (State state) =
    Maybe.map (Html.map Msg) <| Page.view state


clearMsg : Msg
clearMsg =
    Msg Page.clearMsg


initMsg : Either Page.FailConfig Page.SuccessConfig -> Msg
initMsg =
    Msg << Page.initMsg


initThemesMsg : Page.SuccessConfig -> Msg
initThemesMsg =
    Msg << Page.initThemesMsg


initErrorMsg : String -> Msg
initErrorMsg =
    Msg << Page.initErrorMsg


brandingSavedMsg : Msg
brandingSavedMsg =
    Msg Page.brandingSavedMsg


themeSavedMsg : Msg
themeSavedMsg =
    Msg Page.themeSavedMsg


initEmpty : ( State, Cmd Msg )
initEmpty =
    let
        ( state, cmd ) =
            Page.initEmpty
    in
    ( State state, Cmd.map Msg cmd )


initSuccess : Page.SuccessConfig -> ( State, Cmd Msg )
initSuccess config =
    let
        ( state, cmd ) =
            Page.initSuccess config
    in
    ( State state, Cmd.map Msg cmd )


initFailure : String -> ( State, Cmd Msg )
initFailure err =
    let
        ( state, cmd ) =
            Page.initFailure err
    in
    ( State state, Cmd.map Msg cmd )
