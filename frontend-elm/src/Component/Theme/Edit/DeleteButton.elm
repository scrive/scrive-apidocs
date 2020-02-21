module Component.Theme.Edit.DeleteButton exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, initialize, themeSavedMsg, update, view)

import Component.Input.SaveButton as SaveButton
import Component.Theme.Data exposing (Theme)
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    ()


type alias OutMsg =
    SaveButton.OutMsg Theme


type alias Msg =
    SaveButton.Msg Theme


type alias State =
    SaveButton.State


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize _ =
    SaveButton.initialize "Delete"


update : UpdateHandler
update =
    SaveButton.update


view : Theme -> ViewHandler
view =
    SaveButton.view


themeSavedMsg : Msg
themeSavedMsg =
    SaveButton.dataSavedMsg
