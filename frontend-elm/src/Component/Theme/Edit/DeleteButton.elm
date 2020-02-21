module Component.Theme.Edit.DeleteButton exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, initialize, themeSavedMsg, update, view)

import Component.Input.Button as Button
import Component.Theme.Data exposing (Theme)
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    ()


type alias OutMsg =
    Button.OutMsg Theme


type alias Msg =
    Button.Msg Theme


type alias State =
    Button.State


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize _ =
    Button.initialize
        { caption = "Delete"
        , buttonType = Button.Danger
        }


update : UpdateHandler
update =
    Button.update


view : Theme -> ViewHandler
view =
    Button.view


themeSavedMsg : Msg
themeSavedMsg =
    Button.dataSavedMsg
