module Component.UserGroup.Tabs.BrandingPage.Edit.SaveButton exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, update, view)

import Component.Input.SaveButton as SaveButton
import Component.UserGroup.Data exposing (BrandingFields)
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    ()


type alias OutMsg =
    SaveButton.OutMsg BrandingFields


type alias Msg =
    SaveButton.Msg BrandingFields


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
    SaveButton.initialize "Save"


update : UpdateHandler
update =
    SaveButton.update


view : BrandingFields -> ViewHandler
view =
    SaveButton.view


brandingSavedMsg : Msg
brandingSavedMsg =
    SaveButton.dataSavedMsg
