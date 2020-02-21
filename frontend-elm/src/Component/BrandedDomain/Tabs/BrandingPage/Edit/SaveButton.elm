module Component.BrandedDomain.Tabs.BrandingPage.Edit.SaveButton exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, update, view)

import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields exposing (BrandingFields)
import Component.Input.Button as Button
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    ()


type alias OutMsg =
    Button.OutMsg BrandingFields


type alias Msg =
    Button.Msg BrandingFields


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
        { caption = "Save"
        , buttonType = Button.Ok
        }


update : UpdateHandler
update =
    Button.update


view : BrandingFields -> ViewHandler
view =
    Button.view


brandingSavedMsg : Msg
brandingSavedMsg =
    Button.dataSavedMsg
