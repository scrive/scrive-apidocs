module Component.BrandedDomain.Tabs.BrandingPage.Edit exposing (Config, InConfig, Msg, OutMsg, State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, update, view, viewPreview)

import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields as BrandingFields
import Component.BrandedDomain.Tabs.BrandingPage.Edit.SaveButton as SaveButton
import Component.Theme.Data exposing (Theme)
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div)


type alias Config =
    BrandingFields.Config


type alias InConfig =
    Pair.Config BrandingFields.Config SaveButton.Config


type alias State =
    Pair.State BrandingFields.State SaveButton.State


type alias Msg =
    Pair.Msg BrandingFields.Msg SaveButton.Msg


type alias OutMsg =
    Pair.Msg BrandingFields.OutMsg SaveButton.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


initialize : Config -> ( State, Cmd Msg )
initialize =
    let
        inInit =
            Pair.liftInit
                BrandingFields.initialize
                SaveButton.initialize
    in
    \config1 ->
        let
            config2 =
                ( config1, () )
        in
        inInit config2


update : UpdateHandler
update =
    Pair.liftUpdate2
        BrandingFields.update
        SaveButton.update


view : List Theme -> ViewHandler
view availableThemes ( state1, state2 ) =
    let
        brandingFields =
            BrandingFields.stateToBrandingFields
                availableThemes
                state1

        brandingFieldsBody =
            List.map
                (Html.map Pair.FirstMsg)
            <|
                BrandingFields.view
                    availableThemes
                    state1

        saveButtonBody =
            Html.map Pair.SecondMsg <|
                SaveButton.view
                    brandingFields
                    state2

        body =
            div [] <|
                brandingFieldsBody
                    ++ [ saveButtonBody ]
    in
    body


brandingSavedMsg : Msg
brandingSavedMsg =
    Pair.SecondMsg SaveButton.brandingSavedMsg


viewPreview : List Theme -> State -> List ( String, Html Never )
viewPreview themes ( state1, _ ) =
    BrandingFields.viewPreview themes state1
