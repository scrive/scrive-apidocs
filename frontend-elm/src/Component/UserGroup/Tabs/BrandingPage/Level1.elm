module Component.UserGroup.Tabs.BrandingPage.Level1 exposing (Config, Msg, OutMsg(..), State, UpdateHandler, ViewHandler, doneSaveBrandingMsg, initialize, settingsMsg, update, view)

import Component.Branding.CreateTheme as CreateTheme
import Component.Branding.Settings as Settings
import Component.Theme.Data exposing (Theme)
import Component.UserGroup.Tabs.BrandingPage.Edit as EditBranding
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div)


type alias Config =
    EditBranding.Config


type alias State =
    Pair.State (Pair.State CreateTheme.State EditBranding.State) Settings.State


type alias Msg =
    Pair.Msg (Pair.Msg CreateTheme.Msg EditBranding.Msg) Settings.Msg


type OutMsg
    = EditBrandingMsg EditBranding.OutMsg
    | CreateThemeMsg CreateTheme.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


initialize : Config -> ( State, Cmd Msg )
initialize =
    let
        inInit =
            Pair.liftInit
                (Pair.liftInit
                    CreateTheme.initialize
                    EditBranding.initialize
                )
                Settings.initTab
    in
    \config1 ->
        inInit ( ( (), config1 ), () )


update : UpdateHandler
update =
    Pair.liftUpdate
        (Pair.liftUpdate
            (Handler.outerMapUpdate
                CreateThemeMsg
                CreateTheme.update
            )
            (Handler.outerMapUpdate
                EditBrandingMsg
                EditBranding.update
            )
        )
        Settings.updateTabState


view : List Theme -> ViewHandler
view availableThemes ( ( state1, state2 ), state3 ) =
    let
        inThemes =
            [ { id = "mail"
              , name = "Scrive email theme"
              }
            , { id = "signview"
              , name = "Scrive signing theme"
              }
            , { id = "service"
              , name = "Scrive service theme"
              }
            ]

        createBody =
            Html.map Pair.FirstMsg <|
                CreateTheme.view
                    inThemes
                    state1

        editBody =
            Html.map Pair.SecondMsg <|
                EditBranding.view
                    availableThemes
                    state2

        previewTabs1 =
            EditBranding.viewPreview
                availableThemes
                state2

        brandingBody =
            div []
                [ editBody
                , createBody
                ]
    in
    Settings.viewBranding state3 brandingBody previewTabs1


doneSaveBrandingMsg : Msg
doneSaveBrandingMsg =
    Pair.FirstMsg <|
        Pair.SecondMsg
            EditBranding.doneSaveBrandingMsg


settingsMsg : Settings.Msg -> Msg
settingsMsg =
    Pair.SecondMsg
