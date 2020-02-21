module Component.UserGroup.Page.Level1 exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, themeSavedMsg, update, view)

import Component.Branding.BrandingPage as BrandingPage
import Component.UserGroup.Data exposing (ThemeSet)
import Component.UserGroup.Tabs as Sections
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    BrandingPage.Config ThemeSet


type alias State =
    Pair.State Sections.State BrandingPage.State


type alias Msg =
    Pair.Msg Sections.Msg BrandingPage.State


type alias OutMsg =
    Sections.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize =
    let
        inInit =
            Pair.liftInit
                Sections.initialize
                BrandingPage.initTab
    in
    \config1 ->
        let
            config2 =
                { availableThemes = config1.availableThemes
                , defaultThemeSet = config1.defaultThemeSet
                , currentThemeSet = config1.currentThemeSet
                , brandingSettings =
                    { browserTitle = config1.browserTitle
                    , smsOriginator = config1.smsOriginator
                    }
                }
        in
        inInit ( config2, () )


update : UpdateHandler
update =
    Pair.liftUpdate
        Sections.update
        BrandingPage.updateTabState


view : ViewHandler
view ( state1, state2 ) =
    let
        ( brandingPage1, themePage1 ) =
            Sections.view state1
    in
    BrandingPage.viewBrandingPage state2 brandingPage1 themePage1


brandingSavedMsg : Msg
brandingSavedMsg =
    Pair.FirstMsg Sections.brandingSavedMsg


themeSavedMsg : Msg
themeSavedMsg =
    Pair.FirstMsg Sections.themeSavedMsg
