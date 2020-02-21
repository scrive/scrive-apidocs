module Component.UserGroup.Tabs.Internal exposing (Config, Init, Msg, OutMsg(..), State, UpdateHandler, ViewHandler, brandingOutMsg, brandingSavedMsg, initialize, themeOutMsg, themeSavedMsg, update, view)

import Component.Branding.CreateTheme as CreateTheme
import Component.Branding.Data exposing (BrandingSettings)
import Component.Input.SaveButton exposing (OutMsg(..))
import Component.Theme.Data as ThemeData exposing (Theme)
import Component.UserGroup.Data exposing (ThemeSet)
import Component.UserGroup.Tabs.BrandingPage as BrandingPage
import Component.UserGroup.Tabs.BrandingPage.Data as BrandingData
import Component.UserGroup.Tabs.BrandingPage.Edit.BrandingFields exposing (BrandingFields)
import Component.UserGroup.Tabs.ThemePage as ThemePage
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    { availableThemes : List Theme
    , brandingSettings : BrandingSettings
    , defaultThemeSet : ThemeSet
    , currentThemeSet : ThemeSet
    }


type alias State =
    Pair.State BrandingPage.State ThemePage.State


type alias Msg =
    Pair.Msg BrandingPage.Msg ThemePage.Msg


type OutMsg
    = SaveThemeMsg Theme
    | DeleteThemeMsg Theme
    | SaveBrandingMsg BrandingFields
    | CreateThemeMsg CreateTheme.NewTheme


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> ( Html Msg, Html Msg )


type alias Init =
    Config -> ( State, Cmd Msg )


themeOutMsg : ThemeData.OutMsg -> OutMsg
themeOutMsg msg1 =
    case msg1 of
        ThemeData.SaveThemeMsg theme ->
            SaveThemeMsg theme
        ThemeData.DeleteThemeMsg theme ->
            DeleteThemeMsg theme


brandingOutMsg : BrandingData.OutMsg -> OutMsg
brandingOutMsg msg =
    case msg of
        BrandingData.SaveBrandingMsg brandingFields ->
            SaveBrandingMsg brandingFields

        BrandingData.CreateThemeMsg newTheme ->
            CreateThemeMsg newTheme


initialize : Init
initialize =
    let
        inInit =
            Pair.liftInit
                BrandingPage.initialize
                ThemePage.initialize
    in
    \config1 ->
        let
            availableThemes =
                config1.availableThemes

            config2 : BrandingPage.Config
            config2 =
                { themesConfig =
                    { mDefaultThemes = Just config1.defaultThemeSet
                    , currentThemes = config1.currentThemeSet
                    , availableThemes = availableThemes
                    }
                , commonFields = config1.brandingSettings
                }

            config3 : ThemePage.Config
            config3 =
                { availableThemes = availableThemes
                , initialThemeIndex = Just 0
                }
        in
        inInit ( config2, config3 )


update : UpdateHandler
update =
    Pair.liftUpdate
        (Handler.outerMapUpdate brandingOutMsg BrandingPage.update)
        (Handler.outerMapUpdate themeOutMsg ThemePage.update)


view : ViewHandler
view ( state1, state2 ) =
    let
        themeBody =
            Html.map Pair.SecondMsg <|
                ThemePage.view state2

        themes =
            ThemePage.stateToThemes state2

        brandingBody =
            Html.map Pair.FirstMsg <|
                BrandingPage.view themes state1
    in
    ( brandingBody, themeBody )


brandingSavedMsg : Msg
brandingSavedMsg =
    Pair.FirstMsg BrandingPage.brandingSavedMsg


themeSavedMsg : Msg
themeSavedMsg =
    Pair.SecondMsg ThemePage.themeSavedMsg
