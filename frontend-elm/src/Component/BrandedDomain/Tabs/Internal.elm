module Component.BrandedDomain.Tabs.Internal exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, brandingOutMsg, brandingSavedMsg, initialize, themeOutMsg, themeSavedMsg, update, view)

import Component.BrandedDomain.Data exposing (Branding, ThemeSet)
import Component.BrandedDomain.Tabs.BrandingPage as BrandingPage
import Component.BrandedDomain.Tabs.BrandingPage.Data as BrandingData
import Component.BrandedDomain.Tabs.Data as Data
import Component.BrandedDomain.Tabs.ThemePage as ThemePage
import Component.Branding.Data exposing (BrandingSettings)
import Component.Input.SaveButton as SaveButton
import Component.Theme.Data as ThemeData exposing (Theme)
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    { availableThemes : List Theme
    , brandingSettings : BrandingSettings
    , mDefaultThemeSet : Maybe ThemeSet
    , currentThemeSet : ThemeSet
    , brandingInfo : Branding
    }


type alias State =
    Pair.State BrandingPage.State ThemePage.State


type alias Msg =
    Pair.Msg BrandingPage.Msg ThemePage.Msg


type alias OutMsg =
    Data.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> ( Html Msg, Html Msg )


type alias Init =
    Config -> ( State, Cmd Msg )


themeOutMsg : ThemeData.OutMsg -> OutMsg
themeOutMsg msg =
    case msg of
        SaveButton.SaveMsg theme ->
            Data.SaveThemeMsg theme


brandingOutMsg : BrandingData.OutMsg -> OutMsg
brandingOutMsg msg =
    case msg of
        BrandingData.SaveBrandingMsg brandingFields ->
            Data.SaveBrandingMsg brandingFields

        BrandingData.CreateThemeMsg newTheme ->
            Data.CreateThemeMsg newTheme


initialize : Init
initialize =
    let
        inInit =
            Pair.liftInit
                BrandingPage.initialize
                ThemePage.initialize

        init : Init
        init config1 =
            let
                availableThemes =
                    config1.availableThemes

                config2 : BrandingPage.Config
                config2 =
                    { themesConfig =
                        { mDefaultThemes = config1.mDefaultThemeSet
                        , currentThemes = config1.currentThemeSet
                        , availableThemes = availableThemes
                        }
                    , brandingInfo = config1.brandingInfo
                    }

                config3 : ThemePage.Config
                config3 =
                    { availableThemes = availableThemes
                    , initialThemeIndex = Just 0
                    }
            in
            inInit ( config2, config3 )
    in
    init


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
