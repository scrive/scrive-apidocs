module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields exposing (BrandingFields, Config, InConfig, Msg, OutMsg, State, UpdateHandler, ViewHandler, initialize, stateToBrandingFields, stateToThemeSet, update, view, viewPreview)

import Component.BrandedDomain.Data exposing (Branding, ThemeSet)
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.SettingsFields2 as SettingsFields exposing (SettingsFields)
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ThemeFields as ThemeFields
import Component.Theme.Data exposing (Theme)
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias BrandingFields =
    { mThemeSet : Maybe ThemeSet
    , brandingFields : SettingsFields
    }


type alias Config =
    { themesConfig : ThemeFields.Config
    , brandingInfo : Branding
    }


type alias InConfig =
    Pair.Config ThemeFields.Config SettingsFields.Config


type alias State =
    Pair.State ThemeFields.State SettingsFields.State


type alias Msg =
    Pair.Msg ThemeFields.Msg SettingsFields.Msg


type alias OutMsg =
    ThemeFields.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> List (Html Msg)


initialize : Config -> ( State, Cmd Msg )
initialize =
    let
        inInit =
            Pair.liftInit
                ThemeFields.initialize
                SettingsFields.initialize
    in
    \config1 ->
        let
            branding =
                config1.brandingInfo

            config2 =
                { url = branding.url
                , favicon = branding.favicon
                , browserTitle = branding.browserTitle
                , smsOriginator = branding.smsOriginator
                , emailOriginator = branding.emailOriginator
                , participantColors = branding.participantColors
                , actionColors = branding.brandingColors
                }

            config3 =
                ( config1.themesConfig, config2 )
        in
        inInit config3


update : UpdateHandler
update =
    Pair.liftUpdate
        ThemeFields.update
    <|
        Handler.outerMapUpdate never SettingsFields.update


view : List Theme -> ViewHandler
view availableThemes =
    Pair.concatView
        (ThemeFields.view availableThemes)
        SettingsFields.view


viewPreview : List Theme -> State -> List ( String, Html Never )
viewPreview availableThemes ( state1, _ ) =
    ThemeFields.viewPreview availableThemes state1


stateToBrandingFields : List Theme -> State -> BrandingFields
stateToBrandingFields availableThemes ( state1, state2 ) =
    let
        mThemeSet =
            ThemeFields.stateToThemeSet
                availableThemes
                state1

        settingsFields =
            SettingsFields.stateToSettingsFields
                state2
    in
    { mThemeSet = mThemeSet
    , brandingFields = settingsFields
    }

stateToThemeSet : List Theme -> State -> Maybe ThemeSet
stateToThemeSet availableThemes state =
    (stateToBrandingFields availableThemes state).mThemeSet
