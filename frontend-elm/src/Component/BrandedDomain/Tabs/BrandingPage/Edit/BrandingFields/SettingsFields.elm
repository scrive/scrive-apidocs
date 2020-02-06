module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.SettingsFields exposing (Config, Init, Msg, SettingsFields, State, UpdateHandler, ViewHandler, initialize, stateToSettingsFields, update, view)

import Component.BrandedDomain.Data exposing (..)
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ActionColors as ActionColors
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.FaviconField as FaviconField
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ParticipantColors as ParticipantColors
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.TextFields as TextFields
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias SettingsFields =
    { url : String
    , favicon : String
    , browserTitle : String
    , smsOriginator : String
    , emailOriginator : String
    , participantColors : ParticipantColors
    , actionColors : ActionColors
    }


type alias Config =
    SettingsFields


type alias State =
    Pair.State TextFields.State (Pair.State FaviconField.State (Pair.State ParticipantColors.State ActionColors.State))


type alias Msg =
    Pair.Msg TextFields.Msg (Pair.Msg FaviconField.Msg (Pair.Msg ParticipantColors.Msg ActionColors.Msg))


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> List (Html Msg)


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize =
    let
        inInit =
            Pair.liftInit
                TextFields.initialize
            <|
                Pair.liftInit
                    FaviconField.initialize
                <|
                    Pair.liftInit
                        ParticipantColors.initialize
                        ActionColors.initialize
    in
    \config1 ->
        let
            config2 =
                { url = config1.url
                , browserTitle = config1.browserTitle
                , smsOriginator = config1.smsOriginator
                , emailOriginator = config1.emailOriginator
                }

            config3 =
                config1.favicon

            config4 =
                config1.participantColors

            config5 =
                config1.actionColors
        in
        inInit ( config2, ( config3, ( config4, config5 ) ) )


update : UpdateHandler
update =
    Pair.liftUpdate
        TextFields.update
    <|
        Pair.liftUpdate
            FaviconField.update
        <|
            Pair.liftUpdate
                ParticipantColors.update
                ActionColors.update


view : ViewHandler
view =
    Pair.concatView
        TextFields.view
    <|
        Pair.concatView
            (Handler.toListViewHandler FaviconField.view)
        <|
            Pair.concatView
                ParticipantColors.view
                ActionColors.view


stateToSettingsFields : State -> SettingsFields
stateToSettingsFields ( state1, ( state2, ( state3, state4 ) ) ) =
    let
        textFields =
            TextFields.stateToTextFields state1

        favicon =
            state2

        participantColors =
            ParticipantColors.stateToColorFields state3

        actionColors =
            ActionColors.stateToColorFields state4

        settingsFields =
            { url = textFields.url
            , favicon = favicon
            , browserTitle = textFields.browserTitle
            , smsOriginator = textFields.smsOriginator
            , emailOriginator = textFields.emailOriginator
            , participantColors = participantColors
            , actionColors = actionColors
            }
    in
    settingsFields
