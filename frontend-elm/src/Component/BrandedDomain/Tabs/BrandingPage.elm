module Component.BrandedDomain.Tabs.BrandingPage exposing (Config, Init, Msg(..), OutMsg, State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, mapMessage, themeFieldPos, update, view)

import Component.BrandedDomain.Tabs.BrandingPage.Data as Data
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields as BrandingFields
import Component.BrandedDomain.Tabs.BrandingPage.One as Base
import Component.Branding.Settings as Settings
import Component.Input.SaveButton as SaveButton
import Component.Theme.Data exposing (Theme)
import Component.UserGroup.Tabs.BrandingPage as UserGroup
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    Base.Config


type alias State =
    Base.State


type Msg
    = BaseMsg Base.Msg
    | ThemeFieldsMsg BrandingFields.OutMsg


type alias OutMsg =
    Data.OutMsg


type alias Init =
    Config -> ( State, Cmd Msg )


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


initialize : Init
initialize config =
    let
        ( state, cmd ) =
            Base.initialize config
    in
    ( state, Cmd.map BaseMsg cmd )


mapMessage : Either Base.OutMsg Base.Msg -> Either OutMsg Msg
mapMessage msg1 =
    case msg1 of
        Left msg2 ->
            case msg2 of
                Base.EditBrandingMsg msg3 ->
                    case msg3 of
                        Pair.FirstMsg msg4 ->
                            Right <| ThemeFieldsMsg msg4

                        Pair.SecondMsg (SaveButton.SaveMsg msg4) ->
                            Left <| Data.SaveBrandingMsg msg4

                Base.CreateThemeMsg (SaveButton.SaveMsg msg3) ->
                    Left <| Data.CreateThemeMsg msg3

        Right msg2 ->
            Right <| BaseMsg msg2


themeFieldPos : BrandingFields.OutMsg -> Int
themeFieldPos msg1 =
    case msg1 of
        Pair.FirstMsg msg2 ->
            UserGroup.themeFieldPos msg2

        Pair.SecondMsg _ ->
            3


update : UpdateHandler
update msg1 state1 =
    case msg1 of
        BaseMsg msg2 ->
            let
                ( state2, cmd ) =
                    Base.update msg2 state1
            in
            ( state2, Cmd.map mapMessage cmd )

        ThemeFieldsMsg msg2 ->
            let
                fieldPos =
                    themeFieldPos msg2

                msg3 =
                    Base.settingsMsg <|
                        Settings.selectTabMsg fieldPos

                ( state2, cmd ) =
                    Base.update msg3 state1
            in
            ( state2, Cmd.map mapMessage cmd )


view : List Theme -> ViewHandler
view availableThemes state =
    Html.map BaseMsg <|
        Base.view availableThemes state


brandingSavedMsg : Msg
brandingSavedMsg =
    BaseMsg Base.brandingSavedMsg
