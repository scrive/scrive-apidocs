module Component.UserGroup.Tabs.ThemePage exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, initialize, stateToThemes, themeSavedMsg, update, view)

import Component.Branding.ThemePage as ThemePage
import Component.Theme.Data exposing (Theme)
import Component.Theme.Edit as ThemeEdit
import Component.UserGroup.Tabs.ThemePage.BrandingPreview as Preview
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    ThemeEdit.Config


type alias Msg =
    Pair.Msg ThemeEdit.Msg Preview.Msg


type alias OutMsg =
    ThemeEdit.OutMsg


type alias State =
    Pair.State ThemeEdit.State Preview.State


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
                ThemeEdit.initialize
                Preview.initialize
    in
    \config1 ->
        inInit ( config1, () )


update : UpdateHandler
update =
    Pair.liftUpdate
        ThemeEdit.update
    <|
        Handler.outerMapUpdate never Preview.update


view : ViewHandler
view ( state1, state2 ) =
    let
        mTheme1 =
            ThemeEdit.currentTheme state1

        mPreviewBody1 =
            Maybe.map
                (\theme -> Preview.view theme state2)
                mTheme1

        themeBody1 =
            ThemeEdit.view state1
    in
    ThemePage.viewThemePage themeBody1 mPreviewBody1


stateToThemes : State -> List Theme
stateToThemes ( state, _ ) =
    ThemeEdit.stateToThemes state

themeSavedMsg : Msg
themeSavedMsg =
    Pair.FirstMsg ThemeEdit.themeSavedMsg
