module Component.UserGroup.Tabs.BrandingPage.Edit.BrandingFields exposing (BrandingFields, Config, InConfig, Msg, OutMsg, State, UpdateHandler, ViewHandler, initialize, stateToBrandingFields, update, view, viewPreview)

import Component.Branding.CommonFields as CommonFields exposing (CommonFields)
import Component.Theme.Data exposing (Theme)
import Component.UserGroup.Data as Data
import Component.UserGroup.Tabs.BrandingPage.Data as Data
import Component.UserGroup.Tabs.BrandingPage.Edit.BrandingFields.ThemeFields as ThemeFields
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias BrandingFields =
    Data.BrandingFields


type alias Config =
    { themesConfig : ThemeFields.Config
    , commonFields : CommonFields
    }


type alias InConfig =
    Pair.Config ThemeFields.Config CommonFields.Config


type alias State =
    Pair.State ThemeFields.State CommonFields.State


type alias Msg =
    Pair.Msg ThemeFields.Msg CommonFields.Msg


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
                CommonFields.initialize
    in
    \config1 ->
        let
            config2 =
                ( config1.themesConfig, config1.commonFields )
        in
        inInit config2


update : UpdateHandler
update =
    Pair.liftUpdate
        ThemeFields.update
    <|
        Handler.outerMapUpdate never CommonFields.update


view : List Theme -> ViewHandler
view availableThemes =
    Pair.concatView
        (ThemeFields.view availableThemes)
        CommonFields.view


viewPreview : List Theme -> State -> List ( String, Html Never )
viewPreview availableThemes ( state1, _ ) =
    ThemeFields.viewPreview availableThemes state1


stateToBrandingFields : List Theme -> State -> BrandingFields
stateToBrandingFields availableThemes ( state1, state2 ) =
    let
        themeSet =
            ThemeFields.stateToThemeSet
                availableThemes
                state1

        commonFields =
            CommonFields.stateToCommonFields
                state2
    in
    { mThemeSet = themeSet
    , commonFields = commonFields
    }
