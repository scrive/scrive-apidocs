module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ThemeFields exposing (Config, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, initialize, stateToThemeSet, update, view, viewPreview)

import Component.BrandedDomain.Data exposing (ThemeSet)
import Component.Branding.ThemeField as ThemeField
import Component.Preview.Login as PreviewLogin
import Component.Theme.Data exposing (Theme)
import Component.UserGroup.Tabs.BrandingPage.Edit.BrandingFields.ThemeFields as UserGroup
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)
import List.Extra as List


type alias Config =
    { mDefaultThemes : Maybe ThemeSet
    , currentThemes : ThemeSet
    , availableThemes : List Theme
    }


type alias Msg =
    Pair.Msg UserGroup.Msg ThemeField.Msg


type alias OutMsg =
    Pair.Msg UserGroup.OutMsg ThemeField.OutMsg


type alias State =
    Pair.State UserGroup.State ThemeField.State


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> List (Html Msg)


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize =
    let
        inInit =
            Pair.liftInit
                UserGroup.initialize
                ThemeField.initialize
    in
    \config1 ->
        let
            mDefaultThemes =
                config1.mDefaultThemes

            currentThemes =
                config1.currentThemes

            availableThemes =
                config1.availableThemes

            findThemeIndex : String -> Maybe Int
            findThemeIndex themeId =
                List.findIndex
                    (\theme -> theme.id == themeId)
                    availableThemes

            loginConfig : ThemeField.Config
            loginConfig =
                { fieldLabel = "Login"
                , mDefaultTheme =
                    Maybe.map .loginTheme mDefaultThemes
                , selectedThemeIndex =
                    findThemeIndex currentThemes.loginTheme.id
                , previewHandler = PreviewLogin.view
                }

            config2 : UserGroup.Config
            config2 =
                { mDefaultThemes =
                    Maybe.map
                        (\defaultThemes ->
                            { emailTheme = defaultThemes.emailTheme
                            , signViewTheme = defaultThemes.signViewTheme
                            , serviceTheme = defaultThemes.serviceTheme
                            }
                        )
                        mDefaultThemes
                , currentThemes =
                    { emailTheme = currentThemes.emailTheme
                    , signViewTheme = currentThemes.signViewTheme
                    , serviceTheme = currentThemes.serviceTheme
                    }
                , availableThemes = availableThemes
                }

            config3 : Pair.Config UserGroup.Config ThemeField.Config
            config3 =
                ( config2, loginConfig )
        in
        inInit config3


update : UpdateHandler
update =
    Pair.liftUpdate2
        UserGroup.update
        ThemeField.update


view : List Theme -> ViewHandler
view availableThemes ( state1, state2 ) =
    let
        body1 =
            UserGroup.view availableThemes state1

        body2 =
            ThemeField.view availableThemes state2

        body3 =
            List.map
                (Html.map Pair.FirstMsg)
                body1

        body4 =
            Html.map Pair.SecondMsg body2
    in
    body3 ++ [ body4 ]


viewPreview : List Theme -> State -> List ( String, Html Never )
viewPreview availableThemes ( state1, state2 ) =
    let
        items1 =
            UserGroup.viewPreview availableThemes state1

        item2 =
            ( state2.fieldLabel
            , ThemeField.viewPreview availableThemes state2
            )
    in
    items1 ++ [ item2 ]


stateToThemeSet : List Theme -> State -> Maybe ThemeSet
stateToThemeSet availableThemes ( state1, state2 ) =
    let
        mThemes1 =
            UserGroup.stateToThemeSet availableThemes state1

        mLoginTheme =
            ThemeField.stateToTheme availableThemes state2

        mThemset =
            case ( mThemes1, mLoginTheme ) of
                ( Just themes1, Just loginTheme ) ->
                    Just <|
                        { emailTheme = themes1.emailTheme
                        , signViewTheme = themes1.signViewTheme
                        , serviceTheme = themes1.serviceTheme
                        , loginTheme = loginTheme
                        }

                _ ->
                    Nothing
    in
    mThemset
