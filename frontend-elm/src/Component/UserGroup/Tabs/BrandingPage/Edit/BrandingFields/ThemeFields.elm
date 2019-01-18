module Component.UserGroup.Tabs.BrandingPage.Edit.BrandingFields.ThemeFields exposing (Config, Init, Msg, OutMsg, State, ThemeSet, UpdateHandler, ViewHandler, initialize, stateToThemeSet, update, view, viewPreview)

import Component.Branding.ThemeField as ThemeField
import Component.Preview.Email as PreviewEmail
import Component.Preview.Service as PreviewService
import Component.Preview.SignView as PreviewSignView
import Component.Theme.Data exposing (Theme)
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)
import List.Extra as List


type alias ThemeSet =
    { emailTheme : Theme
    , signViewTheme : Theme
    , serviceTheme : Theme
    }


type alias Config =
    { defaultThemes : ThemeSet
    , currentThemes : ThemeSet
    , availableThemes : List Theme
    }


type alias Msg =
    Pair.Msg ThemeField.Msg (Pair.Msg ThemeField.Msg ThemeField.Msg)


type alias OutMsg =
    Pair.Msg ThemeField.OutMsg (Pair.Msg ThemeField.OutMsg ThemeField.OutMsg)


type alias State =
    Pair.State ThemeField.State (Pair.State ThemeField.State ThemeField.State)


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
                ThemeField.initialize
            <|
                Pair.liftInit
                    ThemeField.initialize
                    ThemeField.initialize
    in
    \config1 ->
        let
            defaultThemes =
                config1.defaultThemes

            currentThemes =
                config1.currentThemes

            availableThemes =
                config1.availableThemes

            findThemeIndex : String -> Maybe Int
            findThemeIndex themeId =
                List.findIndex
                    (\theme -> theme.id == themeId)
                    availableThemes

            emailConfig =
                { fieldLabel = "Email"
                , defaultTheme =
                    defaultThemes.emailTheme
                , selectedThemeIndex =
                    findThemeIndex currentThemes.emailTheme.id
                , previewHandler = PreviewEmail.view
                }

            signViewConfig =
                { fieldLabel = "Sign View"
                , defaultTheme =
                    defaultThemes.signViewTheme
                , selectedThemeIndex =
                    findThemeIndex currentThemes.signViewTheme.id
                , previewHandler = PreviewSignView.view
                }

            serviceConfig =
                { fieldLabel = "Service"
                , defaultTheme =
                    defaultThemes.serviceTheme
                , selectedThemeIndex =
                    findThemeIndex currentThemes.serviceTheme.id
                , previewHandler = PreviewService.view
                }

            config2 =
                ( emailConfig
                , ( signViewConfig
                  , serviceConfig
                  )
                )
        in
        inInit config2


update : UpdateHandler
update =
    Pair.liftUpdate2
        ThemeField.update
    <|
        Pair.liftUpdate2
            ThemeField.update
            ThemeField.update


view : List Theme -> ViewHandler
view availableThemes =
    let
        inView =
            ThemeField.view availableThemes
    in
    Pair.consView
        inView
    <|
        Pair.consView
            inView
        <|
            Handler.toListViewHandler inView


viewPreview : List Theme -> State -> List ( String, Html Never )
viewPreview availableThemes state =
    let
        inView =
            ThemeField.viewPreview availableThemes

        viewTab :
            ThemeField.State
            -> ( String, Html Never )
        viewTab state2 =
            ( state2.fieldLabel, inView state2 )

        ( emailState, ( signViewState, serviceState ) ) =
            state

        tabItems =
            [ viewTab emailState
            , viewTab signViewState
            , viewTab serviceState
            ]
    in
    tabItems


stateToThemeSet : List Theme -> State -> ThemeSet
stateToThemeSet availableThemes state =
    let
        ( emailState, ( signViewState, serviceState ) ) =
            state

        stateToTheme =
            ThemeField.stateToTheme availableThemes
    in
    { emailTheme = stateToTheme emailState
    , signViewTheme = stateToTheme signViewState
    , serviceTheme = stateToTheme serviceState
    }
