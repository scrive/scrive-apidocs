module Component.UserGroup.Page.Two exposing (Config, Init, Msg(..), NewTheme, OutMsg(..), State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, mapPageMsg, themeSavedMsg, update, view)

import Component.Theme.Data exposing (Theme)
import Component.UserGroup.Data exposing (Branding, ThemeSet)
import Component.UserGroup.Page.One as Page
import Component.UserGroup.Tabs.Internal as Sections
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    { brandingInfo : Branding
    , availableThemes : List Theme
    , defaultThemeSet : ThemeSet
    , currentThemeSet : ThemeSet
    }


type alias NewTheme =
    { userGroupId : String
    , originalThemeId : String
    , newThemeName : String
    }


type alias State =
    { inState : Page.State
    , brandingInfo : Branding
    }


type Msg
    = PageMsg Page.Msg
    | PageOutMsg Page.OutMsg


type OutMsg
    = SaveBrandingMsg Branding
    | SaveThemeMsg String Theme
    | CreateThemeMsg NewTheme


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize config1 =
    let
        branding =
            config1.brandingInfo

        config2 =
            { browserTitle =
                Maybe.withDefault ""
                    branding.browserTitle
            , smsOriginator =
                Maybe.withDefault ""
                    branding.smsOriginator
            , defaultThemeSet = config1.defaultThemeSet
            , currentThemeSet = config1.currentThemeSet
            , availableThemes = config1.availableThemes
            }

        ( state1, cmd1 ) =
            Page.initialize config2

        state2 =
            { inState = state1
            , brandingInfo = branding
            }

        cmd2 =
            Cmd.map PageMsg cmd1
    in
    ( state2, cmd2 )


mapPageMsg : Either Page.OutMsg Page.Msg -> Either OutMsg Msg
mapPageMsg msg1 =
    case msg1 of
        Left msg2 ->
            Right <| PageOutMsg msg2

        Right msg2 ->
            Right <| PageMsg msg2


update : UpdateHandler
update msg1 state1 =
    case msg1 of
        PageMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    Page.update msg2 state1.inState

                cmd2 =
                    Cmd.map mapPageMsg cmd1

                state3 =
                    { state1 | inState = state2 }
            in
            ( state3, cmd2 )

        PageOutMsg msg2 ->
            case msg2 of
                Sections.SaveThemeMsg theme ->
                    let
                        brandingId =
                            state1.brandingInfo.userGroupId

                        cmd1 =
                            Util.msgToCmd <|
                                Left <|
                                    SaveThemeMsg brandingId theme
                    in
                    ( state1, cmd1 )

                Sections.SaveBrandingMsg brandingFields ->
                    let
                        branding1 =
                            state1.brandingInfo

                        commonFields =
                            brandingFields.commonFields
                    in
                    case brandingFields.mThemeSet of
                        Just themeSet ->
                            let
                                branding2 =
                                    { branding1
                                        | browserTitle =
                                            Just commonFields.browserTitle
                                        , smsOriginator =
                                            Just commonFields.smsOriginator
                                        , themeIds =
                                            { emailTheme = Just themeSet.emailTheme.id
                                            , signViewTheme = Just themeSet.signViewTheme.id
                                            , serviceTheme = Just themeSet.serviceTheme.id
                                            }
                                    }

                                state2 =
                                    { state1
                                        | brandingInfo = branding2
                                    }

                                cmd1 =
                                    Util.msgToCmd <|
                                        Left <|
                                            SaveBrandingMsg branding2
                            in
                            ( state2, cmd1 )

                        Nothing ->
                            ( state1, Cmd.none )

                Sections.CreateThemeMsg newTheme ->
                    let
                        newTheme2 =
                            { userGroupId = state1.brandingInfo.userGroupId
                            , originalThemeId = newTheme.originalThemeId
                            , newThemeName = newTheme.newThemeName
                            }

                        cmd1 =
                            Util.msgToCmd <|
                                Left <|
                                    CreateThemeMsg newTheme2
                    in
                    ( state1, cmd1 )


view : ViewHandler
view state =
    Html.map PageMsg <|
        Page.view state.inState


themeSavedMsg : Msg
themeSavedMsg =
    PageMsg Page.themeSavedMsg


brandingSavedMsg : Msg
brandingSavedMsg =
    PageMsg Page.brandingSavedMsg
