module Component.BrandedDomain.Page.Level2 exposing (Config, Init, Msg(..), NewTheme, OutMsg(..), State, UpdateHandler, ViewHandler, doneSaveBrandingMsg, doneDeleteThemeMsg, initialize, mapPageMsg, doneSaveThemeMsg, update, view)

import Component.BrandedDomain.Data exposing (Branding, ThemeSet)
import Component.BrandedDomain.Page.Level1 as Base
import Component.BrandedDomain.Tabs.Data as Tabs
import Component.Theme.Data exposing (Theme)
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    { brandingInfo : Branding
    , availableThemes : List Theme
    , mDefaultThemeSet : Maybe ThemeSet
    , currentThemeSet : ThemeSet
    }


type alias State =
    { inState : Base.State
    , brandingInfo : Branding
    }


type alias NewTheme =
    { brandedDomainId : String
    , originalThemeId : String
    , newThemeName : String
    }


type Msg
    = PageMsg Base.Msg
    | HandleBaseOutMsg Tabs.OutMsg


type OutMsg
    = SaveBrandingMsg Branding
    | SaveThemeMsg String Theme
    | DeleteThemeMsg String Theme
    | CreateThemeMsg NewTheme
    | GoBack


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
            { mDefaultThemeSet = config1.mDefaultThemeSet
            , currentThemeSet = config1.currentThemeSet
            , availableThemes = config1.availableThemes
            , brandingInfo = branding
            }

        ( state1, cmd1 ) =
            Base.initialize config2

        state2 =
            { inState = state1
            , brandingInfo = branding
            }

        cmd2 =
            Cmd.map PageMsg cmd1
    in
    ( state2, cmd2 )


mapPageMsg : Either Base.OutMsg Base.Msg -> Either OutMsg Msg
mapPageMsg msg1 =
    case msg1 of
        Left (Base.BaseOutMsg msg2) ->
            Right <| HandleBaseOutMsg msg2

        Left (Base.GoBack) ->
            Left <| GoBack

        Right msg2 ->
            Right <| PageMsg msg2

update : UpdateHandler
update msg1 state1 =
    case msg1 of
        PageMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    Base.update msg2 state1.inState

                cmd2 =
                    Cmd.map mapPageMsg cmd1

                state3 =
                    { state1 | inState = state2 }
            in
            ( state3, cmd2 )

        HandleBaseOutMsg msg2 ->
            case msg2 of
                Tabs.SaveThemeMsg theme ->
                    let
                        brandingId =
                            state1.brandingInfo.brandedDomainId

                        cmd1 =
                            Util.msgToCmd <|
                                Left <|
                                    SaveThemeMsg brandingId theme
                    in
                    ( state1, cmd1 )


                Tabs.DeleteThemeMsg theme ->
                    let
                        brandingId =
                            state1.brandingInfo.brandedDomainId

                        cmd1 =
                            Util.msgToCmd <|
                                Left <|
                                    DeleteThemeMsg brandingId theme
                    in
                    ( state1, cmd1 )


                Tabs.SaveBrandingMsg fields ->
                    let
                        branding1 =
                            state1.brandingInfo

                        brandingFields =
                            fields.brandingFields
                    in
                    case fields.mThemeSet of
                        Just themeSet ->
                            let
                                branding2 =
                                    { branding1
                                        | url = brandingFields.url
                                        , favicon = brandingFields.favicon
                                        , browserTitle =
                                            brandingFields.browserTitle
                                        , smsOriginator =
                                            brandingFields.smsOriginator
                                        , emailOriginator =
                                            brandingFields.emailOriginator
                                        , themeIds =
                                            { emailTheme = themeSet.emailTheme.id
                                            , signViewTheme = themeSet.signViewTheme.id
                                            , serviceTheme = themeSet.serviceTheme.id
                                            , loginTheme = themeSet.loginTheme.id
                                            }
                                        , participantColors = brandingFields.participantColors
                                        , brandingColors = brandingFields.actionColors
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


                Tabs.CreateThemeMsg newTheme ->
                    let
                        newTheme2 =
                            { brandedDomainId = state1.brandingInfo.brandedDomainId
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
        Base.view state.inState


doneSaveThemeMsg : Msg
doneSaveThemeMsg =
    PageMsg Base.doneSaveThemeMsg


doneSaveBrandingMsg : Msg
doneSaveBrandingMsg =
    PageMsg Base.doneSaveBrandingMsg

doneDeleteThemeMsg : Msg
doneDeleteThemeMsg =
    PageMsg Base.doneDeleteThemeMsg
