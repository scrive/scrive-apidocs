module Component.BrandedDomain.Page.Two exposing (Config, Init, Msg(..), NewTheme, OutMsg(..), State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, mapPageMsg, themeSavedMsg, update, view)

import Component.BrandedDomain.Data exposing (Branding, ThemeSet)
import Component.BrandedDomain.Page.One as Base
import Component.BrandedDomain.Tabs.Data as Tabs
import Component.Theme.Data exposing (Theme)
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    { brandingInfo : Branding
    , availableThemes : List Theme
    , defaultThemeSet : ThemeSet
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
    | PageOutMsg Base.OutMsg


type OutMsg
    = SaveBrandingMsg Branding
    | SaveThemeMsg String Theme
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
            { defaultThemeSet = config1.defaultThemeSet
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
                    Base.update msg2 state1.inState

                cmd2 =
                    Cmd.map mapPageMsg cmd1

                state3 =
                    { state1 | inState = state2 }
            in
            ( state3, cmd2 )

        PageOutMsg msg2 ->
            case msg2 of
                Base.TabsOutMsg (Tabs.SaveThemeMsg theme) ->
                    let
                        brandingId =
                            state1.brandingInfo.brandedDomainId

                        cmd1 =
                            Util.msgToCmd <|
                                Left <|
                                    SaveThemeMsg brandingId theme
                    in
                    ( state1, cmd1 )

                Base.TabsOutMsg (Tabs.SaveBrandingMsg fields) ->
                    let
                        branding1 =
                            state1.brandingInfo

                        brandingFields =
                            fields.brandingFields

                        themeSet =
                            fields.themeSet

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

                Base.TabsOutMsg (Tabs.CreateThemeMsg newTheme) ->
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

                Base.GoBack ->
                    ( state1, Util.msgToCmd <| Left GoBack )


view : ViewHandler
view state =
    Html.map PageMsg <|
        Base.view state.inState


themeSavedMsg : Msg
themeSavedMsg =
    PageMsg Base.themeSavedMsg


brandingSavedMsg : Msg
brandingSavedMsg =
    PageMsg Base.brandingSavedMsg
