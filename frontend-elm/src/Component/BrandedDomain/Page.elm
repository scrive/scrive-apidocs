module Component.BrandedDomain.Page exposing (Config, Init, Msg(..), OutMsg, State, UpdateHandler, ViewHandler, defaultThemeSetFromThemes, defaultView, getBrandingCmd, getThemesCmd, handleOutMsg, initialize, loadCmd, mergeMaybe, pageMsgToMsg, saveBrandingCmd, saveThemeCmd, stateToPageConfig, themeSetFromBranding, update, updatePage, updatePageConfig, updatePageError, view)

import Component.BrandedDomain.Data exposing (Branding, ThemeSet)
import Component.BrandedDomain.Json as BrandingJson
import Component.BrandedDomain.Page.Four as Page
import Component.BrandedDomain.Page.Two as SuccessPage exposing (NewTheme)
import Component.Global.Msg as Global exposing (GlobalMsg(..))
import Component.Theme.Data exposing (Theme)
import Component.Theme.Json as ThemeJson
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html, div, text)
import Http
import Json.Encode as JE
import List.Extra as List
import Util.Http as Util


type alias Config =
    { xtoken : String
    , brandedDomainId : String
    }


type alias State =
    { xtoken : String
    , pageState : Page.State
    , brandedDomainId : String
    , brandingInfo : Maybe Branding
    , availableThemes : Maybe (List Theme)
    }


type Msg
    = PageMsg Page.Msg
    | PageOutMsg Page.OutMsg
    | SetBrandedDomainId String
    | OnAvailableThemesMsg (Result Http.Error (List Theme))
    | OnBrandingInfoMsg (Result Http.Error Branding)
    | ThemeSavedMsg (Result Http.Error ())
    | ThemeCreatedMsg (Result Http.Error ())
    | BrandingSavedMsg (Result Http.Error ())


type alias OutMsg =
    GlobalMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize config =
    let
        brandedDomainId =
            config.brandedDomainId

        ( pageState, cmd1 ) =
            Page.initEmpty

        state =
            { xtoken = config.xtoken
            , pageState = pageState
            , brandedDomainId = brandedDomainId
            , brandingInfo = Nothing
            , availableThemes = Nothing
            }

        cmd2 =
            loadCmd brandedDomainId

        cmd3 =
            Cmd.batch
                [ Cmd.map PageMsg cmd1
                , cmd2
                ]
    in
    ( state, cmd3 )


updatePage : Page.Msg -> State -> ( State, Cmd (Either OutMsg Msg) )
updatePage msg1 state1 =
    let
        ( state2, cmd1 ) =
            Page.update msg1 state1.pageState

        state3 =
            { state1
                | pageState = state2
            }

        cmd2 =
            Cmd.map pageMsgToMsg cmd1
    in
    ( state3, cmd2 )


updatePageConfig : Page.Config -> State -> ( State, Cmd (Either OutMsg Msg) )
updatePageConfig pageConfig state =
    case pageConfig of
        Just config ->
            let
                msg =
                    Page.initMsg config
            in
            updatePage msg state

        Nothing ->
            ( state, Cmd.none )


updatePageError : Http.Error -> State -> ( State, Cmd (Either OutMsg Msg) )
updatePageError error1 state =
    let
        error2 =
            Util.httpErrorToString error1

        msg2 =
            Page.initErrorMsg error2
    in
    updatePage msg2 state


update : UpdateHandler
update msg1 state1 =
    case msg1 of
        PageMsg msg2 ->
            updatePage msg2 state1

        PageOutMsg msg2 ->
            let
                cmd1 =
                    handleOutMsg msg2 state1
            in
            ( state1, cmd1 )

        SetBrandedDomainId brandedDomainId ->
            let
                msg2 =
                    Page.clearMsg

                ( state2, cmd1 ) =
                    updatePage msg2 state1

                cmd2 =
                    Cmd.map Right <| loadCmd brandedDomainId

                cmd3 =
                    Cmd.batch
                        [ cmd1, cmd2 ]
            in
            ( state2, cmd3 )

        OnAvailableThemesMsg result ->
            case result of
                Ok themes ->
                    let
                        state2 =
                            { state1
                                | availableThemes = Just themes
                            }

                        pageConfig =
                            stateToPageConfig state2
                    in
                    updatePageConfig pageConfig state2

                Err error ->
                    updatePageError error state1

        OnBrandingInfoMsg result ->
            case result of
                Ok branding ->
                    let
                        state2 =
                            { state1
                                | brandingInfo = Just branding
                            }

                        pageConfig =
                            stateToPageConfig state2
                    in
                    updatePageConfig pageConfig state2

                Err error ->
                    updatePageError error state1

        ThemeSavedMsg res ->
            let
                ( state2, cmd1 ) =
                    update
                        (PageMsg Page.themeSavedMsg)
                        state1

                outMsg =
                    case res of
                        Ok () ->
                            Global.flashSuccess
                                "Theme saved"

                        Err err ->
                            Global.flashError <|
                                "Error saving theme: "
                                    ++ Util.httpErrorToString err

                cmd2 =
                    Util.msgToCmd <| Left outMsg

                cmd3 =
                    Cmd.batch
                        [ cmd1, cmd2 ]
            in
            ( state2, cmd3 )

        ThemeCreatedMsg res ->
            let
                ( state2, cmd1 ) =
                    initialize
                        { xtoken = state1.xtoken
                        , brandedDomainId = state1.brandedDomainId
                        }

                outMsg =
                    case res of
                        Ok () ->
                            Global.flashSuccess
                                "Theme saved"

                        Err err ->
                            Global.flashError <|
                                "Error saving theme: "
                                    ++ Util.httpErrorToString err

                cmd2 =
                    Util.msgToCmd <| Left outMsg

                cmd3 =
                    Cmd.batch
                        [ Cmd.map Right cmd1, cmd2 ]
            in
            ( state2, cmd3 )

        BrandingSavedMsg res ->
            let
                ( state2, cmd1 ) =
                    update
                        (PageMsg Page.brandingSavedMsg)
                        state1

                outMsg =
                    case res of
                        Ok () ->
                            Global.flashSuccess
                                "Branding saved"

                        Err err ->
                            Global.flashError <|
                                "Error saving branding: "
                                    ++ Util.httpErrorToString err

                cmd2 =
                    Util.msgToCmd <| Left outMsg

                cmd3 =
                    Cmd.batch
                        [ cmd1, cmd2 ]
            in
            ( state2, cmd3 )


defaultView : Html msg
defaultView =
    div []
        [ text "Loading Branded Domain.." ]


view : ViewHandler
view state =
    let
        mBody1 =
            Page.view state.pageState

        mBody2 =
            Maybe.map
                (Html.map PageMsg)
                mBody1

        body =
            Maybe.withDefault
                defaultView
                mBody2
    in
    body


loadCmd : String -> Cmd Msg
loadCmd brandedDomainId =
    Cmd.batch
        [ getBrandingCmd brandedDomainId
        , getThemesCmd brandedDomainId
        ]


getThemesCmd : String -> Cmd Msg
getThemesCmd brandedDomainId =
    let
        brandingUrl =
            "/adminonly/brandeddomain/themes/" ++ brandedDomainId
    in
    Http.get
        { url = brandingUrl
        , expect =
            Http.expectJson OnAvailableThemesMsg ThemeJson.themesDecoder
        }


getBrandingCmd : String -> Cmd Msg
getBrandingCmd brandedDomainId =
    let
        brandingUrl =
            "/adminonly/brandeddomain/details/" ++ brandedDomainId
    in
    Http.get
        { url = brandingUrl
        , expect =
            Http.expectJson
                OnBrandingInfoMsg
                BrandingJson.brandingDecoder
        }


saveBrandingCmd : String -> Branding -> Cmd Msg
saveBrandingCmd xtoken branding =
    let
        brandedDomainId =
            branding.brandedDomainId

        brandingJson =
            JE.encode 0 <|
                BrandingJson.encodeBranding branding

        formBody1 =
            [ ( "domain", brandingJson )
            , ( "xtoken", xtoken )
            ]

        formBody2 =
            Util.formBody formBody1

        cmd1 =
            Http.post
                { url =
                    "/adminonly/brandeddomain/details/change/"
                        ++ brandedDomainId
                , body = formBody2
                , expect =
                    Http.expectWhatever
                        BrandingSavedMsg
                }
    in
    cmd1


saveThemeCmd : String -> String -> Theme -> Cmd Msg
saveThemeCmd xtoken brandedDomainId theme =
    let
        themeJson =
            JE.encode 0 <|
                ThemeJson.encodeTheme theme

        url =
            "/adminonly/brandeddomain/updatetheme/"
                ++ brandedDomainId
                ++ "/"
                ++ theme.id

        formBody1 =
            Util.formBody
                [ ( "theme", themeJson )
                , ( "xtoken", xtoken )
                ]

        cmd1 =
            Http.post
                { url = url
                , body = formBody1
                , expect =
                    Http.expectWhatever
                        ThemeSavedMsg
                }
    in
    cmd1


createThemeCmd : String -> NewTheme -> Cmd Msg
createThemeCmd xtoken newTheme =
    let
        url =
            "/adminonly/brandeddomain/newtheme/"
                ++ newTheme.brandedDomainId
                ++ "/"
                ++ newTheme.originalThemeId

        formBody1 =
            Util.formBody
                [ ( "name", newTheme.newThemeName )
                , ( "xtoken", xtoken )
                ]

        cmd1 =
            Http.post
                { url = url
                , body = formBody1
                , expect =
                    Http.expectWhatever
                        ThemeCreatedMsg
                }
    in
    cmd1


handleOutMsg : Page.OutMsg -> State -> Cmd (Either OutMsg Msg)
handleOutMsg outMsg state =
    case outMsg of
        SuccessPage.SaveBrandingMsg branding ->
            Cmd.map Right <|
                saveBrandingCmd state.xtoken branding

        SuccessPage.SaveThemeMsg brandedDomainId theme ->
            Cmd.map Right <|
                saveThemeCmd state.xtoken brandedDomainId theme

        SuccessPage.CreateThemeMsg newTheme ->
            Cmd.map Right <|
                createThemeCmd state.xtoken newTheme

        SuccessPage.GoBack ->
            Util.msgToCmd <| Left Global.GoToBrandedDomainsTabMsg


pageMsgToMsg : Either Page.OutMsg Page.Msg -> Either OutMsg Msg
pageMsgToMsg msg1 =
    case msg1 of
        Left msg2 ->
            Right <| PageOutMsg msg2

        Right msg2 ->
            Right <| PageMsg msg2


mergeMaybe : Maybe a -> Maybe b -> Maybe ( a, b )
mergeMaybe mx my =
    Maybe.andThen
        (\x ->
            Maybe.map
                (\y ->
                    ( x, y )
                )
                my
        )
        mx


defaultThemeSetFromThemes : List Theme -> Maybe ThemeSet
defaultThemeSetFromThemes availableThemes =
    let
        findTheme : String -> Maybe Theme
        findTheme themeName =
            List.find
                (\theme -> theme.name == themeName)
                availableThemes

        mThemeSet1 =
            mergeMaybe
                (findTheme "Scrive email theme")
            <|
                mergeMaybe
                    (findTheme "Scrive signing theme")
                <|
                    mergeMaybe
                        (findTheme "Scrive service theme")
                        (findTheme "Scrive login theme")

        mThemeSet2 : Maybe ThemeSet
        mThemeSet2 =
            Maybe.map
                (\( emailTheme, ( signViewTheme, ( serviceTheme, loginTheme ) ) ) ->
                    { emailTheme = emailTheme
                    , signViewTheme = signViewTheme
                    , serviceTheme = serviceTheme
                    , loginTheme = loginTheme
                    }
                )
                mThemeSet1
    in
    mThemeSet2


themeSetFromBranding : List Theme -> Branding -> Maybe ThemeSet
themeSetFromBranding availableThemes branding =
    let
        themeIds =
            branding.themeIds

        findTheme : String -> Maybe Theme
        findTheme themeId =
            List.find
                (\theme -> theme.id == themeId)
                availableThemes

        mThemeSet1 =
            mergeMaybe
                (findTheme themeIds.emailTheme)
            <|
                mergeMaybe
                    (findTheme themeIds.signViewTheme)
                <|
                    mergeMaybe
                        (findTheme themeIds.serviceTheme)
                        (findTheme themeIds.loginTheme)

        mThemeSet2 : Maybe ThemeSet
        mThemeSet2 =
            Maybe.map
                (\( emailTheme, ( signViewTheme, ( serviceTheme, loginTheme ) ) ) ->
                    { emailTheme = emailTheme
                    , signViewTheme = signViewTheme
                    , serviceTheme = serviceTheme
                    , loginTheme = loginTheme
                    }
                )
                mThemeSet1
    in
    mThemeSet2


stateToPageConfig : State -> Page.Config
stateToPageConfig state =
    let
        mLoaded =
            mergeMaybe
                state.brandingInfo
            <|
                state.availableThemes

        mConfig =
            Maybe.map
                (\loaded ->
                    let
                        ( brandingInfo, availableThemes ) =
                            loaded

                        mThemeSet1 =
                            themeSetFromBranding
                                availableThemes
                                brandingInfo

                        mThemeSet2 =
                            defaultThemeSetFromThemes
                                availableThemes

                        mConfig2 : Maybe SuccessPage.Config
                        mConfig2 =
                            Maybe.map
                                (\( currentThemeSet, defaultThemeSet ) ->
                                    { brandingInfo = brandingInfo
                                    , availableThemes = availableThemes
                                    , defaultThemeSet = defaultThemeSet
                                    , currentThemeSet = currentThemeSet
                                    }
                                )
                            <|
                                mergeMaybe mThemeSet1 mThemeSet2

                        config3 : Either String SuccessPage.Config
                        config3 =
                            case mConfig2 of
                                Just config4 ->
                                    Right config4

                                Nothing ->
                                    Left "Error loading current theme set"
                    in
                    config3
                )
                mLoaded
    in
    mConfig
