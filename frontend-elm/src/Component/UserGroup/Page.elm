module Component.UserGroup.Page exposing (Config, Init, Msg(..), OutMsg(..), State, UpdateHandler, ViewHandler, currentThemeSetFromBranding, defaultThemeSetFromThemes, defaultView, getBrandingCmd, getDefaultThemesCmd, getThemesCmd, handleOutMsg, initialize, loadCmd, mergeMaybe, pageMsgToMsg, saveBrandingCmd, saveThemeCmd, stateToPageConfig, update, updatePage, updatePageConfig, updatePageError, view)

import Component.FlashMessage as FlashMessage
import Component.Theme.Data exposing (Theme)
import Component.Theme.Json as ThemeJson
import Component.UserGroup.Data exposing (Branding, ThemeSet)
import Component.UserGroup.Json as BrandingJson
import Component.UserGroup.Page.Level3 as Page
import Component.UserGroup.Page.Level2 as SuccessPage exposing (NewTheme)
import Component.Branding.DeleteTheme.Optional as DeleteTheme
import Component.Branding.DeleteTheme.Data as DeleteThemeData
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html, div, text)
import Http
import Json.Encode as JE
import List.Extra as List
import Util.Http as Util


type alias Config =
    { xtoken : String
    , userGroupId : String
    }


type alias State =
    { xtoken : String
    , pageState : Page.State
    , deleteThemeState : DeleteTheme.State
    , userGroupId : String
    , brandingInfo : Maybe Branding
    , defaultThemeSet : Maybe ThemeSet
    , availableThemes : Maybe (List Theme)
    }


type Msg
    = PageMsg Page.Msg
    | PageOutMsg Page.OutMsg
    | SetUserGroupId String
    | DeleteThemeOutMsg DeleteTheme.OutMsg
    | DeleteThemeMsg DeleteTheme.Msg
    | OnAvailableThemesMsg (Result Http.Error (List Theme))
    | OnDefaultThemesMsg (Result Http.Error (List Theme))
    | OnBrandingInfoMsg (Result Http.Error Branding)
    | ThemeSavedMsg (Result Http.Error ())
    | ThemeDeletedMsg (Result Http.Error ())
    | BrandingSavedMsg (Result Http.Error ())
    | ThemeCreatedMsg (Result Http.Error ())


type OutMsg
    = FlashMsg FlashMessage.Msg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize config =
    let
        userGroupId =
            config.userGroupId

        ( pageState, cmd1 ) =
            Page.initEmpty

        state =
            { xtoken = config.xtoken
            , pageState = pageState
            , deleteThemeState = Nothing
            , userGroupId = userGroupId
            , brandingInfo = Nothing
            , defaultThemeSet = Nothing
            , availableThemes = Nothing
            }

        cmd2 =
            loadCmd userGroupId

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

mapDeleteThemeMsg : Either DeleteTheme.OutMsg DeleteTheme.Msg -> Either OutMsg Msg
mapDeleteThemeMsg msg1 = case msg1 of
    Left msg2 ->
        Right <| DeleteThemeOutMsg msg2
    Right msg2 ->
        Right <| DeleteThemeMsg msg2

update : UpdateHandler
update msg1 state1 =
    case msg1 of
        PageMsg msg2 ->
            updatePage msg2 state1

        PageOutMsg msg2 ->
            handleOutMsg msg2 state1

        DeleteThemeMsg msg2 ->
            let
                (state2, cmd1) =
                    DeleteTheme.update msg2 state1.deleteThemeState

                state3 = { state1 | deleteThemeState = state2 }
            in
            (state3, Cmd.map mapDeleteThemeMsg cmd1)

        DeleteThemeOutMsg msg2 ->
            case msg2 of
                DeleteThemeData.ConfirmDeleteMsg theme ->
                    ( state1
                    , Cmd.map Right <| deleteThemeCmd
                        state1.xtoken state1.userGroupId theme
                    )

                DeleteThemeData.CancelDeleteMsg ->
                    update
                        (PageMsg Page.doneDeleteThemeMsg)
                        state1

        SetUserGroupId userGroupId ->
            let
                msg2 =
                    Page.clearMsg

                ( state2, cmd1 ) =
                    updatePage msg2 state1

                cmd2 =
                    Cmd.map Right <| loadCmd userGroupId

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

        OnDefaultThemesMsg result ->
            case result of
                Ok themes ->
                    let
                        mThemeSet =
                            defaultThemeSetFromThemes themes
                    in
                    case mThemeSet of
                        Just themeSet ->
                            let
                                state2 =
                                    { state1
                                        | defaultThemeSet = Just themeSet
                                    }

                                pageConfig =
                                    stateToPageConfig state2
                            in
                            updatePageConfig pageConfig state2

                        Nothing ->
                            let
                                error =
                                    "Error loading default themes from response"
                            in
                            updatePage
                                (Page.initErrorMsg error)
                                state1

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
                        (PageMsg Page.doneSaveThemeMsg)
                        state1

                outMsg =
                    case res of
                        Ok () ->
                            FlashMessage.SuccessMsg
                                "Theme saved"

                        Err err ->
                            FlashMessage.ErrorMsg <|
                                "Error saving theme: "
                                    ++ Util.httpErrorToString err

                cmd2 =
                    Util.msgToCmd <| Left <| FlashMsg outMsg

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
                        , userGroupId = state1.userGroupId
                        }

                outMsg =
                    case res of
                        Ok () ->
                            FlashMessage.SuccessMsg
                                "Theme saved"

                        Err err ->
                            FlashMessage.ErrorMsg <|
                                "Error saving theme: "
                                    ++ Util.httpErrorToString err

                cmd2 =
                    Util.msgToCmd <| Left <| FlashMsg outMsg

                cmd3 =
                    Cmd.batch
                        [ Cmd.map Right cmd1, cmd2 ]
            in
            ( state2, cmd3 )

        ThemeDeletedMsg res ->
            let
                ( state2, cmd1 ) =
                    initialize
                        { xtoken = state1.xtoken
                        , userGroupId = state1.userGroupId
                        }

                outMsg =
                    case res of
                        Ok () ->
                            FlashMessage.SuccessMsg
                                "Theme deleted"

                        Err err ->
                            FlashMessage.ErrorMsg <|
                                "Error deleting theme: "
                                    ++ Util.httpErrorToString err

                cmd2 =
                    Util.msgToCmd <| Left <| FlashMsg outMsg

                cmd3 =
                    Cmd.batch
                        [ Cmd.map Right cmd1, cmd2 ]
            in
            ( state2, cmd3 )

        BrandingSavedMsg res ->
            let
                ( state2, cmd1 ) =
                    initialize
                        { xtoken = state1.xtoken
                        , userGroupId = state1.userGroupId
                        }

                outMsg =
                    case res of
                        Ok () ->
                            FlashMessage.SuccessMsg
                                "Branding saved"

                        Err err ->
                            FlashMessage.ErrorMsg <|
                                "Error saving branding: "
                                    ++ Util.httpErrorToString err

                cmd2 =
                    Util.msgToCmd <| Left <| FlashMsg outMsg

                cmd3 =
                    Cmd.batch
                        [ Cmd.map Right cmd1, cmd2 ]
            in
            ( state2, cmd3 )


defaultView : Html msg
defaultView =
    div []
        [ text "Loading User Group Branding.." ]


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

        mDeleteThemeModal1 =
            Maybe.map (Html.map DeleteThemeMsg) <|
                DeleteTheme.view state.deleteThemeState

        deleteThemeModal2 =
            Maybe.withDefault
                (div [] [])
                mDeleteThemeModal1
    in
    div []
        [ body
        , deleteThemeModal2
        ]


loadCmd : String -> Cmd Msg
loadCmd userGroupId =
    Cmd.batch
        [ getBrandingCmd userGroupId
        , getThemesCmd userGroupId
        , getDefaultThemesCmd
        ]


getThemesCmd : String -> Cmd Msg
getThemesCmd userGroupId =
    let
        brandingUrl =
            "/adminonly/companyadmin/branding/companybranding/themes/" ++ userGroupId
    in
    Http.get
        { url = brandingUrl
        , expect =
            Http.expectJson OnAvailableThemesMsg ThemeJson.themesDecoder
        }


getDefaultThemesCmd : Cmd Msg
getDefaultThemesCmd =
    let
        brandingUrl =
            "/adminonly/companyadmin/branding/companybranding/domainthemes"
    in
    Http.get
        { url = brandingUrl
        , expect =
            Http.expectJson OnDefaultThemesMsg ThemeJson.themesDecoder
        }


getBrandingCmd : String -> Cmd Msg
getBrandingCmd userGroupId =
    let
        brandingUrl =
            "/adminonly/companyadmin/branding/companybranding/" ++ userGroupId
    in
    Http.get
        { url = brandingUrl
        , expect =
            Http.expectJson OnBrandingInfoMsg BrandingJson.brandingDecoder
        }


saveThemeCmd : String -> String -> Theme -> Cmd Msg
saveThemeCmd xtoken userGroupId theme =
    let
        themeJson =
            JE.encode 0 <|
                ThemeJson.encodeTheme theme

        url =
            "/adminonly/companyadmin/branding/companybranding/updatetheme/"
                ++ userGroupId
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

deleteThemeCmd : String -> String -> Theme -> Cmd Msg
deleteThemeCmd xtoken userGroupId theme =
    let
        themeJson =
            JE.encode 0 <|
                ThemeJson.encodeTheme theme

        url =
            "/adminonly/companyadmin/branding/companybranding/deletetheme/"
                ++ userGroupId
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
                        ThemeDeletedMsg
                }
    in
    cmd1


saveBrandingCmd : String -> ThemeSet -> Branding -> Cmd Msg
saveBrandingCmd xtoken defaultThemeSet branding =
    let
        userGroupId =
            branding.userGroupId

        brandingJson =
            JE.encode 0 <|
                BrandingJson.encodeBranding defaultThemeSet branding

        formBody1 =
            [ ( "companyui", brandingJson )
            , ( "xtoken", xtoken )
            ]

        formBody2 =
            Util.formBody formBody1

        url =
            "/adminonly/companyadmin/branding/companybranding/change/"
                ++ userGroupId

        cmd1 =
            Http.post
                { url = url
                , body = formBody2
                , expect =
                    Http.expectWhatever
                        BrandingSavedMsg
                }
    in
    cmd1


createThemeCmd : String -> NewTheme -> Cmd Msg
createThemeCmd xtoken newTheme =
    let
        url =
            "/adminonly/companyadmin/branding/companybranding/newtheme/"
                ++ newTheme.userGroupId
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

handleOutMsg : Page.OutMsg -> State -> (State, Cmd (Either OutMsg Msg))
handleOutMsg outMsg state =
    case outMsg of
        SuccessPage.SaveBrandingMsg branding ->
            let
                mCmd1 =
                    Maybe.map
                        (\defaultThemeSet ->
                            Cmd.map Right <|
                                saveBrandingCmd
                                    state.xtoken
                                    defaultThemeSet
                                    branding
                        )
                        state.defaultThemeSet

                cmd2 =
                    Maybe.withDefault Cmd.none mCmd1
            in
            (state, cmd2)

        SuccessPage.SaveThemeMsg brandedDomainId theme ->
            (state
            ,   Cmd.map Right <|
                    saveThemeCmd state.xtoken brandedDomainId theme
            )

        SuccessPage.DeleteThemeMsg _ theme ->
            let
                (state2, cmd1) = DeleteTheme.initialize <| Just theme

                state3 = { state | deleteThemeState = state2 }
            in
            (state3, Cmd.map (Right << DeleteThemeMsg) cmd1)

        SuccessPage.CreateThemeMsg newTheme ->
            ( state
            ,   Cmd.map Right <|
                    createThemeCmd state.xtoken newTheme
            )

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


currentThemeSetFromBranding : ThemeSet -> List Theme -> Branding -> ThemeSet
currentThemeSetFromBranding defaultThemeSet availableThemes branding =
    let
        findTheme : Maybe String -> Maybe Theme
        findTheme mThemeId =
            Maybe.andThen
                (\themeId ->
                    List.find
                        (\theme -> theme.id == themeId)
                        availableThemes
                )
                mThemeId

        themeIds =
            branding.themeIds
    in
    { emailTheme =
        Maybe.withDefault
            defaultThemeSet.emailTheme
        <|
            findTheme themeIds.emailTheme
    , signViewTheme =
        Maybe.withDefault
            defaultThemeSet.signViewTheme
        <|
            findTheme themeIds.signViewTheme
    , serviceTheme =
        Maybe.withDefault
            defaultThemeSet.serviceTheme
        <|
            findTheme themeIds.serviceTheme
    }


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
                    (findTheme "Scrive service theme")

        mThemeSet2 : Maybe ThemeSet
        mThemeSet2 =
            Maybe.map
                (\( emailTheme, ( signViewTheme, serviceTheme ) ) ->
                    { emailTheme = emailTheme
                    , signViewTheme = signViewTheme
                    , serviceTheme = serviceTheme
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
                mergeMaybe
                    state.availableThemes
                    state.defaultThemeSet

        mConfig =
            Maybe.map
                (\loaded ->
                    let
                        ( brandingInfo, ( availableThemes1, defaultThemeSet ) ) =
                            loaded

                        currentThemeSet =
                            currentThemeSetFromBranding
                                defaultThemeSet
                                availableThemes1
                                brandingInfo

                        config2 =
                            { brandingInfo = brandingInfo
                            , availableThemes = availableThemes1
                            , defaultThemeSet = defaultThemeSet
                            , currentThemeSet = currentThemeSet
                            }
                    in
                    Right config2
                )
                mLoaded
    in
    mConfig
