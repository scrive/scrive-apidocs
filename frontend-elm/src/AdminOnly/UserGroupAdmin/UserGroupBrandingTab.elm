module AdminOnly.UserGroupAdmin.UserGroupBrandingTab exposing (..)

import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.EditUserGroupBranding as EditUserGroupBranding
import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Json exposing (..)
import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Either exposing (Either(..))
import EnumExtra as Enum exposing (Enum)
import FlashMessage exposing (FlashMessage)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Lib.Components.EditTheme as EditTheme
import Lib.Components.PreviewTheme exposing (..)
import Lib.Json.Theme exposing (..)
import Lib.Types.Theme exposing (..)
import List.Extra as List
import Maybe exposing (withDefault)
import Result exposing (Result(..))
import Utils exposing (..)
import Vendor.ColorPickerExtra as ColorPicker
import Vendor.Popover as Popover


tabName : String
tabName =
    "branding"


type Page
    = EditBrandingTab
    | EditThemeTab


enumEditTab : Enum Page
enumEditTab =
    let
        allValues =
            [ EditBrandingTab, EditThemeTab ]

        toString t =
            case t of
                EditBrandingTab ->
                    "branding"

                EditThemeTab ->
                    "theme"

        toHumanString =
            toString
    in
    Enum.makeEnum allValues toString toHumanString


type alias LoadingState =
    { domainThemesLoaded : Bool
    , userGroupBrandingLoaded : Bool
    , userGroupThemesLoaded : Bool
    , inheritedThemesLoaded : Bool
    }


type alias State =
    { loadingState : LoadingState
    , editTabPage : Page
    , editTabState : Tab.State
    , previewTabState : Tab.State
    , userGroupThemes : List Theme
    , inheritedThemes : List Theme
    , domainThemes : Enum.Dict ThemeKind Theme
    , editThemeState :
        { themeBeingEdited : Theme
        , colorPickers : Enum.Dict ColorIdentifier ColorPicker.State
        , popovers : Enum.Dict ColorIdentifier Popover.State
        }
    , editUserGroupBrandingState :
        { brandingBeingEdited : UserGroupBranding
        , inherit : Bool
        }
    , inheritableBranding : Maybe UserGroupBranding
    , brandingInherited : Bool

    -- ^ If `True` then the branding is inherited from the parent group; this
    -- means in particular that the themes referenced by the branding are owned by
    -- that group and are therefore not editable!
    , ugid : String
    }


type Msg
    = EditThemeMsg EditTheme.Msg
    | EditUserGroupBrandingMsg EditUserGroupBranding.Msg
    | SetEditTabStateMsg Tab.State
    | SetPreviewTabStateMsg Tab.State
    | SetDomainThemesMsg (Enum.Dict ThemeKind Theme)
    | SetUserGroupThemesMsg (List Theme)
    | SetInheritedThemesMsg (List Theme)
    | SetUserGroupBrandingMsg UserGroupBranding Bool (Maybe UserGroupBranding)
    | DeleteThemeCallbackMsg ThemeID
    | CreateThemeCallbackMsg Theme ThemeID
    | SaveThemeMsg Theme
    | DoCreateThemeMsg Theme
    | DoDeleteThemeMsg
    | DoSaveThemeMsg
    | DoSaveUserGroupBrandingMsg
    | InheritUserGroupBrandingCallbackMsg
    | PresentFlashMessage FlashMessage


update : (Msg -> msg) -> Globals msg -> Msg -> State -> ( State, Cmd msg )
update embed globals msg =
    case msg of
        SetEditTabStateMsg tabState ->
            setEditTabState tabState

        SetPreviewTabStateMsg tabState ->
            setPreviewTabState tabState

        SetDomainThemesMsg domainThemes ->
            setDomainThemes domainThemes

        SetUserGroupThemesMsg themes ->
            setUserGroupThemes themes

        SetInheritedThemesMsg themes ->
            setInheritedThemes themes

        SetUserGroupBrandingMsg branding brandingInherited brandingInheritable ->
            setUserGroupBranding branding brandingInherited brandingInheritable

        DeleteThemeCallbackMsg id ->
            deleteThemeCallback globals.flashMessage id

        CreateThemeCallbackMsg blueprint newThemeID ->
            createThemeCallback embed (formBody globals) blueprint newThemeID

        SaveThemeMsg theme ->
            saveTheme globals.flashMessage theme

        DoCreateThemeMsg blueprint ->
            doCreateTheme embed (formBody globals) blueprint

        DoDeleteThemeMsg ->
            doDeleteTheme embed (formBody globals)

        DoSaveThemeMsg ->
            doSaveTheme embed (formBody globals)

        DoSaveUserGroupBrandingMsg ->
            doSaveUserGroupBranding embed (formBody globals)

        InheritUserGroupBrandingCallbackMsg ->
            inheritUserGroupBrandingCallback embed (formBody globals)

        EditThemeMsg msg_ ->
            \state ->
                let
                    editThemeReadonly =
                        { availableThemes = Enum.values state.domainThemes ++ state.userGroupThemes
                        }

                    ( newEditThemeState, cmd ) =
                        EditTheme.update (embed << EditThemeMsg) msg_ editThemeReadonly state.editThemeState
                in
                ( { state | editThemeState = newEditThemeState }, cmd )

        EditUserGroupBrandingMsg msg_ ->
            \state ->
                let
                    ( newBrandingState, cmd ) =
                        EditUserGroupBranding.update (embed << EditUserGroupBrandingMsg) msg_ state.editUserGroupBrandingState

                    newBranding =
                        newBrandingState.brandingBeingEdited

                    branding =
                        state.editUserGroupBrandingState.brandingBeingEdited

                    mPreviewThemeKind =
                        List.find (\kind -> Enum.get kind newBranding.themes /= Enum.get kind branding.themes) <|
                            Enum.allValues enumThemeKind

                    newPreviewTabState =
                        Maybe.withDefault state.previewTabState <|
                            Maybe.map (Tab.customInitialState << Enum.toString enumThemeKind) mPreviewThemeKind
                in
                ( { state | editUserGroupBrandingState = newBrandingState, previewTabState = newPreviewTabState }, cmd )

        PresentFlashMessage message ->
            \state -> ( state, globals.flashMessage message )


updatePage : (Msg -> msg) -> { ugid : String, page : Page } -> State -> ( State, Cmd msg )
updatePage embed params state =
    if params.ugid == state.ugid then
        ( { state | editTabPage = params.page }, Cmd.none )

    else
        init embed { page = params.page, ugid = params.ugid }


pageFromModel : State -> Page
pageFromModel =
    .editTabPage


init : (Msg -> msg) -> { page : Page, ugid : String } -> ( State, Cmd msg )
init embed { page, ugid } =
    let
        initialState =
            { loadingState =
                { domainThemesLoaded = False
                , userGroupBrandingLoaded = False
                , userGroupThemesLoaded = False
                , inheritedThemesLoaded = False
                }
            , editTabPage = page
            , editTabState = Tab.customInitialState <| Enum.toString enumEditTab page
            , previewTabState = Tab.customInitialState <| Enum.toString enumThemeKind EmailTheme
            , userGroupThemes = []
            , inheritedThemes = []
            , inheritableBranding = Nothing
            , domainThemes = Enum.empty enumThemeKind
            , editThemeState =
                { themeBeingEdited = errorTheme
                , colorPickers =
                    Enum.fromList enumColorIdentifier <|
                        List.map
                            (\ident ->
                                ( ident, ColorPicker.initWithId <| "colorpicker-" ++ Enum.toString enumColorIdentifier ident )
                            )
                        <|
                            Enum.allValues enumColorIdentifier
                , popovers =
                    Enum.fromList enumColorIdentifier <|
                        List.map (\ident -> ( ident, Popover.initialState )) <|
                            Enum.allValues enumColorIdentifier
                }
            , editUserGroupBrandingState =
                { brandingBeingEdited = defaultUserGroupBranding
                , inherit = False
                }
            , brandingInherited = True
            , ugid = ugid
            }
    in
    ( initialState
    , Cmd.batch
        [ getDomainThemes embed
        , getUserGroupBranding embed ugid
        , getUserGroupThemes embed ugid
        , getInheritedThemes embed ugid
        ]
    )


setEditTabState : Tab.State -> State -> ( State, Cmd msg )
setEditTabState tabState state =
    ( { state | editTabState = tabState }, Cmd.none )


setPreviewTabState : Tab.State -> State -> ( State, Cmd msg )
setPreviewTabState tabState state =
    ( { state | previewTabState = tabState }, Cmd.none )


view : (Msg -> msg) -> State -> Html msg
view embed state =
    if
        state.loadingState.userGroupBrandingLoaded
            && state.loadingState.userGroupThemesLoaded
            && state.loadingState.domainThemesLoaded
            && state.loadingState.inheritedThemesLoaded
    then
        Html.map embed <| viewLoaded state

    else
        text "Loading"


viewLoaded : State -> Html Msg
viewLoaded state =
    let
        editTabConfig =
            Tab.useHash True <| Tab.config SetEditTabStateMsg

        previewTabConfig =
            Tab.useHash False <| Tab.config SetPreviewTabStateMsg

        themeTab =
            if state.editUserGroupBrandingState.inherit then
                text "You can't edit inherited themes, sorry!"

            else
                EditTheme.viewEditTheme
                    { embed = EditThemeMsg
                    , doSaveTheme = DoSaveThemeMsg
                    , doDeleteTheme = DoDeleteThemeMsg
                    , doCreateTheme = DoCreateThemeMsg
                    }
                    { availableThemes = Enum.values state.domainThemes ++ state.userGroupThemes
                    }
                    state.editThemeState

        brandingReadonly =
            { domainThemes = state.domainThemes
            , userGroupThemes = state.userGroupThemes
            , inheritedThemes = state.inheritedThemes
            , inheritableBranding = state.inheritableBranding
            }

        brandingPreview =
            EditUserGroupBranding.brandingPreview
                brandingReadonly
                state.editUserGroupBrandingState

        brandingTab =
            EditUserGroupBranding.viewEditUserGroupBranding
                { embed = EditUserGroupBrandingMsg
                , doSaveUserGroupBranding = DoSaveUserGroupBrandingMsg
                }
                brandingReadonly
                state.editUserGroupBrandingState

        previewThemeSet : ThemeKind -> Theme
        previewThemeSet kind =
            case state.editTabPage of
                EditThemeTab ->
                    state.editThemeState.themeBeingEdited

                EditBrandingTab ->
                    Maybe.withDefault errorTheme <|
                        case Enum.get kind brandingPreview.themes of
                            Nothing ->
                                Enum.get kind state.domainThemes

                            Just id ->
                                List.find (\theme -> theme.id == id) <|
                                    state.userGroupThemes
                                        ++ state.inheritedThemes

        viewThemePreview kind =
            case kind of
                EmailTheme ->
                    viewEmailThemePreview

                SignViewTheme ->
                    viewSignViewThemePreview

                ServiceTheme ->
                    viewServiceThemePreview

        themePreviewItems =
            List.map
                (\kind ->
                    Tab.item
                        { id = Enum.toString enumThemeKind kind
                        , link = Tab.link [] [ text <| Enum.toHumanString enumThemeKind kind ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ]
                                [ viewThemePreview kind <| previewThemeSet kind ]
                        }
                )
            <|
                Enum.allValues enumThemeKind
    in
    Grid.row []
        [ Grid.col [ Col.sm12, Col.md5 ]
            [ Tab.view state.editTabState <|
                Tab.items
                    [ Tab.item
                        { id = Enum.toString enumEditTab EditBrandingTab
                        , link = Tab.link [] [ text "User Group Branding" ]
                        , pane = Tab.pane [ Spacing.mt3 ] [ brandingTab ]
                        }
                    , Tab.item
                        { id = Enum.toString enumEditTab EditThemeTab
                        , link = Tab.link [] [ text "Manage Themes" ]
                        , pane = Tab.pane [ Spacing.mt3 ] [ themeTab ]
                        }
                    ]
                    editTabConfig
            ]
        , Grid.col [ Col.sm12, Col.md7 ]
            [ div [ class "scrive" ]
                [ Tab.view state.previewTabState <|
                    Tab.items themePreviewItems previewTabConfig
                ]
            ]
        ]


setDomainThemes : Enum.Dict ThemeKind Theme -> State -> ( State, Cmd msg )
setDomainThemes domainThemes state =
    let
        editThemeState =
            state.editThemeState

        loadingState =
            state.loadingState

        newState =
            { state
                | domainThemes = domainThemes
                , loadingState = { loadingState | domainThemesLoaded = True }
                , editThemeState =
                    { editThemeState
                        | themeBeingEdited =
                            withDefault editThemeState.themeBeingEdited <|
                                Enum.get EmailTheme domainThemes
                    }
            }
    in
    ( newState, Cmd.none )


getDomainThemes : (Msg -> msg) -> Cmd msg
getDomainThemes embed =
    let
        callback : Result Http.Error (Enum.Dict ThemeKind Theme) -> msg
        callback result =
            case result of
                Ok domainThemes ->
                    embed <| SetDomainThemesMsg domainThemes

                _ ->
                    embed <| PresentFlashMessage <| FlashMessage.error "Failed to get domain themes."

        -- Domain themes are the themes we implicitly get if we set a theme id to
        -- null in the branding. We manually assign negative ids as a hack to a)
        -- express that these should be read-only and b) disambiguate between
        -- them, since they might correspond to the same underlying theme.
        toDomainThemes : Theme -> Theme -> Theme -> Enum.Dict ThemeKind Theme
        toDomainThemes mailTheme signViewTheme serviceTheme =
            Enum.fromList enumThemeKind
                [ ( EmailTheme, { mailTheme | name = "Domain Email Theme", id = -1 } )
                , ( SignViewTheme, { signViewTheme | name = "Domain Sign View Theme", id = -2 } )
                , ( ServiceTheme, { serviceTheme | name = "Domain Service Theme", id = -3 } )
                ]

        decoder : JD.Decoder (Enum.Dict ThemeKind Theme)
        decoder =
            JD.field "themes" <|
                JD.map3 toDomainThemes
                    (JD.index 0 themeDecoder)
                    (JD.index 1 themeDecoder)
                    (JD.index 2 themeDecoder)
    in
    Http.get
        { url = "/adminonly/companyadmin/branding/companybranding/domainthemes"
        , expect = Http.expectJson callback decoder
        }


setUserGroupThemes : List Theme -> State -> ( State, Cmd msg )
setUserGroupThemes themes state =
    let
        loadingState =
            state.loadingState

        newState =
            { state
                | userGroupThemes = themes
                , loadingState = { loadingState | userGroupThemesLoaded = True }
            }
    in
    ( newState, Cmd.none )


setInheritedThemes : List Theme -> State -> ( State, Cmd msg )
setInheritedThemes themes state =
    let
        loadingState =
            state.loadingState

        newState =
            { state
                | inheritedThemes = themes
                , loadingState = { loadingState | inheritedThemesLoaded = True }
            }
    in
    ( newState, Cmd.none )


getUserGroupThemes_ : Bool -> (Msg -> msg) -> String -> Cmd msg
getUserGroupThemes_ inherited embed ugid =
    let
        setThemesMsg =
            if inherited then
                SetInheritedThemesMsg

            else
                SetUserGroupThemesMsg

        callback : Result Http.Error (List Theme) -> msg
        callback result =
            case result of
                Ok themes ->
                    embed <| setThemesMsg themes

                _ ->
                    embed <| PresentFlashMessage <| FlashMessage.error "Failed to get user group themes."
    in
    Http.get
        { url =
            "/adminonly/companyadmin/branding/companybranding/themes/"
                ++ ugid
                ++ "?inherited="
                ++ (if inherited then
                        "true"

                    else
                        "false"
                   )
        , expect = Http.expectJson callback <| JD.field "themes" <| JD.list themeDecoder
        }


getUserGroupThemes : (Msg -> msg) -> String -> Cmd msg
getUserGroupThemes =
    getUserGroupThemes_ False


getInheritedThemes : (Msg -> msg) -> String -> Cmd msg
getInheritedThemes =
    getUserGroupThemes_ True


setUserGroupBranding : UserGroupBranding -> Bool -> Maybe UserGroupBranding -> State -> ( State, Cmd msg )
setUserGroupBranding branding brandingInherited inheritableBranding state =
    let
        newBrandingBeingEdited =
            if brandingInherited then
                defaultUserGroupBranding

            else
                branding

        loadingState =
            state.loadingState

        newState =
            { state
                | loadingState = { loadingState | userGroupBrandingLoaded = True }
                , editUserGroupBrandingState =
                    { brandingBeingEdited = newBrandingBeingEdited
                    , inherit = brandingInherited
                    }
                , brandingInherited = brandingInherited
                , inheritableBranding = inheritableBranding
            }
    in
    ( newState, Cmd.none )


getUserGroupBranding : (Msg -> msg) -> String -> Cmd msg
getUserGroupBranding embed ugid =
    let
        callback : Result Http.Error ( UserGroupBranding, Maybe String, Maybe UserGroupBranding ) -> msg
        callback result =
            case result of
                Ok ( branding, inherited_from, inheritable_preview ) ->
                    embed <| SetUserGroupBrandingMsg branding (isJust inherited_from) inheritable_preview

                _ ->
                    embed <| PresentFlashMessage <| FlashMessage.error "Failed to get user group branding."
    in
    Http.get
        { url = "/adminonly/companyadmin/branding/companybranding/" ++ ugid
        , expect =
            Http.expectJson callback <|
                JD.map3 (\a b c -> ( a, b, c ))
                    brandingDecoder
                    (JD.field "inherited_from" <| JD.nullable JD.string)
                    (JD.field "inheritable_preview" <| JD.maybe brandingDecoder)
        }


doSaveUserGroupBranding : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> State -> ( State, Cmd msg )
doSaveUserGroupBranding embed formBody state =
    if state.brandingInherited /= state.editUserGroupBrandingState.inherit then
        setInheritUserGroupBranding embed formBody state

    else if state.brandingInherited then
        ( state, Cmd.none )

    else
        saveUserGroupBranding embed formBody state


saveUserGroupBranding : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> State -> ( State, Cmd msg )
saveUserGroupBranding embed formBody state =
    let
        callback : Result Http.Error () -> msg
        callback result =
            case result of
                Ok _ ->
                    embed <| PresentFlashMessage <| FlashMessage.success "User group branding saved."

                _ ->
                    embed <| PresentFlashMessage <| FlashMessage.error "Failed to save user group branding."

        cmd =
            Http.post
                { url = "/adminonly/companyadmin/branding/companybranding/change/" ++ state.ugid
                , body =
                    formBody
                        [ ( "companyui"
                          , JE.encode 0 <|
                                encodeUserGroupBranding
                                    state.ugid
                                    state.editUserGroupBrandingState.brandingBeingEdited
                          )
                        ]
                , expect = Http.expectWhatever callback
                }
    in
    ( state, cmd )


inheritUserGroupBrandingCallback : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> State -> ( State, Cmd msg )
inheritUserGroupBrandingCallback embed formBody state =
    if state.editUserGroupBrandingState.inherit then
        let
            newState =
                { state
                    | brandingInherited = True
                    , editUserGroupBrandingState =
                        { brandingBeingEdited = defaultUserGroupBranding
                        , inherit = True
                        }
                }

            brandingSavedMsg =
                embed <|
                    PresentFlashMessage <|
                        FlashMessage.success "User group branding saved."
        in
        ( newState, perform brandingSavedMsg )

    else
        let
            newState =
                { state
                    | brandingInherited = False
                }
        in
        saveUserGroupBranding embed formBody newState


setInheritUserGroupBranding : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> State -> ( State, Cmd msg )
setInheritUserGroupBranding embed formBody state =
    let
        callback result =
            case result of
                Ok _ ->
                    embed InheritUserGroupBrandingCallbackMsg

                _ ->
                    embed <| PresentFlashMessage <| FlashMessage.error "Failed to save user group branding."

        cmd =
            Http.post
                { url = "/adminonly/companyadmin/branding/companybranding/inherit/" ++ state.ugid
                , body =
                    formBody
                        [ ( "inherit"
                          , if state.editUserGroupBrandingState.inherit then
                                "true"

                            else
                                "false"
                          )
                        ]
                , expect = Http.expectWhatever callback
                }
    in
    ( state, cmd )


saveTheme : (FlashMessage -> Cmd msg) -> Theme -> State -> ( State, Cmd msg )
saveTheme presentFlashMessage theme state =
    let
        newState =
            { state
                | userGroupThemes =
                    List.map
                        (\theme_ ->
                            if theme_.id == theme.id then
                                theme

                            else
                                theme_
                        )
                        state.userGroupThemes
            }

        cmd =
            presentFlashMessage <| FlashMessage.success "Theme saved."
    in
    ( newState, cmd )


doSaveTheme : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> State -> ( State, Cmd msg )
doSaveTheme embed formBody state =
    let
        theme =
            state.editThemeState.themeBeingEdited

        callback : Result Http.Error () -> msg
        callback result =
            case result of
                Ok _ ->
                    embed <| SaveThemeMsg theme

                _ ->
                    embed <| PresentFlashMessage <| FlashMessage.error "Failed to save theme."

        cmd =
            Http.post
                { url =
                    "/adminonly/companyadmin/branding/companybranding/updatetheme/"
                        ++ state.ugid
                        ++ "/"
                        ++ String.fromInt theme.id
                , body =
                    formBody
                        [ ( "theme", JE.encode 0 <| encodeTheme theme ) ]
                , expect = Http.expectWhatever callback
                }
    in
    ( state, cmd )


deleteThemeCallback : (FlashMessage -> Cmd msg) -> ThemeID -> State -> ( State, Cmd msg )
deleteThemeCallback presentFlashMessage id state =
    let
        newUserGroupThemes =
            List.filter (\theme -> theme.id /= id) state.userGroupThemes

        editThemeState =
            state.editThemeState

        newThemeBeingEdited =
            if editThemeState.themeBeingEdited.id /= id then
                editThemeState.themeBeingEdited

            else
                withDefault errorTheme <|
                    List.head <|
                        newUserGroupThemes
                            ++ Enum.values state.domainThemes

        newState =
            { state
                | userGroupThemes = newUserGroupThemes
                , editThemeState = { editThemeState | themeBeingEdited = newThemeBeingEdited }
            }

        cmd =
            presentFlashMessage <| FlashMessage.success "Theme deleted."
    in
    ( newState, cmd )


doDeleteTheme : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> State -> ( State, Cmd msg )
doDeleteTheme embed formBody state =
    let
        callback : Result Http.Error () -> msg
        callback result =
            case result of
                Ok _ ->
                    embed <| DeleteThemeCallbackMsg theme.id

                _ ->
                    embed <|
                        PresentFlashMessage <|
                            FlashMessage.error "Failed to get delete theme. You probably tried to delete a theme that is still in use."

        theme =
            state.editThemeState.themeBeingEdited

        cmd =
            Http.post
                { url =
                    "/adminonly/companyadmin/branding/companybranding/deletetheme/"
                        ++ state.ugid
                        ++ "/"
                        ++ String.fromInt theme.id
                , body =
                    formBody
                        [ ( "theme", JE.encode 0 <| encodeTheme theme ) ]
                , expect = Http.expectWhatever callback
                }
    in
    ( state, cmd )



-- we need to update the newly created theme to the correct blueprint


createThemeCallback : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> Theme -> ThemeID -> State -> ( State, Cmd msg )
createThemeCallback embed formBody blueprint newThemeID state =
    let
        newTheme =
            { blueprint
                | id = newThemeID
                , name = "Copy of " ++ blueprint.name
            }

        editThemeState =
            state.editThemeState

        newState =
            { state
                | editThemeState = { editThemeState | themeBeingEdited = newTheme }
                , userGroupThemes = state.userGroupThemes ++ [ newTheme ]
            }
    in
    doSaveTheme embed formBody newState


doCreateTheme : (Msg -> msg) -> (List ( String, String ) -> Http.Body) -> Theme -> State -> ( State, Cmd msg )
doCreateTheme embed formBody blueprint state =
    let
        callback : Result Http.Error Theme -> msg
        callback result =
            case result of
                Ok new ->
                    embed <| CreateThemeCallbackMsg blueprint new.id

                _ ->
                    embed <| PresentFlashMessage <| FlashMessage.error "Failed to create theme."

        cmd =
            Http.post
                { url =
                    "/adminonly/companyadmin/branding/companybranding/newtheme/"
                        ++ state.ugid
                        ++ "/mail"
                , body = formBody [ ( "name", "New theme" ) ]
                , expect = Http.expectJson callback themeDecoder
                }
    in
    ( state, cmd )
