module AdminOnly.AdminOnly exposing
    ( HomeTab(..)
    , Model
    , Msg(..)
    , Page(..)
    , fromPage
    , init
    , pageFromModel
    , routeParser
    , update
    , updatePage
    , view
    )

import AdminOnly.BrandedDomain.BrandedDomainTab as BrandedDomain
import AdminOnly.BrandedDomain.BrandedDomainsTab as BrandedDomainsTab
import AdminOnly.UserAdmin.DocumentsTab.DocumentsTab as DocumentsTab exposing (Config(..))
import AdminOnly.UserAdmin.UserAdmin as UserAdmin
import AdminOnly.UserAdminTab.UserAdminTab as UserAdminTab
import AdminOnly.UserGroupAdmin.UserGroupAdmin as UserGroupAdmin
import AdminOnly.UserGroupAdmin.UserGroupAdminTab as UserGroupAdminTab
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Either exposing (Either(..))
import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Maybe as M
import Url.Parser as UP exposing ((</>), (<?>), Parser)
import Url.Parser.Query as UPQ
import Utils exposing (..)


type Page
    = Home HomeTab
    | UserAdmin UserAdmin.Page
    | UserGroupAdmin UserGroupAdmin.Page
    | BrandedDomain BrandedDomain.Page


type HomeTab
    = UserAdminTab UserAdminTab.Page
    | UserGroupAdminTab UserGroupAdminTab.Page
    | DocumentsTab DocumentsTab.Page
    | BrandedDomainsTab


type alias Model =
    { page : Page
    , tabState : Tab.State
    , mUserAdminTab : Maybe UserAdminTab.Model
    , mUserGroupAdminTab : Maybe UserGroupAdminTab.Model
    , mDocumentsTab : Maybe DocumentsTab.Model
    , mBrandedDomainsTab : Maybe BrandedDomainsTab.Model
    , mUserAdmin : Maybe UserAdmin.Model
    , mUserGroupAdmin : Maybe UserGroupAdmin.Model
    , mBrandedDomain : Maybe BrandedDomain.State
    }


type Msg
    = TabMsg Tab.State
    | UserAdminTabMsg UserAdminTab.Msg
    | UserGroupAdminTabMsg UserGroupAdminTab.Msg
    | UserAdminMsg UserAdmin.Msg
    | UserGroupAdminMsg UserGroupAdmin.Msg
    | DocumentsTabMsg DocumentsTab.Msg
    | BrandedDomainsTabMsg BrandedDomainsTab.Msg
    | BrandedDomainMsg BrandedDomain.Msg


init : (Msg -> msg) -> Globals msg -> Page -> ( Model, Cmd msg )
init embed globals page =
    let
        model =
            { page = Home (UserAdminTab <| UserAdminTab.Page Nothing Nothing)
            , tabState = Tab.customInitialState UserAdminTab.tabName
            , mUserAdminTab = Nothing
            , mUserGroupAdminTab = Nothing
            , mUserAdmin = Nothing
            , mUserGroupAdmin = Nothing
            , mDocumentsTab = Nothing
            , mBrandedDomainsTab = Nothing
            , mBrandedDomain = Nothing
            }
    in
    updatePage embed globals page model


fromPage : Page -> PageUrl
fromPage page =
    case page of
        Home (UserAdminTab tabPage) ->
            let
                pageUrl =
                    UserAdminTab.fromPage tabPage
            in
            { pageUrl | path = [ UserAdminTab.tabName ] }

        Home (UserGroupAdminTab tabPage) ->
            let
                pageUrl =
                    UserGroupAdminTab.fromPage tabPage
            in
            { pageUrl | path = [ UserGroupAdminTab.tabName ] }

        Home (DocumentsTab tabPage) ->
            let
                pageUrl =
                    DocumentsTab.fromPage tabPage
            in
            { pageUrl | path = [ DocumentsTab.tabName ] }

        Home BrandedDomainsTab ->
            { emptyPageUrl | path = [ BrandedDomainsTab.tabName ] }

        UserAdmin subPage ->
            let
                pageUrl =
                    UserAdmin.fromPage subPage
            in
            { pageUrl | path = "useradmin" :: pageUrl.path }

        UserGroupAdmin subPage ->
            let
                pageUrl =
                    UserGroupAdmin.fromPage subPage
            in
            { pageUrl | path = "companyadmin" :: pageUrl.path }

        BrandedDomain subPage ->
            let
                pageUrl =
                    BrandedDomain.fromPage subPage
            in
            { pageUrl | path = "brandeddomain" :: pageUrl.path }


routeParser : Parser (Page -> a) a
routeParser =
    UP.oneOf
        [ UP.map parseUserGroupAdminTab
            (UP.s "page"
                </> UP.s "companyadmin"
                <?> UPQ.string "search"
                <?> UPQ.string "filter"
            )
        , UP.map parseUserAdminTab
            (UP.s "page"
                </> UP.s "salesuseradmin"
                <?> UPQ.string "search"
                <?> UPQ.string "order"
            )
        , UP.map documentsTab
            (UP.s "page" </> UP.s "documents")
        , UP.map (Home BrandedDomainsTab)
            (UP.s "page" </> UP.s "brandeddomain")
        , UP.map UserAdmin
            (UP.s "page" </> UP.s "useradmin" </> UserAdmin.routeParser)
        , UP.map UserGroupAdmin
            (UP.s "page" </> UP.s "companyadmin" </> UserGroupAdmin.routeParser)
        , UP.map BrandedDomain
            (UP.s "page" </> UP.s "brandeddomain" </> BrandedDomain.routeParser)
        , UP.map (Home <| UserAdminTab <| UserAdminTab.Page Nothing Nothing) UP.top
        ]


parseUserGroupAdminTab : Maybe String -> Maybe String -> Page
parseUserGroupAdminTab mSearch mFilter =
    Home <|
        UserGroupAdminTab <|
            UserGroupAdminTab.pageFromFilterSearch mFilter mSearch


parseUserAdminTab : Maybe String -> Maybe String -> Page
parseUserAdminTab mSearch mOrder =
    Home <|
        UserAdminTab <|
            UserAdminTab.pageFromSearchOrder mSearch mOrder


documentsTab : Page
documentsTab =
    Home <|
        DocumentsTab <|
            DocumentsTab.pageFromSearchSortByOrder
                Nothing
                Nothing
                Nothing


pageFromModel : Model -> Maybe Page
pageFromModel model =
    case model.page of
        Home (UserAdminTab _) ->
            model.mUserAdminTab
                |> M.andThen UserAdminTab.pageFromModel
                |> M.map (Home << UserAdminTab)

        Home (UserGroupAdminTab _) ->
            model.mUserGroupAdminTab
                |> M.andThen UserGroupAdminTab.pageFromModel
                |> M.map (Home << UserGroupAdminTab)

        Home (DocumentsTab _) ->
            model.mDocumentsTab
                |> M.andThen DocumentsTab.pageFromModel
                |> M.map (Home << DocumentsTab)

        Home BrandedDomainsTab ->
            Just <| Home BrandedDomainsTab

        UserAdmin _ ->
            model.mUserAdmin |> M.andThen UserAdmin.pageFromModel |> M.map UserAdmin

        UserGroupAdmin _ ->
            model.mUserGroupAdmin |> M.andThen UserGroupAdmin.pageFromModel |> M.map UserGroupAdmin

        BrandedDomain _ ->
            model.mBrandedDomain |> M.andThen BrandedDomain.pageFromModel |> M.map BrandedDomain


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update embed globals msg model =
    case msg of
        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )

        UserAdminTabMsg tabMsg ->
            let updateUserAdminTab = UserAdminTab.update (embed << UserAdminTabMsg) globals tabMsg
                (newUserAdminTab, cmd) = maybeUpdate updateUserAdminTab model.mUserAdminTab
            in ({ model | mUserAdminTab = newUserAdminTab}, cmd)

        UserGroupAdminTabMsg tabMsg ->
            let updateUserGroupAdminTab = UserGroupAdminTab.update (embed << UserGroupAdminTabMsg) globals tabMsg
                (newUserGroupAdminTab, cmd) = maybeUpdate updateUserGroupAdminTab model.mUserGroupAdminTab
            in ({ model | mUserGroupAdminTab = newUserGroupAdminTab}, cmd)

        DocumentsTabMsg tabMsg ->
            let updateDocumentsTab = DocumentsTab.update (embed << DocumentsTabMsg) globals tabMsg
                (newDocumentsTab, cmd) = maybeUpdate updateDocumentsTab model.mDocumentsTab
            in ({ model | mDocumentsTab = newDocumentsTab}, cmd)

        BrandedDomainsTabMsg tabMsg ->
            let updateBrandedDomainsTab = BrandedDomainsTab.update (embed << BrandedDomainsTabMsg) globals tabMsg
                (newBrandedDomainsTab, cmd) = maybeUpdate updateBrandedDomainsTab model.mBrandedDomainsTab
            in ({ model | mBrandedDomainsTab = newBrandedDomainsTab}, cmd)

        UserAdminMsg subMsg ->
            let updateUserAdmin = UserAdmin.update (embed << UserAdminMsg) globals subMsg
                (newUserAdmin, cmd) = maybeUpdate updateUserAdmin model.mUserAdmin
            in ({ model | mUserAdmin = newUserAdmin}, cmd)

        UserGroupAdminMsg subMsg ->
            let updateUserGroupAdmin = UserGroupAdmin.update (embed << UserGroupAdminMsg) globals subMsg
                (newUserGroupAdmin, cmd) = maybeUpdate updateUserGroupAdmin model.mUserGroupAdmin
            in ({ model | mUserGroupAdmin = newUserGroupAdmin}, cmd)

        BrandedDomainMsg subMsg ->
            let updateBrandedDomain = BrandedDomain.update (embed << BrandedDomainMsg) globals subMsg
                (newBrandedDomain, cmd) = maybeUpdate updateBrandedDomain model.mBrandedDomain
            in ({ model | mBrandedDomain = newBrandedDomain}, cmd)


updatePage : (Msg -> msg) -> Globals msg -> Page -> Model -> ( Model, Cmd msg )
updatePage embed globals page model =
    case page of
        Home (UserAdminTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mUserAdminTab
                        |> M.map (UserAdminTab.updatePage (embed << UserAdminTabMsg) tabPage)
                        |> M.withDefault
                            (UserAdminTab.init (embed << UserAdminTabMsg) tabPage)
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState UserAdminTab.tabName
                , mUserAdminTab = Just tab
              }
            , tabCmd
            )

        Home (UserGroupAdminTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mUserGroupAdminTab
                        |> M.map (UserGroupAdminTab.updatePage (embed << UserGroupAdminTabMsg) tabPage)
                        |> M.withDefault
                            (UserGroupAdminTab.init (embed << UserGroupAdminTabMsg) tabPage)
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState UserGroupAdminTab.tabName
                , mUserGroupAdminTab = Just tab
              }
            , tabCmd
            )

        Home (DocumentsTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mDocumentsTab
                        |> M.map (DocumentsTab.updatePage (embed << DocumentsTabMsg) ConfigForAllDocuments tabPage)
                        |> M.withDefault
                            (DocumentsTab.init (embed << DocumentsTabMsg) ConfigForAllDocuments)
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState DocumentsTab.tabName
                , mDocumentsTab = Just tab
              }
            , tabCmd
            )

        Home BrandedDomainsTab ->
            let
                ( tab, tabCmd ) =
                    model.mBrandedDomainsTab
                        |> M.map (\bd -> ( bd, Cmd.none ))
                        |> M.withDefault
                            (BrandedDomainsTab.init (embed << BrandedDomainsTabMsg))
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState BrandedDomainsTab.tabName
                , mBrandedDomainsTab = Just tab
              }
            , tabCmd
            )

        UserAdmin subPage ->
            let
                ( sub, subCmd ) =
                    model.mUserAdmin
                        |> M.map (UserAdmin.updatePage (embed << UserAdminMsg) subPage)
                        |> M.withDefault
                            (UserAdmin.init (embed << UserAdminMsg) subPage)
            in
            ( { model
                | page = page
                , mUserAdmin = Just sub
              }
            , subCmd
            )

        UserGroupAdmin subPage ->
            let
                ( sub, subCmd ) =
                    model.mUserGroupAdmin
                        |> M.map (UserGroupAdmin.updatePage (embed << UserGroupAdminMsg) subPage)
                        |> M.withDefault
                            (UserGroupAdmin.init (embed << UserGroupAdminMsg) globals subPage)
            in
            ( { model
                | page = page
                , mUserGroupAdmin = Just sub
              }
            , subCmd
            )

        BrandedDomain subPage ->
            let
                ( sub, subCmd ) =
                    model.mBrandedDomain
                        |> M.map (BrandedDomain.updatePage (embed << BrandedDomainMsg) subPage)
                        |> M.withDefault
                            (BrandedDomain.init (embed << BrandedDomainMsg) subPage)
            in
            ( { model
                | page = page
                , mBrandedDomain = Just sub
              }
            , subCmd
            )


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    case model.page of
        UserAdmin _ ->
            model.mUserAdmin
                |> M.map (UserAdmin.view <| embed << UserAdminMsg)
                |> M.withDefault viewError

        UserGroupAdmin _ ->
            model.mUserGroupAdmin
                |> M.map (UserGroupAdmin.view <| embed << UserGroupAdminMsg)
                |> M.withDefault viewError

        BrandedDomain _ ->
            model.mBrandedDomain
                |> M.map (BrandedDomain.view <| embed << BrandedDomainMsg)
                |> M.withDefault viewError

        Home _ ->
            Tab.config (embed << TabMsg)
                |> Tab.useHash True
                |> Tab.items
                    [ Tab.item
                        { id = UserAdminTab.tabName
                        , link = Tab.link [ href "/adminonly/page/salesuseradmin" ] [ text "Sales user admin" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mUserAdminTab
                                    |> M.map (UserAdminTab.view <| embed << UserAdminTabMsg)
                                    |> M.withDefault viewError
                                ]
                        }
                    , Tab.item
                        { id = UserGroupAdminTab.tabName
                        , link = Tab.link [ href "/adminonly/page/companyadmin" ] [ text "Company admin" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mUserGroupAdminTab
                                    |> M.map (UserGroupAdminTab.view <| embed << UserGroupAdminTabMsg)
                                    |> M.withDefault viewError
                                ]
                        }
                    , Tab.item
                        { id = DocumentsTab.tabName
                        , link = Tab.link [ href "/adminonly/page/documents" ] [ text "Documents" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mDocumentsTab
                                    |> M.map (DocumentsTab.view <| embed << DocumentsTabMsg)
                                    |> M.withDefault viewError
                                ]
                        }
                    , Tab.item
                        { id = BrandedDomainsTab.tabName
                        , link = Tab.link [ href "/adminonly/page/brandeddomain" ] [ text "Branded domains" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mBrandedDomainsTab
                                    |> M.map (BrandedDomainsTab.view <| embed << BrandedDomainsTabMsg)
                                    |> M.withDefault viewError
                                ]
                        }
                    ]
                |> Tab.view model.tabState
