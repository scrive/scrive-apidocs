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
import Html exposing (text)
import Html.Attributes exposing (href)
import Maybe as M
import Monocle.Optional exposing (Optional)
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


userAdminTabModelLens : Optional Model UserAdminTab.Model
userAdminTabModelLens =
    Optional .mUserAdminTab
        (\b a -> { a | mUserAdminTab = Just b })


userGroupAdminTabModelLens : Optional Model UserGroupAdminTab.Model
userGroupAdminTabModelLens =
    Optional .mUserGroupAdminTab
        (\b a -> { a | mUserGroupAdminTab = Just b })


documentsTabModelLens : Optional Model DocumentsTab.Model
documentsTabModelLens =
    Optional .mDocumentsTab
        (\b a -> { a | mDocumentsTab = Just b })


brandedDomainsTabModelLens : Optional Model BrandedDomainsTab.Model
brandedDomainsTabModelLens =
    Optional .mBrandedDomainsTab
        (\b a -> { a | mBrandedDomainsTab = Just b })


userAdminModelLens : Optional Model UserAdmin.Model
userAdminModelLens =
    Optional .mUserAdmin
        (\b a -> { a | mUserAdmin = Just b })


userGroupAdminModelLens : Optional Model UserGroupAdmin.Model
userGroupAdminModelLens =
    Optional .mUserGroupAdmin
        (\b a -> { a | mUserGroupAdmin = Just b })


brandedDomainModelLens : Optional Model BrandedDomain.State
brandedDomainModelLens =
    Optional .mBrandedDomain
        (\b a -> { a | mBrandedDomain = Just b })


type Msg
    = TabMsg Tab.State
    | UserAdminTabMsg UserAdminTab.Msg
    | UserGroupAdminTabMsg UserGroupAdminTab.Msg
    | UserAdminMsg UserAdmin.Msg
    | UserGroupAdminMsg UserGroupAdmin.Msg
    | DocumentsTabMsg DocumentsTab.Msg
    | BrandedDomainsTabMsg BrandedDomainsTab.Msg
    | BrandedDomainMsg BrandedDomain.Msg


init : Globals msg -> Page -> ( Model, Cmd Msg )
init globals page =
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
    updatePage globals page model


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


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals =
    let
        updateUserAdminTab =
            liftOptionalUpdateHandler
                userAdminTabModelLens
                UserAdminTabMsg
            <|
                UserAdminTab.update globals

        updateUserGroupAdminTab =
            liftOptionalUpdateHandler
                userGroupAdminTabModelLens
                UserGroupAdminTabMsg
            <|
                UserGroupAdminTab.update globals

        updateDocumentsTab =
            liftOptionalUpdateHandler
                documentsTabModelLens
                DocumentsTabMsg
            <|
                DocumentsTab.update globals

        updateBrandedDomainsTab =
            liftOptionalUpdateHandler
                brandedDomainsTabModelLens
                BrandedDomainsTabMsg
            <|
                BrandedDomainsTab.update globals

        updateUserAdmin =
            liftOptionalUpdateHandler
                userAdminModelLens
                UserAdminMsg
            <|
                UserAdmin.update globals

        updateUserGroupAdmin =
            liftOptionalUpdateHandler
                userGroupAdminModelLens
                UserGroupAdminMsg
            <|
                UserGroupAdmin.update globals

        updateBrandedDomain : BrandedDomain.Msg -> Model -> ( Model, Cmd (Either msg Msg) )
        updateBrandedDomain msg model =
            case model.mBrandedDomain of
              Nothing -> (model, Cmd.none)  -- this really should be an internal error
              Just brandedDomainTab ->
                let (newBrandedDomainTab, cmd) =
                      BrandedDomain.update
                        { embed = Right << BrandedDomainMsg
                        , presentFlashMessage = Cmd.map Left << globals.flashMessage
                        , formBody = formBody globals
                        , gotoBrandedDomainsTab = Cmd.map Left globals.gotoBrandedDomainsTab
                        } msg brandedDomainTab
                in ({ model | mBrandedDomain = Just newBrandedDomainTab }, cmd)
    in
    \msg model ->
        case msg of
            TabMsg state ->
                ( { model | tabState = state }
                , Cmd.none
                )

            UserAdminTabMsg tabMsg ->
                updateUserAdminTab tabMsg model

            UserGroupAdminTabMsg tabMsg ->
                updateUserGroupAdminTab tabMsg model

            DocumentsTabMsg tabMsg ->
                updateDocumentsTab tabMsg model

            BrandedDomainsTabMsg tabMsg ->
                updateBrandedDomainsTab tabMsg model

            UserAdminMsg subMsg ->
                updateUserAdmin subMsg model

            UserGroupAdminMsg subMsg ->
                updateUserGroupAdmin subMsg model

            BrandedDomainMsg subMsg ->
                updateBrandedDomain subMsg model


updatePage : Globals msg -> Page -> Model -> ( Model, Cmd Msg )
updatePage globals page model =
    case page of
        Home (UserAdminTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mUserAdminTab
                        |> M.map (UserAdminTab.updatePage tabPage)
                        |> M.withDefault
                            (UserAdminTab.init tabPage)
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState UserAdminTab.tabName
                , mUserAdminTab = Just tab
              }
            , liftCmd UserAdminTabMsg tabCmd
            )

        Home (UserGroupAdminTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mUserGroupAdminTab
                        |> M.map (UserGroupAdminTab.updatePage tabPage)
                        |> M.withDefault
                            (UserGroupAdminTab.init tabPage)
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState UserGroupAdminTab.tabName
                , mUserGroupAdminTab = Just tab
              }
            , liftCmd UserGroupAdminTabMsg tabCmd
            )

        Home (DocumentsTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mDocumentsTab
                        |> M.map (DocumentsTab.updatePage ConfigForAllDocuments tabPage)
                        |> M.withDefault
                            (DocumentsTab.init ConfigForAllDocuments)
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState DocumentsTab.tabName
                , mDocumentsTab = Just tab
              }
            , liftCmd DocumentsTabMsg tabCmd
            )

        Home BrandedDomainsTab ->
            let
                ( tab, tabCmd ) =
                    model.mBrandedDomainsTab
                        |> M.map (\bd -> ( bd, Cmd.none ))
                        |> M.withDefault
                            BrandedDomainsTab.init
            in
            ( { model
                | page = page
                , tabState = Tab.customInitialState BrandedDomainsTab.tabName
                , mBrandedDomainsTab = Just tab
              }
            , liftCmd BrandedDomainsTabMsg tabCmd
            )

        UserAdmin subPage ->
            let
                ( sub, subCmd ) =
                    model.mUserAdmin
                        |> M.map (UserAdmin.updatePage subPage)
                        |> M.withDefault
                            (UserAdmin.init subPage)
            in
            ( { model
                | page = page
                , mUserAdmin = Just sub
              }
            , liftCmd UserAdminMsg subCmd
            )

        UserGroupAdmin subPage ->
            let
                ( sub, subCmd ) =
                    model.mUserGroupAdmin
                        |> M.map (UserGroupAdmin.updatePage subPage)
                        |> M.withDefault
                            (UserGroupAdmin.init globals subPage)
            in
            ( { model
                | page = page
                , mUserGroupAdmin = Just sub
              }
            , liftCmd UserGroupAdminMsg subCmd
            )

        BrandedDomain subPage ->
            let
                ( sub, subCmd ) =
                    model.mBrandedDomain
                        |> M.map (BrandedDomain.updatePage identity subPage)
                        |> M.withDefault
                            (BrandedDomain.init
                                identity
                                subPage
                            )
            in
            ( { model
                | page = page
                , mBrandedDomain = Just sub
              }
            , liftCmd BrandedDomainMsg subCmd
            )


view : Model -> Render msg Msg
view model =
    case model.page of
        UserAdmin _ ->
            model.mUserAdmin
                |> M.map (innerHtml << liftHtml UserAdminMsg << UserAdmin.view)
                |> M.withDefault viewError

        UserGroupAdmin _ ->
            model.mUserGroupAdmin
                |> M.map (liftInnerHtml UserGroupAdminMsg << UserGroupAdmin.view)
                |> M.withDefault viewError

        BrandedDomain _ ->
            model.mBrandedDomain
                |> M.map (innerHtml << liftHtml BrandedDomainMsg << BrandedDomain.view)
                |> M.withDefault viewError

        Home _ ->
            Tab.config (Either.Right << TabMsg)
                |> Tab.useHash True
                |> Tab.items
                    [ Tab.item
                        { id = UserAdminTab.tabName
                        , link = Tab.link [ href "/adminonly/page/salesuseradmin" ] [ text "Sales user admin" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mUserAdminTab
                                    |> M.map (innerHtml << liftHtml UserAdminTabMsg << UserAdminTab.view)
                                    |> M.withDefault viewError
                                ]
                        }
                    , Tab.item
                        { id = UserGroupAdminTab.tabName
                        , link = Tab.link [ href "/adminonly/page/companyadmin" ] [ text "Company admin" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mUserGroupAdminTab
                                    |> M.map (innerHtml << liftHtml UserGroupAdminTabMsg << UserGroupAdminTab.view)
                                    |> M.withDefault viewError
                                ]
                        }
                    , Tab.item
                        { id = DocumentsTab.tabName
                        , link = Tab.link [ href "/adminonly/page/documents" ] [ text "Documents" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mDocumentsTab
                                    |> M.map (innerHtml << liftHtml DocumentsTabMsg << DocumentsTab.view)
                                    |> M.withDefault viewError
                                ]
                        }
                    , Tab.item
                        { id = BrandedDomainsTab.tabName
                        , link = Tab.link [ href "/adminonly/page/brandeddomain" ] [ text "Branded domains" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] <|
                                [ model.mBrandedDomainsTab
                                    |> M.map (innerHtml << liftHtml BrandedDomainsTabMsg << BrandedDomainsTab.view)
                                    |> M.withDefault viewError
                                ]
                        }
                    ]
                |> Tab.view model.tabState
