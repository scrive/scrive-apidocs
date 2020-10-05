module AdminOnly.UserAdmin.UserAdmin exposing
    ( Model
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

import AdminOnly.UserAdmin.DetailsTab.DetailsTab as DetailsTab
import AdminOnly.UserAdmin.DocumentsTab.DocumentsTab as DocumentsTab exposing (Config(..))
import AdminOnly.UserGroupAdmin.StatisticsTab as StatisticsTab
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Dict
import Html exposing (Html, text)
import Maybe as M
import Url.Parser as UP exposing ((</>), (<?>), Parser)
import Url.Parser.Query as UPQ
import Utils exposing (..)


type Page
    = Home String HomeTab


userID : Page -> String
userID page =
    case page of
        Home uid _ ->
            uid


type HomeTab
    = DetailsTab
    | StatisticsTab StatisticsTab.Page
    | DocumentsTab DocumentsTab.Page


type alias Model =
    { page : Page
    , tabState : Tab.State
    , mDetailsTab : Maybe DetailsTab.Model
    , mStatisticsTab : Maybe StatisticsTab.Model
    , mDocumentsTab : Maybe DocumentsTab.Model
    }


type Msg
    = TabMsg Tab.State
    | DetailsTabMsg DetailsTab.Msg
    | StatisticsTabMsg StatisticsTab.Msg
    | DocumentsTabMsg DocumentsTab.Msg


init : (Msg -> msg) -> Page -> ( Model, Cmd msg )
init embed page =
    let
        model =
            { page = Home "" DetailsTab
            , mDetailsTab = Nothing
            , mStatisticsTab = Nothing
            , mDocumentsTab = Nothing
            , tabState = Tab.customInitialState DetailsTab.tabName
            }
    in
    updatePage embed page model


fromPage : Page -> PageUrl
fromPage page =
    let
        ( tabName, pageUrl ) =
            case page of
                Home _ DetailsTab ->
                    ( DetailsTab.tabName, emptyPageUrl )

                Home _ (StatisticsTab tabPage) ->
                    ( StatisticsTab.tabName, StatisticsTab.fromPage tabPage )

                Home _ (DocumentsTab tabPage) ->
                    ( DocumentsTab.tabName, DocumentsTab.fromPage tabPage )
    in
    { pageUrl | path = [ userID page ], fragment = Just tabName }


routeParser : Parser (Page -> a) a
routeParser =
    UP.map
        (\uid mFragment mPagination mSearch mSortBy mOrder mTab ->
            parseHomeTab mFragment mPagination mSearch mSortBy mOrder mTab
                |> M.withDefault DetailsTab
                |> Home uid
        )
        (UP.string
            </> UP.fragment identity
            <?> UPQ.int "p"
            <?> UPQ.string "search"
            <?> UPQ.string "sort_by"
            <?> UPQ.string "order"
            <?> UPQ.string "subTab"
        )


parseHomeTab : Maybe String -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe HomeTab
parseHomeTab mFragment mPagination mSearch mSortBy mOrder mTab =
    let
        maping =
            Dict.fromList
                [ ( DetailsTab.tabName, DetailsTab )
                , ( StatisticsTab.tabName, StatisticsTab <| StatisticsTab.pageFromTab mTab )
                , ( DocumentsTab.tabName, DocumentsTab <| DocumentsTab.pageFromSearchSortByOrder mSearch mSortBy mOrder mPagination )
                ]
    in
    mFragment |> Maybe.andThen (\fragment -> Dict.get fragment maping)


pageFromModel : Model -> Maybe Page
pageFromModel model =
    case model.page of
        Home uid DetailsTab ->
            Just <| Home uid DetailsTab

        Home uid (StatisticsTab _) ->
            model.mStatisticsTab
                |> M.andThen StatisticsTab.pageFromModel
                |> M.map (Home uid << StatisticsTab)

        Home uid (DocumentsTab _) ->
            model.mDocumentsTab
                |> M.andThen DocumentsTab.pageFromModel
                |> M.map (Home uid << DocumentsTab)


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update embed globals msg model =
    case msg of
        TabMsg state ->
            ( { model | tabState = state }
            , if state == Tab.customInitialState "goback" then
                globals.gotoUserAdminTab

              else
                Cmd.none
            )

        DetailsTabMsg tabMsg ->
            let
                updateDetailsTab =
                    DetailsTab.update (embed << DetailsTabMsg) globals tabMsg

                ( newDetailsTab, cmd ) =
                    maybeUpdate updateDetailsTab model.mDetailsTab
            in
            ( { model | mDetailsTab = newDetailsTab }, cmd )

        StatisticsTabMsg tabMsg ->
            let
                updateStatisticsTab =
                    StatisticsTab.update (embed << StatisticsTabMsg) globals tabMsg

                ( newStatisticsTab, cmd ) =
                    maybeUpdate updateStatisticsTab model.mStatisticsTab
            in
            ( { model | mStatisticsTab = newStatisticsTab }, cmd )

        DocumentsTabMsg tabMsg ->
            let
                updateDocumentsTab =
                    DocumentsTab.update (embed << DocumentsTabMsg) globals tabMsg

                ( newDocumentsTab, cmd ) =
                    maybeUpdate updateDocumentsTab model.mDocumentsTab
            in
            ( { model | mDocumentsTab = newDocumentsTab }, cmd )


updatePage : (Msg -> msg) -> Page -> Model -> ( Model, Cmd msg )
updatePage embed page model =
    case page of
        Home _ DetailsTab ->
            let
                ( tab, tabCmd ) =
                    model.mDetailsTab
                        |> M.map
                            (\t ->
                                ( t
                                , Cmd.map (embed << DetailsTabMsg) <|
                                    DetailsTab.getUserCmd (userID page)
                                )
                            )
                        |> M.withDefault
                            (DetailsTab.init (embed << DetailsTabMsg) (userID page))
            in
            ( { model
                | tabState = Tab.customInitialState DetailsTab.tabName
                , page = page
                , mDetailsTab = Just tab
              }
            , tabCmd
            )

        Home _ (StatisticsTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mStatisticsTab
                        |> M.map (StatisticsTab.updatePage (embed << StatisticsTabMsg) tabPage (StatisticsTab.ConfigForUser <| userID page))
                        |> M.withDefault
                            (StatisticsTab.init (embed << StatisticsTabMsg)
                                tabPage
                                (StatisticsTab.ConfigForUser <| userID page)
                            )
            in
            ( { model
                | mStatisticsTab = Just tab
                , page = page
                , tabState = Tab.customInitialState StatisticsTab.tabName
              }
            , tabCmd
            )

        Home _ (DocumentsTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mDocumentsTab
                        |> M.map (DocumentsTab.updatePage (embed << DocumentsTabMsg) (ConfigForUser <| userID page) tabPage)
                        |> M.withDefault
                            (DocumentsTab.init (embed << DocumentsTabMsg) (ConfigForUser <| userID page) tabPage.paginationPageNum)
            in
            ( { model
                | mDocumentsTab = Just tab
                , page = page
                , tabState = Tab.customInitialState DocumentsTab.tabName
              }
            , tabCmd
            )


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    Tab.config (embed << TabMsg)
        |> Tab.useHash True
        |> Tab.items
            [ Tab.item
                { id = "goback"
                , link = Tab.link [] [ text "<" ]
                , pane = Tab.pane [ Spacing.mt3 ] []
                }
            , Tab.item
                { id = DetailsTab.tabName
                , link = Tab.link [] [ text "User details" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ] <|
                        [ model.mDetailsTab
                            |> M.map
                                (DetailsTab.view <| embed << DetailsTabMsg)
                            |> M.withDefault viewError
                        ]
                }
            , Tab.item
                { id = StatisticsTab.tabName
                , link = Tab.link [] [ text "Statistics" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ] <|
                        [ model.mStatisticsTab
                            |> M.map (StatisticsTab.view <| embed << StatisticsTabMsg)
                            |> M.withDefault viewError
                        ]
                }
            , Tab.item
                { id = DocumentsTab.tabName
                , link = Tab.link [] [ text "Documents" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ] <|
                        [ model.mDocumentsTab
                            |> M.map (DocumentsTab.view <| embed << DocumentsTabMsg)
                            |> M.withDefault viewError
                        ]
                }
            ]
        |> Tab.view model.tabState
