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
import Monocle.Optional exposing (Optional)
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


detailsModelLens : Optional Model DetailsTab.Model
detailsModelLens =
    Optional .mDetailsTab
        (\b a -> { a | mDetailsTab = Just b })


statisticsModelLens : Optional Model StatisticsTab.Model
statisticsModelLens =
    Optional .mStatisticsTab
        (\b a -> { a | mStatisticsTab = Just b })


documentsModelLens : Optional Model DocumentsTab.Model
documentsModelLens =
    Optional .mDocumentsTab
        (\b a -> { a | mDocumentsTab = Just b })


init : Page -> ( Model, Cmd Msg )
init page =
    let
        model =
            { page = Home "" DetailsTab
            , mDetailsTab = Nothing
            , mStatisticsTab = Nothing
            , mDocumentsTab = Nothing
            , tabState = Tab.customInitialState DetailsTab.tabName
            }
    in
    updatePage page model


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
        (\uid mFragment mSearch mSortBy mOrder mTab ->
            parseHomeTab mFragment mSearch mSortBy mOrder mTab
                |> M.withDefault DetailsTab
                |> Home uid
        )
        (UP.string
            </> UP.fragment identity
            <?> UPQ.string "search"
            <?> UPQ.string "sort_by"
            <?> UPQ.string "order"
            <?> UPQ.string "subTab"
        )


parseHomeTab : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe HomeTab
parseHomeTab mFragment mSearch mSortBy mOrder mTab =
    let
        maping =
            Dict.fromList
                [ ( DetailsTab.tabName, DetailsTab )
                , ( StatisticsTab.tabName, StatisticsTab <| StatisticsTab.pageFromTab mTab )
                , ( DocumentsTab.tabName, DocumentsTab <| DocumentsTab.pageFromSearchSortByOrder mSearch mSortBy mOrder )
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


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals =
    let
        updateDetails =
            liftOptionalUpdateHandler
                detailsModelLens
                DetailsTabMsg
            <|
                DetailsTab.update globals

        updateStatistics =
            liftOptionalUpdateHandler
                statisticsModelLens
                StatisticsTabMsg
            <|
                StatisticsTab.update globals

        updateDocuments =
            liftOptionalUpdateHandler
                documentsModelLens
                DocumentsTabMsg
            <|
                DocumentsTab.update globals
    in
    \msg model ->
        case msg of
            TabMsg state ->
                ( { model | tabState = state }
                , if state == Tab.customInitialState "goback" then
                    outerCmd globals.gotoUserAdminTab

                  else
                    Cmd.none
                )

            DetailsTabMsg tabMsg ->
                updateDetails tabMsg model

            StatisticsTabMsg tabMsg ->
                updateStatistics tabMsg model

            DocumentsTabMsg tabMsg ->
                updateDocuments tabMsg model


updatePage : Page -> Model -> ( Model, Cmd Msg )
updatePage page model =
    case page of
        Home _ DetailsTab ->
            let
                ( tab, tabCmd ) =
                    model.mDetailsTab
                        |> M.map (\t -> ( t, DetailsTab.getUserCmd (userID page) ))
                        |> M.withDefault
                            (DetailsTab.init (userID page))
            in
            ( { model
                | tabState = Tab.customInitialState DetailsTab.tabName
                , page = page
                , mDetailsTab = Just tab
              }
            , liftCmd DetailsTabMsg tabCmd
            )

        Home _ (StatisticsTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mStatisticsTab
                        |> M.map (StatisticsTab.updatePage tabPage (StatisticsTab.ConfigForUser <| userID page))
                        |> M.withDefault
                            (StatisticsTab.init tabPage
                                (StatisticsTab.ConfigForUser <| userID page)
                            )
            in
            ( { model
                | mStatisticsTab = Just tab
                , page = page
                , tabState = Tab.customInitialState StatisticsTab.tabName
              }
            , liftCmd StatisticsTabMsg tabCmd
            )

        Home _ (DocumentsTab tabPage) ->
            let
                ( tab, tabCmd ) =
                    model.mDocumentsTab
                        |> M.map (DocumentsTab.updatePage (ConfigForUser <| userID page) tabPage)
                        |> M.withDefault
                            (DocumentsTab.init (ConfigForUser <| userID page))
            in
            ( { model
                | mDocumentsTab = Just tab
                , page = page
                , tabState = Tab.customInitialState DocumentsTab.tabName
              }
            , liftCmd DocumentsTabMsg tabCmd
            )


view : Model -> Html Msg
view model =
    Tab.config TabMsg
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
                                (\model2 ->
                                    liftHtml DetailsTabMsg <| DetailsTab.view model2
                                )
                            |> M.withDefault viewError
                        ]
                }
            , Tab.item
                { id = StatisticsTab.tabName
                , link = Tab.link [] [ text "Statistics" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ] <|
                        [ model.mStatisticsTab
                            |> M.map (liftHtml StatisticsTabMsg << StatisticsTab.view)
                            |> M.withDefault viewError
                        ]
                }
            , Tab.item
                { id = DocumentsTab.tabName
                , link = Tab.link [] [ text "Documents" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ] <|
                        [ model.mDocumentsTab
                            |> M.map (liftHtml DocumentsTabMsg << DocumentsTab.view)
                            |> M.withDefault viewError
                        ]
                }
            ]
        |> Tab.view model.tabState
