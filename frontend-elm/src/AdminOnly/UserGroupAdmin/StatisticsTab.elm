module AdminOnly.UserGroupAdmin.StatisticsTab exposing
    ( Config(..)
    , Model
    , Msg
    , Page
    , fromPage
    , init
    , pageFromModel
    , pageFromTab
    , tabName
    , tabShareableLinkName
    , update
    , updatePage
    , view
    )

import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html, a, div, h2, h4, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder)
import List as L
import Maybe as M
import Return exposing (..)
import Url.Builder as UB
import Utils exposing (..)


type alias Model =
    { config : Config
    , page : Page
    , tabState : Tab.State
    , sDaysStats : Status (Stats StatsRowData)
    , sMonthsStats : Status (Stats StatsRowData)
    , sSLDaysStats : Status (Stats SLStatsRowData)
    , sSLMonthsStats : Status (Stats SLStatsRowData)
    }


type Config
    = ConfigForUserGroup String
    | ConfigForUser String


type alias Page =
    Bool


type alias StatsRow rowData =
    { row : rowData
    , date : Date
    , isOpened : Bool
    , subStats : List (StatsSubRow rowData)
    }


type alias StatsSubRow rowData =
    { row : rowData
    , date : Date
    , displayID : String
    }


type alias StatsRowData =
    { sent : Int
    , closed : Int
    , signatures : Int
    }


type alias SLStatsRowData =
    { closed : Int
    , sent : Int
    }


type alias Date =
    String


type alias Stats rowData =
    { daysOrMonths : DaysOrMonths
    , isShareableLink : Bool
    , rows : List (StatsRow rowData)
    }


type DaysOrMonths
    = Days
    | Months


type Msg
    = TabMsg Tab.State
    | GotStats DaysOrMonths (Result Http.Error (Stats StatsRowData))
    | GotSLStats DaysOrMonths (Result Http.Error (Stats SLStatsRowData))
    | StatsRowClicked DaysOrMonths Bool Date


tabName : String
tabName =
    "stats"


tabShareableLinkName : String
tabShareableLinkName =
    "stats-shareable-link"


init : (Msg -> msg) -> Page -> Config -> Return msg Model
init embed page config =
    let
        model =
            { config = config
            , page = False
            , tabState = Tab.customInitialState tabName
            , sDaysStats = Loading
            , sMonthsStats = Loading
            , sSLDaysStats = Loading
            , sSLMonthsStats = Loading
            }
    in
    updatePage embed page config model


getStatsCmd : DaysOrMonths -> Model -> Cmd Msg
getStatsCmd daysOrMonths model =
    Http.get
        { url = statsUrl daysOrMonths model
        , expect =
            if model.page then
                Http.expectJson (GotSLStats daysOrMonths) (statsSLDecoder daysOrMonths)

            else
                Http.expectJson (GotStats daysOrMonths) (statsDecoder model.config daysOrMonths)
        }


statsUrl : DaysOrMonths -> Model -> String
statsUrl daysOrMonths model =
    let
        ( pathSection, id, withCompany ) =
            case model.config of
                ConfigForUserGroup ugid ->
                    ( "companyadmin", ugid, ite model.page "" "?withCompany=true" )

                ConfigForUser uid ->
                    ( "useradmin", uid, "" )
    in
    "/adminonly/"
        ++ pathSection
        ++ "/"
        ++ ite model.page "shareablelinkstats" "usagestats"
        ++ "/"
        ++ daysOrMonthsToString daysOrMonths
        ++ "/"
        ++ id
        ++ withCompany


daysOrMonthsToString : DaysOrMonths -> String
daysOrMonthsToString daysOrMonths =
    case daysOrMonths of
        Days ->
            "days"

        Months ->
            "months"


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


pageFromTab : Maybe String -> Page
pageFromTab mTab =
    mTab == Just tabShareableLinkName


fromPage : Page -> PageUrl
fromPage isShareableLink =
    { emptyPageUrl
        | query = ite isShareableLink [ UB.string "subTab" tabShareableLinkName ] []
    }


updatePage : (Msg -> msg) -> Page -> Config -> Model -> Return msg Model
updatePage embed page config model0 =
    let
        model =
            { model0
                | config = config
                , page = page
                , tabState = Tab.customInitialState <| ite page tabShareableLinkName tabName
            }
    in
    return model <| Cmd.map embed <| Cmd.batch [ getStatsCmd Days model, getStatsCmd Months model ]


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update _ globals msg model =
    case msg of
        TabMsg state ->
            let
                model1 =
                    { model
                        | tabState = state
                        , page = state == Tab.customInitialState tabShareableLinkName
                    }
            in
            return model1 globals.setPageUrlFromModel

        GotStats daysOrMonths result ->
            let
                sStats =
                    case result of
                        Ok stats ->
                            Success stats

                        Err _ ->
                            Failure
            in
            singleton <|
                case daysOrMonths of
                    Days ->
                        { model | sDaysStats = sStats }

                    Months ->
                        { model | sMonthsStats = sStats }

        GotSLStats daysOrMonths result ->
            let
                sStats =
                    case result of
                        Ok stats ->
                            Success stats

                        Err _ ->
                            Failure
            in
            singleton <|
                case daysOrMonths of
                    Days ->
                        { model | sSLDaysStats = sStats }

                    Months ->
                        { model | sSLMonthsStats = sStats }

        StatsRowClicked daysOrMonths isShareableLink date ->
            singleton <|
                case ( daysOrMonths, isShareableLink ) of
                    ( Days, False ) ->
                        { model | sDaysStats = statusMap (toggleDateRow date) model.sDaysStats }

                    ( Days, True ) ->
                        { model | sSLDaysStats = statusMap (toggleDateRow date) model.sSLDaysStats }

                    ( Months, False ) ->
                        { model | sMonthsStats = statusMap (toggleDateRow date) model.sMonthsStats }

                    ( Months, True ) ->
                        { model | sSLMonthsStats = statusMap (toggleDateRow date) model.sSLMonthsStats }


toggleDateRow : String -> Stats rowData -> Stats rowData
toggleDateRow date stats =
    { stats
        | rows =
            (\f -> L.map f stats.rows) <|
                \row ->
                    if date == row.date then
                        { row | isOpened = not row.isOpened }

                    else
                        row
    }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    Tab.config TabMsg
        |> Tab.useHash False
        |> Tab.items
            [ Tab.item
                { id = tabName
                , link = Tab.link [] [ text "Usage stats" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ] <|
                        [ h2 [ class "text-center mt-5" ] [ text "Past 30 days" ]
                        , viewStats Days model model.sDaysStats
                        , h2 [ class "text-center mt-5" ] [ text "Past 6 months" ]
                        , viewStats Months model model.sMonthsStats
                        ]
                }
            , Tab.item
                { id = tabShareableLinkName
                , link = Tab.link [] [ text "Shareable link stats" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ] <|
                        [ h2 [ class "text-center mt-5" ] [ text "Past 30 days" ]
                        , viewSLStats Days model model.sSLDaysStats
                        , h2 [ class "text-center mt-5" ] [ text "Past 6 months" ]
                        , viewSLStats Months model model.sSLMonthsStats
                        ]
                }
            ]
        |> Tab.view model.tabState
        |> Html.map embed


viewStats : DaysOrMonths -> Model -> Status (Stats StatsRowData) -> Html Msg
viewStats daysOrMonths model sStats =
    case sStats of
        Loading ->
            h4 [] [ text "Loading ..." ]

        Failure ->
            h4 [] [ text "Failure ..." ]

        Success stats ->
            div [ class "container-fluid" ]
                [ Table.table
                    { options = [ Table.striped, Table.hover, Table.small ]
                    , thead =
                        Table.thead []
                            [ Table.tr [ Table.rowAttr <| class "d-flex" ] <|
                                L.filterMap identity
                                    [ Just <| th "col-3" [ text "Date" ]
                                    , case model.config of
                                        ConfigForUser _ ->
                                            Nothing

                                        ConfigForUserGroup _ ->
                                            Just <| th "col-3" [ text "Sender" ]
                                    , Just <| th "col-2" [ text "Closed documents" ]
                                    , Just <| th "col-2" [ text "Sent documents" ]
                                    , Just <| th "col-2" [ text "ClosedSignatures" ]
                                    ]
                            ]
                    , tbody =
                        Table.tbody [] <|
                            L.concatMap (viewStatsRow model daysOrMonths) stats.rows
                    }
                , div [ class "text-center" ]
                    [ a [ href <| statsUrl daysOrMonths model ] [ text "Open as JSON" ] ]
                ]


td : String -> List (Html msg) -> Table.Cell msg
td col html =
    Table.td [ Table.cellAttr <| class col ] html


th : String -> List (Html msg) -> Table.Cell msg
th col =
    Table.th [ Table.cellAttr <| class col ]


viewStatsRow : Model -> DaysOrMonths -> StatsRow StatsRowData -> List (Table.Row Msg)
viewStatsRow model daysOrMonths row =
    Table.tr
        ((Table.rowAttr <| class "d-flex")
            :: (case model.config of
                    ConfigForUser _ ->
                        []

                    ConfigForUserGroup _ ->
                        [ Table.rowAttr <| onClick <| StatsRowClicked daysOrMonths False row.date
                        , Table.rowAttr <| class "clickable-row"
                        ]
               )
        )
        (L.filterMap identity <|
            [ Just <| td "col-3" [ text row.date ]
            , case model.config of
                ConfigForUser _ ->
                    Nothing

                ConfigForUserGroup _ ->
                    Just <| td "col-3" [ text "Organisation total" ]
            , Just <| td "col-2" [ text <| String.fromInt row.row.closed ]
            , Just <| td "col-2" [ text <| String.fromInt row.row.sent ]
            , Just <| td "col-2" [ text <| String.fromInt row.row.signatures ]
            ]
        )
        :: (ite (not row.isOpened) [] <|
                L.map viewStatsSubRow row.subStats
           )


viewStatsSubRow : StatsSubRow StatsRowData -> Table.Row msg
viewStatsSubRow subRow =
    Table.tr [ Table.rowAttr <| class "d-flex" ]
        [ td "col-3" []
        , td "col-3"
            [ -- \u{2022}
              text "• "
            , text subRow.displayID
            ]
        , td "col-2" [ text <| String.fromInt subRow.row.closed ]
        , td "col-2" [ text <| String.fromInt subRow.row.sent ]
        , td "col-2" [ text <| String.fromInt subRow.row.signatures ]
        ]


viewSLStats : DaysOrMonths -> Model -> Status (Stats SLStatsRowData) -> Html Msg
viewSLStats daysOrMonths model statsStatus =
    case statsStatus of
        Loading ->
            h4 [] [ text "Loading ..." ]

        Failure ->
            h4 [] [ text "Failure ..." ]

        Success stats ->
            div [ class "container-fluid" ]
                [ Table.table
                    { options = [ Table.striped, Table.hover, Table.small ]
                    , thead =
                        Table.thead []
                            [ Table.tr [ Table.rowAttr <| class "d-flex" ]
                                [ th "col-3" [ text "Date" ]
                                , th "col-3" [ text "Template" ]
                                , th "col-3" [ text "Closed documents" ]
                                , th "col-3" [ text "Sent documents" ]
                                ]
                            ]
                    , tbody =
                        Table.tbody [] <|
                            L.concatMap (viewSLStatsRow daysOrMonths) stats.rows
                    }
                , div [ class "text-center" ]
                    [ a [ href <| statsUrl daysOrMonths model ] [ text "Open as JSON" ] ]
                ]


viewSLStatsRow : DaysOrMonths -> StatsRow SLStatsRowData -> List (Table.Row Msg)
viewSLStatsRow daysOrMonths row =
    Table.tr
        [ Table.rowAttr <| class "d-flex"
        , Table.rowAttr <| onClick <| StatsRowClicked daysOrMonths True row.date
        , Table.rowAttr <| class "clickable-row"
        ]
        [ td "col-3" [ text row.date ]
        , td "col-3" [ text "Organisation total" ]
        , td "col-3" [ text <| String.fromInt row.row.closed ]
        , td "col-3" [ text <| String.fromInt row.row.sent ]
        ]
        :: (ite (not row.isOpened) [] <|
                L.map viewSLStatsSubRow row.subStats
           )


viewSLStatsSubRow : StatsSubRow SLStatsRowData -> Table.Row msg
viewSLStatsSubRow subRow =
    Table.tr [ Table.rowAttr <| class "d-flex" ]
        [ td "col-3" []
        , td "col-3"
            [ -- \u{2022}
              text "• "
            , text subRow.displayID
            ]
        , td "col-3" [ text <| String.fromInt subRow.row.closed ]
        , td "col-3" [ text <| String.fromInt subRow.row.sent ]
        ]


statsDecoder : Config -> DaysOrMonths -> Decoder (Stats StatsRowData)
statsDecoder config daysOrMonths =
    JD.field "stats" <|
        JD.map (Stats daysOrMonths False) <|
            JD.list <|
                JD.map4 StatsRow
                    statsRowDecoder
                    (JD.field "date" JD.string)
                    (JD.succeed False)
                    (case config of
                        ConfigForUser _ ->
                            JD.succeed []

                        ConfigForUserGroup _ ->
                            JD.field "user_stats" <|
                                JD.list <|
                                    JD.map3 StatsSubRow
                                        statsRowDecoder
                                        (JD.field "date" JD.string)
                                        (JD.map2 (\a b -> ( a, b ))
                                            (JD.field "name" JD.string)
                                            (JD.field "email" JD.string)
                                            |> JD.andThen
                                                (\( name, email ) ->
                                                    String.trim name
                                                        |> stringNonEmpty
                                                        |> M.withDefault email
                                                        |> JD.succeed
                                                )
                                        )
                    )


statsSLDecoder : DaysOrMonths -> Decoder (Stats SLStatsRowData)
statsSLDecoder daysOrMonths =
    JD.field "stats" <|
        JD.map (Stats daysOrMonths True) <|
            JD.list <|
                JD.map4 StatsRow
                    statsSLRowDecoder
                    (JD.field "date" JD.string)
                    (JD.succeed False)
                    (JD.field "template_stats" <|
                        JD.list <|
                            JD.map3 StatsSubRow
                                statsSLRowDecoder
                                (JD.field "date" JD.string)
                                (JD.field "title" JD.string)
                    )


statsRowDecoder : Decoder StatsRowData
statsRowDecoder =
    JD.map3 StatsRowData
        (JD.field "sent" JD.int)
        (JD.field "closed" JD.int)
        (JD.field "signatures" JD.int)


statsSLRowDecoder : Decoder SLStatsRowData
statsSLRowDecoder =
    JD.map2 SLStatsRowData
        (JD.field "sent" JD.int)
        (JD.field "closed" JD.int)
