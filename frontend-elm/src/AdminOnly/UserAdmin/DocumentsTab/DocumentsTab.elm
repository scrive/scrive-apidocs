module AdminOnly.UserAdmin.DocumentsTab.DocumentsTab exposing
    ( Config(..)
    , Model
    , Msg
    , Page
    , fromPage
    , init
    , pageFromModel
    , pageFromSearchSortByOrder
    , tabName
    , tabTemplates
    , update
    , updatePage
    , view
    )

import AdminOnly.UserAdmin.DocumentsTab.Document
    exposing
        ( Document
        , FieldType(..)
        , SignatoryRole(..)
        , documentAuthor
        , documentsDecoder
        , enumDocumentStatus
        , enumDocumentType
        , extendedDocumentStatus
        , signatoryFieldText
        , signatoryFullName
        , signatorySmartName
        )
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import EnumExtra as Enum exposing (Enum)
import Html exposing (Html, a, br, div, text)
import Html.Attributes exposing (attribute, class, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import List as L
import Maybe as M
import Maybe.Extra as M
import Return exposing (..)
import Time exposing (Month(..), Posix, toDay, toHour, toMinute, toMonth, toYear, utc)
import Url.Builder as UB
import Utils exposing (..)
import Vendor.PaginationExtra as Pagination


type alias Page =
    { mSearch : Maybe String
    , mSorting : Maybe (Sorting SortColumn)
    , paginationPageNum : Int
    }


type Config
    = ConfigForUser String
    | ConfigForAllDocuments
    | ConfigForUserGroupDocs String
    | ConfigForUserGroupTmpl String


type alias Model =
    { page : Page
    , config : Config
    , sDocumentList : Status (List Document)
    , search : String
    , mPaginationTotal : Maybe Int
    }


type SortColumn
    = SCMtime
    | SCAuthor
    | SCTitle
    | SCStatus


type Msg
    = GotDocumentList (Result Http.Error ( Int, List Document ))
    | SetSearch String
    | FormSubmitted
    | TableRowClicked String
    | TableHeaderClicked SortColumn
    | PaginationMsg Int


tabName : String
tabName =
    "documents"


tabTemplates : String
tabTemplates =
    "templates"


defaultSorting : Sorting SortColumn
defaultSorting =
    Sorting SCMtime SODesc


init : (Msg -> msg) -> Config -> Int -> Return msg Model
init embed config paginationPageNum =
    let
        model =
            { page = Page Nothing Nothing paginationPageNum
            , config = config
            , sDocumentList = Loading
            , search = ""
            , mPaginationTotal = Nothing
            }
    in
    return model <| Cmd.map embed <| getDocumentsCmd model


getDocumentsCmd : Model -> Cmd Msg
getDocumentsCmd model =
    let
        search =
            model.page.mSearch
                |> M.map (\s -> [ "{\"filter_by\":\"text\",\"text\":\"" ++ s ++ "\"}" ])
                |> M.withDefault []

        filter filters =
            "&filter=[" ++ String.join "," filters ++ "]"

        sorting =
            model.page.mSorting
                |> M.withDefault defaultSorting
                |> (\s ->
                        "&sorting=[{\"sort_by\":\""
                            ++ encodeSortColumn s.column
                            ++ "\",\"order\":\""
                            ++ Enum.toString enumSortOrder s.order
                            ++ "\"}]"
                   )

        offset =
            "?offset="
                ++ (String.fromInt <| Pagination.pageNumToOffset model.page.paginationPageNum)

        max =
            "&max=" ++ String.fromInt Pagination.itemsPerPage

        params =
            offset
                ++ max
                ++ (case model.config of
                        ConfigForAllDocuments ->
                            ""

                        -- no searching and sorting allowed
                        ConfigForUser uid ->
                            "&userid=" ++ uid ++ filter search ++ sorting

                        ConfigForUserGroupDocs ugid ->
                            "&companyid=" ++ ugid ++ filter search ++ sorting

                        ConfigForUserGroupTmpl ugid ->
                            "&companyid="
                                ++ ugid
                                ++ filter ("{\"filter_by\":\"is_template\"}" :: search)
                                ++ sorting
                   )
    in
    Http.get
        { url = "/adminonly/documentslist" ++ params
        , expect = Http.expectJson GotDocumentList documentsDecoder
        }


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update _ globals msg model =
    let
        page =
            model.page
    in
    case msg of
        SetSearch search ->
            singleton { model | search = search }

        FormSubmitted ->
            return { model | page = { page | mSearch = stringNonEmpty model.search } }
                -- we do not initiate the search from here, because it will be
                -- triggered by the Url change
                globals.setPageUrlFromModel

        GotDocumentList result ->
            singleton <|
                case result of
                    Ok ( total, documentList ) ->
                        { model | sDocumentList = Success documentList, mPaginationTotal = Just total }

                    Err _ ->
                        { model | sDocumentList = Failure, mPaginationTotal = Nothing }

        TableRowClicked documentID ->
            return model <| globals.gotoDaveDocument documentID

        TableHeaderClicked column ->
            return { model | page = { page | mSorting = Just <| toggleSorting column defaultSorting model.page.mSorting } }
                -- we do not initiate the search from here, because it will be
                -- triggered by the Url change
                globals.setPageUrlFromModel

        PaginationMsg pageNum ->
            return { model | page = { page | paginationPageNum = pageNum } }
                globals.setPageUrlFromModel


updatePage : (Msg -> msg) -> Config -> Page -> Model -> Return msg Model
updatePage embed config page model =
    let
        model1 =
            { model
                | page = page
                , config = config
                , search = M.withDefault "" page.mSearch
            }
    in
    return model1 <| Cmd.map embed <| getDocumentsCmd model1


pageFromSearchSortByOrder : Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Page
pageFromSearchSortByOrder mSearch mSortByStr mOrderStr mPaginationPageNum =
    Page (mSearch |> M.andThen stringNonEmpty)
        (mSortingFromSortByOrder enumSortColumn mSortByStr mOrderStr)
        (M.withDefault 1 mPaginationPageNum)


fromPage : Page -> PageUrl
fromPage page =
    { emptyPageUrl
        | query =
            mSearchToQuery page.mSearch
                ++ mSortingToQuery enumSortColumn page.mSorting
                ++ ite (page.paginationPageNum == 1) [] [ UB.int "p" page.paginationPageNum ]
    }


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        viewSearchForm =
            [ Form.formInline [ class "justify-content-end", onSubmit <| FormSubmitted ]
                [ Input.text
                    [ Input.attrs
                        [ onInput <| SetSearch
                        , value model.search
                        , placeholder <|
                            case model.config of
                                ConfigForUserGroupTmpl _ ->
                                    "Filter templates"

                                _ ->
                                    "Filter documents"
                        ]
                    ]
                , Button.button
                    [ Button.secondary
                    , Button.attrs [ attribute "type" "submit", class "ml-sm-2", value "submit" ]
                    ]
                    [ text "Search" ]
                ]
            ]
    in
    Html.map embed <|
        div [] <|
            (if useSortingAndSearch model then
                viewSearchForm

             else
                []
            )
                ++ [ div [ class "mt-3", class "container-fluid" ]
                        [ case model.sDocumentList of
                            Failure ->
                                text "Failure"

                            Loading ->
                                text "Loading"

                            Success documentList ->
                                viewDocuments model documentList
                        ]
                   ]
                ++ [ Pagination.view
                        model.page.paginationPageNum
                        model.mPaginationTotal
                        PaginationMsg
                   ]


useSortingAndSearch : Model -> Bool
useSortingAndSearch model =
    model.config /= ConfigForAllDocuments


viewDocuments : Model -> List Document -> Html Msg
viewDocuments model documents =
    let
        th colClass column =
            Table.th <|
                if useSortingAndSearch model then
                    [ TableHeaderClicked column
                        |> onClick
                        |> Table.cellAttr
                    , Table.cellAttr <| class "clickable-cell"
                    , Table.cellAttr <| class colClass
                    ]

                else
                    [ Table.cellAttr <| class colClass ]

        sorting =
            M.withDefault defaultSorting model.page.mSorting
    in
    case model.config of
        ConfigForUserGroupTmpl _ ->
            Table.table
                { options = [ Table.striped, Table.hover, Table.small ]
                , thead =
                    Table.thead []
                        [ Table.tr [ Table.rowAttr <| class "row" ]
                            [ th "col-2" SCMtime [ text "Last event", sortIndicator SCMtime sorting ]
                            , th "col-5" SCTitle [ text "Title", sortIndicator SCTitle sorting ]
                            , th "col-3" SCAuthor [ text "Owner", sortIndicator SCAuthor sorting ]
                            , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Shareable link" ]
                            , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Shared" ]
                            ]
                        ]
                , tbody =
                    Table.tbody [] <|
                        L.map viewTemplate documents
                }

        _ ->
            Table.table
                { options = [ Table.striped, Table.hover, Table.small ]
                , thead =
                    Table.thead []
                        [ Table.tr [ Table.rowAttr <| class "row" ]
                            [ th "col-2" SCMtime [ text "Dates", sortIndicator SCMtime sorting ]
                            , th "col-2" SCAuthor [ text "Author", sortIndicator SCAuthor sorting ]
                            , th "col-3" SCTitle [ text "Title", sortIndicator SCTitle sorting ]
                            , th "col-1" SCStatus [ text "Status", sortIndicator SCStatus sorting ]
                            , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Type" ]
                            , Table.th [ Table.cellAttr <| class "col-3" ] [ text "Signatories" ]
                            ]
                        ]
                , tbody =
                    Table.tbody [] <|
                        L.map viewDocument documents
                }


td : List (Table.CellOption msg) -> List (Html msg) -> Table.Cell msg
td extraAttrs content =
    let
        attrs =
            [ Table.cellAttr <| class "align-middle" ] ++ extraAttrs
    in
    Table.td attrs [ a [] content ]


colAttr : String -> Table.CellOption msg
colAttr colStr =
    Table.cellAttr <| class colStr


viewDocument : Document -> Table.Row Msg
viewDocument document =
    Table.tr
        [ Table.rowAttr <| onClick <| TableRowClicked document.id
        , Table.rowAttr <| class "clickable-row"
        , Table.rowAttr <| class "row"
        ]
        [ td [ colAttr "col-2" ] [ text <| viewTime document.mTime, br [] [], text <| viewTime document.cTime ]
        , [ FTName, FTEmail, FTCompany ]
            |> L.filterMap (\ft -> M.map text <| authorFieldText ft document)
            |> L.intersperse (br [] [])
            |> td [ colAttr "col-2" ]
        , td [ colAttr "col-3" ] [ text document.title ]
        , td [ colAttr "col-1" ] [ text <| extendedDocumentStatus document ]
        , td [ colAttr "col-1" ] [ text <| Enum.toHumanString enumDocumentType document.isTemplate ]
        , document.signatories
            |> L.filter (\s -> s.role == SignatoryRoleSigningParty)
            |> L.map (text << signatorySmartName)
            |> L.intersperse (br [] [])
            |> td [ colAttr "col-3" ]
        ]


authorFieldText : FieldType -> Document -> Maybe String
authorFieldText ft document =
    let
        fieldText signatory =
            case ft of
                FTName ->
                    signatoryFullName signatory

                _ ->
                    signatoryFieldText ft signatory
    in
    documentAuthor document
        |> M.andThen fieldText


viewTime : Posix -> String
viewTime dt =
    String.fromInt (toYear utc dt)
        ++ "-"
        ++ (String.padLeft 2 '0' <| String.fromInt <| monthToInt <| toMonth utc dt)
        ++ "-"
        ++ (String.padLeft 2 '0' <| String.fromInt <| toDay utc dt)
        ++ " "
        ++ (String.padLeft 2 '0' <| String.fromInt <| toHour utc dt)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt <| toMinute utc dt)


viewTemplate : Document -> Table.Row Msg
viewTemplate template =
    Table.tr
        [ Table.rowAttr <| onClick <| TableRowClicked template.id
        , Table.rowAttr <| class "clickable-row"
        , Table.rowAttr <| class "row"
        ]
        [ td [ colAttr "col-2" ] [ text <| viewTime template.mTime ]
        , td [ colAttr "col-5" ] [ text template.title ]
        , [ FTName, FTEmail ]
            |> L.filterMap (\ft -> M.map text <| authorFieldText ft template)
            |> L.intersperse (br [] [])
            |> td [ colAttr "col-3" ]
        , td [ colAttr "col-1" ] [ text <| ite (isJust template.shareableLink) "✔" "" ] -- \{2714}
        , td [ colAttr "col-1" ] [ text <| ite template.isShared "✔" "" ] -- \{2714}
        ]


allSortColumns : List SortColumn
allSortColumns =
    [ SCMtime, SCAuthor, SCTitle, SCStatus ]


encodeSortColumn : SortColumn -> String
encodeSortColumn column =
    case column of
        SCMtime ->
            "mtime"

        SCAuthor ->
            "author"

        SCTitle ->
            "title"

        SCStatus ->
            "status"


enumSortColumn : Enum SortColumn
enumSortColumn =
    Enum.makeEnum allSortColumns encodeSortColumn encodeSortColumn
