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
        , signatoryFieldText
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
import Time exposing (Month(..), Posix, toDay, toHour, toMinute, toMonth, toYear, utc)
import Utils exposing (..)


type alias Page =
    { mSearch : Maybe String
    , mSorting : Maybe (Sorting SortColumn)
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
    }


type SortColumn
    = SCMtime
    | SCAuthor
    | SCTitle
    | SCStatus


type Msg
    = GotDocumentList (Result Http.Error (List Document))
    | SetSearch String
    | FormSubmitted
    | TableRowClicked String
    | TableHeaderClicked SortColumn


tabName : String
tabName =
    "documents"


tabTemplates : String
tabTemplates =
    "templates"


defaultSorting : Sorting SortColumn
defaultSorting =
    Sorting SCMtime SODesc


init : Config -> ( Model, Cmd Msg )
init config =
    let
        model =
            { page = Page Nothing Nothing
            , config = config
            , sDocumentList = Loading
            , search = ""
            }
    in
    ( model, getDocumentsCmd model )


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

        params =
            case model.config of
                ConfigForAllDocuments ->
                    ""

                -- no searching and sorting allowed
                ConfigForUser uid ->
                    "&userid=" ++ uid ++ filter search ++ sorting

                ConfigForUserGroupDocs ugid ->
                    "&companyid=" ++ ugid ++ filter search ++ sorting

                ConfigForUserGroupTmpl ugid ->
                    "&companyid=" ++ ugid ++ filter ("{\"filter_by\":\"is_template\"}" :: search) ++ sorting
    in
    Http.get
        { url = "/adminonly/documentslist?offset=0&max=100" ++ params
        , expect = Http.expectJson GotDocumentList documentsDecoder
        }


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals msg model =
    let
        page =
            model.page
    in
    case msg of
        SetSearch search ->
            ( { model | search = search }, Cmd.none )

        FormSubmitted ->
            ( { model | page = { page | mSearch = stringNonEmpty model.search } }
            , -- we do not initiate the search from here, because it will be
              -- triggered by the Url change
              outerCmd <| globals.setPageUrlFromModel
            )

        GotDocumentList result ->
            case result of
                Ok sDocumentList ->
                    ( { model | sDocumentList = Success sDocumentList }, Cmd.none )

                Err _ ->
                    ( { model | sDocumentList = Failure }, Cmd.none )

        TableRowClicked documentID ->
            ( model
            , outerCmd <| globals.gotoDaveDocument documentID
            )

        TableHeaderClicked column ->
            ( { model | page = { page | mSorting = Just <| toggleSorting column defaultSorting model.page.mSorting } }
            , -- we do not initiate the search from here, because it will be
              -- triggered by the Url change
              outerCmd <| globals.setPageUrlFromModel
            )


updatePage : Config -> Page -> Model -> ( Model, Cmd Msg )
updatePage config page model =
    let
        model1 =
            { model
                | page = page
                , config = config
                , search = M.withDefault "" page.mSearch
            }
    in
    ( model1, getDocumentsCmd model1 )


pageFromSearchSortByOrder : Maybe String -> Maybe String -> Maybe String -> Page
pageFromSearchSortByOrder mSearch mSortByStr mOrderStr =
    Page (mSearch |> M.andThen stringNonEmpty)
        (mSortingFromSortByOrder enumSortColumn mSortByStr mOrderStr)


fromPage : Page -> PageUrl
fromPage page =
    { emptyPageUrl
        | query =
            mSearchToQuery page.mSearch
                ++ mSortingToQuery enumSortColumn page.mSorting
    }


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


view : Model -> Html Msg
view model =
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
                    Table.simpleThead
                        [ th "col-3" SCMtime [ text "Last event", sortIndicator SCMtime sorting ]
                        , th "col-4" SCTitle [ text "Title", sortIndicator SCTitle sorting ]
                        , th "col-3" SCAuthor [ text "Owner", sortIndicator SCAuthor sorting ]
                        , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Shareable link" ]
                        , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Shared" ]
                        ]
                , tbody =
                    Table.tbody [] <|
                        L.map viewTemplate documents
                }

        _ ->
            Table.table
                { options = [ Table.striped, Table.hover, Table.small ]
                , thead =
                    Table.simpleThead
                        [ th "col-2" SCMtime [ text "Dates", sortIndicator SCMtime sorting ]
                        , th "col-2" SCAuthor [ text "Author", sortIndicator SCAuthor sorting ]
                        , th "col-3" SCTitle [ text "Title", sortIndicator SCTitle sorting ]
                        , th "col-1" SCStatus [ text "Status", sortIndicator SCStatus sorting ]
                        , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Type" ]
                        , Table.th [ Table.cellAttr <| class "col-3" ] [ text "Signatories" ]
                        ]
                , tbody =
                    Table.tbody [] <|
                        L.map viewDocument documents
                }


td : List (Html msg) -> Table.Cell msg
td content =
    Table.td [ Table.cellAttr <| class "align-middle" ] [ a [] content ]


viewDocument : Document -> Table.Row Msg
viewDocument document =
    Table.tr
        [ Table.rowAttr <| onClick <| TableRowClicked document.id
        , Table.rowAttr <| class "clickable-row"
        ]
        [ td [ text <| viewTime document.mTime, br [] [], text <| viewTime document.cTime ]
        , [ FTName, FTEmail, FTCompany ]
            |> L.filterMap (\ft -> M.map text <| authorFieldText ft document)
            |> L.intersperse (br [] [])
            |> td
        , td [ text document.title ]
        , td [ text <| Enum.toHumanString enumDocumentStatus document.status ]
        , td [ text <| Enum.toHumanString enumDocumentType document.isTemplate ]
        , document.signatories
            |> L.filter (\s -> s.role == SignatoryRoleSigningParty)
            |> L.map (text << signatorySmartName)
            |> L.intersperse (br [] [])
            |> td
        ]


authorFieldText : FieldType -> Document -> Maybe String
authorFieldText ft document =
    documentAuthor document
        |> M.andThen (signatoryFieldText ft)


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
        ]
        [ td [ text <| viewTime template.mTime ]
        , td [ text template.title ]
        , [ FTName, FTEmail ]
            |> L.filterMap (\ft -> M.map text <| authorFieldText ft template)
            |> L.intersperse (br [] [])
            |> td
        , td [ text <| ite (isJust template.shareableLink) "✔" "" ] -- \{2714}
        , td [ text <| ite template.isShared "✔" "" ] -- \{2714}
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
