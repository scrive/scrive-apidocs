module AdminOnly.UserAdminTab.UserAdminTab exposing
    ( Model
    , Msg
    , Page
    , fromPage
    , init
    , pageFromModel
    , pageFromSearchOrder
    , tabName
    , update
    , updatePage
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import EnumExtra as Enum
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)
import List as L
import Maybe as M
import Result as R
import Return exposing (..)
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)
import Url.Builder as UB
import Utils exposing (..)
import Vendor.PaginationExtra as Pagination


type alias Page =
    { mSearch : Maybe String
    , mSorting : Maybe (Sorting SortColumn)
    , paginationPageNum : Int
    }


type alias Model =
    { page : Page
    , sUserList : Status (List User)
    , search : String
    , mPaginationTotal : Maybe Int
    }


type SortColumn
    = SCTosDate
    | SCName


defaultSorting : Sorting SortColumn
defaultSorting =
    Sorting SCName SOAsc


type Msg
    = GotUserList (Result Http.Error ( Int, List User ))
    | SetSearch String
    | FormSubmitted
    | TableRowClicked String
    | TableHeaderClicked SortColumn
    | PaginationMsg Int


tabName : String
tabName =
    "salesuseradmin"


init : (Msg -> msg) -> Page -> Return msg Model
init embed page =
    let
        model =
            { page = page
            , sUserList = Loading
            , search = ""
            , mPaginationTotal = Nothing
            }
    in
    return model <| Cmd.map embed <| getUsersCmd model


getUsersCmd : Model -> Cmd Msg
getUsersCmd model =
    let
        search =
            model.search
                |> stringNonEmpty
                |> M.map (\s -> "&text=" ++ s)
                |> M.withDefault ""

        sorting =
            model.page.mSorting
                |> M.withDefault defaultSorting
                |> (\s ->
                        case s.column of
                            SCTosDate ->
                                "&tosSorting=" ++ Enum.toString enumSortOrder s.order

                            SCName ->
                                ""
                   )

        offset =
            "?offset="
                ++ (String.fromInt <| Pagination.pageNumToOffset model.page.paginationPageNum)

        limit =
            "&limit=" ++ String.fromInt Pagination.itemsPerPage
    in
    Http.get
        { url = "/adminonly/userslist" ++ offset ++ limit ++ search ++ sorting
        , expect = Http.expectJson GotUserList usersDecoder
        }


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed globals msg model =
    case msg of
        SetSearch search ->
            singleton { model | search = search }

        FormSubmitted ->
            let
                page =
                    model.page
            in
            return { model | page = { page | mSearch = stringNonEmpty model.search } }
                -- we do not initiate the search from here, because it will be
                -- triggered by the Url change
                globals.setPageUrlFromModel

        GotUserList result ->
            singleton <|
                case result of
                    Ok ( total, userList ) ->
                        { model | sUserList = Success userList, mPaginationTotal = Just total }

                    Err _ ->
                        { model | sUserList = Failure, mPaginationTotal = Nothing }

        TableRowClicked uid ->
            return model <| globals.gotoUser uid

        TableHeaderClicked column ->
            let
                nameOnlyAsc sorting =
                    ite (sorting.column == SCName) { sorting | order = SOAsc } sorting

                page =
                    model.page
            in
            return { model | page = { page | mSorting = Just <| nameOnlyAsc <| toggleSorting column defaultSorting model.page.mSorting } }
                -- we do not initiate the search from here, because it will be
                -- triggered by the Url change
                globals.setPageUrlFromModel

        PaginationMsg pageNum ->
            let
                page =
                    model.page
            in
            return { model | page = { page | paginationPageNum = pageNum } } globals.setPageUrlFromModel


updatePage : (Msg -> msg) -> Page -> Model -> Return msg Model
updatePage embed page model =
    let
        model1 =
            { model | page = page, search = M.withDefault "" page.mSearch }
    in
    return model1 <| Cmd.map embed <| getUsersCmd model1


fromPage : Page -> PageUrl
fromPage page =
    let
        sortQuery =
            page.mSorting
                |> M.map
                    (\s ->
                        case s.column of
                            SCName ->
                                []

                            SCTosDate ->
                                [ UB.string "order" <| Enum.toString enumSortOrder s.order ]
                    )
                |> M.withDefault []

        pagination =
            ite (page.paginationPageNum == 1) [] [ UB.int "p" page.paginationPageNum ]
    in
    { emptyPageUrl | query = mSearchToQuery page.mSearch ++ sortQuery ++ pagination }


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


pageFromSearchOrder : Maybe String -> Maybe String -> Maybe Int -> Page
pageFromSearchOrder mSearch0 mOrderStr mPaginationPageNum =
    let
        mSearch =
            mSearch0 |> M.andThen stringNonEmpty

        mOrder =
            mOrderStr |> M.andThen (Enum.findEnumValue enumSortOrder >> R.toMaybe)
    in
    Page mSearch
        (M.map (Sorting SCTosDate) mOrder)
        (M.withDefault 1 mPaginationPageNum)


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    Html.map embed <|
        div []
            [ Form.formInline [ class "justify-content-end", onSubmit FormSubmitted ]
                [ Input.text
                    [ Input.attrs
                        [ onInput SetSearch
                        , value model.search
                        , placeholder "Username, email or company name"
                        ]
                    ]
                , Button.button
                    [ Button.secondary
                    , Button.attrs [ attribute "type" "submit", class "ml-sm-2", value "submit" ]
                    ]
                    [ text "Search" ]
                ]
            , div [ class "mt-3", class "container-fluid" ]
                [ case model.sUserList of
                    Failure ->
                        text "Failure"

                    Loading ->
                        text "Loading"

                    Success sUserList ->
                        viewUsers model sUserList
                ]
            , Pagination.view
                model.page.paginationPageNum
                model.mPaginationTotal
                PaginationMsg
            ]


type alias User =
    { id : String
    , username : String
    , email : String
    , company : String
    , usergroup : String
    , phone : String
    , tos : Maybe Posix
    , twoFAActive : Bool
    }


viewDate : Posix -> String
viewDate dt =
    String.fromInt (toDay utc dt)
        ++ " "
        ++ viewMonth (toMonth utc dt)
        ++ " "
        ++ String.fromInt (toYear utc dt)


usersDecoder : Decoder ( Int, List User )
usersDecoder =
    D.map2 Tuple.pair
        (D.field "total_matching" D.int)
        (D.field "users"
            (D.list
                (D.map8 User
                    (D.field "id" D.string)
                    (D.field "username" D.string)
                    (D.field "email" D.string)
                    (D.field "company" D.string)
                    (D.field "usergroup" D.string)
                    (D.field "phone" D.string)
                    (D.field "tos" (datetimeDecoder |> D.nullable))
                    (D.field "twofactor_active" D.bool)
                )
            )
        )


viewUsers : Model -> List User -> Html Msg
viewUsers model users =
    let
        th colClass column =
            Table.th <|
                [ TableHeaderClicked column |> onClick |> Table.cellAttr
                , Table.cellAttr <| class "clickable-cell"
                , Table.cellAttr <| class colClass
                ]

        sorting =
            M.withDefault defaultSorting model.page.mSorting
    in
    Table.table
        { options = [ Table.striped, Table.hover, Table.small ]
        , thead =
            Table.thead []
                [ Table.tr [ Table.rowAttr <| class "row" ]
                    [ th "col-2" SCName [ text "Username", sortIndicator SCName sorting ]
                    , Table.th [ Table.cellAttr <| class "col-3" ] [ text "Email" ]
                    , Table.th [ Table.cellAttr <| class "col-2" ] [ text "Company" ]
                    , Table.th [ Table.cellAttr <| class "col-1" ] [ text "User Group" ]
                    , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Phone" ]
                    , th "col-2" SCTosDate [ text "TOS date", sortIndicator SCTosDate sorting ]
                    , Table.th [ Table.cellAttr <| class "col-1" ] [ text "2FA" ]
                    ]
                ]
        , tbody =
            Table.tbody [] <|
                L.map viewUser users
        }


viewUser : User -> Table.Row Msg
viewUser user =
    let
        colAttr colStr =
            Table.cellAttr <| class colStr
    in
    Table.tr
        [ Table.rowAttr <| onClick <| TableRowClicked user.id
        , Table.rowAttr <| class "clickable-row"
        , Table.rowAttr <| class "row"
        ]
        [ Table.td [ colAttr "col-2" ] [ text user.username ]
        , Table.td [ colAttr "col-3" ] [ text user.email ]
        , Table.td [ colAttr "col-2" ] [ text user.company ]
        , Table.td [ colAttr "col-1" ] [ text user.usergroup ]
        , Table.td [ colAttr "col-1" ] [ text user.phone ]
        , Table.td [ colAttr "col-2" ] [ text <| M.withDefault "" <| M.map viewDate user.tos ]
        , Table.td [ colAttr "col-1" ] [ text <| ite user.twoFAActive "x" "" ]
        ]
