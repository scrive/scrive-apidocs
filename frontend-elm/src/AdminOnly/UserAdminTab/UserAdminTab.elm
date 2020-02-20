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

import AdminOnly.UserAdminTab.CreateUserModal as CreateUserModal exposing (Config(..))
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import EnumExtra as Enum
import FlashMessage
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)
import List as L
import Maybe as M
import Result as R
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)
import Url.Builder as UB
import Utils exposing (..)


type alias Page =
    { mSearch : Maybe String
    , mSorting : Maybe (Sorting SortColumn)
    }


type alias Model =
    { createUserModal : CreateUserModal.Model
    , page : Page
    , sUserList : Status (List User)
    , search : String
    }


type SortColumn
    = SCTosDate
    | SCName


defaultSorting : Sorting SortColumn
defaultSorting =
    Sorting SCName SOAsc


type Msg
    = GotUserList (Result Http.Error (List User))
    | CreateUserModalMsg CreateUserModal.Msg
    | CreateUserClicked
    | SetSearch String
    | FormSubmitted
    | TableRowClicked String
    | TableHeaderClicked SortColumn


tabName : String
tabName =
    "salesuseradmin"


init : Page -> ( Model, Cmd Msg )
init page =
    let
        ( createUserModal, createUserModalCmd ) =
            CreateUserModal.init CreateUserWithRootUserGroup

        model =
            { createUserModal = createUserModal
            , page = page
            , sUserList = Loading
            , search = ""
            }
    in
    ( model
    , Cmd.batch
        [ liftCmd CreateUserModalMsg createUserModalCmd
        , getUsersCmd model
        ]
    )


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
    in
    Http.get
        { url = "/adminonly/userslist?limit=101&offset=0" ++ search ++ sorting
        , expect = Http.expectJson GotUserList usersDecoder
        }


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals =
    let
        updateCreateUserModal =
            CreateUserModal.update globals
    in
    \msg model ->
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
                  outerCmd globals.setPageUrlFromModel
                )

            GotUserList result ->
                case result of
                    Ok usersList ->
                        ( { model | sUserList = Success usersList }, Cmd.none )

                    Err _ ->
                        ( { model | sUserList = Failure }, Cmd.none )

            CreateUserModalMsg createUserModalMsg ->
                let
                    ( createUserModal, createUserModalCmd, userWasCreated ) =
                        updateCreateUserModal createUserModalMsg model.createUserModal

                    reloadCmd =
                        if userWasCreated then
                            Cmd.batch
                                [ innerCmd <| getUsersCmd model
                                , outerCmd <| globals.flashMessage <| FlashMessage.success "User was created."
                                ]

                        else
                            Cmd.none
                in
                ( { model | createUserModal = createUserModal }
                , Cmd.batch [ innerCmd <| liftCmd CreateUserModalMsg createUserModalCmd, reloadCmd ]
                )

            CreateUserClicked ->
                ( { model | createUserModal = CreateUserModal.show model.createUserModal }, Cmd.none )

            TableRowClicked uid ->
                ( model, outerCmd <| globals.gotoUser uid )

            TableHeaderClicked column ->
                let
                    nameOnlyAsc sorting =
                        ite (sorting.column == SCName) { sorting | order = SOAsc } sorting
                in
                ( { model | page = { page | mSorting = Just <| nameOnlyAsc <| toggleSorting column defaultSorting model.page.mSorting } }
                , -- we do not initiate the search from here, because it will be
                  -- triggered by the Url change
                  outerCmd globals.setPageUrlFromModel
                )


updatePage : Page -> Model -> ( Model, Cmd Msg )
updatePage page model =
    let
        model1 =
            { model | page = page, search = M.withDefault "" page.mSearch }
    in
    ( model1, getUsersCmd model1 )


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
    in
    { emptyPageUrl | query = mSearchToQuery page.mSearch ++ sortQuery }


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


pageFromSearchOrder : Maybe String -> Maybe String -> Page
pageFromSearchOrder mSearch0 mOrderStr =
    let
        mSearch =
            mSearch0 |> M.andThen stringNonEmpty

        mOrder =
            mOrderStr |> M.andThen (Enum.findEnumValue enumSortOrder >> R.toMaybe)
    in
    Page mSearch <| M.map (Sorting SCTosDate) mOrder


view : Model -> Html Msg
view model =
    div []
        [ Grid.row [ Row.betweenXs ]
            [ Grid.col []
                [ Button.button [ Button.success, Button.attrs [ onClick CreateUserClicked ] ]
                    [ text "Create user with empty company"
                    ]
                ]
            , Grid.col []
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
                ]
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
        , liftHtml CreateUserModalMsg <| CreateUserModal.view model.createUserModal
        ]


type alias User =
    { id : String
    , username : String
    , email : String
    , companyPosition : String
    , company : String
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


usersDecoder : Decoder (List User)
usersDecoder =
    D.field "users"
        (D.list
            (D.map8 User
                (D.field "id" D.string)
                (D.field "username" D.string)
                (D.field "email" D.string)
                (D.field "companyposition" D.string)
                (D.field "company" D.string)
                (D.field "phone" D.string)
                (D.field "tos" (datetimeDecoder |> D.nullable))
                (D.field "twofactor_active" D.bool)
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
                    , Table.th [ Table.cellAttr <| class "col-1" ] [ text "Position" ]
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
        , Table.td [ colAttr "col-1" ] [ text user.companyPosition ]
        , Table.td [ colAttr "col-1" ] [ text user.phone ]
        , Table.td [ colAttr "col-2" ] [ text <| M.withDefault "" <| M.map viewDate user.tos ]
        , Table.td [ colAttr "col-1" ] [ text <| ite user.twoFAActive "x" "" ]
        ]
