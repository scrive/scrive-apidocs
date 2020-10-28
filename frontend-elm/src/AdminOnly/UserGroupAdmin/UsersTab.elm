module AdminOnly.UserGroupAdmin.UsersTab exposing
    ( Model
    , Msg
    , Page
    , fromPage
    , init
    , pageFromModel
    , pageFromSearchSortByOrder
    , tabName
    , update
    , updatePage
    , view
    )

import AdminOnly.Types.UserGroup as UserGroup exposing (UserGroup)
import AdminOnly.Types.UserGroup.Cmd as Cmd
import AdminOnly.UserAdmin.CreateUserModal as CreateUserModal
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import EnumExtra as Enum exposing (Enum, findEnumValue)
import FlashMessage
import Html exposing (Html, div, h5, text)
import Html.Attributes exposing (attribute, class, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)
import List as L
import Maybe as M
import Return exposing (..)
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)
import Utils exposing (..)


type alias Page =
    { mSearch : Maybe String
    , mSorting : Maybe (Sorting SortColumn)
    }


type alias Model =
    { createUserModal : CreateUserModal.Model
    , page : Page
    , sUserList : Status (List User)
    , sUserGroup : Status UserGroup
    , search : String
    }


type Msg
    = GotUserList (Result Http.Error (List User))
    | GotUserGroup (Result Http.Error UserGroup)
    | CreateUserModalMsg CreateUserModal.Msg
    | UserCreatedCallback CreateUserModal.UserCreated
    | CreateUserClicked
    | SetSearch String
    | FormSubmitted
    | TableRowClicked String
    | TableHeaderClicked SortColumn


type SortColumn
    = SCName
    | SCEmail
    | SCRole
    | SC2FA


defaultSorting : Sorting SortColumn
defaultSorting =
    Sorting SCName SOAsc


tabName : String
tabName =
    "users"


init : (Msg -> msg) -> String -> Page -> Return msg Model
init embed ugid page =
    let
        ( createUserModal, createUserModalCmd ) =
            CreateUserModal.init (CreateUserModal.CreateUserInUserGroup ugid)

        model =
            { createUserModal = createUserModal
            , page = page
            , sUserList = Loading
            , sUserGroup = Loading
            , search = ""
            }
    in
    return model <|
        Cmd.map embed <|
            Cmd.batch [ getUsersCmd ugid model, Cmd.getUserGroup GotUserGroup ugid, createUserModalCmd ]


getUsersCmd : String -> Model -> Cmd Msg
getUsersCmd ugid model =
    let
        search =
            model.search
                |> stringNonEmpty
                |> M.map (\s -> "text=" ++ s)
                |> M.withDefault ""

        sorting =
            model.page.mSorting
                |> M.withDefault defaultSorting
                |> (\s ->
                        "sorting="
                            ++ Enum.toString enumSortColumn s.column
                            ++ "&order="
                            ++ Enum.toString enumSortOrder s.order
                   )
    in
    Http.get
        { url = "/adminonly/companyaccounts/" ++ ugid ++ "?" ++ String.join "&" [ search, sorting ]
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
            return { model | page = { page | mSearch = Just model.search } }
                -- we do not initiate the search from here, because it will be
                -- triggered by the Url change
                globals.setPageUrlFromModel

        GotUserList result ->
            singleton <|
                case result of
                    Ok usersList ->
                        { model | sUserList = Success usersList }

                    Err _ ->
                        { model | sUserList = Failure }

        GotUserGroup result ->
            singleton <|
                case result of
                    Ok userGroup ->
                        { model | sUserGroup = Success userGroup }

                    Err _ ->
                        { model | sUserGroup = Failure }

        CreateUserModalMsg createUserModalMsg ->
            let
                ( createUserModal2, cmd ) =
                    CreateUserModal.update (embed << CreateUserModalMsg) (embed << UserCreatedCallback) globals createUserModalMsg model.createUserModal
            in
            return { model | createUserModal = createUserModal2 } cmd

        UserCreatedCallback userCreated ->
            return model <|
                case userCreated of
                    Just (Ok str) ->
                        -- User created successfully
                        Cmd.batch
                            [ globals.setPageUrlFromModel
                            , globals.flashMessage <| FlashMessage.success str
                            ]

                    Just (Err str) ->
                        -- Failed to create a user
                        globals.flashMessage <| FlashMessage.error str

                    Nothing ->
                        -- No attempt was made to create a user
                        Cmd.none

        CreateUserClicked ->
            singleton { model | createUserModal = CreateUserModal.show model.createUserModal }

        TableRowClicked uid ->
            return model <| globals.gotoUser uid

        TableHeaderClicked column ->
            let
                page =
                    model.page
            in
            return { model | page = { page | mSorting = Just <| toggleSorting column defaultSorting model.page.mSorting } }
                -- we do not initiate the search from here, because it will be
                -- triggered by the Url change
                globals.setPageUrlFromModel


updatePage : (Msg -> msg) -> String -> Page -> Model -> Return msg Model
updatePage embed ugid page model =
    let
        ( createUserModal, createUserModalCmd ) =
            CreateUserModal.init (CreateUserModal.CreateUserInUserGroup ugid)

        model1 =
            { model
                | page = page
                , createUserModal = createUserModal
                , search = M.withDefault "" page.mSearch
                , sUserList = Loading
                , sUserGroup = Loading
            }
    in
    return model1 <|
        Cmd.map embed <|
            Cmd.batch
                [ getUsersCmd ugid model1
                , Cmd.getUserGroup GotUserGroup ugid
                , createUserModalCmd
                ]


fromPage : Page -> PageUrl
fromPage page =
    { emptyPageUrl
        | query =
            mSearchToQuery page.mSearch
                ++ mSortingToQuery enumSortColumn page.mSorting
    }


pageFromSearchSortByOrder : Maybe String -> Maybe String -> Maybe String -> Page
pageFromSearchSortByOrder mSearch mSortByStr mOrderStr =
    Page (mSearch |> M.andThen stringNonEmpty)
        (mSortingFromSortByOrder enumSortColumn mSortByStr mOrderStr)


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        usersNotAllowedView =
            infoText "Users are not allowed in billable user group!"

        addUserButton userGroup =
            if not userGroup.isBillable then
                [ Button.button [ Button.success, Button.attrs [ onClick CreateUserClicked ] ]
                    [ text "Add new user in company" ]
                ]

            else
                []

        usersListView userList =
            div [ class "mt-3", class "container-fluid" ] [ viewUsers model userList ]

        noUsersView =
            let
                title =
                    case model.page.mSearch of
                        Just _ ->
                            "No user matching search criteria was found."

                        Nothing ->
                            "There are no users in this user group. Start by adding some."
            in
            infoText title

        infoText msg =
            h5 [ class "text-center pt-5" ] [ text msg ]

        loaded ( userList, userGroup ) =
            Html.map embed <|
                div []
                    [ Grid.row [ Row.betweenXs ]
                        [ Grid.col [] <| addUserButton userGroup
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
                    , case ( List.isEmpty userList, userGroup.isBillable ) of
                        ( False, False ) ->
                            usersListView userList

                        ( True, False ) ->
                            noUsersView

                        ( False, True ) ->
                            div [] [ usersListView userList, usersNotAllowedView ]

                        ( True, True ) ->
                            usersNotAllowedView
                    , CreateUserModal.view CreateUserModalMsg model.createUserModal
                    ]
    in
    statusView loaded <| statusMerge2 model.sUserList model.sUserGroup


type alias User =
    { id : String
    , fullname : String
    , email : String
    , role : Role
    , tos : Maybe Posix
    , twoFAActive : Bool
    }


type Role
    = RoleAdmin
    | RoleStandard
    | RoleInvite


viewDate : Posix -> String
viewDate dt =
    String.fromInt (toDay utc dt)
        ++ " "
        ++ viewMonth (toMonth utc dt)
        ++ " "
        ++ String.fromInt (toYear utc dt)


usersDecoder : Decoder (List User)
usersDecoder =
    D.field "accounts"
        (D.list
            (D.map6 User
                (D.field "id" D.string)
                (D.field "fullname" D.string)
                (D.field "email" D.string)
                (D.field "role" (D.string |> D.andThen roleDecoder))
                (D.field "tos" (datetimeDecoder |> D.nullable))
                (D.field "twofactor_active" D.bool)
            )
        )


roleDecoder : String -> Decoder Role
roleDecoder roleString =
    case findEnumValue enumRole roleString of
        Err _ ->
            D.fail <| "Cannot parse role: " ++ roleString

        Ok role ->
            D.succeed role


encodeRole : Role -> String
encodeRole role =
    case role of
        RoleAdmin ->
            "RoleAdmin"

        RoleStandard ->
            "RoleStandard"

        RoleInvite ->
            "RoleInvite"


fromRole : Role -> String
fromRole role =
    case role of
        RoleAdmin ->
            "Admin"

        RoleStandard ->
            "Standard"

        RoleInvite ->
            "!User in different company!"


enumRole : Enum Role
enumRole =
    Enum.makeEnum [ RoleAdmin, RoleStandard, RoleInvite ] encodeRole fromRole


viewUsers : Model -> List User -> Html Msg
viewUsers model users =
    let
        th colClass column =
            Table.th <|
                [ TableHeaderClicked column
                    |> onClick
                    |> Table.cellAttr
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
                    [ th "col-3" SCName [ text "Username", sortIndicator SCName sorting ]
                    , th "col-3" SCEmail [ text "Email", sortIndicator SCEmail sorting ]
                    , th "col-3" SCRole [ text "Role", sortIndicator SCRole sorting ]
                    , Table.th [ Table.cellAttr <| class "col-2" ] [ text "TOS date" ]
                    , th "col-1" SC2FA [ text "2FA", sortIndicator SC2FA sorting ]
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
        [ Table.td [ colAttr "col-3" ] [ text user.fullname ]
        , Table.td [ colAttr "col-3" ] [ text user.email ]
        , Table.td [ colAttr "col-3" ] [ text <| Enum.toHumanString enumRole user.role ]
        , Table.td [ colAttr "col-2" ] [ text <| M.withDefault "" <| M.map viewDate user.tos ]
        , Table.td [ colAttr "col-1" ] [ text <| ite user.twoFAActive "x" "" ]
        ]


encodeSortColumn : SortColumn -> String
encodeSortColumn column =
    case column of
        SCName ->
            "fullname"

        SCEmail ->
            "email"

        SCRole ->
            "role"

        SC2FA ->
            "twofactor_active"


enumSortColumn : Enum SortColumn
enumSortColumn =
    Enum.makeEnum [ SCName, SCEmail, SCRole, SC2FA ] encodeSortColumn encodeSortColumn
