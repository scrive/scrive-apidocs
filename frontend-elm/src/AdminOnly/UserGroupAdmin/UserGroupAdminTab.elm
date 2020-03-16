module AdminOnly.UserGroupAdmin.UserGroupAdminTab exposing
    ( Model
    , Msg
    , Page
    , fromPage
    , init
    , pageFromFilterSearch
    , pageFromModel
    , tabName
    , update
    , updatePage
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Enum exposing (Enum, findEnumValue, makeEnum)
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class, method, placeholder, selected, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)
import List as L
import Maybe as M
import Time exposing (Month(..))
import Url.Builder as UB
import Url.Parser exposing (map)
import Utils exposing (..)


type alias Page =
    { mFilter : Maybe Filter
    , mSearch : Maybe String
    }


type Filter
    = All
    | WithMoreThanOneUser
    | WithNonFreePricePlan


type alias Model =
    { page : Page
    , userGroupList : UserGroupListState
    , search : String
    }


type UserGroupListState
    = Failure
    | Loading
    | Success (List UserGroup)


type Msg
    = GotUserGroupList (Result Http.Error (List UserGroup))
    | SetSearch String
    | SetFilter String
    | FormSubmitted
    | TableRowClicked String


tabName : String
tabName =
    "companyadmin"


init : (Msg -> msg) -> Page -> ( Model, Cmd msg )
init embed page =
    let
        model =
            { page = page
            , userGroupList = Loading
            , search = ""
            }
    in
    ( model, Cmd.map embed <| getUserGroupsCmd model )


getUserGroupsCmd : Model -> Cmd Msg
getUserGroupsCmd model =
    let
        search =
            M.withDefault "" <| M.map (\s -> "&text=" ++ s) model.page.mSearch

        filter =
            case M.withDefault WithNonFreePricePlan model.page.mFilter of
                All ->
                    "&allCompanies=true"

                -- that is the default behaviour
                WithMoreThanOneUser ->
                    ""

                WithNonFreePricePlan ->
                    "&allCompanies=true&nonFree=true"
    in
    Http.get
        { url = "/adminonly/companies?limit=101&offset=0" ++ search ++ filter
        , expect = Http.expectJson GotUserGroupList userGroupsDecoder
        }


pageFromFilterSearch : Maybe String -> Maybe String -> Page
pageFromFilterSearch mFilterStr mSearch =
    let
        mFilter =
            mFilterStr
                |> M.andThen
                    (\filterStr ->
                        case filterStr of
                            "moreThanOneUser" ->
                                Just WithMoreThanOneUser

                            "all" ->
                                Just All

                            _ ->
                                Nothing
                    )
    in
    { mFilter = mFilter, mSearch = mSearch }


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update _ globals msg model =
    let
        page =
            model.page
    in
    case msg of
        SetSearch search ->
            ( { model | search = search }, Cmd.none )

        SetFilter filterString ->
            case findEnumValue enumFilter filterString of
                Err _ ->
                    ( model, Cmd.none )

                Ok filter ->
                    ( { model | page = { page | mFilter = Just filter } }
                    , -- we do not initiate the search from here, because it will be
                      -- triggered by the Url change
                      globals.setPageUrlFromModel
                    )

        FormSubmitted ->
            ( { model | page = { page | mSearch = stringNonEmpty model.search } }
            , -- we do not initiate the search from here, because it will be
              -- triggered by the Url change
              globals.setPageUrlFromModel
            )

        GotUserGroupList result ->
            case result of
                Ok userGroupList ->
                    ( { model | userGroupList = Success userGroupList }, Cmd.none )

                Err _ ->
                    ( { model | userGroupList = Failure }, Cmd.none )

        TableRowClicked userGroupID ->
            ( model
            , globals.gotoUserGroup userGroupID
            )


updatePage : (Msg -> msg) -> Page -> Model -> ( Model, Cmd msg )
updatePage embed page model0 =
    let
        model =
            { model0
                | page = page
                , search = M.withDefault "" page.mSearch
            }
    in
    ( model, Cmd.map embed <| getUserGroupsCmd model )


fromPage : Page -> PageUrl
fromPage page =
    let
        filterQ =
            case page.mFilter of
                Nothing ->
                    []

                Just All ->
                    [ UB.string "filter" "all" ]

                Just WithMoreThanOneUser ->
                    [ UB.string "filter" "moreThanOneUser" ]

                -- that is the default behaviour
                Just WithNonFreePricePlan ->
                    []

        searchQ =
            M.withDefault [] <| M.map (\s -> [ UB.string "search" s ]) page.mSearch
    in
    { emptyPageUrl | query = filterQ ++ searchQ }


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        filter =
            M.withDefault WithNonFreePricePlan <| model.page.mFilter
    in
    Html.map embed <| div []
        [ Form.formInline [ class "justify-content-end", method "get", onSubmit FormSubmitted ]
            [ Select.select [ Select.onChange <| SetFilter ] <|
                L.map
                    (\f ->
                        Select.item
                            [ value <| encodeFilter f
                            , selected <| f == filter
                            ]
                            [ text <| textFromFilter f ]
                    )
                <|
                    allFilters
            , Input.text
                [ Input.attrs
                    [ onInput SetSearch
                    , value model.search
                    , class "ml-sm-2"
                    , placeholder "Username, email or company name"
                    ]
                ]
            , Button.button
                [ Button.secondary
                , Button.attrs [ attribute "type" "submit", class "ml-sm-2", value "submit" ]
                ]
                [ text "Search" ]
            ]
        , div [ class "mt-3" ]
            [ case model.userGroupList of
                Failure ->
                    text "Failure"

                Loading ->
                    text "Loading"

                Success userGroupList ->
                    viewUserGroups model userGroupList
            ]
        ]


type alias UserGroup =
    { id : String
    , name : String
    , number : String
    , address : String
    , zip : String
    , city : String
    , country : String
    }


userGroupsDecoder : Decoder (List UserGroup)
userGroupsDecoder =
    D.field "companies"
        (D.list
            (D.map7 UserGroup
                (D.field "id" D.string)
                (D.field "companyname" D.string)
                (D.field "companynumber" D.string)
                (D.field "companyaddress" D.string)
                (D.field "companyzip" D.string)
                (D.field "companycity" D.string)
                (D.field "companycountry" D.string)
            )
        )


viewUserGroups : Model -> List UserGroup -> Html Msg
viewUserGroups model userGroups =
    let
        colAttr colStr =
            Table.cellAttr <| class colStr
    in
    Table.table
        { options = [ Table.striped, Table.hover, Table.small ]
        , thead =
            Table.thead []
                [ Table.tr [ Table.rowAttr <| class "row" ]
                    [ Table.th [ colAttr "col-3" ] [ text "Name" ]
                    , Table.th [ colAttr "col-2" ] [ text "Number" ]
                    , Table.th [ colAttr "col-2" ] [ text "Address" ]
                    , Table.th [ colAttr "col-1" ] [ text "Zip" ]
                    , Table.th [ colAttr "col-1" ] [ text "City" ]
                    , Table.th [ colAttr "col-1" ] [ text "Country" ]
                    , Table.th [ colAttr "col-2" ] [ text "ID" ]
                    ]
                ]
        , tbody =
            Table.tbody [] <|
                L.map (viewUserGroup model) userGroups
        }


viewUserGroup : Model -> UserGroup -> Table.Row Msg
viewUserGroup _ userGroup =
    let
        colAttr colStr =
            Table.cellAttr <| class colStr
    in
    Table.tr
        [ Table.rowAttr <| onClick <| TableRowClicked userGroup.id
        , Table.rowAttr <| class "clickable-row"
        , Table.rowAttr <| class "row"
        ]
        [ Table.td [ colAttr "col-3" ] [ text userGroup.name ]
        , Table.td [ colAttr "col-2" ] [ text userGroup.number ]
        , Table.td [ colAttr "col-2" ] [ text userGroup.address ]
        , Table.td [ colAttr "col-1" ] [ text userGroup.zip ]
        , Table.td [ colAttr "col-1" ] [ text userGroup.city ]
        , Table.td [ colAttr "col-1" ] [ text userGroup.country ]
        , Table.td [ colAttr "col-2" ] [ text userGroup.id ]
        ]


allFilters : List Filter
allFilters =
    [ WithNonFreePricePlan
    , WithMoreThanOneUser
    , All
    ]


enumFilter : Enum Filter
enumFilter =
    makeEnum allFilters encodeFilter


encodeFilter : Filter -> String
encodeFilter filter =
    case filter of
        All ->
            "all"

        WithMoreThanOneUser ->
            "moreThanOneUser"

        WithNonFreePricePlan ->
            "nonFree"


textFromFilter : Filter -> String
textFromFilter filter =
    case filter of
        All ->
            "All"

        WithMoreThanOneUser ->
            "With more than one user"

        WithNonFreePricePlan ->
            "With non-free price plan"
