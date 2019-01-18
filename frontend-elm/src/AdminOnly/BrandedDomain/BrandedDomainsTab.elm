module AdminOnly.BrandedDomain.BrandedDomainsTab exposing
    ( Model
    , Msg
    , init
    , tabName
    , update
    , view
    )

import AdminOnly.BrandedDomain.Branding exposing (BrandedDomain, brandedDomainListDecoder)
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import FlashMessage
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import List as L
import Time exposing (Month(..))
import Url.Parser exposing (map)
import Utils
    exposing
        ( Action
        , Globals
        , Status(..)
        , formBody
        , innerCmd
        , outerCmd
        )


type alias Model =
    { sBrandedDomains : Status (List BrandedDomain)
    }


type Msg
    = GotBrandedDomainList (Result Http.Error (List BrandedDomain))
    | TableRowClicked String
    | CreateBrandedDomainClicked
    | GotCreateBrandedDomainResponse (Result Http.Error String)


tabName : String
tabName =
    "brandeddomain"


init : ( Model, Cmd Msg )
init =
    let
        model =
            { sBrandedDomains = Loading
            }
    in
    ( model, getBrandedDomainsCmd )


getBrandedDomainsCmd : Cmd Msg
getBrandedDomainsCmd =
    Http.get
        { url = "/adminonly/brandeddomainslist"
        , expect = Http.expectJson GotBrandedDomainList brandedDomainListDecoder
        }


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals msg model =
    case msg of
        GotBrandedDomainList result ->
            case result of
                Ok brandedDomainList ->
                    ( { model | sBrandedDomains = Success brandedDomainList }, Cmd.none )

                Err _ ->
                    ( { model | sBrandedDomains = Failure }, Cmd.none )

        TableRowClicked brandedDomainID ->
            ( model
            , outerCmd <| globals.gotoBrandedDomain brandedDomainID
            )

        CreateBrandedDomainClicked ->
            ( model
            , innerCmd <|
                Http.post
                    { url = "/adminonly/brandeddomain/create"
                    , body = formBody globals []
                    , expect =
                        Http.expectJson GotCreateBrandedDomainResponse
                            (JD.field "id" JD.string)
                    }
            )

        GotCreateBrandedDomainResponse response ->
            case response of
                Ok bdID ->
                    ( model
                    , outerCmd <|
                        Cmd.batch
                            [ globals.flashMessage <| FlashMessage.success "New Branded domain created."
                            , globals.gotoBrandedDomain bdID
                            ]
                    )

                Err _ ->
                    ( model
                    , outerCmd <| globals.flashMessage <| FlashMessage.error "Cannot create Branded domain."
                    )


view : Model -> Html Msg
view model =
    div []
        [ Grid.row []
            [ Grid.col []
                [ Button.button [ Button.success, Button.attrs [ onClick CreateBrandedDomainClicked ] ]
                    [ text "Create branded domain"
                    ]
                ]
            ]
        , div [ class "mt-3", class "container-fluid" ]
            [ case model.sBrandedDomains of
                Failure ->
                    text "Failure"

                Loading ->
                    text "Loading"

                Success brandedDomains ->
                    brandedDomains
                        |> L.filter (not << .mainDomain)
                        |> viewBrandedDomains model
            ]
        ]


viewBrandedDomains : Model -> List BrandedDomain -> Html Msg
viewBrandedDomains model brandedDomains =
    let
        th colClass =
            Table.th [ Table.cellAttr <| class colClass ]
    in
    Table.table
        { options = [ Table.striped, Table.hover, Table.small ]
        , thead =
            Table.simpleThead
                [ th "col-1" []
                , th "col-5" [ text "URL" ]
                , th "col-5" [ text "Email originator" ]
                , th "col-1" []
                ]
        , tbody =
            Table.tbody [] <|
                L.map (viewBrandedDomain model) brandedDomains
        }


viewBrandedDomain : Model -> BrandedDomain -> Table.Row Msg
viewBrandedDomain _ bd =
    Table.tr
        [ Table.rowAttr <| onClick <| TableRowClicked bd.id
        , Table.rowAttr <| class "clickable-row"
        ]
        [ Table.td [] [ text bd.id ]
        , Table.td [] [ text bd.url ]
        , Table.td [] [ text bd.emailOriginator ]
        , Table.td []
            [ div
                [ style "max-width" "25px"
                , style "max-width" "50px"
                , style "background-image" ("url('" ++ bd.favicon ++ "')")
                ]
                []
            ]
        ]
