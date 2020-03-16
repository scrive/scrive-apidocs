module Main exposing (main)

--import Debug

import AdminOnly.AdminOnly as AdminOnly
import AdminOnly.BrandedDomain.BrandedDomainsTab as BrandedDomainsTab
import AdminOnly.UserAdminTab.UserAdminTab as UserAdminTab
import AdminOnly.UserGroupAdmin.UserGroupAdminTab as UserGroupAdminTab
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Cookie
import Dict exposing (Dict)
import FlashMessage
import Html exposing (Html, br, div, footer, h1, header, img, text)
import Html.Attributes exposing (class, href, src, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline as JDP
import List as L
import Maybe as M
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>))
import Util.Http as Util
import Utils exposing (..)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Page
    = AdminOnly AdminOnly.Page
    | NotFound


type alias Flags =
    { cookies : Dict String String
    , cdnBaseUrl : String
    }


type alias Model =
    { page : Page
    , url : Url
    , mAdminOnly : Maybe AdminOnly.Model
    , navbarState : Navbar.State
    , navKey : Navigation.Key
    , flashMessage : FlashMessage.Model Msg
    , globals : Globals Msg
    , state : State
    }


type State
    = OkState
    | ErrorState String


type Msg
    = UrlChanged Url
    | ClickedLink UrlRequest
    | AdminOnlyMsg AdminOnly.Msg
    | NavbarMsg Navbar.State
    | AddFlashMessage FlashMessage.FlashMessage
    | FlashMessageMsg FlashMessage.Msg
    | SetPageUrl PageUrl
    | SetPageUrlFromModel
    | Logout
    | OnLoggedOut (Result Http.Error ())


init : Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flagsValue url0 navKey =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        flagsResult : Result JD.Error Flags
        flagsResult =
            JD.decodeValue decodeFlags flagsValue

        globals0 =
            { xtoken = ""
            , cdnBaseUrl = ""
            , flashMessage = perform << AddFlashMessage
            , setPageUrlFromModel = perform SetPageUrlFromModel
            , gotoUserGroupUsers =
                \ugid ->
                    perform <|
                        SetPageUrl <|
                            { path = [ "adminonly", "page", "companyadmin", ugid ]
                            , query = []
                            , fragment = Just "users"
                            }
            , gotoDaveDocument =
                \documentID ->
                    perform <|
                        SetPageUrl <|
                            { path = [ "dave", "document", documentID ]
                            , query = []
                            , fragment = Nothing
                            }
            , gotoUser =
                \uid ->
                    perform <|
                        SetPageUrl <|
                            { path = [ "adminonly", "page", "useradmin", uid ]
                            , query = []
                            , fragment = Nothing
                            }
            , gotoUserGroup =
                \userGroupID ->
                    perform <|
                        SetPageUrl <|
                            { path = [ "adminonly", "page", "companyadmin", userGroupID ]
                            , query = []
                            , fragment = Nothing
                            }
            , gotoUserGroupAdminTab =
                perform <|
                    SetPageUrl <|
                        { path = [ "adminonly", "page", UserGroupAdminTab.tabName ]
                        , query = []
                        , fragment = Nothing
                        }
            , gotoUserAdminTab =
                perform <|
                    SetPageUrl <|
                        { path = [ "adminonly", "page", UserAdminTab.tabName ]
                        , query = []
                        , fragment = Nothing
                        }
            , gotoBrandedDomain =
                \brandedDomainID ->
                    perform <|
                        SetPageUrl <|
                            { path = [ "adminonly", "page", "brandeddomain", brandedDomainID ]
                            , query = []
                            , fragment = Nothing
                            }
            , gotoBrandedDomainsTab =
                perform <|
                    SetPageUrl <|
                        { path = [ "adminonly", "page", BrandedDomainsTab.tabName ]
                        , query = []
                        , fragment = Nothing
                        }
            }

        ( state, globals ) =
            case flagsResult of
                Ok flags ->
                    let
                        globals1 =
                            { globals0
                                | cdnBaseUrl = flags.cdnBaseUrl
                            }
                    in
                    case Dict.get "xtoken" flags.cookies of
                        Nothing ->
                            ( ErrorState "no xtoken in cookies", globals1 )

                        Just xtoken ->
                            ( OkState
                            , { globals1
                                | xtoken = xtoken
                              }
                            )

                Err _ ->
                    ( ErrorState "cannot parse cookies", globals0 )

        ( flashModel, flashCmd ) =
            FlashMessage.init FlashMessageMsg

        model =
            { navbarState = navbarState
            , state = state
            , navKey = navKey
            , page = NotFound
            , url = url
            , flashMessage = flashModel
            , mAdminOnly = Nothing
            , globals = globals
            }

        ( url, page ) =
            decode url0
                |> M.map (\p -> ( replacePageUrl url0 <| fromPage p, p ))
                |> M.withDefault ( url0, NotFound )

        ( mAdminOnly, adminOnlyCmd ) =
            case page of
                AdminOnly adminOnlyPage ->
                    AdminOnly.init AdminOnlyMsg globals adminOnlyPage
                        |> (\( ao, aoCmd ) -> ( Just ao, aoCmd ))

                NotFound ->
                    ( Nothing, Cmd.none )
    in
    ( { model | mAdminOnly = mAdminOnly, page = page }
    , Cmd.batch
        [ navbarCmd
        , flashCmd
        , adminOnlyCmd
        ]
    )


decodeFlags : Decoder Flags
decodeFlags =
    JD.succeed Flags
        |> JDP.required "cookie" (JD.string |> JD.andThen Cookie.decoder)
        |> JDP.required "cdnBaseUrl" JD.string


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just page ->
            let
                newUrl =
                    replacePageUrl url <| fromPage page
            in
            case page of
                AdminOnly adminOnlyPage ->
                    let
                        ( adminOnly, adminOnlyCmd ) =
                            case model.mAdminOnly of
                                Nothing ->
                                    AdminOnly.init
                                        AdminOnlyMsg
                                        model.globals
                                        adminOnlyPage

                                Just adminOnly0 ->
                                    AdminOnly.updatePage
                                        AdminOnlyMsg
                                        model.globals
                                        adminOnlyPage
                                        adminOnly0
                    in
                    ( { model | mAdminOnly = Just adminOnly, url = newUrl }
                    , adminOnlyCmd
                    )

                _ ->
                    ( model, Cmd.none )


decode : Url -> Maybe Page
decode url =
    UP.parse routeParser url


routeParser : UP.Parser (Page -> a) a
routeParser =
    UP.map AdminOnly
        (UP.s "adminonly"
            </> AdminOnly.routeParser
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , FlashMessage.subscriptions model.flashMessage
        ]


fromPage : Page -> PageUrl
fromPage page =
    case page of
        NotFound ->
            { path = [], fragment = Nothing, query = [] }

        AdminOnly adminOnlyPage ->
            let
                adminOnlyPagePath =
                    AdminOnly.fromPage adminOnlyPage
            in
            { adminOnlyPagePath
                | path =
                    [ "adminonly", "page" ] ++ adminOnlyPagePath.path
            }


replacePageUrl : Url -> PageUrl -> Url
replacePageUrl url pageUrl =
    let
        query =
            if L.isEmpty pageUrl.query then
                Nothing

            else
                Just <| String.dropLeft 1 <| UB.toQuery pageUrl.query

        path =
            "/" ++ String.join "/" pageUrl.path
    in
    { url | query = query, path = path, fragment = pageUrl.fragment }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    case decode url of
                        Just page ->
                            let
                                newUrl =
                                    replacePageUrl url <| fromPage page
                            in
                            ( { model | url = newUrl }
                            , Navigation.pushUrl model.navKey <| Url.toString <| newUrl
                            )

                        Nothing ->
                            ( model, Navigation.load <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            urlUpdate url model

        NavbarMsg state ->
            ( { model | navbarState = state }
            , Cmd.none
            )

        AdminOnlyMsg adminOnlyMsg ->
            let updateAdminOnly = AdminOnly.update AdminOnlyMsg model.globals adminOnlyMsg
                (newAdminOnly, cmd) = maybeUpdate updateAdminOnly model.mAdminOnly
            in ({ model | mAdminOnly = newAdminOnly}, cmd)

        AddFlashMessage flash ->
            ( { model | flashMessage = FlashMessage.addFlashMessage flash model.flashMessage }
            , Cmd.none
            )

        FlashMessageMsg flashMessageMsg ->
            let
                ( flashModel, flashCmd ) =
                    FlashMessage.update flashMessageMsg model.flashMessage
            in
            ( { model | flashMessage = flashModel }, flashCmd )

        SetPageUrl pageUrl ->
            let
                newUrl =
                    Url.toString <| replacePageUrl model.url pageUrl

                urlIsInternal =
                    L.head pageUrl.path == Just "adminonly"
            in
            if urlIsInternal then
                ( model, Navigation.pushUrl model.navKey newUrl )

            else
                ( model, Navigation.load newUrl )

        SetPageUrlFromModel ->
            let
                newUrl =
                    Url.toString <| replacePageUrl model.url <| fromPage <| pageFromModel model
            in
            ( model, Navigation.pushUrl model.navKey newUrl )

        Logout ->
            ( model, logoutCmd )

        OnLoggedOut res ->
            case res of
                Ok () ->
                    ( model, Navigation.load "/" )

                Err err ->
                    let
                        cmd =
                            model.globals.flashMessage <|
                                FlashMessage.error <|
                                    "Error logging out: "
                                        ++ Util.httpErrorToString err
                    in
                    ( model, cmd )


pageFromModel : Model -> Page
pageFromModel model =
    case model.page of
        -- the withDefault is just workaround, because the synchronization of Page
        -- and Model is not guaranteed by types
        -- In practice the withDefault shall never be used.
        AdminOnly _ ->
            model.mAdminOnly
                |> M.andThen AdminOnly.pageFromModel
                |> M.map AdminOnly
                |> M.withDefault NotFound

        NotFound ->
            NotFound


view : Model -> Browser.Document Msg
view model =
    { title = "Scrive AdminOnly"
    , body =
        [ div [ class "main-container d-flex flex-column" ]
            [ menu model
            , mainContent model
            , footerContent model
            ]
        , FlashMessage.view model.flashMessage
        ]
    }


menu : Model -> Html Msg
menu model =
    let
        cdnBaseUrl =
            model.globals.cdnBaseUrl
    in
    header [ class "bg-dark" ]
        [ Navbar.config NavbarMsg
            |> Navbar.dark
            |> Navbar.withAnimation
            |> Navbar.container
            |> Navbar.brand [ href "#" ]
                [ img
                    [ src <| cdnBaseUrl ++ "/adminonly-assets/images/scrive_logo.png"
                    , width 120
                    ]
                    []
                ]
            |> Navbar.items
                [ itemLinkButton [ href "/newdocument" ] [ text "Start new process" ]
                , itemLinkButton [ href "/fromtemplate" ] [ text "Start from template" ]
                , Navbar.itemLink [ href "/d" ] [ text "E-Archive" ]
                , Navbar.itemLink [ href "/account" ] [ text "Account" ]
                , Navbar.itemLink [ href "/adminonly-old" ] [ text "Old AdminOnly" ]
                , Navbar.itemLink
                    [ href "#", onClick Logout ]
                    [ text "Log out" ]
                ]
            |> Navbar.view model.navbarState
        ]


itemLinkButton : List (Html.Attribute msg) -> List (Html.Html msg) -> Navbar.Item msg
itemLinkButton attributes =
    Navbar.itemLink (class "btn btn-outline-secondary mx-2" :: attributes)


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [ class "pb-5" ] <|
        case model.page of
            AdminOnly _ ->
                [ model.mAdminOnly
                    |> M.map (AdminOnly.view AdminOnlyMsg)
                    |> M.withDefault viewError
                ]

            NotFound ->
                pageNotFound


footerContent : Model -> Html Msg
footerContent model =
    let
        cdnBaseUrl =
            model.globals.cdnBaseUrl
    in
    footer [ class "mt-auto" ]
        [ div [ class "text-center text-secondary py-3" ]
            [ text "Powered by"
            , br [] []
            , img [ src <| cdnBaseUrl ++ "/adminonly-assets/images/logo-small-grey.png", width 80 ] []
            ]
        ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]


logoutCmd : Cmd Msg
logoutCmd =
    Http.post
        { url = "/logout_ajax"
        , expect = Http.expectWhatever OnLoggedOut
        , body = Http.emptyBody
        }
