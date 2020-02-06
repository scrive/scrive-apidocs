module AdminOnly.BrandedDomain.BrandedDomain exposing
    ( Model
    , Msg
    , Page
    , fromPage
    , init
    , pageFromModel
    , routeParser
    , update
    , updatePage
    , view
    )

import Component.BrandedDomain.Page as Page
import Component.Global.Msg as Global
import Either exposing (Either(..))
import Html
import Url.Parser as UP exposing ((<?>), Parser)
import Url.Parser.Query as UPQ
import Utils
    exposing
        ( Action
        , Globals
        , PageUrl
        , Render
        )


type alias Page =
    { bdID : String
    }


type alias Model =
    { page : Page
    , pageState : Page.State
    }


type Msg
    = PageMsg Page.Msg
    | PageOutMsg Page.OutMsg


init : String -> Page -> ( Model, Cmd Msg )
init xtoken page =
    let
        ( pageState, cmd1 ) =
            Page.initialize
                { xtoken = xtoken
                , brandedDomainId = page.bdID
                }

        model =
            { page = page
            , pageState = pageState
            }

        ( model2, cmd2 ) =
            updatePage page model

        cmd3 =
            Cmd.batch
                [ Cmd.map PageMsg cmd1
                , cmd2
                ]
    in
    ( model2, cmd3 )


fromPage : Page -> PageUrl
fromPage _ =
    Utils.emptyPageUrl


routeParser : Parser (Page -> a) a
routeParser =
    UP.map
        (\bdid _ _ _ ->
            { bdID = bdid
            }
        )
        (UP.string
            <?> UPQ.string "settingsTab"
            <?> UPQ.string "previewTab"
            <?> UPQ.string "theme"
        )


pageFromModel : Model -> Maybe Page
pageFromModel model =
    Just model.page


mergePageMsg : Either Page.OutMsg Page.Msg -> Msg
mergePageMsg msg1 =
    case msg1 of
        Left msg2 ->
            PageOutMsg msg2

        Right msg2 ->
            PageMsg msg2


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals msg1 state1 =
    case msg1 of
        PageMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    Page.update msg2 state1.pageState

                state3 =
                    { state1
                        | pageState = state2
                    }

                cmd2 =
                    Cmd.map (Right << mergePageMsg) cmd1
            in
            ( state3, cmd2 )

        PageOutMsg msg2 ->
            ( state1
            , Cmd.map Left <|
                Global.initialToFinalGlobal globals msg2
            )


updatePage : Page -> Model -> ( Model, Cmd Msg )
updatePage page state1 =
    let
        ( state2, cmd1 ) =
            Page.update
                (Page.SetBrandedDomainId page.bdID)
                state1.pageState

        state3 =
            { state1
                | pageState = state2
            }

        cmd2 =
            Cmd.map mergePageMsg cmd1
    in
    ( state3, cmd2 )


view : Model -> Render msg Msg
view model =
    Html.map (Right << PageMsg) <|
        Page.view model.pageState
