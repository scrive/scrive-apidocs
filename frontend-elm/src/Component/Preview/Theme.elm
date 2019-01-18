module Component.Preview.Theme exposing (Config, Init, Msg(..), PreviewHandler, State, UpdateHandler, ViewHandler, initialize, tabConfig, update, view)

import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Component.Theme.Data exposing (Theme)
import Either exposing (Either(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Extra as List


type alias PreviewHandler =
    Theme -> Html Never


type alias State =
    { tabState : Tab.State
    , previewHandlers : List ( String, PreviewHandler )
    }


type Msg
    = UpdateTabStateMsg Tab.State


type alias Config =
    { previewHandlers : List ( String, PreviewHandler )
    }


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    Theme -> State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


tabConfig : Tab.Config Msg
tabConfig =
    Tab.config UpdateTabStateMsg
        |> Tab.useHash False


view : ViewHandler
view theme state =
    let
        viewTab :
            Int
            -> ( String, PreviewHandler )
            -> Tab.Item Msg
        viewTab i ( name, preview ) =
            Tab.item
                { id = name ++ "-" ++ String.fromInt i
                , link = Tab.link [] [ text name ]
                , pane =
                    Tab.pane
                        [ Spacing.mt3 ]
                        [ Html.map never <| preview theme
                        ]
                }

        tabItems =
            List.indexedMap viewTab state.previewHandlers

        body =
            div
                [ class "scrive" ]
                [ tabConfig
                    |> Tab.items tabItems
                    |> Tab.view state.tabState
                ]
    in
    body


update : UpdateHandler
update (UpdateTabStateMsg state1) state2 =
    let
        state3 =
            { state2 | tabState = state1 }
    in
    ( state3, Cmd.none )


initialize : Init
initialize config =
    let
        state =
            { previewHandlers = config.previewHandlers
            , tabState = Tab.initialState
            }
    in
    ( state, Cmd.none )
