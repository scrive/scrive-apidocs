module Component.Branding.Settings exposing (Msg, State, initTab, selectTabMsg, tabConfig, updateTabState, viewBranding, viewTab)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type alias Msg =
    Tab.State


type alias State =
    Tab.State


initTab : () -> ( State, Cmd msg )
initTab () =
    ( Tab.initialState, Cmd.none )


updateTabState : State -> State -> ( State, Cmd msg )
updateTabState state _ =
    ( state, Cmd.none )


tabConfig : Tab.Config (Pair.Msg msg Msg)
tabConfig =
    Tab.config Pair.SecondMsg
        |> Tab.useHash False


viewTab :
    Int
    -> ( String, Html Never )
    -> Tab.Item msg
viewTab i ( name, body ) =
    Tab.item
        { id = String.fromInt i
        , link = Tab.link [] [ text name ]
        , pane =
            Tab.pane
                [ Spacing.mt3 ]
                [ Html.map never body
                ]
        }


viewBranding :
    State
    -> Html msg
    -> List ( String, Html Never )
    -> Html (Pair.Msg msg Msg)
viewBranding tabState editBody previewTabs1 =
    let
        previewTabs2 =
            List.indexedMap viewTab previewTabs1

        previewBody =
            div
                [ class "scrive" ]
                [ tabConfig
                    |> Tab.items previewTabs2
                    |> Tab.view tabState
                ]

        body3 =
            Grid.row []
                [ Grid.col
                    [ Col.sm12, Col.md5 ]
                    [ Html.map Pair.FirstMsg editBody
                    ]
                , Grid.col
                    [ Col.sm12, Col.md7 ]
                    [ previewBody
                    ]
                ]
    in
    body3


selectTabMsg : Int -> Msg
selectTabMsg =
    Tab.customInitialState << String.fromInt
