module Component.Input.Select exposing (Config, Init, Msg(..), State, UpdateHandler, ViewHandler, getSelected, initialize, update, view)

import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Either exposing (Either(..))
import Html exposing (Html, text)
import Html.Attributes exposing (selected, value)
import List.Extra as List


type Msg
    = SelectMsg (Maybe Int)


type alias State =
    { fieldLabel : String
    , selectedIndex : Maybe Int
    }


type alias Config =
    State


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize state =
    ( state, Cmd.none )


update : UpdateHandler
update (SelectMsg i) state1 =
    let
        state2 =
            { state1 | selectedIndex = i }
    in
    ( state2, Cmd.none )


view : List String -> ViewHandler
view selections state =
    let
        selectedIndex =
            state.selectedIndex

        viewSelect i selection =
            Select.item
                [ value <| String.fromInt i
                , selected <| Just i == selectedIndex
                ]
                [ text selection
                ]

        body1 =
            List.indexedMap viewSelect selections

        body2 =
            Form.row
                []
                [ Form.colLabel
                    [ Col.sm4, Col.md4, Col.lg4 ]
                    [ text state.fieldLabel ]
                , Form.col
                    [ Col.sm8, Col.md8, Col.lg8 ]
                    [ Select.select
                        [ Select.onChange (SelectMsg << String.toInt)
                        ]
                        body1
                    ]
                ]
    in
    body2


getSelected : List a -> State -> Maybe a
getSelected items state =
    Maybe.andThen
        (\i -> List.getAt i items)
        state.selectedIndex
