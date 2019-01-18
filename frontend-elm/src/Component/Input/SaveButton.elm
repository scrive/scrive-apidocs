module Component.Input.SaveButton exposing (Config, Init, Msg(..), OutMsg(..), State, UpdateHandler, ViewHandler, dataSavedMsg, initialize, update, view)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)


type OutMsg data
    = SaveMsg data


type alias Config =
    String


type Msg data
    = DataSavedMsg
    | SaveButtonClickedMsg data


type alias State =
    { isDisabled : Bool
    , caption : String
    }


type alias UpdateHandler data =
    Msg data
    -> State
    -> ( State, Cmd (Either (OutMsg data) (Msg data)) )


type alias ViewHandler data =
    State -> Html (Msg data)


type alias Init data =
    Config -> ( State, Cmd (Msg data) )


initialize : Init data
initialize caption =
    ( { isDisabled = False, caption = caption }, Cmd.none )


update : UpdateHandler data
update msg1 state1 =
    case msg1 of
        DataSavedMsg ->
            ( { state1 | isDisabled = False }
            , Cmd.none
            )

        SaveButtonClickedMsg fields ->
            let
                cmd =
                    Util.msgToCmd <|
                        Left <|
                            SaveMsg fields
            in
            ( { state1 | isDisabled = True }
            , cmd
            )


view : data -> ViewHandler data
view data state =
    Grid.row [ Row.leftSm, Row.attrs <| [ class "mb-sm-2", class "mt-sm-2" ] ]
        [ Grid.col [ Col.sm12 ]
            [ Button.button
                [ Button.success
                , Button.attrs
                    [ class "ml-sm-2"
                    , onClick <|
                        SaveButtonClickedMsg data
                    , disabled state.isDisabled
                    ]
                ]
                [ text state.caption ]
            ]
        ]


dataSavedMsg : Msg data
dataSavedMsg =
    DataSavedMsg
