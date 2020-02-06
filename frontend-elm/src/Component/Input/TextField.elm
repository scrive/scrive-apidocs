module Component.Input.TextField exposing (Config, Init, Msg(..), State, UpdateHandler, ViewHandler, fieldValue, initialize, update, view)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Either exposing (Either(..))
import Html exposing (Html, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


type alias Config =
    State


type alias State =
    { title : String
    , description : String
    , value : String
    }


type Msg
    = UpdateValueMsg String


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
update (UpdateValueMsg value1) state1 =
    let
        state2 =
            { state1 | value = value1 }
    in
    ( state2, Cmd.none )


view : ViewHandler
view state =
    Form.row
        []
        [ Form.colLabel
            [ Col.sm4, Col.md4, Col.lg4 ]
            [ text state.title ]
        , Form.col
            [ Col.sm8, Col.md8, Col.lg8 ]
            [ Input.text
                [ Input.attrs <|
                    [ value <| state.value
                    , onInput UpdateValueMsg
                    ]
                ]
            , Form.help
                []
                [ text state.description ]
            ]
        ]


fieldValue : State -> String
fieldValue =
    .value
