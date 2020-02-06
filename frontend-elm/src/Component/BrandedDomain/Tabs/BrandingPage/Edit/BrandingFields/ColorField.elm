module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ColorField exposing (Config, Init, Msg, State, UpdateHandler, ViewHandler, initialize, stateToColor, update, view)

import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Color exposing (Color)
import Component.Color.Color as Color
import Either exposing (Either(..))
import Html exposing (Html, text)


type alias Config =
    { id : String
    , title : String
    , initialColor : Color
    }


type alias State =
    { title : String
    , colorState : Color.State
    }


type alias Msg =
    Color.Msg


type alias Init =
    Config -> ( State, Cmd Msg )


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> Html Msg


initialize : Init
initialize config =
    let
        ( colorState, cmd ) =
            Color.initialize
                { id = config.id
                , initialColor = config.initialColor
                }

        state =
            { title = config.title
            , colorState = colorState
            }
    in
    ( state, cmd )


update : UpdateHandler
update msg state1 =
    let
        ( state2, cmd ) =
            Color.update msg state1.colorState

        state3 =
            { state1 | colorState = state2 }
    in
    ( state3, cmd )


view : ViewHandler
view state =
    let
        colorBody =
            Color.view state.colorState

        body =
            Form.row
                []
                [ Form.colLabel
                    [ Col.sm4, Col.md4, Col.lg4 ]
                    [ text state.title ]
                , Form.col
                    [ Col.sm8, Col.md8, Col.lg8 ]
                    [ colorBody
                    ]
                ]
    in
    body


stateToColor : State -> Color
stateToColor state =
    state.colorState.color
