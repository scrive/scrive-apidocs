module Component.Theme.Single.ColorPair.Internal exposing (ColorHandler, Config, Msg, State, UpdateHandler, ViewHandler, backgroundColorState, initialize, stateToColors, textColorState, update, view)

import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Color exposing (Color)
import Component.Color.Color as Color
import Component.Theme.Data exposing (ColorPair)
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Tuple exposing (first, second)
import Vendor.ColorPickerExtra as ColorPicker


type alias State =
    { title : String
    , description : String
    , colorStates : Pair.State Color.State Color.State
    }


type alias Msg =
    Pair.Msg Color.Msg Color.Msg


type alias Config =
    { id : String
    , title : String
    , description : String
    , initialBackgroundColor : Color
    , initialTextColor : Color
    }


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> Html Msg


backgroundColorState : State -> Color.State
backgroundColorState state =
    first <| state.colorStates


textColorState : State -> Color.State
textColorState state =
    second <| state.colorStates


stateToColors : State -> ColorPair
stateToColors state =
    let
        backgroundColor =
            (backgroundColorState state).color

        textColor =
            (textColorState state).color
    in
    { backgroundColor = backgroundColor
    , textColor = textColor
    }


type alias ColorHandler =
    { update : Color.Msg -> Color.State -> ( Color.State, Cmd Color.Msg )
    , view : Color.State -> Html Color.Msg
    }


initialize : Config -> ( State, Cmd Msg )
initialize =
    let
        initColors =
            Pair.liftInit
                Color.initialize
                Color.initialize
    in
    \config1 ->
        let
            ( colorStates, cmd1 ) =
                initColors
                    ( { id = config1.id ++ "-background"
                      , initialColor = config1.initialBackgroundColor
                      }
                    , { id = config1.id ++ "-text"
                      , initialColor = config1.initialTextColor
                      }
                    )

            state1 : State
            state1 =
                { title = config1.title
                , description = config1.description
                , colorStates = colorStates
                }
        in
        ( state1, cmd1 )


update : UpdateHandler
update =
    let
        updateColors =
            Pair.liftUpdate Color.update Color.update
    in
    \msg1 state1 ->
        let
            ( state2, cmd1 ) =
                updateColors msg1 state1.colorStates

            state3 =
                { state1 | colorStates = state2 }
        in
        ( state3, cmd1 )


view : ViewHandler
view =
    let
        viewColors =
            Pair.liftView Color.view Color.view
    in
    \state1 ->
        let
            ( backgroundBody, textBody ) =
                viewColors state1.colorStates

            backgroundState =
                backgroundColorState state1

            textState =
                textColorState state1

            backgroundColor =
                ColorPicker.color2Hex backgroundState.color

            textColor =
                ColorPicker.color2Hex textState.color

            title =
                state1.title

            description =
                state1.description

            formBody =
                [ div []
                    [ backgroundBody
                    , textBody
                    ]
                , div
                    [ style "background-color" backgroundColor
                    , style "color" textColor
                    , class "p-sm-2"
                    , class "mt-sm-2"
                    ]
                    [ text description ]
                ]
        in
        Form.row
            []
            [ Form.colLabel
                [ Col.sm3, Col.md3, Col.lg3 ]
                [ text title ]
            , Form.col
                [ Col.sm9, Col.md9, Col.lg9 ]
                formBody
            ]
