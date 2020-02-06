module Component.Color.Color exposing (Config, Msg(..), State, UpdateHandler, ViewHandler, initialize, popoverIsActive, update, view)

import Bootstrap.Form.Input as Input
import Color exposing (Color)
import Either exposing (Either(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style, value)
import Vendor.ColorPickerExtra as ColorPicker
import Vendor.Popover as Popover


type alias State =
    { popoverState : Popover.State
    , colorPickerState : ColorPicker.State
    , color : Color
    }


type Msg
    = ColorPickerMsg ColorPicker.Msg
    | PopoverStateUpdateMsg Popover.State


type alias Config =
    { id : String
    , initialColor : Color
    }


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd Msg )


type alias ViewHandler =
    State -> Html Msg


initialize : Config -> ( State, Cmd Msg )
initialize config =
    let
        state =
            { popoverState = Popover.initialState
            , colorPickerState = ColorPicker.initWithId config.id
            , color = config.initialColor
            }
    in
    ( state, Cmd.none )


update : Msg -> State -> ( State, Cmd (Either Never Msg) )
update msg state1 =
    case msg of
        ColorPickerMsg msg2 ->
            let
                color1 =
                    state1.color

                ( state2, mColor ) =
                    ColorPicker.update msg2 color1 state1.colorPickerState

                color2 =
                    Maybe.withDefault color1 mColor

                state3 =
                    { state1 | colorPickerState = state2, color = color2 }
            in
            ( state3, Cmd.none )

        PopoverStateUpdateMsg state2 ->
            let
                state3 =
                    { state1 | popoverState = state2 }
            in
            ( state3, Cmd.none )


popoverIsActive : Popover.State -> Bool
popoverIsActive (Popover.State rawState) =
    rawState.isActive


view : State -> Html Msg
view state =
    let
        color =
            state.color

        popoverState =
            state.popoverState

        colorPickerState =
            state.colorPickerState

        popoverClickHandler =
            Popover.onClick
                popoverState
                PopoverStateUpdateMsg

        overlay =
            if popoverIsActive popoverState then
                [ div
                    (class "color-picker-overlay"
                        :: popoverClickHandler
                    )
                    []
                ]

            else
                []

        body =
            div [] <|
                [ div [] <|
                    [ Input.text
                        [ Input.attrs <|
                            [ value <| ColorPicker.color2Hex color
                            , style "width" "80%"
                            , style "display" "inline-block"
                            ]
                                ++ popoverClickHandler
                        ]
                    , button
                        ([ style "background-color" <| ColorPicker.color2Hex color
                         , class "color-display"
                         ]
                            ++ popoverClickHandler
                        )
                        []
                    ]
                ]
                    ++ overlay

        content =
            Html.map ColorPickerMsg <|
                ColorPicker.view
                    color
                    colorPickerState

        config =
            Popover.config body
                |> Popover.right
                |> Popover.content [] [ content ]
    in
    div []
        [ Popover.view popoverState config
        ]
