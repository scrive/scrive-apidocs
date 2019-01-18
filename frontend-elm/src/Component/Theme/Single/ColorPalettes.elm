module Component.Theme.Single.ColorPalettes exposing (Config, Init, Msg, State, UpdateHandler, ViewHandler, initialize, stateToThemeColors, update, view)

import Component.Theme.Data exposing (ThemeColors)
import Component.Theme.Single.ColorPair as ColorPair
import Compose.Handler as Handler
import Compose.Pair as PairHandler
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    ThemeColors


type alias Msg =
    PairHandler.Msg ColorPair.Msg (PairHandler.Msg ColorPair.Msg (PairHandler.Msg ColorPair.Msg (PairHandler.Msg ColorPair.Msg ColorPair.Msg)))


type alias State =
    PairHandler.State ColorPair.State (PairHandler.State ColorPair.State (PairHandler.State ColorPair.State (PairHandler.State ColorPair.State ColorPair.State)))


type alias Init =
    Config -> ( State, Cmd Msg )


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> List (Html Msg)


initialize : Init
initialize =
    let
        inInit =
            PairHandler.liftInit
                ColorPair.initialize
            <|
                PairHandler.liftInit
                    ColorPair.initialize
                <|
                    PairHandler.liftInit
                        ColorPair.initialize
                    <|
                        PairHandler.liftInit
                            ColorPair.initialize
                            ColorPair.initialize
    in
    \theme ->
        let
            brandConfig =
                { id = "theme-color-main"
                , initialBackgroundColor =
                    theme.brandColors.backgroundColor
                , initialTextColor =
                    theme.brandColors.textColor
                , title = "Main color"
                , description =
                    "The colour that will appear on larger areas such as the background of the headers. "
                        ++ "This colour will also be the background behind your logo."
                }

            actionConfig =
                { id = "theme-color-primary-actions"
                , initialBackgroundColor =
                    theme.actionColors.backgroundColor
                , initialTextColor =
                    theme.actionColors.textColor
                , title = "Primary actions"
                , description =
                    "Action buttons, arrows, mandatory actions and elements that require attention. "
                        ++ "We recommend your primary brand colour or a colour that stands out."
                }

            secondaryActionConfig =
                { id = "theme-color-secondary-actions"
                , initialBackgroundColor =
                    theme.secondaryActionColors.backgroundColor
                , initialTextColor =
                    theme.secondaryActionColors.textColor
                , title = "Secondary actions"
                , description =
                    "Optional actions and links. We recommend a lighter colour, "
                        ++ "one that compliments your primary action colour."
                }

            positiveConfig =
                { id = "theme-color-positive"
                , initialBackgroundColor =
                    theme.positiveColors.backgroundColor
                , initialTextColor =
                    theme.positiveColors.textColor
                , title = "Positive"
                , description =
                    "This colour will appear on notification such as \"Saved\" and \"Success\". "
                        ++ "We recommend an intuitively positive colour like green."
                }

            negativeConfig =
                { id = "theme-color-negative"
                , initialBackgroundColor =
                    theme.negativeColors.backgroundColor
                , initialTextColor =
                    theme.negativeColors.textColor
                , title = "Negative"
                , description =
                    "This colour will appear on notifications such as \"Error\". "
                        ++ "We recommend an intuitively negative colour such as red."
                }

            inConfig =
                ( brandConfig
                , ( actionConfig
                  , ( secondaryActionConfig
                    , ( positiveConfig
                      , negativeConfig
                      )
                    )
                  )
                )
        in
        inInit inConfig


update : UpdateHandler
update =
    PairHandler.liftUpdate
        ColorPair.update
    <|
        PairHandler.liftUpdate
            ColorPair.update
        <|
            PairHandler.liftUpdate
                ColorPair.update
            <|
                PairHandler.liftUpdate
                    ColorPair.update
                    ColorPair.update


view : ViewHandler
view =
    PairHandler.consView
        ColorPair.view
    <|
        PairHandler.consView
            ColorPair.view
        <|
            PairHandler.consView
                ColorPair.view
            <|
                PairHandler.consView
                    ColorPair.view
                <|
                    Handler.toListViewHandler ColorPair.view


stateToThemeColors : State -> ThemeColors
stateToThemeColors state =
    let
        ( brandState, ( actionState, ( secondaryActionState, ( positiveState, negativeState ) ) ) ) =
            state
    in
    { brandColors = ColorPair.stateToColors brandState
    , actionColors = ColorPair.stateToColors actionState
    , secondaryActionColors = ColorPair.stateToColors secondaryActionState
    , positiveColors = ColorPair.stateToColors positiveState
    , negativeColors = ColorPair.stateToColors negativeState
    }
