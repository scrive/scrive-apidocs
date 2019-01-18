module Component.Theme.Single exposing (Config, Init, Msg(..), State, ThemeId, ThemeLogo, ThemeName, UpdateHandler, ViewHandler, initialize, stateToTheme, update, view, viewThemeLogo, viewThemeName)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Color exposing (Color)
import Component.Input.Icon as InputImage
import Component.Theme.Data exposing (Theme)
import Component.Theme.Single.ColorPalettes as ColorPalettes
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src, style, value)
import Html.Events exposing (onInput)
import Vendor.ColorPickerExtra exposing (color2Hex)


type alias ThemeId =
    String


type alias ThemeName =
    String


type alias ThemeLogo =
    String


type alias Config =
    Theme


type alias State =
    { themeId : ThemeId
    , themeFont : String
    , themeName : ThemeName
    , logoState : InputImage.State
    , colorPalettesState : ColorPalettes.State
    }


type Msg
    = SetThemeNameMsg ThemeName
    | ColorPalettesMsg ColorPalettes.Msg
    | LogoMsg InputImage.Msg


type alias Init =
    Config -> ( State, Cmd Msg )


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> Html Msg


initialize : Init
initialize theme =
    let
        ( colorState, colorCmd ) =
            ColorPalettes.initialize theme.themeColors

        state =
            { themeId = theme.id
            , themeFont = theme.font
            , themeName = theme.name
            , logoState = theme.logo
            , colorPalettesState = colorState
            }

        cmd =
            Cmd.batch
                [ Cmd.map ColorPalettesMsg colorCmd
                ]
    in
    ( state, cmd )


update : UpdateHandler
update msg1 state1 =
    case msg1 of
        SetThemeNameMsg themeName ->
            let
                state2 =
                    { state1 | themeName = themeName }
            in
            ( state2, Cmd.none )

        LogoMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    InputImage.update msg2 state1.logoState

                state3 =
                    { state1 | logoState = state2 }

                cmd2 =
                    Util.liftCmd LogoMsg cmd1
            in
            ( state3, cmd2 )

        ColorPalettesMsg msg2 ->
            let
                ( state2, cmd1 ) =
                    ColorPalettes.update
                        msg2
                        state1.colorPalettesState

                state3 =
                    { state1 | colorPalettesState = state2 }

                cmd2 =
                    Util.liftCmd ColorPalettesMsg cmd1
            in
            ( state3, cmd2 )


viewThemeLogo : ThemeLogo -> Color -> Html Msg
viewThemeLogo logo brandBackgroundColor =
    Form.row
        []
        [ Form.colLabel
            [ Col.sm3, Col.md3, Col.lg3 ]
            [ text "Logo" ]
        , Form.col
            [ Col.sm9, Col.md9, Col.lg9 ]
            [ Html.map LogoMsg <| InputImage.view logo
            , div
                [ class "mt-sm-2"
                , style "background-color" <|
                    color2Hex brandBackgroundColor
                ]
                [ img
                    [ class "p-sm-2"
                    , style "max-width" "100%"
                    , style "max-height" "100px"
                    , src logo
                    ]
                    []
                ]
            ]
        ]


viewThemeName : ThemeName -> Html Msg
viewThemeName themeName =
    Form.row []
        [ Form.colLabel
            [ Col.sm3, Col.md3, Col.lg3 ]
            [ text "Name of theme" ]
        , Form.col
            [ Col.sm9, Col.md9, Col.lg9 ]
            [ Input.text
                [ Input.attrs
                    [ value themeName
                    , onInput <|
                        SetThemeNameMsg
                    ]
                ]
            ]
        ]


view : ViewHandler
view state =
    let
        themeColors =
            ColorPalettes.stateToThemeColors state.colorPalettesState

        brandBackgroundColor =
            themeColors.brandColors.backgroundColor

        colorBody =
            ColorPalettes.view
                state.colorPalettesState

        themeNameBody =
            viewThemeName state.themeName

        logoBody =
            viewThemeLogo state.logoState brandBackgroundColor

        body1 =
            div [] <|
                [ themeNameBody
                , logoBody
                ]
                    ++ List.map (Html.map ColorPalettesMsg) colorBody
    in
    body1


stateToTheme : State -> Theme
stateToTheme state1 =
    let
        themeColors =
            ColorPalettes.stateToThemeColors
                state1.colorPalettesState

        theme =
            { id = state1.themeId
            , name = state1.themeName
            , logo = state1.logoState
            , font = state1.themeFont
            , themeColors = themeColors
            }
    in
    theme
