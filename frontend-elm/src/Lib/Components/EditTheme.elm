module Lib.Components.EditTheme exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Color exposing (Color)
import EnumExtra as Enum
import File exposing (File)
import File.Select as FileSelect
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, disabled, selected, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Lib.Types.Theme exposing (..)
import List exposing (map)
import List.Extra as List
import Maybe exposing (withDefault)
import Return exposing (..)
import String
import Task
import Tuple exposing (second)
import Utils exposing (..)
import Vendor.ColorPickerExtra as ColorPicker
import Vendor.Popover as Popover


type alias EditThemeState =
    { themeBeingEdited : Theme
    , colorPickers : Enum.Dict ColorIdentifier ColorPicker.State
    , partialColors : Enum.Dict ColorIdentifier String
    , popovers : Enum.Dict ColorIdentifier Popover.State
    }


getThemeColor : EditThemeState -> ColorIdentifier -> Color
getThemeColor state ident =
    withDefault (Color.rgb 0 0 0) <| Enum.get ident state.themeBeingEdited.colors


getColorPicker : EditThemeState -> ColorIdentifier -> ColorPicker.State
getColorPicker state ident =
    withDefault ColorPicker.empty <| Enum.get ident state.colorPickers


getPartialColor : EditThemeState -> ColorIdentifier -> Maybe String
getPartialColor state ident =
    Enum.get ident state.partialColors


getPopover : EditThemeState -> ColorIdentifier -> Popover.State
getPopover state ident =
    withDefault Popover.initialState <| Enum.get ident state.popovers


type alias EditThemeReadonly =
    { availableThemes : List Theme
    }


type Msg
    = SetActiveThemeMsg ThemeID
    | UpdateColorPickerMsg ColorIdentifier ColorPicker.Msg
    | SetPopoverMsg ColorIdentifier Popover.State
    | SetThemeNameMsg String
    | OpenLogoFileSelectMsg
    | LoadLogoFileMsg File
    | SetLogoMsg String
    | SetFontMsg Font
    | OnColorTextChangedMsg ColorIdentifier String
    | OnColorTextCommittedMsg ColorIdentifier


update : (Msg -> msg) -> Msg -> EditThemeReadonly -> EditThemeState -> Return msg EditThemeState
update embed msg read =
    case msg of
        SetActiveThemeMsg id ->
            setActiveTheme read id

        UpdateColorPickerMsg colorIdentifier msg_ ->
            updateColorPicker colorIdentifier msg_

        SetPopoverMsg colorIdentifier popover ->
            setPopover colorIdentifier popover

        SetThemeNameMsg name ->
            setThemeName name

        OpenLogoFileSelectMsg ->
            openLogoFileSelect embed

        LoadLogoFileMsg file ->
            loadLogoFile embed file

        SetLogoMsg content ->
            setLogo content

        SetFontMsg font ->
            setFont font

        OnColorTextChangedMsg colorIdentifier colorText ->
            updatePartialColor colorIdentifier colorText
                >> updateColorIfValidInput colorIdentifier colorText
                >> singleton

        OnColorTextCommittedMsg colorIdentifier ->
            commitPartialColor colorIdentifier >> singleton



{- Theme selector -}
-- implements SetActiveThemeMsg


setActiveTheme : EditThemeReadonly -> ThemeID -> EditThemeState -> Return msg EditThemeState
setActiveTheme read id state =
    let
        activeTheme =
            withDefault state.themeBeingEdited <|
                List.find (\theme -> theme.id == id) read.availableThemes
    in
    singleton <| { state | themeBeingEdited = activeTheme }



-- The 'theme selector' is a drop-down list of theme names to select.


viewThemeSelector : (Msg -> msg) -> EditThemeReadonly -> Theme -> Form.Col msg
viewThemeSelector embed read selectedTheme =
    let
        onSelect idString =
            SetActiveThemeMsg <| withDefault -1 (String.toInt idString)

        themeItem : Theme -> Select.Item msg
        themeItem theme =
            Select.item
                [ value <| String.fromInt theme.id
                , selected <| theme.id == selectedTheme.id
                ]
                [ text theme.name ]
    in
    Form.col
        [ Col.sm8, Col.md8, Col.lg8 ]
        [ Select.select
            [ Select.onChange (embed << onSelect) ]
            (List.map themeItem read.availableThemes)
        ]



-- implements SetFontMsg


setFont : Font -> EditThemeState -> Return msg EditThemeState
setFont font state =
    let
        theme =
            state.themeBeingEdited
    in
    singleton <| { state | themeBeingEdited = { theme | font = font } }



-- A drop-down list of font names to select and a font sample


viewFontSelector : (Msg -> msg) -> Theme -> Form.Col msg
viewFontSelector embed selectedTheme =
    let
        onSelect fontString =
            SetFontMsg <| withDefault defaultFont (Enum.fromString enumFont fontString)

        fontItem : ( Font, String ) -> Select.Item msg
        fontItem ( font, label ) =
            Select.item
                [ value <| Enum.toString enumFont font
                , selected <| font == selectedTheme.font
                ]
                [ text label ]

        fontsWithLabels =
            List.map (\f -> ( f, Enum.toHumanString enumFont f )) enumFont.allValues
                |> List.sortBy second
    in
    Form.col
        [ Col.sm8, Col.md8, Col.lg8 ]
        [ Select.select
            [ Select.onChange (embed << onSelect)
            , Select.disabled (selectedTheme.id < 0)
            ]
            (List.map fontItem fontsWithLabels)
        , div
            [ style "background-color" <| "#ffffff"
            , style "font-family" <| Enum.toString enumFont selectedTheme.font
            , class "p-sm-2"
            , class "mt-sm-2"
            ]
            [ text "Font sample. This font will be used in emails." ]
        ]



{- Color editor -}
-- implements UpdateColorPickerMsg


updateColorPicker : ColorIdentifier -> ColorPicker.Msg -> EditThemeState -> Return msg EditThemeState
updateColorPicker colorIdentifier msg state =
    let
        theme =
            state.themeBeingEdited

        ( newColorPicker, mNewColor ) =
            ColorPicker.update msg
                (getThemeColor state colorIdentifier)
                (getColorPicker state colorIdentifier)

        newColorPickers =
            Enum.insert colorIdentifier newColorPicker state.colorPickers

        newColor =
            withDefault (getThemeColor state colorIdentifier) mNewColor

        newColors =
            Enum.insert colorIdentifier newColor theme.colors
    in
    singleton <| { state | colorPickers = newColorPickers, themeBeingEdited = { theme | colors = newColors } }



-- implements SetPopoverMsg


setPopover : ColorIdentifier -> Popover.State -> EditThemeState -> Return msg EditThemeState
setPopover colorIdentifier popover state =
    let
        newPopovers =
            Enum.insert colorIdentifier popover state.popovers
    in
    singleton <| { state | popovers = newPopovers }



-- A 'color editor' consists of a text field containing the hexadecimal
-- representation of the color, as well as a button that when clicked opens the
-- colour picker.


viewEditColor : (Msg -> msg) -> ColorIdentifier -> EditThemeState -> Html msg
viewEditColor embed colorIdentifier state =
    let
        color =
            getThemeColor state colorIdentifier

        colorPicker =
            getColorPicker state colorIdentifier

        colorText =
            Maybe.withDefault (ColorPicker.color2Hex color) <| getPartialColor state colorIdentifier

        popover =
            getPopover state colorIdentifier

        readonly =
            state.themeBeingEdited.id <= 0

        closePopoverOverlay =
            let
                (Popover.State rawState) =
                    popover
            in
            if rawState.isActive then
                div (class "color-picker-overlay" :: Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)) []

            else
                div [] []
    in
    Popover.view popover <|
        Popover.content []
            [ Html.map (embed << UpdateColorPickerMsg colorIdentifier) <| ColorPicker.view color colorPicker ]
        <|
            Popover.right <|
                Popover.config <|
                    div []
                        [ Input.text
                            [ Input.attrs <|
                                [ value <| colorText
                                , style "width" "80%"
                                , style "display" "inline-block"
                                , disabled readonly
                                , onInput <| embed << OnColorTextChangedMsg colorIdentifier
                                , onBlur <| embed <| OnColorTextCommittedMsg colorIdentifier
                                ]
                                    ++ Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)
                            ]
                        , Html.button
                            ([ style "background-color" <| ColorPicker.color2Hex color
                             , class "color-display"
                             , disabled readonly
                             ]
                                ++ Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)
                            )
                            []
                        , closePopoverOverlay
                        ]



{- Theme editor -}
-- implements SetThemeNameMsg


setThemeName : String -> EditThemeState -> Return msg EditThemeState
setThemeName name state =
    let
        themeBeingEdited =
            state.themeBeingEdited
    in
    singleton <| { state | themeBeingEdited = { themeBeingEdited | name = name } }



-- implements OpenLogoFileSelectMsg


openLogoFileSelect : (Msg -> msg) -> EditThemeState -> Return msg EditThemeState
openLogoFileSelect embed state =
    let
        callback : File -> msg
        callback file =
            embed <| LoadLogoFileMsg file
    in
    return state <| FileSelect.file [ "image/png", "image/jpg", "image/jpeg" ] callback



-- implements LoadLogoFileMsg


loadLogoFile : (Msg -> msg) -> File -> EditThemeState -> Return msg EditThemeState
loadLogoFile embed file state =
    let
        callback : String -> msg
        callback contents =
            embed <| SetLogoMsg contents
    in
    return state <| Task.perform callback <| File.toUrl file



-- implements SetLogoMsg


setLogo : String -> EditThemeState -> Return msg EditThemeState
setLogo content state =
    let
        theme =
            state.themeBeingEdited
    in
    singleton <| { state | themeBeingEdited = { theme | logo = content } }



{- The 'theme editor' consists of
   - a theme selector
   - create/delete theme buttons
   - an editable theme name field
   - colour editors
   - a 'save changes' button
-}


viewEditTheme :
    { embed : Msg -> msg
    , doSaveTheme : msg
    , doDeleteTheme : msg
    , doCreateTheme : Theme -> msg
    }
    -> EditThemeReadonly
    -> EditThemeState
    -> Html msg
viewEditTheme params read state =
    let
        readonly =
            state.themeBeingEdited.id <= 0

        viewEditColorPair : { kind : ColorKind, description : String } -> Html msg
        viewEditColorPair { kind, description } =
            let
                backgroundColorIdentifier =
                    { kind = kind, component = BackgroundColor }

                backgroundColor =
                    getThemeColor state backgroundColorIdentifier

                textColorIdentifier =
                    { kind = kind, component = TextColor }

                textColor =
                    getThemeColor state textColorIdentifier
            in
            Form.row []
                [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ]
                    [ text <| Enum.toHumanString enumColorKind kind ++ " colour" ]
                , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
                    [ viewEditColor params.embed backgroundColorIdentifier state
                    , viewEditColor params.embed textColorIdentifier state
                    , div
                        [ style "background-color" <| ColorPicker.color2Hex backgroundColor
                        , style "color" <| ColorPicker.color2Hex textColor
                        , class "p-sm-2"
                        , class "mt-sm-2"
                        ]
                        [ text description ]
                    ]
                ]
    in
    div []
        [ Form.row []
            [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Selected theme" ]
            , viewThemeSelector params.embed read state.themeBeingEdited
            ]
        , Form.row []
            [ Form.col [ Col.sm4, Col.md4, Col.lg4 ] []
            , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
                [ Button.button
                    [ Button.danger
                    , Button.attrs
                        [ class "ml-sm-2", onClick params.doDeleteTheme, disabled readonly ]
                    ]
                    [ text "Delete theme" ]
                , Button.button
                    [ Button.success
                    , Button.attrs
                        [ class "ml-sm-2"
                        , onClick <| params.doCreateTheme state.themeBeingEdited
                        ]
                    ]
                    [ text "Create theme" ]
                , Form.help []
                    [ text <|
                        "Note that you can't delete "
                            ++ "the 'domain themes'."
                    ]
                ]
            ]
        , Form.row []
            [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Theme name" ]
            , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
                [ Input.text [ Input.attrs [ value state.themeBeingEdited.name, onInput (params.embed << SetThemeNameMsg), disabled readonly ] ] ]
            ]
        , Form.row []
            [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Logo" ]
            , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
                [ div
                    [ class "mt-sm-2"
                    , style "background-color" <| ColorPicker.color2Hex <| getThemeColor state { kind = BrandColors, component = BackgroundColor }
                    ]
                    [ img [ class "p-sm-2", style "max-width" "100%", style "max-height" "100px", src state.themeBeingEdited.logo ] []
                    ]
                , Button.button
                    [ Button.secondary, Button.attrs [ onClick (params.embed OpenLogoFileSelectMsg), type_ "button", disabled readonly ] ]
                    [ text "Select image" ]
                ]
            ]
        , viewEditColorPair
            { kind = BrandColors
            , description =
                "The colour that will appear on larger areas such as "
                    ++ "the background of the headers. This colour will also be the "
                    ++ "background behind your logo."
            }
        , viewEditColorPair
            { kind = ActionColors
            , description =
                "Action buttons, arrows, mandatory actions and elements "
                    ++ " that require attention. We recommend your primary brand colour "
                    ++ "or a colour that stands out."
            }
        , viewEditColorPair
            { kind = SecondaryActionColors
            , description =
                "Optional actions and links. We recommend a lighter "
                    ++ "colour, one that compliments your primary action colour."
            }
        , viewEditColorPair
            { kind = PositiveColors
            , description =
                "This colour will appear on notification such as "
                    ++ "\"Saved\" and \"Success\". We recommend an intuitively positive "
                    ++ "colour like green."
            }
        , viewEditColorPair
            { kind = NegativeColors
            , description =
                "This colour will appear on notifications such as "
                    ++ "\"Error\". We recommend an intuitively negative colour such as "
                    ++ "red."
            }
        , Form.row []
            [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Font" ]
            , viewFontSelector params.embed state.themeBeingEdited
            ]
        , Form.row []
            [ Form.col [ Col.sm12 ]
                [ Button.button
                    [ Button.success
                    , Button.attrs
                        [ class "ml-sm-2", onClick params.doSaveTheme, disabled readonly ]
                    ]
                    [ text "Save changes" ]
                ]
            ]
        ]


setColor : ColorIdentifier -> Color -> EditThemeState -> EditThemeState
setColor ident color state =
    let
        theme =
            state.themeBeingEdited
    in
    { state | themeBeingEdited = { theme | colors = Enum.insert ident color theme.colors } }



-- implements OnColorTextChangedMsg


updatePartialColor : ColorIdentifier -> String -> EditThemeState -> EditThemeState
updatePartialColor ident colorText state =
    { state | partialColors = Enum.insert ident colorText state.partialColors }


updateColorIfValidInput : ColorIdentifier -> String -> EditThemeState -> EditThemeState
updateColorIfValidInput colorIdentifier colorText =
    case ColorPicker.hex2Color colorText of
        Nothing ->
            identity

        Just color ->
            setColor colorIdentifier color



-- implements OnColorTextCommittedMsg


commitPartialColor : ColorIdentifier -> EditThemeState -> EditThemeState
commitPartialColor ident state =
    { state | partialColors = Enum.remove ident state.partialColors }
