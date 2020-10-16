module AdminOnly.BrandedDomain.EditBrandedDomain exposing (..)

import AdminOnly.BrandedDomain.Types exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Color exposing (Color)
import EnumExtra as Enum
import File exposing (File)
import File.Select as FileSelect
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, selected, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Lib.Types.Theme exposing (Theme, ThemeID)
import Return exposing (..)
import Task
import Vendor.ColorPickerExtra as ColorPicker
import Vendor.Popover as Popover


type alias EditBrandedDomainState =
    { brandedDomainBeingEdited : BrandedDomain
    , colorPickers : Enum.Dict ColorIdentifier ColorPicker.State
    , partialColors : Enum.Dict ColorIdentifier String
    , popovers : Enum.Dict ColorIdentifier Popover.State
    }


getDomainColor : EditBrandedDomainState -> ColorIdentifier -> Color
getDomainColor state ident =
    Maybe.withDefault (Color.rgb 0 0 0) <| Enum.get ident state.brandedDomainBeingEdited.colors


getColorPicker : EditBrandedDomainState -> ColorIdentifier -> ColorPicker.State
getColorPicker state ident =
    Maybe.withDefault ColorPicker.empty <| Enum.get ident state.colorPickers


getPartialColor : EditBrandedDomainState -> ColorIdentifier -> Maybe String
getPartialColor state ident =
    Enum.get ident state.partialColors


getPopover : EditBrandedDomainState -> ColorIdentifier -> Popover.State
getPopover state ident =
    Maybe.withDefault Popover.initialState <| Enum.get ident state.popovers


type Msg
    = SetUrlMsg String
    | SetSmsOriginatorMsg String
    | SetEmailOriginatorMsg String
    | SetThemeMsg ThemeKind ThemeID
    | SetBrowserTitleMsg String
    | SetColorMsg ColorIdentifier Color
    | OpenFaviconFileSelectMsg
    | LoadFaviconFileMsg File
    | SetFaviconMsg String
    | UpdateColorPickerMsg ColorIdentifier ColorPicker.Msg
    | SetPopoverMsg ColorIdentifier Popover.State
    | OnColorTextChangedMsg ColorIdentifier String
    | OnColorTextCommittedMsg ColorIdentifier


update : (Msg -> msg) -> Msg -> EditBrandedDomainState -> Return msg EditBrandedDomainState
update embed msg =
    let
        modifyBrandedDomain : (BrandedDomain -> BrandedDomain) -> EditBrandedDomainState -> Return msg EditBrandedDomainState
        modifyBrandedDomain f state =
            singleton
                { state
                    | brandedDomainBeingEdited =
                        f state.brandedDomainBeingEdited
                }

        updateColorIfValidInput colorIdentifier colorText =
            case ColorPicker.hex2Color colorText of
                Nothing ->
                    singleton

                Just color ->
                    modifyBrandedDomain <| setColor colorIdentifier color
    in
    case msg of
        SetUrlMsg url ->
            modifyBrandedDomain <| setUrl url

        SetSmsOriginatorMsg str ->
            modifyBrandedDomain <| setSmsOriginator str

        SetEmailOriginatorMsg str ->
            modifyBrandedDomain <| setEmailOriginator str

        SetThemeMsg kind id ->
            modifyBrandedDomain <| setTheme kind id

        SetBrowserTitleMsg str ->
            modifyBrandedDomain <| setBrowserTitle str

        SetColorMsg ident color ->
            modifyBrandedDomain <| setColor ident color

        OpenFaviconFileSelectMsg ->
            openFaviconFileSelect embed

        LoadFaviconFileMsg file ->
            loadFaviconFile embed file

        SetFaviconMsg content ->
            modifyBrandedDomain <| setFavicon content

        UpdateColorPickerMsg colorIdentifier msg_ ->
            updateColorPicker colorIdentifier msg_

        SetPopoverMsg colorIdentifier popover ->
            setPopover colorIdentifier popover

        OnColorTextChangedMsg colorIdentifier colorText ->
            updatePartialColor colorIdentifier colorText
                >> updateColorIfValidInput colorIdentifier colorText

        OnColorTextCommittedMsg colorIdentifier ->
            commitPartialColor colorIdentifier >> singleton



{- Theme selector -}
-- implements SetThemeMsg


setTheme : ThemeKind -> ThemeID -> BrandedDomain -> BrandedDomain
setTheme kind id state =
    { state | themes = Enum.insert kind id state.themes }



-- The 'theme selector' is a drop-down list of theme names to select.


viewThemeSelector :
    { embed : Msg -> msg
    , availableThemes : List Theme
    }
    -> ThemeKind
    -> BrandedDomain
    -> Form.Col msg
viewThemeSelector { embed, availableThemes } kind state =
    let
        onSelect : String -> msg
        onSelect =
            embed
                << SetThemeMsg kind
                << Maybe.withDefault -1
                {- this should be an internal error -} << String.toInt

        themeItem : Theme -> Select.Item msg
        themeItem theme =
            Select.item
                [ value <| String.fromInt theme.id
                , selected <|
                    case Enum.get kind state.themes of
                        Just id ->
                            theme.id == id

                        Nothing ->
                            False

                {- this should be an internal error -}
                ]
                [ text theme.name ]
    in
    Form.col
        [ Col.sm8, Col.md8, Col.lg8 ]
        [ Select.select
            [ Select.onChange onSelect ]
            (List.map themeItem availableThemes)
        ]



{- Color editor -}
-- implements UpdateColorPickerMsg


updateColorPicker : ColorIdentifier -> ColorPicker.Msg -> EditBrandedDomainState -> Return msg EditBrandedDomainState
updateColorPicker colorIdentifier msg state =
    let
        brandedDomain =
            state.brandedDomainBeingEdited

        ( newColorPicker, mNewColor ) =
            ColorPicker.update msg
                (getDomainColor state colorIdentifier)
                (getColorPicker state colorIdentifier)

        newColorPickers =
            Enum.insert colorIdentifier newColorPicker state.colorPickers

        newColor =
            Maybe.withDefault (getDomainColor state colorIdentifier) mNewColor

        newColors =
            Enum.insert colorIdentifier newColor brandedDomain.colors
    in
    singleton
        { state
            | colorPickers = newColorPickers
            , brandedDomainBeingEdited = { brandedDomain | colors = newColors }
        }



-- implements SetPopoverMsg


setPopover : ColorIdentifier -> Popover.State -> EditBrandedDomainState -> Return msg EditBrandedDomainState
setPopover colorIdentifier popover state =
    let
        newPopovers =
            Enum.insert colorIdentifier popover state.popovers
    in
    singleton { state | popovers = newPopovers }



-- A 'color editor' consists of a text field containing the hexadecimal
-- representation of the color, as well as a button that when clicked opens the
-- colour picker.


viewEditColor : (Msg -> msg) -> ColorIdentifier -> EditBrandedDomainState -> Html msg
viewEditColor embed colorIdentifier state =
    let
        color =
            getDomainColor state colorIdentifier

        colorPicker =
            getColorPicker state colorIdentifier

        colorText =
            Maybe.withDefault (ColorPicker.color2Hex color) <| getPartialColor state colorIdentifier

        popover =
            getPopover state colorIdentifier

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
                                , onInput <| embed << OnColorTextChangedMsg colorIdentifier
                                , onBlur <| embed <| OnColorTextCommittedMsg colorIdentifier
                                ]
                                    ++ Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)
                            ]
                        , Html.button
                            ([ style "background-color" <| ColorPicker.color2Hex color
                             , class "color-display"
                             ]
                                ++ Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)
                            )
                            []
                        , closePopoverOverlay
                        ]



{- Branded domain editor -}
-- implements SetUrlMsg


setUrl : String -> BrandedDomain -> BrandedDomain
setUrl url state =
    { state | url = url }



-- implements SetSmsOriginatorMsg


setSmsOriginator : String -> BrandedDomain -> BrandedDomain
setSmsOriginator str state =
    { state | smsOriginator = str }



-- implements SetEmailOriginatorMsg


setEmailOriginator : String -> BrandedDomain -> BrandedDomain
setEmailOriginator str state =
    { state | emailOriginator = str }



-- implements SetBrowserTitleMsg


setBrowserTitle : String -> BrandedDomain -> BrandedDomain
setBrowserTitle title state =
    { state | browserTitle = title }



-- implements SetColorMsg


setColor : ColorIdentifier -> Color -> BrandedDomain -> BrandedDomain
setColor ident color state =
    { state | colors = Enum.insert ident color state.colors }



-- implements OpenFaviconFileSelectMsg


openFaviconFileSelect : (Msg -> msg) -> state -> Return msg state
openFaviconFileSelect embed state =
    let
        callback : File -> msg
        callback file =
            embed <| LoadFaviconFileMsg file
    in
    return state <| FileSelect.file [ "image/png", "image/jpg", "image/jpeg" ] callback



-- implements LoadFaviconFileMsg


loadFaviconFile : (Msg -> msg) -> File -> state -> Return msg state
loadFaviconFile embed file state =
    let
        callback : String -> msg
        callback contents =
            embed <| SetFaviconMsg contents
    in
    return state <| Task.perform callback <| File.toUrl file



-- implements SetFaviconMsg


setFavicon : String -> BrandedDomain -> BrandedDomain
setFavicon content state =
    { state | favicon = content }


viewEditBrandedDomain :
    { embed : Msg -> msg
    , doSaveBrandedDomain : msg -- does post request and pop-over
    , availableThemes : List Theme
    }
    -> EditBrandedDomainState
    -> Html msg
viewEditBrandedDomain { embed, doSaveBrandedDomain, availableThemes } state =
    let
        brandedDomain =
            state.brandedDomainBeingEdited

        textFieldRow label val msg description =
            Form.row []
                [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text label ]
                , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
                    [ Input.text
                        [ Input.attrs
                            [ value val, onInput (embed << msg) ]
                        ]
                    , Form.help [] [ text description ]
                    ]
                ]

        themeSelectRow kind =
            Form.row []
                [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ]
                    [ text <| Enum.toHumanString enumThemeKind kind ++ " Theme" ]
                , viewThemeSelector { embed = embed, availableThemes = availableThemes } kind brandedDomain
                ]

        viewEditColorRow ident =
            Form.row []
                [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ]
                    [ text <| Enum.toHumanString enumColorIdentifier ident ]
                , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
                    [ viewEditColor embed ident state
                    ]
                ]
    in
    div [] <|
        [ Form.row []
            [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Favicon" ]
            , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
                [ div [] [ img [ style "max-width" "50px", src brandedDomain.favicon ] [] ]
                , Button.button
                    [ Button.secondary, Button.attrs [ onClick (embed OpenFaviconFileSelectMsg), type_ "button" ] ]
                    [ text "Select image" ]
                ]
            ]
        , textFieldRow "Url" brandedDomain.url SetUrlMsg "The full web address for the domain."
        , textFieldRow "SMS originator" brandedDomain.smsOriginator SetSmsOriginatorMsg "The name displayed to the recipient when receiving an SMS. Maximum 11 alpha-numeric characters."
        , textFieldRow "Email originator" brandedDomain.emailOriginator SetEmailOriginatorMsg "The name displayed to the recipient when receiving emails."
        , textFieldRow "Browser title" brandedDomain.browserTitle SetBrowserTitleMsg "The text at the top of the browser."
        , themeSelectRow EmailTheme
        , themeSelectRow SignViewTheme
        , themeSelectRow ServiceTheme
        , themeSelectRow LoginTheme
        ]
            ++ List.map viewEditColorRow (Enum.allValues enumColorIdentifier)
            ++ [ Form.row [ Row.rightSm ]
                    [ Form.col [ Col.sm12 ]
                        [ Button.button [ Button.success, Button.attrs [ class "ml-sm-2", onClick doSaveBrandedDomain ] ]
                            [ text "Save changes" ]
                        ]
                    ]
               ]



-- implements OnColorTextChangedMsg


updatePartialColor : ColorIdentifier -> String -> EditBrandedDomainState -> EditBrandedDomainState
updatePartialColor ident colorText state =
    { state | partialColors = Enum.insert ident colorText state.partialColors }



-- implements OnColorTextCommittedMsg


commitPartialColor : ColorIdentifier -> EditBrandedDomainState -> EditBrandedDomainState
commitPartialColor ident state =
    { state | partialColors = Enum.remove ident state.partialColors }
