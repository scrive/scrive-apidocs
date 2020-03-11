module AdminOnly.UserGroupAdmin.UserGroupBrandingTab.EditTheme exposing (..)

import Html exposing (Html, text, div, img)
import String
import Maybe exposing (withDefault)
import List exposing (map)
import List.Extra as List
import File.Select as FileSelect
import Utils exposing (..)
import File exposing (File)
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Vendor.Popover as Popover
import Bootstrap.Form.Input as Input
import Bootstrap.Form as Form
import Html.Attributes exposing (selected, value, class, disabled, style, src, type_)
import Html.Events exposing (onInput, onClick)
import Vendor.ColorPickerExtra as ColorPicker
import Task
import EnumExtra as Enum
import Maybe.Extra as Maybe
import Color exposing (Color)

import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)

type alias EditThemeState =
  { themeBeingEdited : Theme
  , colorPickers : Enum.Dict ColorIdentifier ColorPicker.State
  , popovers : Enum.Dict ColorIdentifier Popover.State
  }

getThemeColor : EditThemeState -> ColorIdentifier -> Color
getThemeColor state ident = withDefault (Color.rgb 0 0 0) <| Enum.get ident state.themeBeingEdited.colors

getColorPicker : EditThemeState -> ColorIdentifier -> ColorPicker.State
getColorPicker state ident = withDefault ColorPicker.empty <| Enum.get ident state.colorPickers

getPopover : EditThemeState -> ColorIdentifier -> Popover.State
getPopover state ident = withDefault Popover.initialState <| Enum.get ident state.popovers

type alias EditThemeReadonly =
  { availableThemes : List Theme
  , domainThemes : Enum.Dict ThemeKind Theme
  }

type Msg = SetActiveThemeMsg ThemeID (Maybe ThemeKind)
         | UpdateColorPickerMsg ColorIdentifier ColorPicker.Msg
         | SetPopoverMsg ColorIdentifier Popover.State
         | SetThemeNameMsg String
         | OpenLogoFileSelectMsg
         | LoadLogoFileMsg File
         | SetLogoMsg String

update : (Msg -> msg) -> Msg -> EditThemeReadonly -> EditThemeState -> (EditThemeState, Cmd msg)
update embed msg read = case msg of
  SetActiveThemeMsg id fromDomainTheme -> setActiveTheme read id fromDomainTheme
  UpdateColorPickerMsg colorIdentifier msg_ -> updateColorPicker colorIdentifier msg_
  SetPopoverMsg colorIdentifier popover -> setPopover colorIdentifier popover
  SetThemeNameMsg name -> setThemeName name
  OpenLogoFileSelectMsg -> openLogoFileSelect embed
  LoadLogoFileMsg file -> loadLogoFile embed file
  SetLogoMsg content -> setLogo content


{- Theme selector -}

-- implements SetActiveThemeMsg
setActiveTheme : EditThemeReadonly -> ThemeID -> Maybe ThemeKind -> EditThemeState -> (EditThemeState, Cmd msg)
setActiveTheme read id fromDomainTheme state =
  let activeTheme =
        Maybe.andThen (\k -> Enum.get k read.domainThemes) fromDomainTheme
        |> Maybe.orElse (List.find (\theme -> theme.id == id) read.availableThemes)
        |> withDefault state.themeBeingEdited
  in ({ state | themeBeingEdited = activeTheme }, Cmd.none)

-- The 'theme selector' is a drop-down list of theme names to select.
viewThemeSelector : (Msg -> msg) -> EditThemeReadonly -> Theme -> Form.Col msg
viewThemeSelector embed read selectedTheme =
  let stringToMaybeThemeKind str = case str of
        "e" -> Just EmailTheme
        "v" -> Just SignViewTheme
        "s" -> Just ServiceTheme
        _ -> Nothing

      stringFromMaybeThemeKind kind = case kind of
        Nothing -> "n"
        Just EmailTheme -> "e"
        Just SignViewTheme -> "v"
        Just ServiceTheme -> "s"

      onSelect idString =
        let fromDomainTheme = stringToMaybeThemeKind <| String.left 1 idString
            id = withDefault -1 (String.toInt <| String.dropLeft 1 idString)
        in SetActiveThemeMsg id fromDomainTheme

      themeItem : Theme -> Select.Item msg
      themeItem theme = Select.item
        [ value <| String.append (stringFromMaybeThemeKind theme.fromDomainTheme) (String.fromInt theme.id)
        , selected <| theme.id == selectedTheme.id && theme.fromDomainTheme == selectedTheme.fromDomainTheme]
        [ text theme.name ]

      themes = Enum.values read.domainThemes ++ read.availableThemes
  in  Form.col
        [ Col.sm8, Col.md8, Col.lg8 ]
        [ Select.select
          [ Select.onChange (embed << onSelect) ]
          (List.map themeItem themes)
        ]


{- Color editor -}

-- implements UpdateColorPickerMsg
updateColorPicker : ColorIdentifier -> ColorPicker.Msg -> EditThemeState -> (EditThemeState, Cmd msg)
updateColorPicker colorIdentifier msg state =
  let theme = state.themeBeingEdited

      (newColorPicker, mNewColor) =
        ColorPicker.update msg (getThemeColor state colorIdentifier)
                               (getColorPicker state colorIdentifier)
      newColorPickers = Enum.insert colorIdentifier newColorPicker state.colorPickers

      newColor = withDefault (getThemeColor state colorIdentifier) mNewColor
      newColors = Enum.insert colorIdentifier newColor theme.colors

  in ({ state | colorPickers = newColorPickers, themeBeingEdited = { theme | colors = newColors } }, Cmd.none)

-- implements SetPopoverMsg
setPopover : ColorIdentifier -> Popover.State -> EditThemeState -> (EditThemeState, Cmd msg)
setPopover colorIdentifier popover state =
  let newPopovers = Enum.insert colorIdentifier popover state.popovers
  in ({ state | popovers = newPopovers }, Cmd.none)

-- A 'color editor' consists of a text field containing the hexadecimal
-- representation of the color, as well as a button that when clicked opens the
-- colour picker.
viewEditColor : (Msg -> msg) -> ColorIdentifier -> EditThemeState -> Html msg
viewEditColor embed colorIdentifier state =
  let color = getThemeColor state colorIdentifier
      colorPicker = getColorPicker state colorIdentifier
      popover = getPopover state colorIdentifier
      readonly = isJust state.themeBeingEdited.fromDomainTheme
      closePopoverOverlay =
        let (Popover.State rawState) = popover
        in if rawState.isActive
          then div (class "color-picker-overlay" :: Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)) []
          else div [][]
  in Popover.view popover
      <| Popover.content []
          [ Html.map (embed << UpdateColorPickerMsg colorIdentifier) <| ColorPicker.view color colorPicker ]
          <| Popover.right
          <| Popover.config
          <| div []
              [ Input.text
                [ Input.attrs <|
                  [ value <| ColorPicker.color2Hex color
                  , style "width" "80%"
                  , style "display" "inline-block"
                  , disabled readonly
                  ] ++ Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)
                ]
              , Html.button
                ([ style "background-color" <| ColorPicker.color2Hex color
                , class "color-display"
                , disabled readonly
                ] ++ Popover.onClick popover (embed << SetPopoverMsg colorIdentifier)) []
              , closePopoverOverlay ]


{- Theme editor -}

-- implements SetThemeNameMsg
setThemeName : String -> EditThemeState -> (EditThemeState, Cmd msg)
setThemeName name state =
  let themeBeingEdited = state.themeBeingEdited
  in ({ state | themeBeingEdited = {themeBeingEdited | name = name} }, Cmd.none)

-- implements OpenLogoFileSelectMsg
openLogoFileSelect : (Msg -> msg) -> EditThemeState -> (EditThemeState, Cmd msg)
openLogoFileSelect embed state =
  let callback : File -> msg
      callback file = embed <| LoadLogoFileMsg file
  in (state, FileSelect.file ["image/png"] callback)

-- implements LoadLogoFileMsg
loadLogoFile : (Msg -> msg) -> File -> EditThemeState -> (EditThemeState, Cmd msg)
loadLogoFile embed file state =
  let callback : String -> msg
      callback contents = embed <| SetLogoMsg contents
  in (state, Task.perform callback <| File.toUrl file)

-- implements SetLogoMsg
setLogo : String -> EditThemeState -> (EditThemeState, Cmd msg)
setLogo content state =
  let theme = state.themeBeingEdited
  in ( { state | themeBeingEdited = { theme | logo = content } }, Cmd.none )

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
  } -> EditThemeReadonly -> EditThemeState -> Html msg
viewEditTheme params read state =
  let readonly = isJust state.themeBeingEdited.fromDomainTheme

      viewEditColorPair :
        {kind : ColorKind, description : String} -> Html msg
      viewEditColorPair {kind, description} =
        let backgroundColorIdentifier = {kind = kind, component = BackgroundColor}
            backgroundColor = getThemeColor state backgroundColorIdentifier
            textColorIdentifier = {kind = kind, component = TextColor}
            textColor = getThemeColor state textColorIdentifier
        in Form.row []
            [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ]
              [ text <| Enum.toHumanString enumColorKind kind ++ " colour" ]
            , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
              [ viewEditColor params.embed backgroundColorIdentifier state
              , viewEditColor params.embed textColorIdentifier state
              , div
                [ style "background-color" <| ColorPicker.color2Hex backgroundColor
                , style "color" <| ColorPicker.color2Hex textColor
                , class "p-sm-2", class "mt-sm-2" ]
                [ text description ]
              ]
            ]

  in div []
      [ Form.row []
        [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Selected theme" ]
        , viewThemeSelector params.embed read state.themeBeingEdited ]
      , Form.row []
        [ Form.col [ Col.sm4, Col.md4, Col.lg4 ] []
        , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
            [ Button.button [ Button.danger, Button.attrs
              [ class "ml-sm-2", onClick params.doDeleteTheme, disabled readonly ] ]
              [ text "Delete theme" ]
            , Button.button [ Button.success, Button.attrs
                [ class "ml-sm-2"
                , onClick <| params.doCreateTheme state.themeBeingEdited
                ]
              ]
              [ text "Create theme" ]
            , Form.help [] [ text <| "Note that you can't delete "
                ++ "the 'domain themes'." ]
            ]
        ]
      , Form.row []
        [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Theme name" ]
        , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
            [ Input.text [Input.attrs [ value state.themeBeingEdited.name, onInput (params.embed << SetThemeNameMsg), disabled readonly ] ] ]
        ]
      , Form.row []
        [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Logo" ]
        , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
          [ div
            [ class "mt-sm-2"
            , style "background-color" <| ColorPicker.color2Hex <| getThemeColor state {kind = BrandColors, component = BackgroundColor}
            ]
            [ img [ class "p-sm-2", style "max-width" "100%", style "max-height" "100px", src state.themeBeingEdited.logo] []
            ]
          , Button.button
              [ Button.secondary, Button.attrs [ onClick (params.embed OpenLogoFileSelectMsg), type_ "button", disabled readonly ] ]
              [ text "Select image" ]
          ]
        ]
      , viewEditColorPair
          { kind = BrandColors
          , description = "The colour that will appear on larger areas such as "
            ++ "the background of the headers. This colour will also be the "
            ++ "background behind your logo." }
      , viewEditColorPair
          { kind = ActionColors
          , description = "Action buttons, arrows, mandatory actions and elements "
            ++ " that require attention. We recommend your primary brand colour "
            ++ "or a colour that stands out." }
      , viewEditColorPair
          { kind = SecondaryActionColors
          , description = "Optional actions and links. We recommend a lighter "
            ++ "colour, one that compliments your primary action colour." }
      , viewEditColorPair
          { kind = PositiveColors
          , description = "This colour will appear on notification such as "
            ++ "\"Saved\" and \"Success\". We recommend an intuitively positive "
            ++ "colour like green." }
      , viewEditColorPair
          { kind = NegativeColors
          , description = "This colour will appear on notifications such as "
            ++ "\"Error\". We recommend an intuitively negative colour such as "
            ++ "red." }
      , Form.row []
        [ Form.col [ Col.sm12 ]
            [ Button.button [ Button.success, Button.attrs
              [ class "ml-sm-2", onClick params.doSaveTheme, disabled readonly ] ]
            [ text "Save changes" ] ]
        ]
      ]
