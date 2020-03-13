module Lib.Theme.Types exposing (..)

import Color exposing (Color)
import EnumExtra as Enum exposing (Enum)

type ColorKind = BrandColors | ActionColors | SecondaryActionColors
  | PositiveColors | NegativeColors

type ColorComponent = TextColor | BackgroundColor

type alias ColorIdentifier =
  { kind : ColorKind
  , component : ColorComponent }

enumColorKind : Enum ColorKind
enumColorKind =
  let allValues = [ BrandColors, ActionColors, SecondaryActionColors,
        PositiveColors, NegativeColors ]
      -- chosen to coincide with JSON names
      toString k = case k of
        BrandColors -> "brand"
        ActionColors -> "action"
        SecondaryActionColors -> "actionSecondary"
        PositiveColors -> "positive"
        NegativeColors -> "negative"
      toHumanString k = case k of
        BrandColors -> "Brand"
        ActionColors -> "Primary action"
        SecondaryActionColors -> "Secondary action"
        PositiveColors -> "Positive"
        NegativeColors -> "Negative"
  in Enum.makeEnum allValues toString toHumanString

enumColorComponent : Enum ColorComponent
enumColorComponent =
  let allValues = [ TextColor, BackgroundColor ]
      -- chosen to coincide with JSON names
      toString c = case c of
        TextColor -> "TextColor"
        BackgroundColor -> "Color"
      toHumanString c = case c of
        TextColor -> "Text Colour"
        BackgroundColor -> "Background Colour"
  in Enum.makeEnum allValues toString toHumanString

enumColorIdentifier : Enum ColorIdentifier
enumColorIdentifier =
  let allValues =
        List.concatMap
          (\k -> List.map (\c -> {kind = k, component = c})
            <| Enum.allValues enumColorComponent )
          <| Enum.allValues enumColorKind
      toString ident = Enum.toString enumColorKind ident.kind
        ++ Enum.toString enumColorComponent ident.component
      toHumanString ident =
        Enum.toHumanString enumColorKind ident.kind
        ++ " " ++ Enum.toHumanString enumColorComponent ident.component
  in Enum.makeEnum allValues toString toHumanString

type alias ThemeID = Int
type alias Theme =
    { id : ThemeID
    -- ^ Note that only positive ids are valid, but we are also using negative
    -- ids as a hack to express (and disambiguate) certain read-only themes. See
    -- `getDomainThemes` in AdminOnly.UserGroupAdmin.UserGroupBrandingTab.
    , name : String
    , logo : String
    , font : String  -- not implemented in adminonly
    , colors : Enum.Dict ColorIdentifier Color
    }

errorTheme : Theme
errorTheme =
  { id = -1
  , name = "### Internal Error ###"
  , logo = ""
  , font = ""
  , colors = Enum.empty enumColorIdentifier
  }
