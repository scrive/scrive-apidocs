module AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)

import Color exposing (Color, rgb, rgb255)
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
    , fromDomainTheme : Maybe ThemeKind
    -- ^ `Just` means this theme is inherited from the branded domain; such
    -- themes are not editable.
    , name : String
    , logo : String
    , font : String  -- not implemented in adminonly
    , colors : Enum.Dict ColorIdentifier Color
    }

errorTheme : Theme
errorTheme =
  { id = -1
  , fromDomainTheme = Just EmailTheme  -- effectively read-only!
  , name = "### Internal Error ###"
  , logo = ""
  , font = ""
  , colors = Enum.empty enumColorIdentifier
  }

type ThemeKind = EmailTheme | SignViewTheme | ServiceTheme

enumThemeKind : Enum ThemeKind
enumThemeKind =
  let allValues = [ EmailTheme, SignViewTheme, ServiceTheme ]
      -- chosen to coincide with JSON names
      toString k = case k of
        EmailTheme -> "mailTheme"
        SignViewTheme -> "signviewTheme"
        ServiceTheme -> "serviceTheme"
      toHumanString k = case k of
        EmailTheme -> "Email"
        SignViewTheme -> "Sign View"
        ServiceTheme -> "Service"
  in Enum.makeEnum allValues toString toHumanString

type alias UserGroupBranding =
    { themes : Enum.Dict ThemeKind ThemeID
    -- ^ `Nothing` means theme is inherited from branded domain (see
    -- fromDomainTheme field).
    , browserTitle : Maybe String
    , smsOriginator : Maybe String
    , favicon : Maybe String
    }

fallbackUserGroupBranding : UserGroupBranding
fallbackUserGroupBranding =
  { themes = Enum.empty enumThemeKind
  , browserTitle = Nothing
  , smsOriginator = Nothing
  , favicon = Nothing
  }
