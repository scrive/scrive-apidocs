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

type Font
  = ArialBlack
  | ArialNarrow
  | ComicSans
  | CourierNew
  | SourceSansPro
  | Garamond
  | Georgia
  | TimesNewRoman
  | Tahoma
  | Trebuchet
  | Verdana
  | Arial
  | Helvetica

defaultFont : Font
defaultFont =
    SourceSansPro

enumFont : Enum Font
enumFont =
  let allValues =
        [ ArialBlack
        , ArialNarrow
        , ComicSans
        , CourierNew
        , SourceSansPro
        , Garamond
        , Georgia
        , TimesNewRoman
        , Tahoma
        , Trebuchet
        , Verdana
        , Arial
        , Helvetica
        ]

      toString f = case f of
        ArialBlack -> "\"arial black\",sans-serif"
        ArialNarrow -> "\"arial narrow\",sans-serif"
        ComicSans -> "\"comic sans ms\",sans-serif"
        CourierNew -> "\"courier new\",monospace"
        SourceSansPro -> "\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif"
        Garamond -> "garamond,serif"
        Georgia -> "georgia,serif"
        TimesNewRoman -> "\"times new roman\",serif"
        Tahoma -> "tahoma,sans-serif"
        Trebuchet -> "\"trebuchet ms\",sans-serif"
        Verdana -> "verdana,sans-serif"
        Arial -> "arial,helvetica,sans-serif"
        Helvetica -> "helvetica,sans-serif"

      toHumanString f = case f of
        ArialBlack -> "Wide"
        ArialNarrow -> "Narrow"
        ComicSans -> "Comic Sans MS"
        CourierNew -> "Courier New"
        SourceSansPro -> "Source Sans Pro"
        Garamond -> "Garamond"
        Georgia -> "Georgia"
        TimesNewRoman -> "Serif"
        Tahoma -> "Tahoma"
        Trebuchet -> "Trebuchet MS"
        Verdana -> "Verdana"
        Arial -> "Sans Serif"
        Helvetica -> "Helvetica"

  in Enum.makeEnum allValues toString toHumanString

type alias ThemeID = Int
type alias Theme =
    { id : ThemeID
    -- ^ Note that only positive ids are valid, but we are also using negative
    -- ids as a hack to express (and disambiguate) certain read-only themes. See
    -- `getDomainThemes` in AdminOnly.UserGroupAdmin.UserGroupBrandingTab.
    , name : String
    , logo : String
    , font : Font
    , colors : Enum.Dict ColorIdentifier Color
    }

errorTheme : Theme
errorTheme =
  { id = -1
  , name = "### Internal Error ###"
  , logo = ""
  , font = defaultFont
  , colors = Enum.empty enumColorIdentifier
  }
