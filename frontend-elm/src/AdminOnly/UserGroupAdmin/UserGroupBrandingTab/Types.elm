module AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)

import EnumExtra as Enum exposing (Enum)
import Lib.Theme.Types exposing (ThemeID)

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
    -- ^ If a theme kind is missing then that theme is inherited from the
    -- branded domain.
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
