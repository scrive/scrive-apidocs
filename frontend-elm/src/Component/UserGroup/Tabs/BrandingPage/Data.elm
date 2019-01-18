module Component.UserGroup.Tabs.BrandingPage.Data exposing (OutMsg(..))

import Component.Branding.CreateTheme as CreateTheme
import Component.UserGroup.Data exposing (BrandingFields)


type OutMsg
    = SaveBrandingMsg BrandingFields
    | CreateThemeMsg CreateTheme.NewTheme
