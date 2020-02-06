module Component.BrandedDomain.Tabs.BrandingPage.Data exposing (OutMsg(..))

import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields exposing (BrandingFields)
import Component.Branding.CreateTheme as CreateTheme


type OutMsg
    = SaveBrandingMsg BrandingFields
    | CreateThemeMsg CreateTheme.NewTheme
