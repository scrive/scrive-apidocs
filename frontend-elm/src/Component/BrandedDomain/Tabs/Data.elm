module Component.BrandedDomain.Tabs.Data exposing (OutMsg(..))

import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields exposing (BrandingFields)
import Component.Branding.CreateTheme as CreateTheme
import Component.Theme.Data exposing (Theme)


type OutMsg
    = SaveThemeMsg Theme
    | DeleteThemeMsg Theme
    | SaveBrandingMsg BrandingFields
    | CreateThemeMsg CreateTheme.NewTheme
