module Component.UserGroup.Data exposing (Branding, BrandingFields, ThemeSet)

import Component.Branding.CommonFields exposing (CommonFields)
import Component.Theme.Data exposing (Theme)


type alias ThemeSet =
    { emailTheme : Theme
    , signViewTheme : Theme
    , serviceTheme : Theme
    }


type alias Branding =
    { userGroupId : String
    , browserTitle : Maybe String
    , smsOriginator : Maybe String
    , favicon : Maybe String
    , themeIds :
        { emailTheme : Maybe String
        , signViewTheme : Maybe String
        , serviceTheme : Maybe String
        }
    }


type alias BrandingFields =
    { themeSet : ThemeSet
    , commonFields : CommonFields
    }
