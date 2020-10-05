module AdminOnly.UserGroupAdmin.UserGroupBrandingTab.ThemePreview exposing (viewPreviewTheme)

import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)
import Color exposing (rgb)
import Component.Preview.Email
import Component.Preview.Service
import Component.Preview.SignView
import Component.Theme.Data
import EnumExtra as Enum
import Html exposing (Html)
import Lib.Types.Theme exposing (..)



-- 'massage' `Theme` into `Component.Theme.Data.Theme`


massage : Theme -> Component.Theme.Data.Theme
massage theme =
    let
        colorPair kind =
            { textColor =
                Maybe.withDefault (rgb 0 0 0) <|
                    Enum.get { kind = kind, component = TextColor } theme.colors
            , backgroundColor =
                Maybe.withDefault (rgb 1 1 1) <|
                    Enum.get { kind = kind, component = BackgroundColor } theme.colors
            }
    in
    { id = ""
    , name = theme.name
    , font = theme.font
    , logo = theme.logo
    , themeColors =
        { brandColors = colorPair BrandColors
        , actionColors = colorPair ActionColors
        , secondaryActionColors = colorPair SecondaryActionColors
        , positiveColors = colorPair PositiveColors
        , negativeColors = colorPair NegativeColors
        }
    }


viewPreviewTheme : ThemeKind -> Theme -> Html msg
viewPreviewTheme kind =
    case kind of
        EmailTheme ->
            Component.Preview.Email.view << massage

        SignViewTheme ->
            Component.Preview.SignView.view << massage

        ServiceTheme ->
            Component.Preview.Service.view << massage
