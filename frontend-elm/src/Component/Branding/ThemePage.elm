module Component.Branding.ThemePage exposing (defaultPreview, viewThemePage)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div, text)


defaultPreview : Html msg
defaultPreview =
    div []
        [ text "Select a theme to preview"
        ]


viewThemePage :
    Html msg1
    -> Maybe (Html msg2)
    -> Html (Pair.Msg msg1 msg2)
viewThemePage themeBody1 mPreviewBody1 =
    let
        previewBody2 =
            Maybe.withDefault
                defaultPreview
                mPreviewBody1
    in
    Grid.row []
        [ Grid.col
            [ Col.sm12, Col.md5 ]
            [ Html.map Pair.FirstMsg themeBody1
            ]
        , Grid.col
            [ Col.sm12, Col.md7 ]
            [ Html.map Pair.SecondMsg previewBody2
            ]
        ]
