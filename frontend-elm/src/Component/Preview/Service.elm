module Component.Preview.Service exposing (view)

import Component.Theme.Data exposing (Theme)
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (class, src, style)
import Vendor.ColorPickerExtra exposing (color2Hex)


view : Theme -> Html msg
view theme =
    let
        themeColors =
            theme.themeColors

        actionTextColor =
            themeColors.actionColors.textColor

        actionBackgroundColor =
            themeColors.actionColors.backgroundColor

        brandTextColor =
            themeColors.brandColors.textColor

        brandBackgroundColor =
            themeColors.brandColors.backgroundColor
    in
    div [ class "service-preview" ]
        [ div
            [ class "service-preview-header"
            , style "background-color" <|
                color2Hex brandBackgroundColor
            ]
            [ div
                [ class "service-preview-logo" ]
                [ a [ class "hoverable" ] [ img [ src theme.logo ] [] ] ]
            , div [ class "service-preview-innerheader" ]
                [ a
                    [ class "button service-preview-header-button main button-small"
                    , style "background-color" <|
                        color2Hex brandBackgroundColor
                    , style "border-color" <|
                        color2Hex brandTextColor
                    ]
                    [ div
                        [ class "label"
                        , style "color" <|
                            color2Hex brandTextColor
                        ]
                        [ span []
                            [ text "Start new process" ]
                        ]
                    ]
                , a
                    [ class "button service-preview-header-button main button-small"
                    , style "background-color" <| color2Hex brandBackgroundColor
                    , style "border-color" <| color2Hex brandTextColor
                    ]
                    [ div
                        [ class "label"
                        , style "color" <| color2Hex brandTextColor
                        ]
                        [ span []
                            [ text "Start from template" ]
                        ]
                    ]
                , div
                    [ class "service-preview-header-text"
                    , style "color" <| color2Hex brandTextColor
                    ]
                    [ a [ class "hoverable" ]
                        [ text "E-archive" ]
                    , span []
                        []
                    ]
                , div
                    [ class "service-preview-header-text"
                    , style "color" <| color2Hex brandTextColor
                    ]
                    [ a [ class "hoverable" ]
                        [ text "Account" ]
                    , span []
                        []
                    ]
                , div
                    [ class "service-preview-header-text"
                    , style "color" <| color2Hex brandTextColor
                    ]
                    [ a [ class "hoverable" ]
                        [ text "Log out" ]
                    ]
                ]
            ]
        , div [ class "service-preview-content" ]
            [ img [ src "/img/user_details_preview.png" ]
                []
            , a
                [ class "button action button-small"
                , style "background-color" <| color2Hex actionBackgroundColor
                , style "color" <| color2Hex actionTextColor
                ]
                [ div [ class "label" ]
                    [ span []
                        [ text "Save" ]
                    ]
                ]
            ]
        , div [ class "service-preview-footer" ]
            [ div []
                [ text "Powered by Scrive" ]
            ]
        ]
