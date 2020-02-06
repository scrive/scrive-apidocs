module Component.Preview.Login exposing (view)

import Component.Theme.Data exposing (Theme)
import Html exposing (Html, a, div, img, input, p, span, text)
import Html.Attributes exposing (class, placeholder, readonly, src, style, type_, value)
import Url
import Vendor.ColorPickerExtra exposing (color2Hex)


view : Theme -> Html msg
view theme =
    let
        themeColors =
            theme.themeColors

        brandTextColor =
            themeColors.brandColors.textColor

        brandBackgroundColor =
            themeColors.brandColors.backgroundColor
    in
    div
        [ class "login-preview"
        , style "background-color" <|
            color2Hex brandBackgroundColor
        ]
        [ div [ class "logo-wrapper" ]
            [ img [ src <| theme.logo ] []
            , div
                [ class "divider-line"
                , style
                    "background-image"
                    ("url(/colored_image?file=divider-line.png&color="
                        ++ Url.percentEncode
                            (color2Hex brandTextColor)
                        ++ ")"
                    )
                ]
                []
            , p
                [ class "small"
                , style "color" <|
                    color2Hex brandTextColor
                ]
                [ text "E-signing powered by Scrive" ]
            ]
        , div [ class "content-container", style "font-family" theme.font ]
            [ div [ class "content" ]
                [ div [ style "width" "200px", class "info-text-input readonly" ]
                    [ input [ type_ "text", readonly True, placeholder "Email address", value "" ] []
                    , div [ class "after" ] []
                    ]
                , div [ style "width" "200px", class "info-text-input readonly" ]
                    [ input [ type_ "text", readonly True, placeholder "Password", value "" ] []
                    , div [ class "internal-button-wrapper" ]
                        [ div [ class "internal-button" ]
                            [ text "Forgot?" ]
                        ]
                    , div [ class "after" ] []
                    ]
                , a
                    [ class "button main"
                    , style "background-color" <| color2Hex brandBackgroundColor
                    , style "border-color" <| color2Hex brandTextColor
                    , style "color" <| color2Hex brandTextColor
                    ]
                    [ div [ class "label" ] [ span [] [ text "Log in" ] ] ]
                , p [ style "color" <| color2Hex brandTextColor ]
                    [ span [] [ text "Don't have an account?" ]
                    , a [ style "color" <| color2Hex brandTextColor ] [ text "Sign up for free" ]
                    ]
                , div [ class "select-container" ]
                    [ div
                        [ class "select "
                        , style "width" "150px"
                        , style "background-color" <| color2Hex brandBackgroundColor
                        , style "border" ("1px solid " ++ color2Hex brandTextColor)
                        , style "color" <| color2Hex brandTextColor
                        ]
                        [ div [ class "select-button" ]
                            [ div [ class "select-button-left" ] []
                            , div
                                [ class "select-button-label"
                                , style "width" "150px"
                                , style "background-color" <|
                                    color2Hex brandBackgroundColor
                                , style "color" <| color2Hex brandTextColor
                                ]
                                [ text "English" ]
                            , div
                                [ class "select-button-right"
                                , style "background-image"
                                    ("url(/colored_image?file=select-arrow-for-branding.png&color="
                                        ++ Url.percentEncode (color2Hex brandTextColor)
                                        ++ ")"
                                    )
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
