module Component.Preview.Email exposing (view)

import Component.Theme.Data exposing (Theme)
import Html exposing (Html, a, div, img, p, strong, text)
import Html.Attributes exposing (class, src, style)
import Vendor.ColorPickerExtra exposing (color2Hex)


view : Theme -> Html msg
view theme =
    div
        [ class "email-preview"
        , style "backgroundColor" <|
            color2Hex theme.themeColors.brandColors.backgroundColor
        ]
        [ div [ class "logo-wrapper" ] [ img [ src theme.logo ] [] ]
        , div [ class "content-container", style "fontFamily" theme.font ]
            [ div [ class "content" ]
                [ div [ class "document-preview" ] [ img [ src "/img/document.png" ] [] ]
                , div [ class "invitation" ]
                    [ p []
                        [ -- localization.companyBranding.brandingPreview.emailContent
                          strong [] <| [ text "John Smith" ]
                        , text " has invited you to e-sign the document "
                        , strong [] <| [ text "Demo Document" ]
                        , text "."
                        ]
                    , p []
                        [ strong [] <|
                            [ -- localization.companyBranding.brandingPreview.emailInstructions
                              text "Click the button for further instructions."
                            ]
                        ]
                    , a
                        [ class "button action"
                        , style "backgroundColor" <| color2Hex theme.themeColors.actionColors.backgroundColor
                        , style "color" <| color2Hex theme.themeColors.actionColors.textColor
                        ]
                        [ div [ class "label" ]
                            [ -- localization.companyBranding.brandingPreview.emailButtonLabel
                              text "Go to document"
                            ]
                        ]
                    ]
                ]
            , div [ class "footer" ]
                [ p []
                    [ -- localization.companyBranding.brandingPreview.emailFooter
                      text "This email contains confidential information and should not be forwarded. Disclosure, copying, distribution or other processing of the information contained in this message is prohibited and may be unlawful. If you have received this in error please notify the sender and delete immediately."
                    ]
                ]
            ]
        ]
