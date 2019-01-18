module Component.BrandedDomain.Json exposing (brandingDecoder, colorDecoder, encodeBranding)

import Color exposing (Color)
import Component.BrandedDomain.Data exposing (Branding)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Vendor.ColorPickerExtra exposing (color2Hex, hex2Color)


colorDecoder : Decoder Color
colorDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                hex2Color str
                    |> Maybe.map JD.succeed
                    |> Maybe.withDefault (JD.fail <| "Cannot parse color: " ++ str)
            )


brandingDecoder : Decoder Branding
brandingDecoder =
    let
        buildBranding id browserTitle mainDomain url favicon emailOriginator smsOriginator mailTheme signviewTheme serviceTheme loginTheme sentColor draftColor signedColor openedColor reviewedColor initatedColor cancelledColor deliveredColor participantColor1 participantColor2 participantColor3 participantColor4 participantColor5 participantColor6 =
            { brandedDomainId = id
            , browserTitle = browserTitle
            , mainDomain = mainDomain
            , url = url
            , favicon = favicon
            , emailOriginator = emailOriginator
            , smsOriginator = smsOriginator
            , themeIds =
                { emailTheme = mailTheme
                , signViewTheme = signviewTheme
                , serviceTheme = serviceTheme
                , loginTheme = loginTheme
                }
            , brandingColors =
                { sentColor = sentColor
                , draftColor = draftColor
                , signedColor = signedColor
                , openedColor = openedColor
                , reviewedColor = reviewedColor
                , initatedColor = initatedColor
                , cancelledColor = cancelledColor
                , deliveredColor = deliveredColor
                }
            , participantColors =
                { participantColor1 = participantColor1
                , participantColor2 = participantColor2
                , participantColor3 = participantColor3
                , participantColor4 = participantColor4
                , participantColor5 = participantColor5
                , participantColor6 = participantColor6
                }
            }
    in
    JD.succeed buildBranding
        |> JDP.required "id" JD.string
        |> JDP.required "browserTitle" JD.string
        |> JDP.required "mainDomain" JD.bool
        |> JDP.required "url" JD.string
        |> JDP.required "favicon" JD.string
        |> JDP.required "emailOriginator" JD.string
        |> JDP.required "smsOriginator" JD.string
        |> JDP.required "mailTheme" JD.string
        |> JDP.required "signviewTheme" JD.string
        |> JDP.required "serviceTheme" JD.string
        |> JDP.required "loginTheme" JD.string
        |> JDP.required "sentColor" colorDecoder
        |> JDP.required "draftColor" colorDecoder
        |> JDP.required "signedColor" colorDecoder
        |> JDP.required "openedColor" colorDecoder
        |> JDP.required "reviewedColor" colorDecoder
        |> JDP.required "initatedColor" colorDecoder
        |> JDP.required "cancelledColor" colorDecoder
        |> JDP.required "deliveredColor" colorDecoder
        |> JDP.required "participantColor1" colorDecoder
        |> JDP.required "participantColor2" colorDecoder
        |> JDP.required "participantColor3" colorDecoder
        |> JDP.required "participantColor4" colorDecoder
        |> JDP.required "participantColor5" colorDecoder
        |> JDP.required "participantColor6" colorDecoder


encodeBranding : Branding -> JE.Value
encodeBranding branding =
    JE.object <|
        [ ( "ready", JE.bool True )
        , ( "dirty", JE.bool True )
        , ( "id", JE.string branding.brandedDomainId )
        , ( "mainDomain", JE.bool branding.mainDomain )
        , ( "url", JE.string branding.url )
        , ( "browserTitle", JE.string branding.browserTitle )
        , ( "emailOriginator", JE.string branding.emailOriginator )
        , ( "smsOriginator", JE.string branding.smsOriginator )
        , ( "favicon", JE.string branding.favicon )
        , ( "mailTheme", JE.string branding.themeIds.emailTheme )
        , ( "signviewTheme", JE.string branding.themeIds.signViewTheme )
        , ( "serviceTheme", JE.string branding.themeIds.serviceTheme )
        , ( "loginTheme", JE.string branding.themeIds.loginTheme )
        , ( "cancelledColor"
          , JE.string <|
                color2Hex branding.brandingColors.cancelledColor
          )
        , ( "deliveredColor"
          , JE.string <|
                color2Hex branding.brandingColors.deliveredColor
          )
        , ( "draftColor"
          , JE.string <|
                color2Hex branding.brandingColors.draftColor
          )
        , ( "initatedColor"
          , JE.string <|
                color2Hex branding.brandingColors.initatedColor
          )
        , ( "openedColor"
          , JE.string <|
                color2Hex branding.brandingColors.openedColor
          )
        , ( "reviewedColor"
          , JE.string <|
                color2Hex branding.brandingColors.reviewedColor
          )
        , ( "sentColor"
          , JE.string <|
                color2Hex branding.brandingColors.sentColor
          )
        , ( "signedColor"
          , JE.string <|
                color2Hex branding.brandingColors.signedColor
          )
        , ( "participantColor1"
          , JE.string <|
                color2Hex branding.participantColors.participantColor1
          )
        , ( "participantColor2"
          , JE.string <|
                color2Hex branding.participantColors.participantColor2
          )
        , ( "participantColor3"
          , JE.string <|
                color2Hex branding.participantColors.participantColor3
          )
        , ( "participantColor4"
          , JE.string <|
                color2Hex branding.participantColors.participantColor4
          )
        , ( "participantColor5"
          , JE.string <|
                color2Hex branding.participantColors.participantColor5
          )
        , ( "participantColor6"
          , JE.string <|
                color2Hex branding.participantColors.participantColor6
          )
        ]
