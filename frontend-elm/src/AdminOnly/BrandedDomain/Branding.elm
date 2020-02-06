module AdminOnly.BrandedDomain.Branding exposing (BrandedDomain, brandedDomainColorKeyAccessPairs, brandedDomainDecoder, brandedDomainKeyAccessPairs, brandedDomainListDecoder, colorDecoder)

import Color exposing (Color)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Maybe as M
import Vendor.ColorPickerExtra exposing (hex2Color)


type alias BrandedDomain =
    { browserTitle : String
    , cancelledColor : Color
    , deliveredColor : Color
    , draftColor : Color
    , emailOriginator : String
    , favicon : String
    , id : String
    , initatedColor : Color
    , loginTheme : String
    , mailTheme : String
    , mainDomain : Bool
    , openedColor : Color
    , participantColor1 : Color
    , participantColor2 : Color
    , participantColor3 : Color
    , participantColor4 : Color
    , participantColor5 : Color
    , participantColor6 : Color
    , reviewedColor : Color
    , sentColor : Color
    , serviceTheme : String
    , signedColor : Color
    , signviewTheme : String
    , smsOriginator : String
    , url : String
    }


colorDecoder : Decoder Color
colorDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                hex2Color str
                    |> M.map JD.succeed
                    |> M.withDefault (JD.fail <| "Cannot parse color: " ++ str)
            )


brandedDomainColorKeyAccessPairs : List ( String, ( BrandedDomain -> Color, BrandedDomain -> Color -> BrandedDomain ) )
brandedDomainColorKeyAccessPairs =
    [ ( "cancelledColor", ( .cancelledColor, \bd v -> { bd | cancelledColor = v } ) )
    , ( "deliveredColor", ( .deliveredColor, \bd v -> { bd | deliveredColor = v } ) )
    , ( "draftColor", ( .draftColor, \bd v -> { bd | draftColor = v } ) )
    , ( "initatedColor", ( .initatedColor, \bd v -> { bd | initatedColor = v } ) )
    , ( "openedColor", ( .openedColor, \bd v -> { bd | openedColor = v } ) )
    , ( "participantColor1", ( .participantColor1, \bd v -> { bd | participantColor1 = v } ) )
    , ( "participantColor2", ( .participantColor2, \bd v -> { bd | participantColor2 = v } ) )
    , ( "participantColor3", ( .participantColor3, \bd v -> { bd | participantColor3 = v } ) )
    , ( "participantColor4", ( .participantColor4, \bd v -> { bd | participantColor4 = v } ) )
    , ( "participantColor5", ( .participantColor5, \bd v -> { bd | participantColor5 = v } ) )
    , ( "participantColor6", ( .participantColor6, \bd v -> { bd | participantColor6 = v } ) )
    , ( "reviewedColor", ( .reviewedColor, \bd v -> { bd | reviewedColor = v } ) )
    , ( "sentColor", ( .sentColor, \bd v -> { bd | sentColor = v } ) )
    , ( "signedColor", ( .signedColor, \bd v -> { bd | signedColor = v } ) )
    ]


brandedDomainKeyAccessPairs : List ( String, ( BrandedDomain -> String, BrandedDomain -> String -> BrandedDomain ) )
brandedDomainKeyAccessPairs =
    [ ( "browserTitle", ( .browserTitle, \bd v -> { bd | browserTitle = v } ) )
    , ( "emailOriginator", ( .emailOriginator, \bd v -> { bd | emailOriginator = v } ) )
    , ( "favicon", ( .favicon, \bd v -> { bd | favicon = v } ) )
    , ( "id", ( .id, \bd v -> { bd | id = v } ) )
    , ( "loginTheme", ( .loginTheme, \bd v -> { bd | loginTheme = v } ) )
    , ( "mailTheme", ( .mailTheme, \bd v -> { bd | mailTheme = v } ) )
    , ( "serviceTheme", ( .serviceTheme, \bd v -> { bd | serviceTheme = v } ) )
    , ( "signviewTheme", ( .signviewTheme, \bd v -> { bd | signviewTheme = v } ) )
    , ( "smsOriginator", ( .smsOriginator, \bd v -> { bd | smsOriginator = v } ) )
    , ( "url", ( .url, \bd v -> { bd | url = v } ) )
    ]


brandedDomainListDecoder : Decoder (List BrandedDomain)
brandedDomainListDecoder =
    JD.field "domains" <|
        JD.list <|
            brandedDomainDecoder


brandedDomainDecoder : Decoder BrandedDomain
brandedDomainDecoder =
    JD.succeed BrandedDomain
        |> JDP.required "browserTitle" JD.string
        |> JDP.required "cancelledColor" colorDecoder
        |> JDP.required "deliveredColor" colorDecoder
        |> JDP.required "draftColor" colorDecoder
        |> JDP.required "emailOriginator" JD.string
        |> JDP.required "favicon" JD.string
        |> JDP.required "id" JD.string
        |> JDP.required "initatedColor" colorDecoder
        |> JDP.required "loginTheme" JD.string
        |> JDP.required "mailTheme" JD.string
        |> JDP.required "mainDomain" JD.bool
        |> JDP.required "openedColor" colorDecoder
        |> JDP.required "participantColor1" colorDecoder
        |> JDP.required "participantColor2" colorDecoder
        |> JDP.required "participantColor3" colorDecoder
        |> JDP.required "participantColor4" colorDecoder
        |> JDP.required "participantColor5" colorDecoder
        |> JDP.required "participantColor6" colorDecoder
        |> JDP.required "reviewedColor" colorDecoder
        |> JDP.required "sentColor" colorDecoder
        |> JDP.required "serviceTheme" JD.string
        |> JDP.required "signedColor" colorDecoder
        |> JDP.required "signviewTheme" JD.string
        |> JDP.required "smsOriginator" JD.string
        |> JDP.required "url" JD.string
