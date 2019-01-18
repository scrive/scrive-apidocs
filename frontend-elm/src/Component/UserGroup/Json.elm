module Component.UserGroup.Json exposing (brandingDecoder, encodeBranding, maybeStringField)

import Component.UserGroup.Data exposing (Branding, ThemeSet)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


brandingDecoder : Decoder Branding
brandingDecoder =
    let
        buildBranding userGroupId browserTitle favicon smsOriginator mailTheme signviewTheme serviceTheme =
            { userGroupId = userGroupId
            , browserTitle = browserTitle
            , smsOriginator = smsOriginator
            , favicon = favicon
            , themeIds =
                { emailTheme = mailTheme
                , signViewTheme = signviewTheme
                , serviceTheme = serviceTheme
                }
            }
    in
    JD.succeed buildBranding
        |> JDP.required "companyid" JD.string
        |> JDP.required "browserTitle" (JD.string |> JD.nullable)
        |> JDP.required "favicon" (JD.string |> JD.nullable)
        |> JDP.required "smsOriginator" (JD.string |> JD.nullable)
        |> JDP.required "mailTheme" (JD.string |> JD.nullable)
        |> JDP.required "signviewTheme" (JD.string |> JD.nullable)
        |> JDP.required "serviceTheme" (JD.string |> JD.nullable)


maybeStringField : Maybe String -> JE.Value
maybeStringField mStr =
    Maybe.withDefault JE.null <|
        Maybe.map JE.string mStr


encodeBranding : ThemeSet -> Branding -> JE.Value
encodeBranding defaultThemeSet branding =
    let
        maybeThemeId : String -> Maybe String -> JE.Value
        maybeThemeId defaultThemeId mThemeId =
            if mThemeId == Just defaultThemeId then
                JE.null

            else
                maybeStringField mThemeId
    in
    JE.object
        [ ( "ready", JE.bool True )
        , ( "dirty", JE.bool False )
        , ( "companyid", JE.string branding.userGroupId )
        , ( "browserTitle", maybeStringField branding.browserTitle )
        , ( "smsOriginator", maybeStringField branding.smsOriginator )
        , ( "favicon", maybeStringField branding.favicon )
        , ( "mailTheme"
          , maybeThemeId
                defaultThemeSet.emailTheme.id
                branding.themeIds.emailTheme
          )
        , ( "signviewTheme"
          , maybeThemeId
                defaultThemeSet.signViewTheme.id
                branding.themeIds.signViewTheme
          )
        , ( "serviceTheme"
          , maybeThemeId
                defaultThemeSet.serviceTheme.id
                branding.themeIds.serviceTheme
          )
        ]
