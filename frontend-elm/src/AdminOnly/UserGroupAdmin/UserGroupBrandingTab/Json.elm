module AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Json exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Vendor.ColorPickerExtra exposing (color2Hex, hex2Color)
import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)
import EnumExtra as Enum
import Maybe.Extra as Maybe

brandingDecoder : Decoder UserGroupBranding
brandingDecoder =
    let
        buildBranding browserTitle favicon smsOriginator mEmailTheme mSignviewTheme mServiceTheme =
            { browserTitle = browserTitle
            , smsOriginator = smsOriginator
            , favicon = favicon
            , themes = Enum.fromList enumThemeKind
                <| Maybe.values [mEmailTheme, mSignviewTheme, mServiceTheme]
            }
        maybeTheme : ThemeKind -> Decoder (Maybe (ThemeKind, ThemeID))
        maybeTheme k = JD.oneOf
          [ JD.null Nothing
          , JD.map (Maybe.map (\i -> (k,i)) << String.toInt) JD.string ]
        requiredThemeKind k = JDP.required (Enum.toString enumThemeKind k) <| maybeTheme k
    in
    JD.succeed buildBranding
        |> JDP.required "browserTitle" (JD.nullable JD.string)
        |> JDP.required "favicon" (JD.nullable JD.string)
        |> JDP.required "smsOriginator" (JD.nullable JD.string)
        |> requiredThemeKind EmailTheme
        |> requiredThemeKind SignViewTheme
        |> requiredThemeKind ServiceTheme


themeDecoder : Decoder Theme
themeDecoder =
  let buildTheme id name logo font colors =
          { id = id
          , fromDomainTheme = Nothing
          , name = name
          , logo = logo
          , font = font
          , colors = colors
          }
      decodeId = JD.string
        |> JD.andThen (\str -> case String.toInt str of
                                  Just int -> JD.succeed int
                                  Nothing -> JD.fail "failed to decode id string")
      decodeColor ident =
        JD.field (Enum.toString enumColorIdentifier ident)
          (JD.string |> JD.andThen
              (\str ->
                  hex2Color str
                      |> Maybe.map JD.succeed
                      |> Maybe.withDefault (JD.fail <| "Cannot parse color: " ++ str)
              )
          )
      decodeColors idents = case idents of
        [] -> JD.succeed (Enum.empty enumColorIdentifier)
        ident :: idents_ -> decodeColor ident
          |> JD.andThen (\color -> JD.map (Enum.insert ident color) <| decodeColors idents_)
  in JD.succeed buildTheme
        |> JDP.required "id" decodeId
        |> JDP.required "name" JD.string
        |> JDP.required "logo" JD.string
        |> JDP.required "font" JD.string
        |> JDP.custom (decodeColors (Enum.allValues enumColorIdentifier))


encodeTheme : Theme -> JE.Value
encodeTheme theme =
  let colorField (i, c) = (Enum.toString enumColorIdentifier i, JE.string <| color2Hex c)
      colorFields = List.map colorField <| Enum.toList theme.colors
  in JE.object <|
        [ ( "id", JE.string <| String.fromInt theme.id )
        , ( "name", JE.string theme.name )
        , ( "logo", JE.string theme.logo )
        , ( "font", JE.string theme.font )
        ] ++ colorFields


encodeUserGroupBranding : String -> UserGroupBranding -> JE.Value
encodeUserGroupBranding ugid branding =
  let themeKindField k =
        (Enum.toString enumThemeKind k,
          Maybe.withDefault JE.null
          <| Maybe.map (JE.string << String.fromInt)
          <| Enum.get k branding.themes)
      maybeString mStr = case mStr of
        Nothing -> JE.null
        Just str -> JE.string str
  in JE.object
      [ ( "companyid", JE.string ugid )
      , ( "browserTitle", maybeString branding.browserTitle )
      , ( "smsOriginator", maybeString branding.smsOriginator )
      , ( "favicon", maybeString branding.favicon )
      , themeKindField EmailTheme
      , themeKindField SignViewTheme
      , themeKindField ServiceTheme
      ]
