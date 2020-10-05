module AdminOnly.Types.UserGroup.SmsProvider exposing
    ( SmsProvider(..)
    , decoder
    , enum
    )

import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as JD exposing (Decoder)


type SmsProvider
    = SmsDefault
    | SmsTeliaCallGuide


decoder : String -> Decoder SmsProvider
decoder smsProviderString =
    case findEnumValue enum smsProviderString of
        Err _ ->
            JD.fail <| "Cannot parse sms provider: " ++ smsProviderString

        Ok smsProvider ->
            JD.succeed smsProvider


enum : Enum SmsProvider
enum =
    let
        values =
            [ SmsDefault, SmsTeliaCallGuide ]

        toStr provides =
            case provides of
                SmsDefault ->
                    "SMSDefault"

                SmsTeliaCallGuide ->
                    "SMSTeliaCallGuide"
    in
    makeEnum values toStr toStr
