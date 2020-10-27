module Authentication exposing
    ( Authentication(..)
    , decoder
    , enumAuthentication
    )

import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as D exposing (Decoder)


type Authentication
    = Native
    | SSO


enumAuthentication : Enum Authentication
enumAuthentication =
    makeEnum allAuthentications encodeAuthenticationShort encodeAuthentication


decoder : String -> Decoder Authentication
decoder authString =
    case findEnumValue enumAuthentication authString of
        Err _ ->
            D.fail <| "Cannot parse authentication: " ++ authString

        Ok language ->
            D.succeed language


allAuthentications : List Authentication
allAuthentications =
    [ Native
    , SSO
    ]


encodeAuthenticationShort : Authentication -> String
encodeAuthenticationShort auth =
    case auth of
        Native ->
            "native"

        SSO ->
            "sso"


encodeAuthentication : Authentication -> String
encodeAuthentication auth =
    case auth of
        Native ->
            "Password login"

        SSO ->
            "SSO"
