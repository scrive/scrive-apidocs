module Language exposing
    ( Language(..)
    , decoder
    , enumLanguage
    )

import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as D exposing (Decoder)


type Language
    = Swedish
    | English
    | German
    | French
    | Dutch
    | Italian
    | Norwegian
    | Portuguese
    | Spanish
    | Danish
    | Greek
    | Finnish
    | Icelandic
    | Estonian
    | Latvian
    | Czech
    | Polish


allLanguages : List Language
allLanguages =
    [ Swedish
    , English
    , German
    , French
    , Dutch
    , Italian
    , Norwegian
    , Portuguese
    , Spanish
    , Danish
    , Greek
    , Finnish
    , Icelandic
    , Estonian
    , Latvian
    , Czech
    , Polish
    ]


enumLanguage : Enum Language
enumLanguage =
    makeEnum allLanguages encodeLanguageShort encodeLanguage


decoder : String -> Decoder Language
decoder languageString =
    case findEnumValue enumLanguage languageString of
        Err _ ->
            D.fail <| "Cannot parse language: " ++ languageString

        Ok language ->
            D.succeed language


encodeLanguageShort : Language -> String
encodeLanguageShort lang =
    case lang of
        Swedish ->
            "sv"

        English ->
            "en"

        German ->
            "de"

        French ->
            "fr"

        Dutch ->
            "nl"

        Italian ->
            "it"

        Norwegian ->
            "no"

        Portuguese ->
            "pt"

        Spanish ->
            "es"

        Danish ->
            "da"

        Greek ->
            "el"

        Finnish ->
            "fi"

        Icelandic ->
            "is"

        Estonian ->
            "et"

        Latvian ->
            "lv"

        Czech ->
            "cs"

        Polish ->
            "pl"


encodeLanguage : Language -> String
encodeLanguage lang =
    case lang of
        Swedish ->
            "Swedish"

        English ->
            "English"

        German ->
            "German"

        French ->
            "French"

        Dutch ->
            "Dutch"

        Italian ->
            "Italian"

        Norwegian ->
            "Norwegian"

        Portuguese ->
            "Portuguese"

        Spanish ->
            "Spanish"

        Danish ->
            "Danish"

        Greek ->
            "Greek"

        Finnish ->
            "Finnish"

        Icelandic ->
            "Icelandic"

        Estonian ->
            "Estonian"

        Latvian ->
            "Latvian"

        Czech ->
            "Czech"

        Polish ->
            "Polish"
