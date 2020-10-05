module Lib.Json.Extras exposing (..)

import Json.Decode as JD exposing (Decoder)
import Url exposing (Url)



-- | Generalisation of withDefault : a -> Maybe a -> a; corresponds to
-- fromMaybeM (and fromMaybe).


withDefaultDecoder : Decoder a -> Decoder (Maybe a) -> Decoder a
withDefaultDecoder da dma =
    dma
        |> JD.andThen
            (\ma ->
                case ma of
                    Just a ->
                        JD.succeed a

                    Nothing ->
                        da
            )


quotedIntDecoder : Decoder Int
quotedIntDecoder =
    JD.string
        |> JD.map String.toInt
        |> withDefaultDecoder (JD.fail "Failed to decode quoted Int.")


urlDecoder : Decoder Url
urlDecoder =
    JD.string |> JD.map Url.fromString |> withDefaultDecoder (JD.fail "Failed to parse Url")


constantStringDecoder : String -> Decoder String
constantStringDecoder str =
    JD.string
        |> JD.andThen
            (\str_ ->
                if str == str_ then
                    JD.succeed str

                else
                    JD.fail <|
                        "Failed to decode constant, "
                            ++ "expected \""
                            ++ str
                            ++ "\" "
                            ++ "but got \""
                            ++ str_
                            ++ "\"."
            )
