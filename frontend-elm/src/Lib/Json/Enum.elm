module Lib.Json.Enum exposing (..)

import Enum exposing (Enum)
import Json.Decode as JD exposing (Decoder)


enumDecoder : Enum a -> Decoder a
enumDecoder enum =
    JD.string |> JD.andThen (Enum.decodeEnumValue enum)
