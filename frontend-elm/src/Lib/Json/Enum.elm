module Lib.Json.Enum exposing (..)

import Json.Decode as JD exposing (Decoder)
import Enum exposing (Enum)

enumDecoder : Enum a -> Decoder a
enumDecoder enum = JD.string |> JD.andThen (Enum.decodeEnumValue enum)
