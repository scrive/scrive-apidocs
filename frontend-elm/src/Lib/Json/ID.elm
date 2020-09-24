module Lib.Json.ID exposing (..)

import Json.Decode as JD exposing (Decoder)

import Lib.Types.ID exposing (..)
import Lib.Json.Extras exposing (..)

idDecoder : Decoder (ID a)
idDecoder = JD.string |> JD.map unsafeToId
