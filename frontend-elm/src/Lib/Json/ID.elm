module Lib.Json.ID exposing (..)

import Json.Decode as JD exposing (Decoder)
import Lib.Json.Extras exposing (..)
import Lib.Types.ID exposing (..)


idDecoder : Decoder (ID a)
idDecoder =
    JD.string |> JD.map unsafeToId
