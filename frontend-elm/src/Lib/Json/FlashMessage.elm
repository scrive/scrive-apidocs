module Lib.Json.FlashMessage exposing (..)

import Json.Decode as JD exposing (Decoder)
import Lib.Json.Extras exposing (constantStringDecoder)
import Lib.Types.FlashMessage exposing (FlashMessage(..))


flashMessageDecoder : Decoder FlashMessage
flashMessageDecoder =
    JD.oneOf
        [ JD.field "type" (constantStringDecoder "success")
            |> JD.andThen
                (\_ -> JD.field "content" JD.string)
            |> JD.map FlashSuccess
        , JD.field "type" (constantStringDecoder "error")
            |> JD.andThen
                (\_ -> JD.field "content" JD.string)
            |> JD.map FlashError
        ]
