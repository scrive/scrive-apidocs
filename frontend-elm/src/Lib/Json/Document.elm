module Lib.Json.Document exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Lib.Json.ID exposing (idDecoder)
import Lib.Json.SignatoryLink exposing (signatoryLinkDecoder)
import Lib.Types.Document exposing (..)
import Lib.Types.UrlString exposing (UrlString(..))
import Utils exposing (datetimeDecoder)


documentStatusDecoder : Decoder DocumentStatus
documentStatusDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "preparation" ->
                        JD.succeed Preparation

                    "pending" ->
                        JD.succeed Pending

                    "closed" ->
                        JD.succeed Closed

                    "canceled" ->
                        JD.succeed Canceled

                    "timedout" ->
                        JD.succeed Timedout

                    "rejected" ->
                        JD.succeed Rejected

                    "document_error" ->
                        JD.succeed DocumentError

                    _ ->
                        JD.fail <| "Cannot parse DocumentStatus: " ++ s
            )


documentDecoder : Decoder Document
documentDecoder =
    JD.succeed DocumentRecord
        |> JDP.required "id" idDecoder
        |> JDP.required "title" JD.string
        |> JDP.required "parties" (JD.list signatoryLinkDecoder)
        |> JDP.required "ctime" datetimeDecoder
        |> JDP.required "mtime" datetimeDecoder
        |> JDP.required "status" documentStatusDecoder
        |> JDP.required "lang" (JD.string |> JD.map Lang)
        |> JDP.required "is_template" JD.bool
        |> JDP.required "is_shared" JD.bool
        |> JDP.optional "shareable_link" (JD.string |> JD.map (Just << UrlString)) Nothing
        |> JD.map Document
