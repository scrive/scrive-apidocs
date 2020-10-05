module AdminOnly.Types.UserGroup.DeletionRequest exposing
    ( DeletionRequest
    , decoder
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP


type alias DeletionRequest =
    { requestedBy : String
    , requestedDeletionDate : String
    , signedOffBy : Maybe String
    }


decoder : Decoder DeletionRequest
decoder =
    JD.succeed DeletionRequest
        |> DP.required "requested_by" JD.string
        |> DP.required "requested_deletion_date" JD.string
        |> DP.optional "signed_off_by" (JD.nullable JD.string) Nothing
