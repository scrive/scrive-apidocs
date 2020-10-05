module AdminOnly.Types.UserGroup.Address exposing
    ( Address
    , decoder
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP


type alias Address =
    { address : String
    , city : String
    , country : String
    , zip : String
    , companyNumber : String
    }


decoder : Decoder Address
decoder =
    JD.succeed Address
        |> DP.required "address" JD.string
        |> DP.required "city" JD.string
        |> DP.required "country" JD.string
        |> DP.required "zip" JD.string
        |> DP.required "companynumber" JD.string
