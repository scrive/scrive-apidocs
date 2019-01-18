module Util.Http exposing (formBody, httpErrorToString)

import Http
import Url


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadStatus code ->
            "Server returned HTTP status " ++ String.fromInt code

        Http.BadBody message ->
            "Error parsing response body: " ++ message

        _ ->
            "Encountered HTTP error"


formBody : List ( String, String ) -> Http.Body
formBody entries1 =
    let
        entries2 =
            List.map
                (\( name, value ) ->
                    Url.percentEncode name
                        ++ "="
                        ++ Url.percentEncode value
                )
                entries1

        body1 =
            String.join "&" entries2

        body2 =
            Http.stringBody
                "application/x-www-form-urlencoded; charset=UTF-8"
                body1
    in
    body2
