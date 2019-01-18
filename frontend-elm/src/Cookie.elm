module Cookie exposing (cookieString2Dict, decoder)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import List as L
import Maybe as M
import String exposing (join, split, trim)
import Url


cookieString2Dict : String -> Maybe (Dict String String)
cookieString2Dict cookie =
    let
        decodePair keyValueString =
            case split "=" keyValueString of
                keyString :: rest ->
                    case ( Url.percentDecode keyString, Url.percentDecode (join "=" rest) ) of
                        ( Just key, Just value ) ->
                            [ ( key, value ) ]

                        _ ->
                            []

                _ ->
                    []

        allAreDecoded =
            L.all (not << L.isEmpty) pairs

        pairs =
            L.map decodePair <| L.map trim <| split ";" cookie
    in
    if allAreDecoded then
        Just <| Dict.fromList <| L.concat pairs

    else
        Nothing


decoder : String -> Decoder (Dict String String)
decoder cookie =
    M.withDefault (D.fail "Cannot parse cookies.") <| M.map D.succeed <| cookieString2Dict cookie
