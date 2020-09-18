module Lib.Misc.Http exposing (..)

import Http exposing (..)
import Json.Encode exposing (..)
import Url

encodeError : Error -> Value
encodeError error = case error of
  BadUrl str -> object [("tag", string "BadUrl"), ("url", string str)]
  Timeout -> object [("tag", string "Timeout")]
  NetworkError -> object [("tag", string "NetworkError")]
  BadStatus code -> object [("tag", string "BadStatus"), ("status", int code)]
  BadBody body -> object [("tag", string "BadBody"), ("body", string body)]

formBody : { a | xtoken : String } -> List ( String, String ) -> Body
formBody globals object =
    (object ++ [ ( "xtoken", globals.xtoken ) ])
        |> List.map (\(n, v) -> Url.percentEncode n ++ "=" ++ Url.percentEncode v)
        |> String.join "&"
        |> stringBody "application/x-www-form-urlencoded; charset=UTF-8"
