module Lib.Flow exposing (flowPost)

import Http exposing (Body, Expect, Header, header, request)


flowRequest :
    { method : String
    , url : String
    , body : Body
    , expect : Expect msg
    , mXToken : Maybe String
    }
    -> Cmd msg
flowRequest r =
    Http.request
        { method = r.method
        , headers =
            case r.mXToken of
                Just xtoken ->
                    [ xtokenHeader xtoken ]

                Nothing ->
                    []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


flowPost :
    { url : String
    , body : Body
    , expect : Expect msg
    , xtoken : String
    }
    -> Cmd msg
flowPost r =
    flowRequest
        { method = "POST"
        , url = r.url
        , body = r.body
        , expect = r.expect
        , mXToken = Just r.xtoken
        }



-- TODO: Add functions for more http methods


xtokenHeader : String -> Header
xtokenHeader val =
    header "X-Scrive-XToken" val
