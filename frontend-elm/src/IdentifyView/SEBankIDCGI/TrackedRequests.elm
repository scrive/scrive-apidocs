module IdentifyView.SEBankIDCGI.TrackedRequests exposing (..)

import Http exposing (..)

-- A 'tracker' string identifies http requests, which allows us to cancel them.
type Tracker = Tracker String

cancelRequest : Tracker -> Cmd msg
cancelRequest (Tracker tracker) = Http.cancel tracker

trackedPostRequest :
    { url : String
    , body : Body
    , expect : Expect msg
    , tracker : Tracker
    }
  -> Cmd msg
trackedPostRequest arg = request
  { method = "POST"
  , headers = []
  , url = arg.url
  , body = arg.body
  , expect = arg.expect
  , timeout = Nothing
  , tracker = case arg.tracker of Tracker tracker -> Just tracker
  }

trackedGetRequest :
    { url : String
    , expect : Expect msg
    , tracker : Tracker
    }
  -> Cmd msg
trackedGetRequest arg = request
  { method = "GET"
  , headers = []
  , url = arg.url
  , body = emptyBody
  , expect = arg.expect
  , timeout = Nothing
  , tracker = case arg.tracker of Tracker tracker -> Just tracker
  }
