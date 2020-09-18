module Lib.Misc.Document exposing (..)

import List.Extra exposing (find)

import Lib.Types.Document exposing (..)
import Lib.Types.SignatoryLink exposing (..)
import Lib.Misc.SignatoryLink exposing (..)

getAuthorName : Document -> Maybe String
getAuthorName (Document doc) =
  find (\sl -> case sl of SignatoryLink {isAuthor} -> isAuthor) doc.parties
  |> Maybe.andThen getSmartName
