module Lib.Misc.Document exposing (..)

import Lib.Misc.SignatoryLink exposing (..)
import Lib.Types.Document exposing (..)
import Lib.Types.SignatoryLink exposing (..)
import List.Extra exposing (find)


getAuthorName : Document -> Maybe String
getAuthorName (Document doc) =
    find
        (\sl ->
            case sl of
                SignatoryLink { isAuthor } ->
                    isAuthor
        )
        doc.parties
        |> Maybe.andThen getSmartName
