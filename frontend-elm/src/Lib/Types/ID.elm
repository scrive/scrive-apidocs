module Lib.Types.ID exposing (..)


type ID a
    = ID String


unsafeToId : String -> ID a
unsafeToId =
    ID


showId : ID a -> String
showId (ID id) =
    id
