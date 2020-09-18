module Lib.Types.UrlString exposing (..)

-- we might need syntactic equality, so we can't use a semantic Url type
type UrlString = UrlString String
