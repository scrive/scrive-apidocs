module Utils.String where

import Data.String.Utils

import KontraPrelude

escapeString :: String -> String
escapeString =  concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c   = [c]

escapeHTML :: String -> String
escapeHTML =  concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape c   = [c]

unescapeHTML :: String -> String
unescapeHTML = replace "&lt;" "<"
             . replace "&gt;" ">"
             . replace "&amp;" "&"

-- | Pick first non-empty element or return empty list
firstNonEmpty :: [String] -> String
firstNonEmpty = fromMaybe "" . find (not . null) . map strip
