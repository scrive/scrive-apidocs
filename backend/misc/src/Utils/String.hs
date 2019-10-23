module Utils.String where

import qualified Data.Text as T

escapeString :: Text -> Text
escapeString = T.concatMap escape
  where
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '&'  = "&amp;"
    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape c    = T.singleton c

escapeHTML :: Text -> Text
escapeHTML = T.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape c   = T.singleton c

unescapeHTML :: Text -> Text
unescapeHTML = T.replace "&lt;" "<" . T.replace "&gt;" ">" . T.replace "&amp;" "&"

-- | Pick first non-empty element or return empty list
firstNonEmpty :: [Text] -> Text
firstNonEmpty = fromMaybe "" . find (not . T.null) . map T.strip
