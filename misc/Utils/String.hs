module Utils.String(concatChunks, pureString, maxLev, maybeS, escapeString, escapeHTML, unescapeHTML, firstNonEmpty) where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

concatChunks :: BSL.ByteString -> BS.ByteString
concatChunks = BS.concat . BSL.toChunks

pureString :: String -> String
pureString = unwords . words . filter (not . isControl)

maxLev :: String -> String -> Int -> Bool
maxLev s1 s2 m = maxLev' (' ':s1) (' ':s2) m (length s1) (length s2)

maxLev' :: String -> String -> Int -> Int -> Int -> Bool
maxLev' _ _ (-1) _ _ = False
maxLev' s1 s2 m i j
  | i == 0 = m >= j
  | j == 0 = m >= i
  | (s1 !! i) == (s2 !! j) = maxLev' s1 s2 m (i - 1) (j - 1)
  | otherwise = any id [maxLev' s1 s2 (m - 1) (i - 1) j
                       ,maxLev' s1 s2 (m - 1) i (j - 1)
                       ,maxLev' s1 s2 (m - 1) (i - 1) (j - 1)]

maybeS :: Maybe String -> Maybe String
maybeS (Just "") = Nothing
maybeS s  = s

escapeString :: String -> String
escapeString =  concatMap escape
        where escape '<' = "&lt;"
              escape '>' = "&gt;"
              escape '&' = "&amp;"
              escape '"' = "\\\""
              escape '\\' = "\\\\"
              escape c   = [c]


escapeHTML :: String -> String
escapeHTML =  concatMap escape
        where escape '<' = "&lt;"
              escape '>' = "&gt;"
              escape '&' = "&amp;"
              escape c   = [c]

unescapeHTML :: String -> String
unescapeHTML = replace "&lt;" "<"  .
               replace "&gt;" ">"  .
               replace "&amp;" "&" 


-- | Pick first non-empty element or return empty list
firstNonEmpty :: [String] -> String
firstNonEmpty = fromMaybe "" . find (not . null) . map strip
