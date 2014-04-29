module Utils.String(concatChunks, pureString, maxLev, maybeS, escapeString, trim) where

import Data.Char
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

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
