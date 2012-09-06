module Utils.String where

import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

concatChunks :: BSL.ByteString -> BS.ByteString
concatChunks = BS.concat . BSL.toChunks

pureString :: String -> String
pureString = unwords . words . filter (not . isControl)

{- |
    Calculate the Levenshtein distance (edit distance) between two strings
 -}
levenshtein :: String -> String -> Int
levenshtein s1 s2 = levenshtein' (' ':s1) (' ':s2) (length s1) (length s2)

levenshtein' :: String -> String -> Int -> Int -> Int
levenshtein' s1 s2 i j
    | i == 0 = j
    | j == 0 = i
    | (s1 !! i) == (s2 !! j) = levenshtein' s1 s2 (i - 1) (j - 1)
    | otherwise = foldl1 min [(1 + levenshtein' s1 s2 (i - 1) j)        --deletion
                             ,(1 + levenshtein' s1 s2 i (j - 1))        --insertion
                             ,(1 + levenshtein' s1 s2 (i - 1) (j - 1))] --substitution


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
