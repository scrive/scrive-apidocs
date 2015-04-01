module Data.ByteString.Utils (splitEvery) where

import qualified Data.ByteString as BS

import KontraPrelude

splitEvery :: Int -> BS.ByteString -> [BS.ByteString]
splitEvery n = go
    where go bs = case BS.splitAt n bs of
                    (chunk, rest) | BS.null rest -> [chunk]
                                  | otherwise    -> chunk:go rest
