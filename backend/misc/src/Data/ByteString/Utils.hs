module Data.ByteString.Utils
  ( unjsonByteString
  , splitEvery
  ) where

import Data.Functor.Invariant
import Data.Unjson
import qualified Data.ByteString.Char8 as BS

unjsonByteString :: UnjsonDef BS.ByteString
unjsonByteString = invmap BS.pack BS.unpack unjsonDef

splitEvery :: Int -> BS.ByteString -> [BS.ByteString]
splitEvery n = go
  where
    go bs = case BS.splitAt n bs of
      (chunk, rest) | BS.null rest -> [chunk]
                    | otherwise    -> chunk : go rest
