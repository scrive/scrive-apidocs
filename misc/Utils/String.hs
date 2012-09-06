module Utils.String where

import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

concatChunks :: BSL.ByteString -> BS.ByteString
concatChunks = BS.concat . BSL.toChunks

pureString :: String -> String
pureString = unwords . words . filter (not . isControl)

basename :: String -> String
basename filename =
  case break (\x -> (x=='\\') || (x=='/')) filename of
    (_,(_:rest)) -> basename rest
    _            -> takeWhile ((/=) '.') filename
