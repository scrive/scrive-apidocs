module Util.ZipUtil (ZipArchive(..)) where

import Codec.Archive.Zip
import Happstack.Server (ToMessage(..), setHeader)
import qualified Data.ByteString.UTF8 as BS

data ZipArchive = ZipArchive String Archive

instance ToMessage ZipArchive where
  toMessage (ZipArchive _ archive) = fromArchive archive
  toContentType _ = BS.fromString "archive/zip"
  toResponse za@(ZipArchive fn _) =
    setHeader "Content-Disposition" ("attachment;filename=" ++ fn)
      $ setHeader "Content-Type" (BS.toString $ toContentType za)
      $ toResponse (toMessage za)
