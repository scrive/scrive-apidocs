module Util.ImageUtil (preCheckImage) where

import Codec.Picture (decodeImage)
import Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as BS

import Util.PDFUtil (FileError(..))

preCheckImage
  :: MonadBaseControl IO m => BS.ByteString -> m (Either FileError BS.ByteString)
preCheckImage content = do
  return $ case decodeImage content of
    Left  _ -> Left FileFormatError
    Right _ -> Right content
