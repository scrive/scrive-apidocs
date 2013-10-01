module Doc.EvidenceAttachments
  ( Attachment(..)
  , fetch
  , extract
  ) where

import Control.Applicative((<$>))
import DB (MonadDB)
import Data.Maybe (listToMaybe, fromMaybe)
import Doc.DocStateData (Document(..), documentsealedfile)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import File.Storage (getFileIDContents)
import Kontra (KontraMonad)
import Control.Monad.IO.Class
import qualified PdfModel as P
import qualified Amazon as AWS
import Control.Monad

data Attachment = Attachment
  { name     :: BS.ByteString
  , mimetype :: Maybe BS.ByteString
  , content  :: BSL.ByteString
  } deriving (Eq, Ord, Show)

fetch :: (KontraMonad m, MonadDB m, MonadIO m, AWS.AmazonMonad m) => Document -> m [Attachment]
fetch doc = do
  case documentsealedfile doc of
    Nothing -> return []
    Just fid -> do
      extract <$> getFileIDContents fid

extract :: BS.ByteString -> [Attachment]
extract c = fromMaybe [] $ do
  pd <- P.parse c
  P.Array files <- (do
     P.Array [t] <- listToMaybe (P.documentBodies pd)
                    >>= lookup "Root" . P.bodyTrailer
                    >>= P.lookupRef pd "Names"
                    >>= P.lookupRef pd "EmbeddedFiles"
                    >>= P.lookupRef pd "Kids"
     P.lookupRef pd "Names" t)
     `mplus` (do
             listToMaybe (P.documentBodies pd)
                    >>= lookup "Root" . P.bodyTrailer
                    >>= P.lookupRef pd "Names"
                    >>= P.lookupRef pd "EmbeddedFiles"
                    >>= P.lookupRef pd "Names")

  let mkAttachment (P.String False s:r:l) = do
        P.Ref fref <- P.lookupRef pd "EF" r >>= P.lookupRef pd "F"
        P.Indir (P.Dict fd) (Just co) <- P.lookup fref pd
        let a = Attachment{ name     = s
                          , mimetype = lookup "Subtype" fd >>= P.dName
                          , content  = co
                          }
        (a:) <$> mkAttachment l
      mkAttachment [] = return []
      mkAttachment _  = Nothing
  mkAttachment files
