-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocImage
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  not portable
--
-- All that is needed to add image to a document
-----------------------------------------------------------------------------
module Doc.DocAddImage
  ( addImageToDocumentFile
  ) where

import Control.Monad.Catch hiding (handle)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Int
import Log
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BS

import DB
import Doc.AddImageSpec
import Doc.DocumentID
import File.File
import File.Storage
import Kontra
import Log.Identifier
import PdfToolsLambda.Conf
import PdfToolsLambda.Control
import Utils.Directory

-- | Generate file that has the image printed on it
addImageToDocumentFile
  :: ( MonadBaseControl IO m
     , MonadDB m
     , MonadLog m
     , KontraMonad m
     , TemplatesMonad m
     , MonadIO m
     , MonadMask m
     , MonadFileStorage m
     , PdfToolsLambdaMonad m
     , CryptoRNG m
     )
  => DocumentID
  -> File
  -> File
  -> Int32
  -> Double
  -> Double
  -> m (Either String BS.ByteString)
addImageToDocumentFile documentid file@File { fileid } image pageno x y =
  withSystemTempDirectory' ("stamp-" ++ show documentid ++ "-" ++ show fileid ++ "-")
    $ \tmppath -> do
        logInfo "Stamping file with image" $ logObject_ file
        let tmpin = tmppath ++ "/input.pdf"
        content <- getFileContents file
        liftIO $ BS.writeFile tmpin content
        logInfo "Temp file write" $ object
          [ "bytes_written" .= (BS.length content)
          , "originator" .= ("stampDocumentFileWithImage" :: Text)
          ]
        spec <- addImageSpecForDocument tmpin documentid image pageno x y
        runLambdaAddImage tmppath spec


addImageSpecForDocument
  :: ( MonadIO m
     , TemplatesMonad m
     , MonadDB m
     , MonadMask m
     , MonadLog m
     , MonadFileStorage m
     , MonadBaseControl IO m
     )
  => String
  -> DocumentID
  -> File
  -> Int32
  -> Double
  -> Double
  -> m AddImageSpec
addImageSpecForDocument inputpath did imageFile pageno x y = do
  imageContent <- getFileContents imageFile
  return $ AddImageSpec { addImageInput              = inputpath
                        , addImageDocumentNumberText = (show did)
                        , addImageImageBinary        = imageContent
                        , addImageX                  = x
                        , addImageY                  = y
                        , addImagePage               = pageno
                        }

runLambdaAddImage
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , MonadDB m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , PdfToolsLambdaMonad m
     , TemplatesMonad m
     )
  => FilePath
  -> AddImageSpec
  -> m (Either String BS.ByteString)
runLambdaAddImage _tmppath spec = do
  lambdaconf        <- getPdfToolsLambdaEnv
  mWithImageContent <- callPdfToolsAddImage lambdaconf spec
  case mWithImageContent of
    Just withImageContent -> do
      return $ Right withImageContent
    Nothing -> do
      logAttention_ "Adding image in lambda failed"
      -- show JSON'd config as that's what the java app is fed.
      return $ Left "Error when adding image on PDF"
