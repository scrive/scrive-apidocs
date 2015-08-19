{-# LANGUAGE GADTs #-}
module Doc.API.V2.Parameters (
    ApiV2Parameter(..)
  , apiV2ParameterOptional
  , apiV2ParameterDefault
  , apiV2ParameterObligatory
) where

import Control.Monad.IO.Class
import Happstack.Server.RqData
import Happstack.Server.Types
import KontraPrelude
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import API.V2
import DB
import Data.Text hiding (map, reverse, takeWhile)
import Data.Unjson
import Data.Unjson as Unjson
import Doc.Rendering
import File.File(File(..))
import File.Model
import Happstack.Fields
import Kontra
import LiveDocx
import qualified Data.Aeson as Aeson

data ApiV2Parameter a where
  ApiV2ParameterBool  :: Text -> ApiV2Parameter Bool
  ApiV2ParameterInt   :: Text -> ApiV2Parameter Int
  ApiV2ParameterText  :: Text -> ApiV2Parameter Text
  ApiV2ParameterRead  :: Read a => Text -> ApiV2Parameter a
  ApiV2ParameterJSON  :: Text -> UnjsonDef a -> ApiV2Parameter a
  ApiV2ParameterAeson :: Aeson.FromJSON a => Text -> ApiV2Parameter a
  ApiV2ParameterFilePDF        :: Text -> ApiV2Parameter File
  ApiV2ParameterFilePDFOrImage :: Text -> ApiV2Parameter File

-- | Get an obligatory parameter
--
-- Throws a `requestParameterMissing` if the parameter was not provided
apiV2ParameterObligatory :: Kontrakcja m => ApiV2Parameter a -> m a
apiV2ParameterObligatory p = do
  v <- apiV2ParameterOptional p
  case v of
    Just r -> return r
    Nothing -> apiError $ requestParameterMissing (getParameterName p)

-- | Get an optional parameter with a default value
apiV2ParameterDefault :: Kontrakcja m => a -> ApiV2Parameter a -> m a
apiV2ParameterDefault d p = do
  v <- apiV2ParameterOptional p
  case v of
    Just r -> return r
    Nothing -> return d

-- | Get an optional parameter
apiV2ParameterOptional :: Kontrakcja m => ApiV2Parameter a -> m (Maybe a)

apiV2ParameterOptional (ApiV2ParameterInt name) = apiParameterUsingMaybeRead name
apiV2ParameterOptional (ApiV2ParameterText name) = liftM (fmap pack) $ getField $ unpack name
apiV2ParameterOptional (ApiV2ParameterRead name) = apiParameterUsingMaybeRead name

apiV2ParameterOptional (ApiV2ParameterBool name) = do
  mValue <- getField $ unpack name
  case mValue of
    Just "true"  -> return $ Just True
    Just "false" -> return $ Just False
    Just _ -> apiError $ requestParameterParseError name "boolean value should be 'true' or 'false'"
    Nothing -> return Nothing

apiV2ParameterOptional (ApiV2ParameterJSON name jsonDef) = do
  mValue <- getFieldBS (unpack name)
  case mValue of
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Left _ -> apiError $ requestParameterParseError name "Invalid JSON"
      Right paramAeson -> case (Unjson.parse jsonDef paramAeson) of
        (Result res []) -> return $ Just res
        (Result _ errs) -> apiError $ requestParameterParseError name (pack (show errs))
    Nothing -> return Nothing

apiV2ParameterOptional (ApiV2ParameterAeson name) = do
  mValue <- getFieldBS (unpack name)
  case mValue of
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Left _ -> apiError $ requestParameterParseError name "Invalid JSON"
      Right js -> return $ Just js
    Nothing -> return Nothing

apiV2ParameterOptional (ApiV2ParameterFilePDF name) = do
  mValue <- getDataFn' (lookInput $ unpack name)
  case mValue of
    Nothing -> return Nothing
    Just (Input _ Nothing _) -> apiError $ requestParameterInvalid name "file was empty"
    Just (Input contentspec (Just filename'') _contentType) -> do
      ctx <- getContext
      let filename' = reverse . takeWhile (/='\\') . reverse $ filename'' -- Drop filepath for windows
      let mformat = getFileFormatForConversion filename'
      content' <- case contentspec of
        Left filepath -> liftIO $ BS.readFile filepath
        Right content -> return (BS.concat $ BSL.toChunks content)

      (content'', filename) <- case mformat of
        Nothing -> return (content', filename')
        Just format -> do
          eres <- convertToPDF (ctxlivedocxconf ctx) content' format
          case eres of
            Left (LiveDocxIOError e) -> apiError $ requestParameterParseError name $ "LiveDocX conversion IO failed " `append` pack (show e)
            Left (LiveDocxSoapError s)-> apiError $ requestParameterParseError name $ "LiveDocX conversion SOAP failed " `append` pack s
            Right res -> do
              -- change extension from .doc, .docx and others to .pdf
              let filename = takeBaseName filename' ++ ".pdf"
              return $ (res, filename)

      pdfcontent <- do
        res <- preCheckPDF content''
        case res of
          Right r -> return r
          Left _ ->  apiError $ requestParameterParseError name $ "not a valid PDF"

      fileid <- dbUpdate $ NewFile filename pdfcontent
      file <- dbQuery $ GetFileByFileID fileid
      return $ Just file

apiV2ParameterOptional (ApiV2ParameterFilePDFOrImage name) = do
  mValue <- getDataFn' (lookInput $ unpack name)
  case mValue of
    Nothing -> return Nothing
    Just (Input _ Nothing _) -> apiError $ requestParameterInvalid name "file was empty"
    Just (Input contentspec (Just filename') _contentType) -> do
      let filename = reverse . takeWhile (/='\\') . reverse $ filename' -- Drop filepath for windows
      content' <- case contentspec of
        Left filepath -> liftIO $ BS.readFile filepath
        Right content -> return (BS.concat $ BSL.toChunks content)
      let filenameExt = toLower . pack . takeExtension $ filename
          pdfSuffix = ".pdf" == filenameExt
          jpgSufix = ".jpg" == filenameExt || ".jpeg" == filenameExt
          pngSuffix = ".png" == filenameExt
      content <- case (pdfSuffix, jpgSufix || pngSuffix) of
        (True, _) -> do
          res <- preCheckPDF content'
          case res of
            Right r -> return r
            Left _ ->  apiError $ requestParameterParseError name $ "filename suggests PDF, but not a valid PDF"
        (_, True) -> return $ Binary $ content'
        _ -> apiError $ requestParameterParseError name "not a PDF or image (PNG or JPG)"
      fileid <- dbUpdate $ NewFile filename content
      file <- dbQuery $ GetFileByFileID fileid
      return $ Just file

-- * Internal

-- | Helper function for all parameters that can just be parsed using `maybeRead`
apiParameterUsingMaybeRead :: (Kontrakcja m, Read a) => Text -> m (Maybe a)
apiParameterUsingMaybeRead name = do
  mValue <- getField $ unpack name
  case fmap maybeRead mValue of
    Just (Just v) -> return $ Just v
    Just Nothing  -> apiError $ requestParameterParseError name "could not read parameter"
    Nothing -> return Nothing

-- | Helper function to extract name from `ApiV2Parameter`
getParameterName :: ApiV2Parameter a -> Text
getParameterName (ApiV2ParameterBool n) = n
getParameterName (ApiV2ParameterInt n) = n
getParameterName (ApiV2ParameterText n) = n
getParameterName (ApiV2ParameterRead n) = n
getParameterName (ApiV2ParameterJSON n _) = n
getParameterName (ApiV2ParameterAeson n) = n
getParameterName (ApiV2ParameterFilePDF n) = n
getParameterName (ApiV2ParameterFilePDFOrImage n) = n
