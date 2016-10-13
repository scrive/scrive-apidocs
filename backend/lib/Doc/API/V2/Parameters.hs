{-# LANGUAGE GADTs #-}
module Doc.API.V2.Parameters (
    ApiV2Parameter(..)
  , apiV2ParameterOptional
  , apiV2ParameterDefault
  , apiV2ParameterObligatory
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Unjson
import Data.Unjson as Unjson
import Happstack.Server
import System.FilePath
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import API.V2
import DB
import Doc.Rendering
import File.File(File(..))
import File.Model
import Happstack.Fields
import Kontra
import KontraPrelude
import qualified Data.ByteString.RFC2397 as Base64Image

data ApiV2Parameter a where
  ApiV2ParameterBool  :: T.Text -> ApiV2Parameter Bool
  ApiV2ParameterInt   :: T.Text -> ApiV2Parameter Int
  ApiV2ParameterText  :: T.Text -> ApiV2Parameter T.Text
  ApiV2ParameterRead  :: Read a => T.Text -> ApiV2Parameter a
  ApiV2ParameterJSON  :: T.Text -> UnjsonDef a -> ApiV2Parameter a
  -- Param that is text, but we want to reuse definition from Unjson instance
  ApiV2ParameterTextUnjson :: T.Text -> UnjsonDef a -> ApiV2Parameter a
  ApiV2ParameterAeson :: Aeson.FromJSON a => T.Text -> ApiV2Parameter a
  ApiV2ParameterFilePDF        :: T.Text -> ApiV2Parameter File
  ApiV2ParameterFilePDFOrImage :: T.Text -> ApiV2Parameter File
  ApiV2ParameterBase64PNGImage :: T.Text -> ApiV2Parameter File

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
apiV2ParameterOptional (ApiV2ParameterText name) = (fmap T.pack) <$> getField (T.unpack name)
apiV2ParameterOptional (ApiV2ParameterRead name) = apiParameterUsingMaybeRead name

apiV2ParameterOptional (ApiV2ParameterBool name) = do
  mValue <- getField $ T.unpack name
  case mValue of
    Just "true"  -> return $ Just True
    Just "false" -> return $ Just False
    Just _ -> apiError $ requestParameterParseError name "boolean value should be 'true' or 'false'"
    Nothing -> return Nothing


apiV2ParameterOptional (ApiV2ParameterJSON name jsonDef) = do
  mValue <- getFieldBS (T.unpack name)
  case mValue of
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Left _ -> apiError $ requestParameterParseError name "Invalid JSON"
      Right paramAeson -> case (Unjson.parse jsonDef paramAeson) of
        (Result res []) -> return $ Just res
        (Result _ errs) -> apiError $ requestParameterParseError name (T.pack (show errs))
    Nothing -> return Nothing

apiV2ParameterOptional (ApiV2ParameterTextUnjson name jsonDef) = do
  mValue <- getField (T.unpack name)
  case mValue of
    Just paramValue -> case (Unjson.parse jsonDef (Aeson.String $ T.pack paramValue)) of
        (Result res []) -> return $ Just res
        (Result _ errs) -> apiError $ requestParameterParseError name (T.pack (show errs))
    Nothing -> return Nothing


apiV2ParameterOptional (ApiV2ParameterAeson name) = do
  mValue <- getFieldBS (T.unpack name)
  case mValue of
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Left _ -> apiError $ requestParameterParseError name "Invalid JSON"
      Right js -> return $ Just js
    Nothing -> return Nothing

apiV2ParameterOptional (ApiV2ParameterFilePDF name) = do
  mValue <- getDataFn' (lookInput $ T.unpack name)
  case mValue of
    Nothing -> return Nothing
    Just (Input _ Nothing _) -> apiError $ requestParameterInvalid name "file was empty"
    Just (Input contentspec (Just filename') _contentType) -> do
      let filename = reverse . takeWhile (/='\\') . reverse $ filename' -- Drop filepath for windows
      content' <- case contentspec of
        Left filepath -> liftIO $ BS.readFile filepath
        Right content -> return (BS.concat $ BSL.toChunks content)

      pdfcontent <- do
        res <- preCheckPDF content'
        case res of
          Right r -> return r
          Left _ ->  apiError $ requestParameterParseError name $ "not a valid PDF"

      fileid <- dbUpdate $ NewFile filename pdfcontent
      file <- dbQuery $ GetFileByFileID fileid
      return $ Just file

apiV2ParameterOptional (ApiV2ParameterFilePDFOrImage name) = do
  mValue <- getDataFn' (lookInput $ T.unpack name)
  case mValue of
    Nothing -> return Nothing
    Just (Input _ Nothing _) -> apiError $ requestParameterInvalid name "file was empty"
    Just (Input contentspec (Just filename') _contentType) -> do
      let filename = reverse . takeWhile (/='\\') . reverse $ filename' -- Drop filepath for windows
      content' <- case contentspec of
        Left filepath -> liftIO $ BS.readFile filepath
        Right content -> return (BS.concat $ BSL.toChunks content)
      let filenameExt = T.toLower . T.pack . takeExtension $ filename
          pdfSuffix = ".pdf" == filenameExt
          jpgSufix = ".jpg" == filenameExt || ".jpeg" == filenameExt
          pngSuffix = ".png" == filenameExt
      content <- case (pdfSuffix, jpgSufix || pngSuffix) of
        (True, _) -> do
          res <- preCheckPDF content'
          case res of
            Right r -> return r
            Left _ ->  apiError $ requestParameterParseError name $ "filename suggests PDF, but not a valid PDF"
        (_, True) -> return content'
        _ -> apiError $ requestParameterParseError name "not a PDF or image (PNG or JPG)"
      fileid <- dbUpdate $ NewFile filename content
      file <- dbQuery $ GetFileByFileID fileid
      return $ Just file

apiV2ParameterOptional (ApiV2ParameterBase64PNGImage name) = do
  mValue <- getFieldBS (T.unpack name)
  case (Base64Image.decode . BS.concat . BSL.toChunks) <$> mValue of
    Nothing -> return Nothing
    (Just Nothing) -> apiError $ requestParameterParseError name "expected RFC2397 encoded png"
    (Just (Just (_,content))) -> do
      fileid <- dbUpdate $ NewFile "image-param.png" content
      file <- dbQuery $ GetFileByFileID fileid
      return $ Just file

-- * Internal

-- | Helper function for all parameters that can just be parsed using `maybeRead`
apiParameterUsingMaybeRead :: (HasRqData m,ServerMonad m, MonadThrow m, Read a) => T.Text -> m (Maybe a)
apiParameterUsingMaybeRead name = do
  mValue <- getField $ T.unpack name
  case fmap maybeRead mValue of
    Just (Just v) -> return $ Just v
    Just Nothing  -> apiError $ requestParameterParseError name "could not read parameter"
    Nothing -> return Nothing

-- | Helper function to extract name from `ApiV2Parameter`
getParameterName :: ApiV2Parameter a -> T.Text
getParameterName (ApiV2ParameterBool n) = n
getParameterName (ApiV2ParameterInt n) = n
getParameterName (ApiV2ParameterText n) = n
getParameterName (ApiV2ParameterRead n) = n
getParameterName (ApiV2ParameterJSON n _) = n
getParameterName (ApiV2ParameterTextUnjson n _) = n
getParameterName (ApiV2ParameterAeson n) = n
getParameterName (ApiV2ParameterFilePDF n) = n
getParameterName (ApiV2ParameterFilePDFOrImage n) = n
getParameterName (ApiV2ParameterBase64PNGImage n) = n
