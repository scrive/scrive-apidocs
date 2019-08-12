{-# LANGUAGE GADTs #-}
module API.V2.Parameters (
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
import System.FilePath (takeExtension)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified System.FilePath.Windows as Windows

import API.V2
import DB
import File.File (File(..))
import File.Model
import File.Storage
import Happstack.Fields
import Kontra
import Util.ImageUtil
import Util.PDFUtil
import qualified Data.ByteString.RFC2397 as Base64Image
import qualified InputValidation as V

data ApiV2Parameter a where
  ApiV2ParameterFlag  :: T.Text -> ApiV2Parameter Bool
  ApiV2ParameterBool  :: T.Text -> ApiV2Parameter Bool
  ApiV2ParameterInt   :: T.Text -> ApiV2Parameter Int
  ApiV2ParameterDouble   :: T.Text -> ApiV2Parameter Double
  ApiV2ParameterText  :: T.Text -> ApiV2Parameter T.Text
  ApiV2ParameterTextWithValidation
      :: T.Text -> (String -> V.Result String) -> ApiV2Parameter T.Text
  ApiV2ParameterRead  :: Read a => T.Text -> ApiV2Parameter a
  ApiV2ParameterJSON  :: T.Text -> UnjsonDef a -> ApiV2Parameter a
  -- Param that is text, but we want to reuse definition from Unjson instance
  ApiV2ParameterTextUnjson :: T.Text -> UnjsonDef a -> ApiV2Parameter a
  ApiV2ParameterAeson :: Aeson.FromJSON a => T.Text -> ApiV2Parameter a
  ApiV2ParameterFilePDF        :: T.Text -> ApiV2Parameter File
  ApiV2ParameterFilePDFs       :: [T.Text] -> ApiV2Parameter [File]
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
apiV2ParameterOptional (ApiV2ParameterDouble name) = apiParameterUsingMaybeRead name
apiV2ParameterOptional (ApiV2ParameterText name) = fmap T.pack <$> getField (T.unpack name)
apiV2ParameterOptional (ApiV2ParameterRead name) = apiParameterUsingMaybeRead name

apiV2ParameterOptional (ApiV2ParameterTextWithValidation name validate) = do
  mValue <- getField $ T.unpack name
  case fmap validate mValue of
    Nothing -> return Nothing
    Just (V.Good v) -> return $ Just (T.pack v)
    Just V.Bad   -> failValidation
    Just V.Empty -> failValidation
  where
    failValidation = apiError $ requestParameterParseError name
      "validation failed, please check that the parameter format is correct"

apiV2ParameterOptional (ApiV2ParameterFlag name) = do
  mValue <- getField $ T.unpack name
  case mValue of
    Just "false" -> return $ Just False
    Just _ -> return $ Just True
    Nothing -> return $ Just False

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
  mFiles <- apiV2ParameterOptional (ApiV2ParameterFilePDFs [name])
  case mFiles of
    Just [file] -> return $ Just file
    _ -> return Nothing

apiV2ParameterOptional (ApiV2ParameterFilePDFs names) = do
  contentsWithNames <- forM names $ \name -> do
    mValue <- getDataFn' (lookInput $ T.unpack name)
    case mValue of
      Nothing -> return Nothing
      Just (Input contentspec mfilename _contentType) -> do
        content <- case contentspec of
          Left filepath -> liftIO $ BS.readFile filepath
          Right content -> return $ BS.concat $ BSL.toChunks content
        case mfilename of
           Just filename' -> do
             -- Drop filepath for windows
             return $ Just (Windows.takeFileName filename', content)
           Nothing -> do
             case (B64.decode content) of
                Right c -> return $ Just ("", c)
                Left  _ -> apiError $ requestParameterInvalid name
                  "file transferred without multipart should be base64 encoded"
  let contentsWithNames' = catMaybes contentsWithNames
  pdfcontents <- do
    res <- preCheckPDFs $ map snd contentsWithNames'
    case res of
      Right r -> return $ zip (map fst contentsWithNames') r
      Left  _ -> apiError $
        requestParameterParseError (T.intercalate ", " names) $ "not a valid PDF"

  files <- forM pdfcontents $ \(filename, pdfcontent) -> do
    fileid <- saveNewFile filename pdfcontent
    file <- dbQuery $ GetFileByFileID fileid
    return file
  return $ Just files

apiV2ParameterOptional (ApiV2ParameterFilePDFOrImage name) = do
  mValue <- getDataFn' (lookInput $ T.unpack name)
  case mValue of
    Nothing -> return Nothing
    Just (Input _ Nothing _) -> apiError $ requestParameterInvalid name "file was empty"
    Just (Input contentspec (Just filename') _contentType) -> do
      let filename = Windows.takeFileName filename' -- Drop filepath for windows
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
            Left _ ->  apiError $ requestParameterParseError name "filename suggests PDF, but not a valid PDF"
        (_, True) -> do
          when (BS.null content') $ apiError $ requestParameterParseError name "image is empty"
          res <- preCheckImage content'
          case res of
            Right r -> return r
            Left _ ->  apiError $ requestParameterParseError name "filename suggests image, but not a valid PNG/JPG"
        _ -> apiError $ requestParameterParseError name "not a PDF or image (PNG or JPG)"
      fileid <- saveNewFile filename content
      file <- dbQuery $ GetFileByFileID fileid
      return $ Just file

apiV2ParameterOptional (ApiV2ParameterBase64PNGImage name) = do
  mValue <- getFieldBS (T.unpack name)
  case (Base64Image.decode . BS.concat . BSL.toChunks) <$> mValue of
    Nothing -> return Nothing
    (Just Nothing) -> apiError $ requestParameterParseError name "expected RFC2397 encoded png"
    (Just (Just (_,""))) -> apiError $ requestParameterParseError name "image is empty"
    (Just (Just (_,content))) -> do
      fileid <- saveNewFile "image-param.png" content
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
getParameterName (ApiV2ParameterFlag n) = n
getParameterName (ApiV2ParameterBool n) = n
getParameterName (ApiV2ParameterInt n) = n
getParameterName (ApiV2ParameterDouble n) = n
getParameterName (ApiV2ParameterText n) = n
getParameterName (ApiV2ParameterTextWithValidation n _) = n
getParameterName (ApiV2ParameterRead n) = n
getParameterName (ApiV2ParameterJSON n _) = n
getParameterName (ApiV2ParameterTextUnjson n _) = n
getParameterName (ApiV2ParameterAeson n) = n
getParameterName (ApiV2ParameterFilePDF n) = n
getParameterName (ApiV2ParameterFilePDFs ns) = T.intercalate "," ns
getParameterName (ApiV2ParameterFilePDFOrImage n) = n
getParameterName (ApiV2ParameterBase64PNGImage n) = n
