{-# LANGUAGE GADTs #-}
module Doc.API.V2.Parameters (
    ApiV2Parameter(..)
  , ParameterOption(..)
  , apiV2Parameter
  , apiV2Parameter'
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
import Data.Text hiding (reverse, takeWhile)
import Data.Unjson
import Data.Unjson as Unjson
import Doc.Rendering
import File.File(File(..))
import File.Model
import Happstack.Fields
import Kontra
import LiveDocx
import qualified Data.Aeson as Aeson

-- | Parameters are either Obligatory or Optional
-- If they are Optional we may want a default value, or we may want Nothing
data ParameterOption a = Obligatory | Optional | OptionalWithDefault a

data ApiV2Parameter a where
  ApiV2ParameterBool  :: Text -> ParameterOption Bool -> ApiV2Parameter Bool
  ApiV2ParameterInt   :: Text -> ParameterOption Int -> ApiV2Parameter Int
  ApiV2ParameterText  :: Text -> ParameterOption Text -> ApiV2Parameter Text
  ApiV2ParameterRead  :: Read a => Text -> ParameterOption a -> ApiV2Parameter a
  ApiV2ParameterJSON  :: Text -> ParameterOption a -> UnjsonDef a -> ApiV2Parameter a
  ApiV2ParameterAeson :: Aeson.FromJSON a => Text -> ParameterOption a -> ApiV2Parameter a
  ApiV2ParameterFile  :: Text -> ParameterOption File -> ApiV2Parameter File

-- | Same as `apiV2Parameter` except that it fails when we have a Nothing.
-- Given the same parameters it will behave the same way, but instead of giving
-- back a Just it will give back the value.
apiV2Parameter' :: Kontrakcja m => ApiV2Parameter a -> m a
apiV2Parameter' p = do
  v <- apiV2Parameter p
  case v of
    Just r -> return r
    Nothing -> apiError $ requestParameterMissing (getParameterName p)

-- | Gets us all the different types of API parameters by matching proper
-- constructors on `ApiV2Parameter a` which includes `ParameterOption a`
apiV2Parameter :: Kontrakcja m => ApiV2Parameter a -> m (Maybe a)

apiV2Parameter (ApiV2ParameterInt name opt) = apiParameterUsingMaybeRead name opt
apiV2Parameter (ApiV2ParameterText name opt) = apiParameterUsingMaybeRead name opt
apiV2Parameter (ApiV2ParameterRead name opt) = apiParameterUsingMaybeRead name opt

apiV2Parameter (ApiV2ParameterBool name opt) = do
  mValue <- getField $ unpack name
  case mValue of
    Just "true"  -> return $ Just True
    Just "false" -> return $ Just False
    Just _ -> apiError $ requestParameterParseError name "boolean value should be 'true' or 'false'"
    Nothing -> handleParameterOption name opt

apiV2Parameter (ApiV2ParameterJSON name opt jsonDef) = do
  mValue <- getFieldBS (unpack name)
  case mValue of
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Left _ -> apiError $ requestParameterParseError name "Invalid JSON"
      Right paramAeson -> case (Unjson.parse jsonDef paramAeson) of
        (Result res []) -> return $ Just res
        (Result _ errs) -> apiError $ requestParameterParseError name (pack (show errs))
    Nothing -> handleParameterOption name opt

apiV2Parameter (ApiV2ParameterAeson name opt) = do
  mValue <- getFieldBS (unpack name)
  case mValue of
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Left _ -> apiError $ requestParameterParseError name "Invalid JSON"
      Right js -> return $ Just js
    Nothing -> handleParameterOption name opt

apiV2Parameter (ApiV2ParameterFile name opt) = do
  mValue <- getDataFn' (lookInput $ unpack name)
  case mValue of
    Nothing -> handleParameterOption name opt
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

-- | Helper function for all parameters that can just be parsed using `maybeRead`
apiParameterUsingMaybeRead :: (Kontrakcja m, Read a) => Text -> ParameterOption a -> m (Maybe a)
apiParameterUsingMaybeRead name opt = do
  mValue <- getField $ unpack name
  case fmap maybeRead mValue of
    Just (Just v) -> return $ Just v
    Just Nothing  -> apiError $ requestParameterParseError name "could not read parameter"
    Nothing -> handleParameterOption name opt

-- | Helper function to extract name from `ApiV2Parameter`
getParameterName :: ApiV2Parameter a -> Text
getParameterName (ApiV2ParameterBool n _) = n
getParameterName (ApiV2ParameterInt n _) = n
getParameterName (ApiV2ParameterText n _) = n
getParameterName (ApiV2ParameterRead n _) = n
getParameterName (ApiV2ParameterJSON n _ _) = n
getParameterName (ApiV2ParameterAeson n _) = n
getParameterName (ApiV2ParameterFile n _) = n

-- | Helper function to handle when getting the parameter gives us `Nothing`
handleParameterOption :: Kontrakcja m => Text -> ParameterOption a -> m (Maybe a)
handleParameterOption _ Optional = return Nothing
handleParameterOption _ (OptionalWithDefault d) = return $ Just d
handleParameterOption n Obligatory = apiError $ requestParameterMissing n
