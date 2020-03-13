module PdfToolsLambda.Control
  ( callPdfToolsSealingPrim
  , callPdfToolsPresealingPrim
  , callPdfToolsCleaningPrim
  , callPdfToolsAddImagePrim
  , callPdfToolsPadesSignPrim
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Crypto.RNG
import Crypto.RNG.Utils
import Data.Aeson (ToJSON)
import Data.Char
import Data.Time
import GHC.Generics
import Log
import System.Exit
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.JSON as JSON
import qualified Text.JSON.FromJSValue as JSON
import qualified Text.JSON.Gen as JSON

import DB
import Doc.AddImageSpec
import Doc.SealSpec
import FileStorage.Amazon
import FileStorage.Amazon.S3Env
import FileStorage.Class
import Log.Utils
import PdfToolsLambda.Class
import PdfToolsLambda.Conf
import PdfToolsLambda.Spec
import Utils.IO

data PdfToolsAction =
    PdfToolsActionSealing
  | PdfToolsActionCleaning
  | PdfToolsActionAddImage
  | PdfToolsActionPadesSign
  deriving (Generic)

instance ToJSON PdfToolsAction

pdfToolsActionName :: PdfToolsAction -> Text
pdfToolsActionName PdfToolsActionSealing   = "seal"
pdfToolsActionName PdfToolsActionCleaning  = "clean"
pdfToolsActionName PdfToolsActionAddImage  = "addimage"
pdfToolsActionName PdfToolsActionPadesSign = "padesSign"

callPdfToolsSealingPrim
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => SealSpec
  -> PdfToolsLambdaEnv
  -> m (Maybe BS.ByteString)
callPdfToolsSealingPrim spec lc = do
  inputData <- sealSpecToLambdaSpec spec
  executePdfToolsLambdaActionCall lc PdfToolsActionSealing inputData

callPdfToolsPresealingPrim
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PreSealSpec
  -> PdfToolsLambdaEnv
  -> m (Maybe BS.ByteString)
callPdfToolsPresealingPrim spec lc = do
  inputData <- presealSpecToLambdaSpec spec
  executePdfToolsLambdaActionCall lc PdfToolsActionSealing inputData

callPdfToolsCleaningPrim
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => BSL.ByteString
  -> PdfToolsLambdaEnv
  -> m (Maybe BS.ByteString)
callPdfToolsCleaningPrim inputFileContent lc = do
  executePdfToolsLambdaActionCall lc PdfToolsActionCleaning inputFileContent

callPdfToolsAddImagePrim
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => AddImageSpec
  -> PdfToolsLambdaEnv
  -> m (Maybe BS.ByteString)
callPdfToolsAddImagePrim spec lc = do
  inputData <- addImageSpecToLambdaSpec spec
  executePdfToolsLambdaActionCall lc PdfToolsActionAddImage inputData

padesSignSpecToLambdaSpec :: GlobalSignConfig -> PadesSignSpec -> BSL.ByteString
padesSignSpecToLambdaSpec gs PadesSignSpec {..} =
  Aeson.encode
    $ Aeson.object
    $ [ "documentNumberText" .= documentNumberText
      , "mainFileInput"
        .= Aeson.object ["base64Content" .= (T.decodeUtf8 $ B64.encode inputFileContent)]
      , "apiKey" .= (gs ^. #apiKey)
      , "apiPassword" .= (gs ^. #apiPassword)
      , "sslCertBase64" .= (gs ^. #certificate)
      , "sslCertSecret" .= (gs ^. #certificatePassword)
      ]

callPdfToolsPadesSignPrim
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PadesSignSpec
  -> PdfToolsLambdaEnv
  -> m (Maybe BS.ByteString)
callPdfToolsPadesSignPrim spec lc = do
  case lc ^. #globalSign of
    Just gs -> do
      let inputData = padesSignSpecToLambdaSpec gs spec
      executePdfToolsLambdaActionCall lc PdfToolsActionPadesSign inputData
    Nothing -> do
      logInfo_
        "Error calling padesSign lambda: global_sign field not available in pdftools_lambda config."
      return Nothing

--
executePdfToolsLambdaActionCall
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PdfToolsLambdaEnv
  -> PdfToolsAction
  -> BSL.ByteString
  -> m (Maybe BS.ByteString)
executePdfToolsLambdaActionCall lc action inputData = do
  logInfo_ "Uploading data to s3 for lambda"
  uploadedDataFileName <- sendDataFileToAmazon (lc ^. #s3Env) inputData
  case uploadedDataFileName of
    Nothing -> do
      logAttention_ "Failed to upload data to s3 for lambda"
      return Nothing
    Just s3FileName -> do
      logInfo_ "Data uploaded to s3 for lambda"
      (exitcode, stdout, stderr) <- readCurl
        [ "-X"
        , "POST"
        , "-H"
        , "x-api-key: " ++ T.unpack (lc ^. #lambda ^. #apiKey)
        , "-H"
        , "Content-Type: text/plain"
        , "--data"
        , "@-"
        , T.unpack $ lc ^. #lambda ^. #gatewayUrl
        ]
        (BSL.fromString $ JSON.encode $ JSON.runJSONGen $ do
          JSON.value "action" $ T.unpack $ pdfToolsActionName action
          JSON.value "s3FileName" $ T.unpack s3FileName
        )
      case exitcode of
        ExitFailure msg -> do
          logInfo "Failed to receive data from lambda" $ object
            [ "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          return $ Nothing
        ExitSuccess -> do
          logInfo_ "Response from lambda received"
          parseResponse action lc stdout

parseResponse
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PdfToolsAction
  -> PdfToolsLambdaEnv
  -> BSL.ByteString
  -> m (Maybe BS.ByteString)
parseResponse action lc stdout = do
  case parsePdfToolsLambdaResponse stdout of
    Right resultS3Name -> do
      mresdata <- getDataFromAmazon (lc ^. #s3Env) resultS3Name
      case mresdata of
        Just resdata -> return $ Just $ BSL.toStrict resdata
        Nothing      -> do
          logAttention "Failed to fetch lambda call result from S3" $ object
            [ "action" .= action
            , "stdout" `equalsExternalBSL` stdout
            , "resultS3Name" .= resultS3Name
            ]
          return Nothing
    Left errorMessage -> do
      logInfo "Lambda call failed" $ object
        [ "action" .= action
        , "stdout" `equalsExternalBSL` stdout
        , "errorMessage" .= errorMessage
        ]
      return $ Nothing

parsePdfToolsLambdaResponse :: BSL.ByteString -> Either Text Text
parsePdfToolsLambdaResponse bsj = case JSON.decode (BSL.toString bsj) of
  JSON.Ok jsvalue -> runIdentity $ JSON.withJSValue jsvalue $ do
    ms3FileName <- JSON.fromJSValueField "resultS3FileName"
    case ms3FileName of
      Just s3FileName -> return $ Right s3FileName
      _               -> do
        merror <- JSON.fromJSValueField "error"
        return . Left $ fromMaybe "No error message provided" merror
  _ -> Left "Response is not a valid json"

sendDataFileToAmazon
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m, MonadThrow m)
  => AmazonS3Env
  -> BSL.ByteString
  -> m (Maybe Text)
sendDataFileToAmazon env content = do
  flip catch (\(_ :: FileStorageException) -> return Nothing) $ do
    randomPart <- randomString 10 ['a' .. 'z']
    timePart   <- filter isDigit <$> show <$> liftBase getCurrentTime
    let name = T.pack $ randomPart <> timePart
    saveContentsToAmazon env name content
    return $ Just name

getDataFromAmazon
  :: (MonadBase IO m, MonadCatch m, MonadLog m, MonadThrow m)
  => AmazonS3Env
  -> Text
  -> m (Maybe BSL.ByteString)
getDataFromAmazon env name =
  (Just <$> getContentsFromAmazon env name)
    `catch` (\(_ :: FileStorageException) -> return Nothing)
