module PdfToolsLambda.Control (
    callPdfToolsSealing
  , callPdfToolsPresealing
  , callPdfToolsCleaning
  , callPdfToolsAddImage
) where

import Control.Monad.Base
import Control.Monad.Catch
import Crypto.RNG
import Crypto.RNG.Utils
import Data.Char
import Data.Time
import Log
import System.Exit
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text as T
import qualified Text.JSON as JSON
import qualified Text.JSON.Gen as JSON

import DB
import Doc.AddImageSpec
import Doc.SealSpec
import FileStorage.Amazon
import FileStorage.Amazon.S3Env
import FileStorage.Class
import Log.Utils
import PdfToolsLambda.Conf
import PdfToolsLambda.Response
import PdfToolsLambda.Spec
import Utils.IO

data PdfToolsAction =
    PdfToolsActionSealing
  | PdfToolsActionCleaning
  | PdfToolsActionAddImage

pdfToolsActionName :: PdfToolsAction -> Text
pdfToolsActionName PdfToolsActionSealing  = "seal"
pdfToolsActionName PdfToolsActionCleaning = "clean"
pdfToolsActionName PdfToolsActionAddImage = "addimage"

callPdfToolsSealing
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadDB m, MonadLog m)
  => PdfToolsLambdaEnv
  -> SealSpec
  -> m (Maybe BS.ByteString)
callPdfToolsSealing lc spec = do
  inputData <- sealSpecToLambdaSpec spec
  executePdfToolsLambdaActionCall lc PdfToolsActionSealing inputData

callPdfToolsPresealing
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadDB m, MonadLog m)
  => PdfToolsLambdaEnv
  -> PreSealSpec
  -> m (Maybe BS.ByteString)
callPdfToolsPresealing lc spec = do
  inputData <- presealSpecToLambdaSpec spec
  executePdfToolsLambdaActionCall lc PdfToolsActionSealing inputData

callPdfToolsCleaning
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PdfToolsLambdaEnv
  -> BSL.ByteString
  -> m (Maybe BS.ByteString)
callPdfToolsCleaning lc inputFileContent = do
  executePdfToolsLambdaActionCall lc PdfToolsActionCleaning inputFileContent

callPdfToolsAddImage
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadDB m, MonadLog m)
  => PdfToolsLambdaEnv
  -> AddImageSpec
  -> m (Maybe BS.ByteString)
callPdfToolsAddImage lc spec = do
  inputData <- addImageSpecToLambdaSpec spec
  executePdfToolsLambdaActionCall lc PdfToolsActionAddImage inputData

--
executePdfToolsLambdaActionCall
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PdfToolsLambdaEnv
  -> PdfToolsAction
  -> BSL.ByteString
  -> m (Maybe BS.ByteString)
executePdfToolsLambdaActionCall lc action inputData = do
  logInfo_ "Uploading data to s3 for lamda"
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
        , "x-api-key: " ++ lc ^. #apiKey
        , "-H"
        , "Content-Type: text/plain"
        , "--data"
        , "@-"
        , lc ^. #gatewayUrl
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
          case action of
            PdfToolsActionSealing  -> parseSealingResponse lc stdout
            PdfToolsActionAddImage -> parseAddImageResponse lc stdout
            PdfToolsActionCleaning -> parseCleaningResponse lc stdout

parseSealingResponse
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PdfToolsLambdaEnv
  -> BSL.ByteString
  -> m (Maybe BS.ByteString)
parseSealingResponse lc stdout = do
  case (parsePdfToolsLambdaSealingResponse stdout) of
    SealSuccess resultS3Name -> do
      mresdata <- getDataFromAmazon (lc ^. #s3Env) resultS3Name
      case mresdata of
        Just resdata -> return $ Just $ BSL.toStrict resdata
        Nothing      -> do
          logAttention "Failed to fetch lambda sealing result from S3"
            $ object
            $ ["stdout" `equalsExternalBSL` stdout, "resultS3Name" .= resultS3Name]
          return Nothing
    SealFail errorMessage -> do
      logInfo "Lambda sealing failed"
        $ object ["stdout" `equalsExternalBSL` stdout, "errorMessage" .= errorMessage]
      return $ Nothing

parseCleaningResponse
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PdfToolsLambdaEnv
  -> BSL.ByteString
  -> m (Maybe BS.ByteString)
parseCleaningResponse lc stdout = do
  case (parsePdfToolsLambdaCleaningResponse stdout) of
    CleanSuccess resultS3Name -> do
      mresdata <- getDataFromAmazon (lc ^. #s3Env) resultS3Name
      case mresdata of
        Just resdata -> return $ Just $ BSL.toStrict resdata
        Nothing      -> do
          logAttention "Failed to fetch lambda cleaning result from S3"
            $ object
            $ ["stdout" `equalsExternalBSL` stdout, "resultS3Name" .= resultS3Name]
          return Nothing
    CleanFail errorMessage -> do
      logInfo "Lambda cleaning failed"
        $ object ["stdout" `equalsExternalBSL` stdout, "errorMessage" .= errorMessage]
      return $ Nothing

parseAddImageResponse
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m)
  => PdfToolsLambdaEnv
  -> BSL.ByteString
  -> m (Maybe BS.ByteString)
parseAddImageResponse lc stdout = do
  case (parsePdfToolsLambdaAddImageResponse stdout) of
    AddImageSuccess resultS3Name -> do
      mresdata <- getDataFromAmazon (lc ^. #s3Env) resultS3Name
      case mresdata of
        Just resdata -> return $ Just $ BSL.toStrict resdata
        Nothing      -> do
          logAttention "Failed to fetch lambda add image result from S3"
            $ object
            $ ["stdout" `equalsExternalBSL` stdout, "resultS3Name" .= resultS3Name]
          return Nothing
    AddImageFail errorMessage -> do
      logInfo "Lambda add image failed"
        $ object ["stdout" `equalsExternalBSL` stdout, "errorMessage" .= errorMessage]
      return $ Nothing


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
