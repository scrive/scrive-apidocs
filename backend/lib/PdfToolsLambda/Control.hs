module PdfToolsLambda.Control (
    callPdfToolsSealing
  , callPdfToolsPresealing
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
import qualified Text.JSON as JSON
import qualified Text.JSON.Gen as JSON

import DB
import Doc.SealSpec
import FileStorage.Amazon
import FileStorage.Amazon.Config
import FileStorage.Class
import Log.Utils
import PdfToolsLambda.Conf
import PdfToolsLambda.Response
import PdfToolsLambda.Spec
import Utils.IO

callPdfToolsSealing ::
  (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadDB m, MonadLog m) =>
  PdfToolsLambdaConf -> SealSpec -> m (Maybe BS.ByteString)
callPdfToolsSealing lc spec = do
  inputData <- sealSpecToLambdaSpec spec
  executePdfToolsLambdaSealCall lc inputData

callPdfToolsPresealing ::
  (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadDB m, MonadLog m) =>
  PdfToolsLambdaConf -> PreSealSpec -> m (Maybe BS.ByteString)
callPdfToolsPresealing lc spec = do
  inputData <- presealSpecToLambdaSpec spec
  executePdfToolsLambdaSealCall lc inputData

executePdfToolsLambdaSealCall
  :: (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadDB m, MonadLog m)
  => PdfToolsLambdaConf -> BSL.ByteString -> m (Maybe BS.ByteString)
executePdfToolsLambdaSealCall lc inputData = do
  let amazonConfig = get pdfToolsLambdaS3Config lc
  uploadedDataFileName <- sendDataFileToAmazon amazonConfig inputData
  case uploadedDataFileName of
    Nothing -> do
      logAttention_ "Failed to upload data to s3 for lambda"
      return Nothing
    Just s3FileName -> do
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "POST"
          , "-H",  "x-api-key: " ++ get pdfToolsLambdaApiKey lc
          , "-H", "Content-Type: text/plain"
          , "--data", "@-"
          ,  get pdfToolsLambdaGatewayUrl lc
          ]
          (BSL.fromString $ JSON.encode $ JSON.runJSONGen $ do
            JSON.value "action" ("seal"::String)
            JSON.value "s3FileName" s3FileName
          )
      case exitcode of
        ExitFailure msg -> do
          logInfo "Failed to receive data from lambda" $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          return $ Nothing
        ExitSuccess -> do
          case (parsePdfToolsLambdaSealingResponse stdout) of
            SealSuccess resultS3Name -> do
              mresdata <- getDataFromAmazon amazonConfig resultS3Name
              case mresdata of
                Just resdata -> return $ Just $ BSL.toStrict resdata
                Nothing -> do
                  logAttention "Failed to fetch lambda result from S3" $ object $ [
                      "stdout" `equalsExternalBSL` stdout
                    , "stderr" `equalsExternalBSL` stderr
                    , "s3FileName" .= s3FileName
                    ]
                  return Nothing
            SealFail errorMessage -> do
              logInfo "Lambda sealing failed" $ object [
                  "stdout" `equalsExternalBSL` stdout
                , "stderr" `equalsExternalBSL` stderr
                , "errorMessage" .= errorMessage
                ]
              return $ Nothing

sendDataFileToAmazon :: ( CryptoRNG m, MonadBase IO m, MonadCatch m
                        , MonadLog m, MonadThrow m )
                     => AmazonConfig -> BSL.ByteString -> m (Maybe String)
sendDataFileToAmazon config content = do
  flip catch (\(_ :: FileStorageException) -> return Nothing) $ do
    randomPart <- randomString 10 ['a'..'z']
    timePart   <- filter isDigit <$> show <$> liftBase getCurrentTime
    let name = randomPart ++ timePart
    saveContentsToAmazon config name content
    return $ Just name

getDataFromAmazon :: (MonadBase IO m, MonadCatch m, MonadLog m, MonadThrow m)
                  => AmazonConfig -> String -> m (Maybe BSL.ByteString)
getDataFromAmazon config name =
  (Just <$> getContentsFromAmazon config name)
    `catch` (\(_ :: FileStorageException) -> return Nothing)
