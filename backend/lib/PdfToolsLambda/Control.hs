module PdfToolsLambda.Control (
    callPdfToolsSealing
  , callPdfToolsPresealing
) where

import Control.Monad.Base
import Crypto.RNG
import Crypto.RNG.Utils
import Data.Char
import Data.Time
import Log
import System.Exit
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.AWSResult as AWS
import qualified Network.AWS.S3Object as AWS
import qualified Text.JSON as JSON
import qualified Text.JSON.Gen as JSON

import Amazon.Config
import DB
import Doc.SealSpec
import Log.Utils
import PdfToolsLambda.Conf
import PdfToolsLambda.Response
import PdfToolsLambda.Spec
import Utils.IO

callPdfToolsSealing ::
  (MonadDB m, MonadLog m, MonadBase IO m, CryptoRNG m) =>
  PdfToolsLambdaConf -> SealSpec -> m (Maybe BS.ByteString)
callPdfToolsSealing lc spec = do
  inputData <- sealSpecToLambdaSpec spec
  excutePdfToolsLambdaSealCall lc inputData

callPdfToolsPresealing ::
  (MonadDB m, MonadLog m, MonadBase IO m, CryptoRNG m) =>
  PdfToolsLambdaConf -> PreSealSpec -> m (Maybe BS.ByteString)
callPdfToolsPresealing lc spec = do
  inputData <- presealSpecToLambdaSpec spec
  excutePdfToolsLambdaSealCall lc inputData

excutePdfToolsLambdaSealCall ::   (MonadDB m, MonadLog m, MonadBase IO m, CryptoRNG m) =>
  PdfToolsLambdaConf -> BSL.ByteString -> m (Maybe BS.ByteString)
excutePdfToolsLambdaSealCall lc inputData = do
  uploadedDataFileName <- sendDataFileToAmazon (get pdfToolsLambdaS3Config lc) inputData
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
              mresdata <- getDataFromAmazon (get pdfToolsLambdaS3Config lc) resultS3Name
              case mresdata of
                Just resdata -> do
                  return $ Just $ BSL.toStrict resdata
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

sendDataFileToAmazon :: (MonadBase IO m, MonadLog m, CryptoRNG m)
                      => AmazonConfig -> BSL.ByteString -> m (Maybe String)
sendDataFileToAmazon (s3bucket, s3key, s3secret) content = do
  randomPart <- randomString 10 ['a'..'z']
  timePart   <- filter isDigit <$> show <$> liftBase getCurrentTime
  let name = randomPart ++ timePart
      s3Conn = AWS.amazonS3Connection s3key s3secret
      s3Obj = AWS.S3Object { AWS.obj_bucket = s3bucket
                           , AWS.obj_name   = name
                           , AWS.content_type = ""
                           , AWS.obj_headers = []
                           , AWS.obj_data = content
                           }
  result <- liftBase $ sendObjectMIC s3Conn s3Obj
  case result of
    Right _ -> return $ Just name
    Left err -> do
      logAttention  "Failed to upload data to AWS to be used by lambda" $ object [
          "s3bucket" .= s3bucket
        , "name" .= name
        , "error" .= show err
        ]
      return Nothing
  where
    sendObjectMIC :: AWS.AWSConnection -> AWS.S3Object -> IO (AWS.AWSResult ())
    sendObjectMIC aws obj = AWS.sendObject aws obj_w_header where
      obj_w_header = obj { AWS.obj_headers = (AWS.obj_headers obj) ++ md5_header }
      md5_header = [("Content-MD5", (mkMD5 (AWS.obj_data obj)))]
      mkMD5 = BS.unpack . Base64.encode . MD5.hashlazy



getDataFromAmazon :: (MonadBase IO m, MonadLog m, CryptoRNG m)
                      => AmazonConfig -> String -> m (Maybe BSL.ByteString)
getDataFromAmazon (s3bucket, s3key, s3secret) name = do
  let s3Conn = AWS.amazonS3Connection s3key s3secret
      s3Obj = AWS.S3Object { AWS.obj_bucket = s3bucket
                           , AWS.obj_name   = name
                           , AWS.content_type = ""
                           , AWS.obj_headers = []
                           , AWS.obj_data = BSL.empty
                           }
  result <- liftBase $ AWS.getObject s3Conn s3Obj
  case result of
    Right res -> return $ Just $ AWS.obj_data res
    Left err -> do
      logAttention  "Failed to download result of AWS lambda from S3 bucket" $ object [
          "s3bucket" .= s3bucket
        , "name" .= name
        , "error" .= show err
        ]
      return Nothing

