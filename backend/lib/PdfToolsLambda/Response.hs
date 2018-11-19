module PdfToolsLambda.Response (
    parsePdfToolsLambdaSealingResponse
  , PdfToolsLambdaSealingResponse(..)
  , parsePdfToolsLambdaCleaningResponse
  , PdfToolsLambdaCleaningResponse(..)
) where

import Data.Functor.Identity
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL

data PdfToolsLambdaSealingResponse = SealSuccess String | SealFail String

parsePdfToolsLambdaSealingResponse :: BSL.ByteString -> PdfToolsLambdaSealingResponse
parsePdfToolsLambdaSealingResponse bsj = case (decode $ BSL.toString bsj) of
  Ok jsvalue -> runIdentity $ withJSValue jsvalue $ do
    ms3FileName <- fromJSValueField "resultS3FileName"
    case ms3FileName of
      Just s3FileName -> return $ SealSuccess s3FileName
      _ -> do
        merror <- fromJSValueField "error"
        return $ SealFail $ fromMaybe "No error message provided" merror
  _ -> SealFail "Response is not a valid json"


data PdfToolsLambdaCleaningResponse = CleanSuccess String | CleanFail String

parsePdfToolsLambdaCleaningResponse :: BSL.ByteString -> PdfToolsLambdaCleaningResponse
parsePdfToolsLambdaCleaningResponse bsj = case (decode $ BSL.toString bsj) of
  Ok jsvalue -> runIdentity $ withJSValue jsvalue $ do
    ms3FileName <- fromJSValueField "resultS3FileName"
    case ms3FileName of
      Just s3FileName -> return $ CleanSuccess s3FileName
      _ -> do
        merror <- fromJSValueField "error"
        return $ CleanFail $ fromMaybe "No error message provided" merror
  _ -> CleanFail "Response is not a valid json"


