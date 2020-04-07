module EID.EIDService.Communication (
    createTransactionWithEIDService
  , startTransactionWithEIDService
  , getTransactionFromEIDService
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Aeson as A ((.=), object)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Log
import System.Exit
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import EID.EIDService.Conf
import EID.EIDService.JSON
import EID.EIDService.Types
import Kontra hiding (InternalError)
import Log.Identifier
import Log.Utils
import Utils.IO

data CallType = Create | Start | Fetch deriving Show

guardExitCode
  :: (MonadLog m, MonadBase IO m)
  => CallType
  -> Text
  -> (ExitCode, BSL.ByteString, BSL.ByteString)
  -> m ()
guardExitCode calltype provider (exitcode, stdout, stderr) = case exitcode of
  ExitFailure msg -> do
    let verb = T.toLower . T.pack $ show calltype
    logAttention
        ("Failed to " <> verb <> " new transaction (eidservice/" <> provider <> ")")
      $ object
          [ "stdout" `equalsExternalBSL` stdout
          , "stderr" `equalsExternalBSL` stderr
          , "errorMessage" .= msg
          ]
    internalError
  ExitSuccess -> do
    let verb = T.pack $ show calltype <> "ed"
    logInfo (verb <> " new transaction (eidservice/" <> provider <> ")") $ object
      ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]

cURLCall
  :: (MonadBase IO m, MonadLog m, HasEIDServiceName a)
  => EIDServiceConf
  -> CallType
  -> a
  -> Text
  -> Maybe BSL.ByteString
  -> m BSL.ByteString
cURLCall conf calltype provider endpoint mjsonData = do
  let verb = case calltype of
        Create -> "POST"
        Start  -> "POST"
        Fetch  -> "GET"
  (exitcode, stdout, stderr) <-
    readCurl
        (  ["-X", verb]
        ++ ["-H", "Authorization: Bearer " <> T.unpack (eidServiceToken conf)]
        ++ ["-H", "Content-Type: application/json"]
        ++ (if isJust mjsonData then ["--data", "@-"] else [])
        ++ [T.unpack $ eidServiceUrl conf <> "/api/v1/transaction/" <> endpoint]
        )
      $ fromMaybe BSL.empty mjsonData
  guardExitCode calltype (toEIDServiceName provider) (exitcode, stdout, stderr)
  return stdout

createTransactionWithEIDService
  :: (Kontrakcja m)
  => EIDServiceConf
  -> EIDServiceProviderParams
  -> m EIDServiceTransactionID
createTransactionWithEIDService conf providerParams = do
  mtid <-
    fmap A.decode
    . cURLCall conf Create providerParams "new"
    . Just
    . encodingToLazyByteString
    $ encodeNewTransactionRequest providerParams
  case mtid of
    Nothing -> do
      logAttention_ "Failed to read 'id' from create transaction response"
      internalError
    Just tid -> return tid

startTransactionWithEIDService
  :: (Kontrakcja m, HasEIDServiceName a)
  => EIDServiceConf
  -> a
  -> EIDServiceTransactionID
  -> m Text
startTransactionWithEIDService conf provider tid = localData [identifier tid] $ do
  murl <- extractEIDServiceURL <$> do
    let endpoint = fromEIDServiceTransactionID tid <> "/start"
    cURLCall conf Start provider endpoint . Just . A.encode . A.toJSON $ object []
  case murl of
    Nothing -> do
      logAttention_ "Failed to parse start transaction response"
      -- TODO: Get rid of all the blank, useless internalError calls in EID.EIDService
      internalError
    Just url -> return url

getTransactionFromEIDService
  :: (MonadLog m, MonadBaseControl IO m, HasEIDServiceName a, FromCompletionDataJSON b)
  => EIDServiceConf
  -> a
  -> EIDServiceTransactionID
  -> m (Maybe EIDServiceTransactionStatus, Maybe b)
getTransactionFromEIDService conf provider tid = localData [identifier tid] $ do
  let endpoint = fromEIDServiceTransactionID tid
  jsonBS <- cURLCall conf Fetch provider endpoint Nothing
  let mstatus = A.decode jsonBS
  case mstatus of
    Just EIDServiceTransactionStatusCompleteAndSuccess ->
      return (mstatus, decodeCompleteTransactionData jsonBS)
    Just _ -> return (mstatus, Nothing)
    _      -> return (Nothing, Nothing)
