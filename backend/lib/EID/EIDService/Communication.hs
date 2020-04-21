module EID.EIDService.Communication (
    createTransactionWithEIDService
  , startTransactionWithEIDService
  , getTransactionFromEIDService
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Aeson
import Log
import System.Exit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import EID.EIDService.Conf
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
    let verb = T.pack $ show calltype
    logInfo ("Success: " <> verb <> " new transaction (eidservice/" <> provider <> ")")
      $ object ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]

cURLCall
  :: (MonadBase IO m, MonadLog m, ToEIDServiceTransactionProvider a)
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
  guardExitCode calltype (toEIDServiceProviderName provider) (exitcode, stdout, stderr)
  return stdout

createTransactionWithEIDService
  :: (Kontrakcja m, ToJSON a, ToEIDServiceTransactionProvider a, FromJSON b)
  => EIDServiceConf
  -> a
  -> m b
createTransactionWithEIDService conf req = do
  mresp <- fmap decode . cURLCall conf Create req "new" . Just $ encode req
  case mresp of
    Nothing -> do
      logAttention_ "Failed to read create transaction response"
      internalError
    Just resp -> return resp

startTransactionWithEIDService
  :: (Kontrakcja m, ToEIDServiceTransactionProvider a, FromJSON b)
  => EIDServiceConf
  -> a
  -> EIDServiceTransactionID
  -> m b
startTransactionWithEIDService conf provider tid = localData [identifier tid] $ do
  mresp <- decode <$> do
    let endpoint = fromEIDServiceTransactionID tid <> "/start"
    cURLCall conf Start provider endpoint . Just . encode . toJSON $ object []
  case mresp of
    Nothing -> do
      logAttention_ "Failed to parse start transaction response"
      -- TODO: Get rid of all the blank, useless internalError calls in EID.EIDService
      internalError
    Just resp -> return resp

getTransactionFromEIDService
  :: (MonadLog m, MonadBaseControl IO m, ToEIDServiceTransactionProvider a, FromJSON b)
  => EIDServiceConf
  -> a
  -> EIDServiceTransactionID
  -> m (Maybe b)
getTransactionFromEIDService conf provider tid = localData [identifier tid] $ do
  let endpoint = fromEIDServiceTransactionID tid
  decode <$> cURLCall conf Fetch provider endpoint Nothing
