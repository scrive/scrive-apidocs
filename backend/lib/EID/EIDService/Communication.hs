module EID.EIDService.Communication (
    createVerimiTransactionWithEIDService
  , startVerimiTransactionWithEIDService
  , CompleteEIDServiceTransactionData(..)
  , checkVerimiTransactionWithEIDService
  ) where

import Data.Aeson ((.=), object)
import Log
import System.Exit
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text as T

import Doc.DocumentID
import Doc.SignatoryLinkID
import EID.EIDService.Conf
import EID.EIDService.Types
import Kontra hiding (InternalError)
import Log.Identifier
import Log.Utils
import Utils.IO

createVerimiTransactionWithEIDService :: Kontrakcja m => EIDServiceConf -> DocumentID -> SignatoryLinkID -> String -> m (EIDServiceTransactionID)
createVerimiTransactionWithEIDService conf did slid redirect = do
      ctx <- getContext
      let redirectUrl = (get ctxDomainUrl ctx) ++ "/eid-service/redirect-endpoint/verimi/" ++ show did ++ "/" ++ show slid ++ "?redirect=" ++ redirect
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "POST"
          , "-H",  "Authorization: Bearer " ++ T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          , "--data", "@-"
          ,  T.unpack (eidServiceUrl conf) ++ "/api/v1/transaction/new"
          ]
          (A.encode . A.toJSON $ object [
              "method" .= ("auth" :: String)
            , "provider" .= ("verimi" :: String)
            , "redirectUrl" .= redirectUrl
            ]
          )
      case exitcode of
        ExitFailure msg -> do
          logAttention "Failed to create new transaction (eidservice/verimi)" $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          internalError
        ExitSuccess -> do
          logInfo "Created new transaction (eidservice/verimi) " $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            ]
          case (decode $ BSL.toString stdout) of
            Ok jsvalue -> withJSValue jsvalue $ do
              mtid <- fromJSValueField "id"
              case (mtid) of
                (Just tid) -> return (unsafeEIDServiceTransactionID $ T.pack tid)
                _ -> do
                  logAttention_ "Failed to read 'id' from create transaction response"
                  internalError
            _ -> do
              logAttention_ "Failed to parse create transaction response"
              internalError

startVerimiTransactionWithEIDService :: Kontrakcja m => EIDServiceConf -> EIDServiceTransactionID ->  m (T.Text)
startVerimiTransactionWithEIDService conf tid = localData [identifier tid] $ do
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "POST"
          , "-H",  "Authorization: Bearer " ++ T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          , "--data", "@-"
          ,  T.unpack $ (eidServiceUrl conf) <> "/api/v1/transaction/" <> (fromEIDServiceTransactionID tid) <> "/start"
          ]
          (A.encode . A.toJSON $ object [
            ]
          )
      case exitcode of
        ExitFailure msg -> do
          logInfo "Failed to start transaction (eidservice/verimi)" $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          internalError
        ExitSuccess -> do
          logInfo "Started transaction (eidservice/verimi) " $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            ]
          case (decode $ BSL.toString stdout) of
            Ok jsvalue -> withJSValue jsvalue $ do
              murl <- fromJSValueFieldCustom "providerInfo" $
                        fromJSValueFieldCustom "verimiAuth" $
                          fromJSValueField"authUrl"
              case (murl) of
                (Just url) -> return (T.pack url)
                _ -> do
                  logAttention_ "Failed to read 'url' from start transaction response"
                  internalError
            _ -> do
              logAttention_ "Failed to parse start transaction response"
              internalError

data CompleteEIDServiceTransactionData = CompleteEIDServiceTransactionData {
    eidtdName :: T.Text
  , eidtdVerifiedEmail :: T.Text
  }

checkVerimiTransactionWithEIDService :: Kontrakcja m => EIDServiceConf -> EIDServiceTransactionID-> m (Maybe EIDServiceTransactionStatus, Maybe CompleteEIDServiceTransactionData)
checkVerimiTransactionWithEIDService conf tid = localData [identifier tid] $ do
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "GET"
          , "-H",  "Authorization: Bearer " ++ T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          ,  T.unpack $ (eidServiceUrl conf) <> "/api/v1/transaction/" <> (fromEIDServiceTransactionID tid)
          ]
          BSL.empty
      case exitcode of
        ExitFailure msg -> do
          logAttention "Failed to fetch transaction (eidservice/verimi)" $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          internalError
        ExitSuccess -> do
          logInfo "Fetched new transaction (eidservice/verimi) " $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            ]
          case (decode $ BSL.toString stdout) of
            Ok jsvalue -> withJSValue jsvalue $ do
              (mstatus :: Maybe String) <- fromJSValueField "status"
              case (mstatus) of
                (Just "new")      -> return $ (Just EIDServiceTransactionStatusNew, Nothing)
                (Just "started")  -> return $ (Just EIDServiceTransactionStatusStarted, Nothing)
                (Just "failed")   -> return $ (Just EIDServiceTransactionStatusFailed, Nothing)
                (Just "complete") -> do
                  td <- fromJSValueFieldCustom "providerInfo" $ do
                          fromJSValueFieldCustom "verimiAuth" $ do
                            fromJSValueFieldCustom "completionData" $ do
                              mname <- fromJSValueField "name"
                              memail <- fromJSValueField "email"
                              memailVerified <- fromJSValueField "emailVerified"
                              case (mname, memail, memailVerified) of
                                (Just name, Just email , Just True) -> return $ Just $ CompleteEIDServiceTransactionData {
                                    eidtdName = T.pack $ name
                                  , eidtdVerifiedEmail = T.pack $ email
                                  }
                                _ -> return Nothing
                  return $ (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
                _ -> return (Nothing, Nothing)
            _ -> return (Nothing, Nothing)
