module EID.EIDService.Communication (
    CompleteVerimiEIDServiceTransactionData(..)
  , CompleteIDINEIDServiceTransactionData(..)
  , createVerimiTransactionWithEIDService
  , startVerimiTransactionWithEIDService
  , checkVerimiTransactionWithEIDService
  , createIDINTransactionWithEIDService
  , startIDINTransactionWithEIDService
  , checkIDINTransactionWithEIDService
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

data CompleteVerimiEIDServiceTransactionData = CompleteVerimiEIDServiceTransactionData {
    eidvtdName :: T.Text
  , eidvtdVerifiedEmail :: T.Text
  }

createVerimiTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> DocumentID
  -> SignatoryLinkID
  -> Text
  -> m (EIDServiceTransactionID)
createVerimiTransactionWithEIDService conf did slid redirect = do
      ctx <- getContext
      let redirectUrl = (get ctxDomainUrl ctx) <> "/eid-service/redirect-endpoint/verimi/"
                          <> showt did <> "/" <> showt slid <> "?redirect=" <> redirect
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "POST"
          , "-H", "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          , "--data", "@-"
          ,  T.unpack (eidServiceUrl conf) <> "/api/v1/transaction/new"
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

startVerimiTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m (T.Text)
startVerimiTransactionWithEIDService conf tid = localData [identifier tid] $ do
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "POST"
          , "-H", "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          , "--data", "@-"
          ,  T.unpack $ (eidServiceUrl conf) <> "/api/v1/transaction/"
              <> (fromEIDServiceTransactionID tid) <> "/start"
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

checkVerimiTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m (Maybe EIDServiceTransactionStatus, Maybe CompleteVerimiEIDServiceTransactionData)
checkVerimiTransactionWithEIDService conf tid = localData [identifier tid] $ do
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "GET"
          , "-H", "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          ,  T.unpack $ (eidServiceUrl conf) <> "/api/v1/transaction/"
              <> (fromEIDServiceTransactionID tid)
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
                (Just "new")      -> return (Just EIDServiceTransactionStatusNew,     Nothing)
                (Just "started")  -> return (Just EIDServiceTransactionStatusStarted, Nothing)
                (Just "failed")   -> return (Just EIDServiceTransactionStatusFailed,  Nothing)
                (Just "complete") -> do
                  td <- fromJSValueFieldCustom "providerInfo" $
                          fromJSValueFieldCustom "verimiAuth" $
                            fromJSValueFieldCustom "completionData" $ do
                              mname          <- fromJSValueField "name"
                              memail         <- fromJSValueField "email"
                              memailVerified <- fromJSValueField "emailVerified"
                              case (mname, memail, memailVerified) of
                                (Just name, Just email , Just True) ->
                                  return $ Just $ CompleteVerimiEIDServiceTransactionData {
                                    eidvtdName = T.pack $ name
                                  , eidvtdVerifiedEmail = T.pack $ email
                                  }
                                _ -> return Nothing
                  return $ (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
                _ -> return (Nothing, Nothing)
            _ -> return (Nothing, Nothing)

data CompleteIDINEIDServiceTransactionData = CompleteIDINEIDServiceTransactionData {
    eiditdName :: T.Text
  , eiditdVerifiedEmail :: T.Text
  , eiditdBirthDate :: T.Text
  , eiditCumstomerID :: T.Text
  }

createIDINTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> DocumentID
  -> SignatoryLinkID
  -> Text
  -> m (EIDServiceTransactionID)
createIDINTransactionWithEIDService conf did slid redirect = do
      ctx <- getContext
      let redirectUrl = (get ctxDomainUrl ctx) <> "/eid-service/redirect-endpoint/idin/"
                          <> showt did <> "/" <> showt slid <> "?redirect=" <> redirect
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "POST"
          , "-H", "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          , "--data", "@-"
          ,  T.unpack (eidServiceUrl conf) <> "/api/v1/transaction/new"
          ]
          (A.encode . A.toJSON $ object [
              "method" .= ("auth" :: String)
            , "provider" .= ("nlIDIN" :: String)
            , "redirectUrl" .= redirectUrl
            , "providerParameters" .= object [
                "nlIDIN" .= object [
                  "requestBirthdate" .= True
                ]
              ]
            ]
          )
      case exitcode of
        ExitFailure msg -> do
          logAttention "Failed to create new transaction (eidservice/idin)" $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          internalError
        ExitSuccess -> do
          logInfo "Created new transaction (eidservice/idin) " $ object [
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

startIDINTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m (T.Text)
startIDINTransactionWithEIDService conf tid = localData [identifier tid] $ do
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "POST"
          , "-H", "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          , "--data", "@-"
          ,  T.unpack $ (eidServiceUrl conf) <> "/api/v1/transaction/"
              <> (fromEIDServiceTransactionID tid) <> "/start"
          ]
          (A.encode . A.toJSON $ object [
            ]
          )
      case exitcode of
        ExitFailure msg -> do
          logInfo "Failed to start transaction (eidservice/idin)" $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          internalError
        ExitSuccess -> do
          logInfo "Started transaction (eidservice/idin) " $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            ]
          case (decode $ BSL.toString stdout) of
            Ok jsvalue -> withJSValue jsvalue $ do
              murl <- fromJSValueFieldCustom "providerInfo" $
                        fromJSValueFieldCustom "nlIDINAuth" $
                          fromJSValueField"authUrl"
              case (murl) of
                (Just url) -> return (T.pack url)
                _ -> do
                  logAttention_ "Failed to read 'url' from start transaction response"
                  internalError
            _ -> do
              logAttention_ "Failed to parse start transaction response"
              internalError

checkIDINTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m (Maybe EIDServiceTransactionStatus, Maybe CompleteIDINEIDServiceTransactionData)
checkIDINTransactionWithEIDService conf tid = localData [identifier tid] $ do
      (exitcode, stdout, stderr) <-
        readCurl [
            "-X", "GET"
          , "-H", "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
          , "-H", "Content-Type: application/json"
          ,  T.unpack $ (eidServiceUrl conf) <> "/api/v1/transaction/"
              <> (fromEIDServiceTransactionID tid)
          ]
          BSL.empty
      case exitcode of
        ExitFailure msg -> do
          logAttention "Failed to fetch transaction (eidservice/idin)" $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
          internalError
        ExitSuccess -> do
          logInfo "Fetched new transaction (eidservice/idin) " $ object [
              "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            ]
          case (decode $ BSL.toString stdout) of
            Ok jsvalue -> withJSValue jsvalue $ do
              (mstatus :: Maybe String) <- fromJSValueField "status"
              case (mstatus) of
                (Just "new")      -> return (Just EIDServiceTransactionStatusNew,     Nothing)
                (Just "started")  -> return (Just EIDServiceTransactionStatusStarted, Nothing)
                (Just "failed")   -> return (Just EIDServiceTransactionStatusFailed,  Nothing)
                (Just "complete") -> do
                  td <- fromJSValueFieldCustom "providerInfo" $
                          fromJSValueFieldCustom "nlIDINAuth" $
                            fromJSValueFieldCustom "completionData" $ do
                              msurname    <- fromJSValueField "legalLastName"
                              minitials   <- fromJSValueField "initials"
                              memail      <- fromJSValueField "email"
                              mdob        <- fromJSValueField "birthDate"
                              mcustomerId <- fromJSValueField "customerId"
                              case (msurname, minitials, memail, mdob, mcustomerId) of
                                (Just surname, Just initials , Just email, Just dob, Just customerId) ->
                                  return $ Just $ CompleteIDINEIDServiceTransactionData {
                                    eiditdName = T.pack $ initials ++ " " ++ surname
                                  , eiditdVerifiedEmail = T.pack email
                                  , eiditdBirthDate = T.pack dob
                                  , eiditCumstomerID = T.pack customerId
                                  }
                                _ -> return Nothing
                  return $ (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
                _ -> return (Nothing, Nothing)
            _ -> return (Nothing, Nothing)
