module EID.EIDService.Communication (
    CompleteNemIDEIDServiceTransactionData(..)
  , CompleteVerimiEIDServiceTransactionData(..)
  , CompleteIDINEIDServiceTransactionData(..)
  , createTransactionWithEIDService
  , startTransactionWithEIDService
  , checkVerimiTransactionWithEIDService
  , checkIDINTransactionWithEIDService
  , checkNemIDTransactionWithEIDService
  , checkNOBankIDTransactionWithEIDService
  , dateOfBirthFromDKPersonalNumber
  , checkFITupasTransactionWithEIDService
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Aeson ((.=), object)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Log
import System.Exit
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text as T

import EID.EIDService.Conf
import EID.EIDService.JSON
import EID.EIDService.Types
import Kontra hiding (InternalError)
import Log.Identifier
import Log.Utils
import Utils.IO

checkNemIDTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m
       ( Maybe EIDServiceTransactionStatus
       , Maybe CompleteNemIDEIDServiceTransactionData
       )
checkNemIDTransactionWithEIDService conf tid = do
  mtd <- getTransactionFromEIDService conf "dkNemID" tid
  case mtd of
    Just td -> getTransactionDataAndStatus td
    _       -> return (Nothing, Nothing)
  where
    getTransactionDataAndStatus
      :: Kontrakcja m
      => JSValue
      -> m
           ( Maybe EIDServiceTransactionStatus
           , Maybe CompleteNemIDEIDServiceTransactionData
           )
    getTransactionDataAndStatus jsValue = withJSValue jsValue $ do
      (mstatus :: Maybe String) <- fromJSValueField "status"
      case (mstatus) of
        (Just "new"     ) -> return (Just EIDServiceTransactionStatusNew, Nothing)
        (Just "started" ) -> return (Just EIDServiceTransactionStatusStarted, Nothing)
        (Just "failed"  ) -> return (Just EIDServiceTransactionStatusFailed, Nothing)
        (Just "complete") -> do
          mip <-
            fromJSValueFieldCustom "providerParameters"
            $ fromJSValueFieldCustom "auth"
            $ fromJSValueFieldCustom "dkNemID"
            $ fromJSValueField "method"
          td <-
            fromJSValueFieldCustom "providerInfo"
            $ fromJSValueFieldCustom "dkNemIDAuth"
            $ fromJSValueFieldCustom "completionData"
            $ do
                mpid <- fromJSValueField "pid"
                mssn <- fromJSValueField "ssn"
                mcer <- fromJSValueFieldCustom "certificateData"
                  $ fromJSValueField "certificate"
                mdn <- fromJSValueFieldCustom "certificateData"
                  $ fromJSValueField "distinguishedName"
                mdob <- return $ dateOfBirthFromDKPersonalNumber <$> mssn
                case (mpid, mssn, mcer, mdn, mdob, mip) of
                  (Just pid, Just ssn, Just certificate, Just distinguishedName, Just dateOfBirth, mInternalProvider)
                    -> return $ Just CompleteNemIDEIDServiceTransactionData
                      { eidnidInternalProvider = resolveInternalProvider mInternalProvider
                      , eidnidSSN               = ssn
                      , eidnidBirthDate         = dateOfBirth
                      , eidnidCertificate       = certificate
                      , eidnidDistinguishedName = distinguishedName
                      , eidnidPid               = pid
                      }
                  _ -> return Nothing
          return (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
        _ -> return (Nothing, Nothing)
    resolveInternalProvider :: Maybe Text -> EIDServiceNemIDInternalProvider
    resolveInternalProvider mt = case mt of
      -- TODO: decide if we want to distinct between personal keycard and employee keycard - this would break backward
      -- data compability, but maybe doesn't hurt us that much?
      Nothing                -> EIDServiceNemIDKeyCard
      Just "PersonalKeycard" -> EIDServiceNemIDKeyCard
      Just "EmployeeKeycard" -> EIDServiceNemIDKeyCard
      Just "EmployeeKeyFile" -> EIDServiceNemIDKeyFile
      Just t ->
        unexpectedError $ "unknown internal provider returned from EID service" <> t

checkNOBankIDTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m
       ( Maybe EIDServiceTransactionStatus
       , Maybe CompleteNOBankIDEIDServiceTransactionData
       )
checkNOBankIDTransactionWithEIDService conf tid = do
  mtd <- getTransactionFromEIDService conf "noBankID" tid
  case mtd of
    Just td -> getTransactionDataAndStatus td
    _       -> return (Nothing, Nothing)
  where
    getTransactionDataAndStatus
      :: Kontrakcja m
      => JSValue
      -> m
           ( Maybe EIDServiceTransactionStatus
           , Maybe CompleteNOBankIDEIDServiceTransactionData
           )
    getTransactionDataAndStatus jsValue = withJSValue jsValue $ do
      (mstatus :: Maybe String) <- fromJSValueField "status"
      case (mstatus) of
        (Just "new"     ) -> return (Just EIDServiceTransactionStatusNew, Nothing)
        (Just "started" ) -> return (Just EIDServiceTransactionStatusStarted, Nothing)
        (Just "failed"  ) -> return (Just EIDServiceTransactionStatusFailed, Nothing)
        (Just "complete") -> do
          td <-
            fromJSValueFieldCustom "providerInfo"
            $ fromJSValueFieldCustom "noBankIDAuth"
            $ fromJSValueFieldCustom "completionData"
            $ do
                mpid <- fromJSValueField "pid"
                mcer <- fromJSValueFieldCustom "certificateData"
                  $ fromJSValueField "certificate"
                midn <- fromJSValueField "issuerDN"
                mdn  <- fromJSValueFieldCustom "certificateData"
                  $ fromJSValueField "distinguishedName"
                mdob <- fromJSValueFieldCustom "profileData"
                  $ fromJSValueField "birthdate"
                mn <- fromJSValueFieldCustom "profileData" $ fromJSValueField "name"
                mphone <- fromJSValueField "phoneNumber"
                mUsedMobileBankID <- fromJSValueField "usedMobileBankID"
                case (mpid, mdn, midn, mUsedMobileBankID) of
                  (Just pid, Just dn, Just idn, Just usedMobileBankID) -> return $ Just
                    CompleteNOBankIDEIDServiceTransactionData
                      { eidnobidInternalProvider        = if usedMobileBankID
                                                            then EIDServiceNOBankIDMobile
                                                            else EIDServiceNOBankIDStandard
                      , eidnobidName                    = mn
                      , eidnobidBirthDate               = mdob
                      , eidnobidDistinguishedName       = dn
                      , eidnobidIssuerDistinguishedName = idn
                      , eidnobidCertificate             = mcer
                      , eidnobidPhoneNumber             = mphone
                      , eidnobidPid                     = pid
                      }
                  _ -> return Nothing
          return (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
        _ -> return (Nothing, Nothing)

dateOfBirthFromDKPersonalNumber :: Text -> Text
dateOfBirthFromDKPersonalNumber personalnumber =
  case T.chunksOf 2 (T.take 6 $ personalnumber) of
    [day, month, year] ->
      let
        yearWithoutCentury = read year
        firstDigitOfSequenceNumber = T.index personalnumber 7
        century = showt $ resolveCentury yearWithoutCentury firstDigitOfSequenceNumber
      in
        day <> "." <> month <> "." <> century <> year
    _ ->
      unexpectedError
        $  "This personal number cannot be formatted to date: "
        <> personalnumber
  where
    resolveCentury :: Int -> Char -> Int
    resolveCentury yearWithoutCentury firstDigitOfSequenceNumber
      | firstDigitOfSequenceNumber < '4'
      = 19
      | firstDigitOfSequenceNumber == '4'
      = if yearWithoutCentury < 37 then 20 else 19
      | firstDigitOfSequenceNumber > '4' && firstDigitOfSequenceNumber < '9'
      = if yearWithoutCentury < 58 then 20 else 18
      | otherwise
      = if yearWithoutCentury < 37 then 20 else 19

data CompleteVerimiEIDServiceTransactionData = CompleteVerimiEIDServiceTransactionData {
    eidvtdName :: T.Text
  , eidvtdVerifiedEmail :: T.Text
  }

checkVerimiTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m
       ( Maybe EIDServiceTransactionStatus
       , Maybe CompleteVerimiEIDServiceTransactionData
       )
checkVerimiTransactionWithEIDService conf tid = do
  mtd <- getTransactionFromEIDService conf "verimi" tid
  case mtd of
    Just td -> getTransactionDataAndStatus td
    _       -> return (Nothing, Nothing)
  where
    getTransactionDataAndStatus
      :: Kontrakcja m
      => JSValue
      -> m
           ( Maybe EIDServiceTransactionStatus
           , Maybe CompleteVerimiEIDServiceTransactionData
           )
    getTransactionDataAndStatus jsValue = withJSValue jsValue $ do
      (mstatus :: Maybe String) <- fromJSValueField "status"
      case (mstatus) of
        (Just "new"     ) -> return (Just EIDServiceTransactionStatusNew, Nothing)
        (Just "started" ) -> return (Just EIDServiceTransactionStatusStarted, Nothing)
        (Just "failed"  ) -> return (Just EIDServiceTransactionStatusFailed, Nothing)
        (Just "complete") -> do
          td <-
            fromJSValueFieldCustom "providerInfo"
            $ fromJSValueFieldCustom "verimiAuth"
            $ fromJSValueFieldCustom "completionData"
            $ do
                mname          <- fromJSValueField "name"
                memail         <- fromJSValueField "email"
                memailVerified <- fromJSValueField "emailVerified"
                case (mname, memail, memailVerified) of
                  (Just name, Just email, Just True) ->
                    return $ Just $ CompleteVerimiEIDServiceTransactionData
                      { eidvtdName          = T.pack $ name
                      , eidvtdVerifiedEmail = T.pack $ email
                      }
                  _ -> return Nothing
          return $ (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
        _ -> return (Nothing, Nothing)

checkIDINTransactionWithEIDService
  :: (MonadLog m, MonadBaseControl IO m)
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m
       ( Maybe EIDServiceTransactionStatus
       , Maybe CompleteIDINEIDServiceTransactionData
       )
checkIDINTransactionWithEIDService conf tid = localData [identifier tid] $ do
  mtd <- getTransactionFromEIDService conf "nlIDIN" tid
  case mtd of
    Just td -> getTransactionDataAndStatus td
    _       -> return (Nothing, Nothing)
  where
    getTransactionDataAndStatus
      :: (MonadLog m, MonadBaseControl IO m)
      => JSValue
      -> m
           ( Maybe EIDServiceTransactionStatus
           , Maybe CompleteIDINEIDServiceTransactionData
           )
    getTransactionDataAndStatus jsValue = withJSValue jsValue $ do
      (mstatus :: Maybe String) <- fromJSValueField "status"
      case (mstatus) of
        (Just "new"     ) -> return (Just EIDServiceTransactionStatusNew, Nothing)
        (Just "started" ) -> return (Just EIDServiceTransactionStatusStarted, Nothing)
        (Just "failed"  ) -> return (Just EIDServiceTransactionStatusFailed, Nothing)
        (Just "complete") -> do
          td <-
            fromJSValueFieldCustom "providerInfo"
            $ fromJSValueFieldCustom "nlIDINAuth"
            $ fromJSValueFieldCustom "completionData"
            $ do
                let valOrEmptyString = maybe "" T.pack
                surname        <- valOrEmptyString <$> fromJSValueField "legalLastName"
                initials       <- valOrEmptyString <$> fromJSValueField "initials"
                dob            <- valOrEmptyString <$> fromJSValueField "birthDate"
                customerId     <- valOrEmptyString <$> fromJSValueField "customerId"
                mTussenvoegsel <- fromJSValueField "legalLastNamePrefix"
                let eiditdName = case mTussenvoegsel of
                      Just tussenvoegsel ->
                        initials <> " " <> tussenvoegsel <> " " <> surname
                      Nothing -> initials <> " " <> surname
                return $ Just $ CompleteIDINEIDServiceTransactionData
                  { eiditdName       = eiditdName
                  , eiditdBirthDate  = dob
                  , eiditdCustomerID = customerId
                  }
          return $ (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
        _ -> return (Nothing, Nothing)

checkFITupasTransactionWithEIDService
  :: (MonadLog m, MonadBaseControl IO m)
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m
       ( Maybe EIDServiceTransactionStatus
       , Maybe CompleteFITupasEIDServiceTransactionData
       )
checkFITupasTransactionWithEIDService conf tid = localData [identifier tid] $ do
  mtd <- getTransactionFromEIDService conf "fiTupas" tid
  case mtd of
    Just td -> getTransactionDataAndStatus td
    _       -> return (Nothing, Nothing)
  where
    getTransactionDataAndStatus
      :: (MonadLog m, MonadBaseControl IO m)
      => JSValue
      -> m
           ( Maybe EIDServiceTransactionStatus
           , Maybe CompleteFITupasEIDServiceTransactionData
           )
    getTransactionDataAndStatus jsValue = withJSValue jsValue $ do
      (mstatus :: Maybe String) <- fromJSValueField "status"
      case (mstatus) of
        (Just "new"     ) -> return (Just EIDServiceTransactionStatusNew, Nothing)
        (Just "started" ) -> return (Just EIDServiceTransactionStatusStarted, Nothing)
        (Just "failed"  ) -> return (Just EIDServiceTransactionStatusFailed, Nothing)
        (Just "complete") -> do
          td <-
            fromJSValueFieldCustom "providerInfo"
            $ fromJSValueFieldCustom "fiTupasAuth"
            $ fromJSValueFieldCustom "completionData"
            $ do
                mname <- fromJSValueFieldCustom "profileData" $ fromJSValueField "name"
                mdob  <- fromJSValueFieldCustom "profileData"
                  $ fromJSValueField "birthdate"
                mdn <- fromJSValueFieldCustom "certificateData"
                  $ fromJSValueField "distinguishedName"
                mbank <- fromJSValueField "bank"
                mpid  <- fromJSValueField "pid"
                mssn  <- fromJSValueField "ssn"
                case (mname, mdob, mdn, mpid) of
                  (Just name, Just dob, Just dn, Just pid) -> return $ Just
                    CompleteFITupasEIDServiceTransactionData
                      { eidtupasName              = name
                      , eidtupasBirthDate         = dob
                      , eidtupasDistinguishedName = dn
                      , eidtupasBank              = mbank
                      , eidtupasPid               = pid
                      , eidtupasSSN               = mssn
                      }
                  _ -> return Nothing
          return $ (Just EIDServiceTransactionStatusCompleteAndSuccess, td)
        _ -> return (Nothing, Nothing)

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
  :: (MonadBase IO m, MonadLog m)
  => EIDServiceConf
  -> CallType
  -> Text
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
  guardExitCode calltype provider (exitcode, stdout, stderr)
  return stdout

createTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceProviderParams
  -> m (EIDServiceTransactionID)
createTransactionWithEIDService conf providerParams = do
  -- TODO: This function has repetition in many places
  let provider = case providerParams of
        EIDServiceProviderParamsVerimi{}   -> "verimi"
        EIDServiceProviderParamsIDIN{}     -> "nlIDIN"
        EIDServiceProviderParamsNemID{}    -> "dkNemID"
        EIDServiceProviderParamsNOBankID{} -> "noBankID"
        EIDServiceProviderParamsFITupas{}  -> "fiTupas"
  stdout <-
    cURLCall conf Create provider "new"
    . Just
    . encodingToLazyByteString
    $ encodeNewTransactionRequest providerParams
  case (decode $ BSL.toString stdout) of
    Ok jsvalue -> withJSValue jsvalue $ do
      mtid <- fromJSValueField "id"
      case (mtid) of
        (Just tid) -> return (unsafeEIDServiceTransactionID $ T.pack tid)
        _          -> do
          logAttention_ "Failed to read 'id' from create transaction response"
          internalError
    _ -> do
      logAttention_ "Failed to parse create transaction response"
      internalError

startTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> Text -> EIDServiceTransactionID -> m Text
startTransactionWithEIDService conf provider tid = localData [identifier tid] $ do
  stdout <- do
    let endpoint = fromEIDServiceTransactionID tid <> "/start"
    cURLCall conf Start provider endpoint . Just . A.encode . A.toJSON $ object []
  extractUrl =<< case (decode $ BSL.toString stdout) of
    Ok jsvalue -> return jsvalue
    _          -> do
      logAttention_ "Failed to parse start transaction response"
      internalError
  where
    -- Move this parsing to the JSON module
    extractUrl :: Kontrakcja m => JSValue -> m Text
    extractUrl jsValue = withJSValue jsValue $ do
      murl <-
        fromJSValueFieldCustom "providerInfo"
        $ fromJSValueFieldCustom (T.unpack provider <> "Auth")
        $ fromJSValueField "authUrl"
      case (murl) of
        (Just url) -> return (T.pack url)
        _          -> do
          logAttention_ "Failed to read 'url' from start transaction response"
          internalError

getTransactionFromEIDService
  :: (MonadLog m, MonadBaseControl IO m)
  => EIDServiceConf
  -> Text
  -> EIDServiceTransactionID
  -> m (Maybe JSValue)
getTransactionFromEIDService conf provider tid = localData [identifier tid] $ do
  stdout <- do
    let endpoint = fromEIDServiceTransactionID tid
    cURLCall conf Fetch provider endpoint Nothing
  case decode $ BSL.toString stdout of
    Ok jsvalue -> return $ Just jsvalue
    _          -> return Nothing
