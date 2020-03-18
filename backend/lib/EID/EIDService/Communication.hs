module EID.EIDService.Communication (
    CompleteNemIDEIDServiceTransactionData(..)
  , CompleteVerimiEIDServiceTransactionData(..)
  , CompleteIDINEIDServiceTransactionData(..)
  , createVerimiTransactionWithEIDService
  , startVerimiTransactionWithEIDService
  , checkVerimiTransactionWithEIDService
  , createIDINTransactionWithEIDService
  , startIDINTransactionWithEIDService
  , checkIDINTransactionWithEIDService
  , createNemIDTransactionWithEIDService
  , startNemIDTransactionWithEIDService
  , checkNemIDTransactionWithEIDService
  , createNOBankIDTransactionWithEIDService
  , startNOBankIDTransactionWithEIDService
  , checkNOBankIDTransactionWithEIDService
  , dateOfBirthFromDKPersonalNumber
  ) where

import Control.Monad.Trans.Control
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

createNemIDTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> Text -> Text -> m (EIDServiceTransactionID)
createNemIDTransactionWithEIDService conf locale redirect = do
  let
    providerParams = Just
      $ object ["dkNemID" .= object ["limitedClientMode" .= True, "uiLocale" .= locale]]
  createTransactionWithEIDService conf "dkNemID" providerParams redirect

createNOBankIDTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> Text
  -> Maybe Text
  -> Text
  -> m (EIDServiceTransactionID)
createNOBankIDTransactionWithEIDService conf ssn mPhone redirect = do
  let phonePair = maybe [] (\p -> ["phoneNumber" .= p]) mPhone
      providerParams =
        Just $ object ["noBankID" .= object (["personalNumber" .= ssn] <> phonePair)]
  createTransactionWithEIDService conf "noBankID" providerParams redirect

startNemIDTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> EIDServiceTransactionID -> m (T.Text)
startNemIDTransactionWithEIDService conf tid = do
  response <- startTransactionWithEIDService conf "dkNemID" tid
  extractUrl response
  where
    extractUrl :: Kontrakcja m => JSValue -> m Text
    extractUrl jsValue = withJSValue jsValue $ do
      murl <-
        fromJSValueFieldCustom "providerInfo"
        $ fromJSValueFieldCustom "dkNemIDAuth"
        $ fromJSValueField "authUrl"
      case (murl) of
        (Just url) -> return (T.pack url)
        _          -> do
          logAttention_ "Failed to read 'url' from start transaction response"
          internalError

startNOBankIDTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> EIDServiceTransactionID -> m (T.Text)
startNOBankIDTransactionWithEIDService conf tid = do
  response <- startTransactionWithEIDService conf "noBankID" tid
  extractUrl response
  where
    extractUrl :: Kontrakcja m => JSValue -> m Text
    extractUrl jsValue = withJSValue jsValue $ do
      murl <-
        fromJSValueFieldCustom "providerInfo"
        $ fromJSValueFieldCustom "noBankIDAuth"
        $ fromJSValueField "authUrl"
      case (murl) of
        (Just url) -> return (T.pack url)
        _          -> do
          logAttention_ "Failed to read 'url' from start transaction response"
          internalError

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

createVerimiTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> DocumentID
  -> SignatoryLinkID
  -> Text
  -> m (EIDServiceTransactionID)
createVerimiTransactionWithEIDService conf did slid redirect = do
  ctx <- getContext
  let redirectUrl =
        (ctx ^. #brandedDomain % #url)
          <> "/eid-service/redirect-endpoint/verimi/"
          <> showt did
          <> "/"
          <> showt slid
          <> "?redirect="
          <> redirect
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "POST"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , "--data"
    , "@-"
    , T.unpack (eidServiceUrl conf) <> "/api/v1/transaction/new"
    ]
    (A.encode . A.toJSON $ object
      [ "method" .= ("auth" :: String)
      , "provider" .= ("verimi" :: String)
      , "redirectUrl" .= redirectUrl
      ]
    )
  case exitcode of
    ExitFailure msg -> do
      logAttention "Failed to create new transaction (eidservice/verimi)" $ object
        [ "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "errorMessage" .= msg
        ]
      internalError
    ExitSuccess -> do
      logInfo "Created new transaction (eidservice/verimi) " $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
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

startVerimiTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> EIDServiceTransactionID -> m (T.Text)
startVerimiTransactionWithEIDService conf tid = localData [identifier tid] $ do
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "POST"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , "--data"
    , "@-"
    , T.unpack
    $  (eidServiceUrl conf)
    <> "/api/v1/transaction/"
    <> (fromEIDServiceTransactionID tid)
    <> "/start"
    ]
    (A.encode . A.toJSON $ object [])
  case exitcode of
    ExitFailure msg -> do
      logInfo "Failed to start transaction (eidservice/verimi)" $ object
        [ "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "errorMessage" .= msg
        ]
      internalError
    ExitSuccess -> do
      logInfo "Started transaction (eidservice/verimi) " $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
      case (decode $ BSL.toString stdout) of
        Ok jsvalue -> withJSValue jsvalue $ do
          murl <-
            fromJSValueFieldCustom "providerInfo"
            $ fromJSValueFieldCustom "verimiAuth"
            $ fromJSValueField "authUrl"
          case (murl) of
            (Just url) -> return (T.pack url)
            _          -> do
              logAttention_ "Failed to read 'url' from start transaction response"
              internalError
        _ -> do
          logAttention_ "Failed to parse start transaction response"
          internalError

checkVerimiTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m
       ( Maybe EIDServiceTransactionStatus
       , Maybe CompleteVerimiEIDServiceTransactionData
       )
checkVerimiTransactionWithEIDService conf tid = localData [identifier tid] $ do
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "GET"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , T.unpack
    $  (eidServiceUrl conf)
    <> "/api/v1/transaction/"
    <> (fromEIDServiceTransactionID tid)
    ]
    BSL.empty
  case exitcode of
    ExitFailure msg -> do
      logAttention "Failed to fetch transaction (eidservice/verimi)" $ object
        [ "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "errorMessage" .= msg
        ]
      internalError
    ExitSuccess -> do
      logInfo "Fetched new transaction (eidservice/verimi) " $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
      case (decode $ BSL.toString stdout) of
        Ok jsvalue -> withJSValue jsvalue $ do
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
        _ -> return (Nothing, Nothing)


createIDINTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> Text -> m EIDServiceTransactionID
createIDINTransactionWithEIDService conf redirectUrl = do
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "POST"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , "--data"
    , "@-"
    , T.unpack (eidServiceUrl conf) <> "/api/v1/transaction/new"
    ]
    (A.encode . A.toJSON $ object
      [ "method" .= ("auth" :: String)
      , "provider" .= ("nlIDIN" :: String)
      , "redirectUrl" .= redirectUrl
      , "providerParameters" .= object ["nlIDIN" .= object ["requestBirthdate" .= True]]
      ]
    )
  case exitcode of
    ExitFailure msg -> do
      logAttention "Failed to create new transaction (eidservice/idin)" $ object
        [ "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "errorMessage" .= msg
        ]
      internalError
    ExitSuccess -> do
      logInfo "Created new transaction (eidservice/idin) " $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
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

startIDINTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> EIDServiceTransactionID -> m (T.Text)
startIDINTransactionWithEIDService conf tid = localData [identifier tid] $ do
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "POST"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , "--data"
    , "@-"
    , T.unpack
    $  (eidServiceUrl conf)
    <> "/api/v1/transaction/"
    <> (fromEIDServiceTransactionID tid)
    <> "/start"
    ]
    (A.encode . A.toJSON $ object [])
  case exitcode of
    ExitFailure msg -> do
      logInfo "Failed to start transaction (eidservice/idin)" $ object
        [ "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "errorMessage" .= msg
        ]
      internalError
    ExitSuccess -> do
      logInfo "Started transaction (eidservice/idin) " $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
      case (decode $ BSL.toString stdout) of
        Ok jsvalue -> withJSValue jsvalue $ do
          murl <-
            fromJSValueFieldCustom "providerInfo"
            $ fromJSValueFieldCustom "nlIDINAuth"
            $ fromJSValueField "authUrl"
          case (murl) of
            (Just url) -> return (T.pack url)
            _          -> do
              logAttention_ "Failed to read 'url' from start transaction response"
              internalError
        _ -> do
          logAttention_ "Failed to parse start transaction response"
          internalError

checkIDINTransactionWithEIDService
  :: (MonadLog m, MonadBaseControl IO m)
  => EIDServiceConf
  -> EIDServiceTransactionID
  -> m
       ( Maybe EIDServiceTransactionStatus
       , Maybe CompleteIDINEIDServiceTransactionData
       )
checkIDINTransactionWithEIDService conf tid = localData [identifier tid] $ do
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "GET"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , T.unpack
    $  (eidServiceUrl conf)
    <> "/api/v1/transaction/"
    <> (fromEIDServiceTransactionID tid)
    ]
    BSL.empty
  case exitcode of
    ExitFailure msg -> do
      logAttention "Failed to fetch transaction (eidservice/idin)" $ object
        [ "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "errorMessage" .= msg
        ]
      internalError
    ExitSuccess -> do
      logInfo "Fetched new transaction (eidservice/idin) " $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
      case (decode $ BSL.toString stdout) of
        Ok jsvalue -> withJSValue jsvalue $ do
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
                    surname <- valOrEmptyString <$> fromJSValueField "legalLastName"
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
        _ -> return (Nothing, Nothing)

-- FIXME those 3 functions below are supposed to be a basis for further refactoring of this file
createTransactionWithEIDService
  :: Kontrakcja m
  => EIDServiceConf
  -> Text
  -> Maybe A.Value
  -> Text
  -> m (EIDServiceTransactionID)
createTransactionWithEIDService conf provider providerParams redirectUrl = do
  let paramsPair = fromMaybe [] $ (\p -> ["providerParameters" .= p]) <$> providerParams
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "POST"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , "--data"
    , "@-"
    , T.unpack (eidServiceUrl conf) <> "/api/v1/transaction/new"
    ]
    (  A.encode
    .  A.toJSON
    $  object
    $  [ "method" .= ("auth" :: String)
       , "provider" .= provider
       , "redirectUrl" .= redirectUrl
       ]
    ++ paramsPair
    )
  case exitcode of
    ExitFailure msg -> do
      logAttention ("Failed to create new transaction (eidservice/" <> provider <> ")")
        $ object
            [ "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
      internalError
    ExitSuccess -> do
      logInfo ("Created new transaction (eidservice/" <> provider <> ")") $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
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

getTransactionFromEIDService
  :: (Kontrakcja m)
  => EIDServiceConf
  -> Text
  -> EIDServiceTransactionID
  -> m (Maybe JSValue)
getTransactionFromEIDService conf loggedName tid = localData [identifier tid] $ do
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "GET"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , T.unpack
    $  (eidServiceUrl conf)
    <> "/api/v1/transaction/"
    <> (fromEIDServiceTransactionID tid)
    ]
    BSL.empty
  case exitcode of
    ExitFailure msg -> do
      logAttention ("Failed to fetch transaction (eidservice/" <> loggedName <> ")")
        $ object
            [ "stdout" `equalsExternalBSL` stdout
            , "stderr" `equalsExternalBSL` stderr
            , "errorMessage" .= msg
            ]
      internalError
    ExitSuccess -> do
      logInfo ("Fetched new transaction (eidservice/" <> loggedName <> ")") $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
      case decode $ BSL.toString stdout of
        Ok jsvalue -> return $ Just jsvalue
        _          -> return Nothing

startTransactionWithEIDService
  :: Kontrakcja m => EIDServiceConf -> Text -> EIDServiceTransactionID -> m JSValue
startTransactionWithEIDService conf providerName tid = localData [identifier tid] $ do
  (exitcode, stdout, stderr) <- readCurl
    [ "-X"
    , "POST"
    , "-H"
    , "Authorization: Bearer " <> T.unpack (eidServiceToken conf)
    , "-H"
    , "Content-Type: application/json"
    , "--data"
    , "@-"
    , T.unpack
    $  (eidServiceUrl conf)
    <> "/api/v1/transaction/"
    <> (fromEIDServiceTransactionID tid)
    <> "/start"
    ]
    (A.encode . A.toJSON $ object [])
  case exitcode of
    ExitFailure msg -> do
      logInfo ("Failed to start transaction (eidservice/" <> providerName <> ")") $ object
        [ "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        , "errorMessage" .= msg
        ]
      internalError
    ExitSuccess -> do
      logInfo ("Started transaction (eidservice/" <> providerName <> ")") $ object
        ["stdout" `equalsExternalBSL` stdout, "stderr" `equalsExternalBSL` stderr]
      case (decode $ BSL.toString stdout) of
        Ok jsvalue -> return jsvalue
        _          -> do
          logAttention_ "Failed to parse start transaction response"
          internalError

