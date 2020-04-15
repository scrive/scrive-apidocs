module EID.EIDService.Control (
    eidServiceRoutes
  ) where

import Data.Aeson ((.=), Value, object)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
import Network.HTTP.Base (urlEncode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import DB
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocUtils
import Doc.SignatoryLinkID
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Provider
import EID.EIDService.Types
import Happstack.Fields
import InputValidation (asValidPhoneForNorwegianBankID, resultToMaybe)
import Kontra hiding (InternalError)
import MinutesTime
import Routing
import Session.Model
import Templates (renderTextTemplate)
import User.Lang
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils

eidServiceRoutes :: Route (Kontra Response)
eidServiceRoutes = choice
  -- start endpoints
  [ dir "start" . param . hPost . toK3 $ startEIDServiceTransaction
  -- redirect endpoints
  , (dir "redirect-endpoint" . param . hGet . toK3)
    redirectEndpointFromEIDServiceTransaction
  ]

eidServiceConf :: Kontrakcja m => Document -> m EIDServiceConf
eidServiceConf doc = do
  ctx <- getContext
  case ctx ^. #eidServiceConf of
    Nothing    -> noConfigurationError "No eid service provided"
    Just conf0 -> do
      let err =
            unexpectedError $ "Impossible happened - no author for document: " <> showt
              (documentid doc)
      authorid <- maybe err return $ maybesignatory =<< getAuthorSigLink doc
      ugwp     <- dbQuery . UserGroupGetWithParentsByUserID $ authorid
      return $ case ugwpSettings ugwp ^. #eidServiceToken of
        Nothing    -> conf0
        Just token -> set #eidServiceToken token conf0

startEIDServiceTransaction
  :: Kontrakcja m
  => (EIDServiceTransactionProvider, EIDServiceEndpointType)
  -> DocumentID
  -> SignatoryLinkID
  -> m Value
startEIDServiceTransaction (provider, epType) did slid = do
  let providerName = toEIDServiceName provider
  logInfo_ $ "EID Service transaction start - for " <> providerName <> " view"
  (doc, sl) <- getDocumentAndSignatoryForEIDAuth did slid -- also access guard
  let authKind = case epType of
        EIDServiceAuthEndpoint -> EIDServiceAuthToView $ mkAuthKind doc
        EIDServiceSignEndpoint -> EIDServiceAuthToSign
  -- TODO: This function should be broken up into logical tasks
  conf           <- eidServiceConf doc
  redirectUrl    <- makeRedirectURL doc sl authKind provider
  providerParams <- case provider of
    EIDServiceTransactionProviderNLIDIN ->
      return EIDServiceProviderParamsNLIDIN { esppRedirectURL = redirectUrl }
    EIDServiceTransactionProviderVerimi ->
      return EIDServiceProviderParamsVerimi { esppRedirectURL = redirectUrl }
    EIDServiceTransactionProviderDKNemID -> do
      let locale = case documentlang doc of
            LANG_SV -> "sv-SE"
            LANG_NO -> "nb-NO"
            LANG_DA -> "da-DK"
            _       -> "en-GB"
      return EIDServiceProviderParamsDKNemID { esppRedirectURL = redirectUrl
                                             , esppUILocale    = locale
                                             }
    EIDServiceTransactionProviderNOBankID -> do
      personalNumberField <-
        guardJust . getFieldByIdentity PersonalNumberFI . signatoryfields $ sl
      ssn <- guardJust . fieldTextValue $ personalNumberField
      let mNonEmptyNOPhone = case getMobile sl of
            "" -> Nothing
            p  -> resultToMaybe . asValidPhoneForNorwegianBankID $ p
      return EIDServiceProviderParamsNOBankID { esppRedirectURL    = redirectUrl
                                              , esppPhoneNumber    = mNonEmptyNOPhone
                                              , esppPersonalNumber = ssn
                                              }
    EIDServiceTransactionProviderFITupas ->
      return EIDServiceProviderParamsFITupas { esppRedirectURL = redirectUrl }
  tid  <- createTransactionWithEIDService conf providerParams
  turl <- startTransactionWithEIDService conf provider tid
  sid  <- getNonTempSessionID
  now  <- currentTime
  let newTransaction = EIDServiceTransactionFromDB
        { estID              = tid
        , estStatus          = EIDServiceTransactionStatusStarted
        , estSignatoryLinkID = signatorylinkid sl
        , estAuthKind        = authKind
        , estProvider        = provider
        , estSessionID       = sid
        , estDeadline        = 60 `minutesAfter` now
        }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return $ object ["accessUrl" .= turl]

-- TODO: construction of redirect URL should be handled by JSON module
makeRedirectURL
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceAuthenticationKind
  -> EIDServiceTransactionProvider
  -> m Text
makeRedirectURL doc sl eidserviceAuthKind provider = do
  ctx        <- getContext
  rdFragment <- case eidserviceAuthKind of
    EIDServiceAuthToView _ -> do
      -- TODO: Why are we getting this redirect from the frontend in the first place?
      rd <- guardJustM $ getField "redirect"
      return $ "?redirect=" <> rd
    EIDServiceAuthToSign -> return ""
  return
    $  (ctx ^. #brandedDomain % #url)
    <> "/eid-service/redirect-endpoint/"
    <> redirectFragment
    <> "/"
    <> showt (documentid doc)
    <> "/"
    <> showt (signatorylinkid sl)
    <> rdFragment
  where
    redirectFragment = case (provider, eidserviceAuthKind) of
      (EIDServiceTransactionProviderNLIDIN{}, EIDServiceAuthToView _) -> "idin-view"
      (EIDServiceTransactionProviderNLIDIN{}  , EIDServiceAuthToSign) -> "idin-sign"
      (EIDServiceTransactionProviderFITupas{}, EIDServiceAuthToView _) -> "fitupas-view"
      (EIDServiceTransactionProviderFITupas{} , EIDServiceAuthToSign) -> "fitupas-sign"
      (EIDServiceTransactionProviderVerimi{}  , _                   ) -> "verimi-view"
      (EIDServiceTransactionProviderDKNemID{} , _                   ) -> "nemid-view"
      (EIDServiceTransactionProviderNOBankID{}, _                   ) -> "nobankid-view"

checkEIDServiceTransactionForSignatory
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceTransactionProvider
  -> EIDServiceAuthenticationKind
  -> m (Maybe (EIDServiceTransactionFromDB, CompleteEIDServiceTransaction))
checkEIDServiceTransactionForSignatory doc sl provider authKind = do
  sessionID <- getNonTempSessionID
  conf      <- eidServiceConf doc
  mest      <- dbQuery
    $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
  case mest of
    Nothing  -> return Nothing
    Just est -> fmap (est, ) <$> getTransactionFromEIDService conf provider (estID est)

-- Redirect endpoints

redirectEndpointFromEIDServiceTransaction
  :: Kontrakcja m
  => (EIDServiceTransactionProvider, EIDServiceEndpointType)
  -> DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromEIDServiceTransaction (provider, authKind) did slid =
  case authKind of
    EIDServiceAuthEndpoint ->
      redirectEndpointFromEIDServiceAuthTransaction provider did slid
    EIDServiceSignEndpoint ->
      redirectEndpointFromEIDServiceSignTransaction provider did slid

-- Auth redirect endpoints

redirectEndpointFromEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromEIDServiceAuthTransaction provider did slid = do
  logInfo_ "EID Service transaction check"
  (doc, sl) <- getDocumentAndSignatoryForEIDAuth did slid -- access guard
  ad        <- getAnalyticsData
  ctx       <- getContext
  rd        <- guardJustM $ getField "redirect"
  res <- checkEIDServiceTransactionForSignatory doc
                                                sl
                                                provider
                                                (EIDServiceAuthToView $ mkAuthKind doc)
  updateAction <- case provider of
    EIDServiceTransactionProviderVerimi   -> return updateVerimiTransactionAfterCheck
    EIDServiceTransactionProviderNLIDIN   -> return updateIDINTransactionAfterCheck
    EIDServiceTransactionProviderDKNemID  -> return updateNemIDTransactionAfterCheck
    EIDServiceTransactionProviderNOBankID -> return updateNOBankIDTransactionAfterCheck
    EIDServiceTransactionProviderFITupas ->
      unexpectedError "FITupas auth not supported via EID service"
  mts <- case res of
    Just (est, ct) -> Just <$> updateAction slid est ct
    _              -> return Nothing
  redirectPage <- renderTextTemplate "postEIDAuthRedirect" $ do
    F.value "redirect" rd
    F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage

-- Sign redirect endpoints

redirectEndpointFromEIDServiceSignTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> DocumentID
  -> SignatoryLinkID
  -> m Response
redirectEndpointFromEIDServiceSignTransaction provider did slid = do
  logInfo_ "EID Service signing transaction check"
  (doc, sl) <- getDocumentAndSignatoryForEIDAuth did slid -- access guard
  ad        <- getAnalyticsData
  ctx       <- getContext
  res       <- checkEIDServiceTransactionForSignatory doc sl provider EIDServiceAuthToSign
  let
    redirectUrl       = "/s/" <> show did <> "/" <> show slid
    correctDataNLIDIN = case snd <$> res of
      Just (CompleteNLIDINEIDServiceTransaction EIDServiceTransactionStatusCompleteAndSuccess (Just _cd))
        -> True
      -- the transaction cannot be just New or Started now,
      -- we were redirected here at the end of EID Auth
      _ -> False
    correctDataFITupas = case snd <$> res of
      Just (CompleteFITupasEIDServiceTransaction EIDServiceTransactionStatusCompleteAndSuccess (Just cd))
        -> T.null (getPersonalNumber sl)
          || isNothing (eidtupasSSN cd)
          || (getPersonalNumber sl == fromMaybe "" (eidtupasSSN cd))
      -- the transaction cannot be just New or Started now,
      -- we were redirected here at the end of EID Auth
      _ -> False
  correctData <- case provider of
    EIDServiceTransactionProviderNLIDIN -> return correctDataNLIDIN
    EIDServiceTransactionProviderFITupas -> return correctDataFITupas
    _ -> unexpectedError "This provider does not support auth at point of signing"
  redirectPage <- renderTextTemplate "postEIDAuthAtSignRedirect" $ do
    F.value "redirect" . B64.encode . BSC8.pack . urlEncode $ redirectUrl
    F.value "incorrect_data" $ not correctData
    F.value "document_id" $ show did
    F.value "signatory_link_id" $ show slid
    standardPageFields ctx Nothing ad
  simpleHtmlResponse redirectPage
