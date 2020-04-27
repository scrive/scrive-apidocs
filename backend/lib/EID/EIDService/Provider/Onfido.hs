module EID.EIDService.Provider.Onfido (
    beginEIDServiceTransaction
  , OnfidoEIDServiceCompletionData(..)
  , completeEIDServiceSignTransaction
 ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Log

import DB
import Doc.DocStateData
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Types
import Happstack.Fields
import Kontra hiding (InternalError)
import Session.Model
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderOnfido

newtype OnfidoEIDServiceProviderParams = OnfidoEIDServiceProviderParams {
    onfidoparamMethod :: OnfidoMethod
  }

instance ToJSON OnfidoEIDServiceProviderParams where
  toJSON _ = Null
  toEncoding OnfidoEIDServiceProviderParams {..} = pairs $ "report" .= onfidoparamMethod

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Text, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind doc sl = do
  method <- case signatorylinkauthenticationtosignmethod sl of
    OnfidoDocumentCheckAuthenticationToSign -> return OnfidoDocumentCheck
    OnfidoDocumentAndPhotoCheckAuthenticationToSign -> return OnfidoDocumentAndPhotoCheck
    method -> do
      logInfo_
        $  "Tried to start Onfido transaction, but signatory was supposed to use "
        <> showt method
        <> "."
      internalError
  ctx             <- getContext
  mkontraRedirect <- case authKind of
    EIDServiceAuthToView _ -> Just <$> guardJustM (getField "redirect")
    EIDServiceAuthToSign   -> return Nothing
  let createReq = CreateEIDServiceTransactionRequest
        { cestDomain             = ctx ^. #brandedDomain % #url
        , cestEIDServiceProvider = provider
        , cestDocumentID         = documentid doc
        , cestSignatoryLinkID    = signatorylinkid sl
        , cestAuthKind           = authKind
        , cestKontraRedirectUrl  = mkontraRedirect
        , cestmProviderParams    = Just $ OnfidoEIDServiceProviderParams method
        }
  -- Onfido transactions are not started from the API, we get the URL via the create call
  trans <- createTransactionWithEIDService conf createReq
  let tid  = cestRespTransactionID trans
      turl = cestRespAccessUrl trans
  return (tid, turl, EIDServiceTransactionStatusNew)

data OnfidoEIDServiceCompletionData = OnfidoEIDServiceCompletionData
  { eidonfidoChecksClear :: !Bool
  , eidonfidoFirstName   :: !Text
  , eidonfidoLastName    :: !Text
  , eidonfidoDateOfBirth :: !Text
  , eidonfidoMethod      :: !OnfidoMethod
  } deriving (Eq, Ord, Show)

instance FromJSON OnfidoEIDServiceCompletionData where
  parseJSON outer = do
    onfidoMethod <-
      withObject "object" (.: "providerParameters") outer
      >>= withObject "object" (.: "auth")
      >>= withObject "object" (.: toEIDServiceProviderName provider)
      >>= withObject "object" (.: "report")
    checksClear <-
      withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: eidServiceFieldName)
      >>= withObject "object" (.: "checksClear")
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: eidServiceFieldName)
      >>= withObject "object" (.: "completionData")
      >>= withObject "object" (.: "documentReportData")
      >>= withObject
            "object"
            (\o -> do
              firstname <- o .: "firstName"
              lastname  <- o .: "lastName"
              dob       <- o .: "dateOfBirth"
              return OnfidoEIDServiceCompletionData { eidonfidoMethod      = onfidoMethod
                                                    , eidonfidoChecksClear = checksClear
                                                    , eidonfidoFirstName   = firstname
                                                    , eidonfidoLastName    = lastname
                                                    , eidonfidoDateOfBirth = dob
                                                    }
            )
    where eidServiceFieldName = toEIDServiceProviderName provider <> "Auth"

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToSign
  mtrans <- runMaybeT $ do
    Just estDB <- dbQuery
      $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    return (trans :: EIDServiceTransactionResponse OnfidoEIDServiceCompletionData)
  case mtrans of
    Nothing -> return False
    Just trans ->
      return $ isSuccessFullTransaction trans && isJust (validateCompletionData trans)
  where
    isSuccessFullTransaction trans =
      estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess

validateCompletionData
  :: EIDServiceTransactionResponse OnfidoEIDServiceCompletionData
  -> Maybe OnfidoEIDServiceCompletionData
validateCompletionData trans = case estRespCompletionData trans of
  Nothing -> Nothing
  Just cd -> if eidonfidoChecksClear cd then Just cd else Nothing
