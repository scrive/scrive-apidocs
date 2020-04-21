module EID.EIDService.Provider.FITupas (
    beginEIDServiceTransaction
  , FITupasEIDServiceCompletionData(..)
  , completeEIDServiceSignTransaction
 ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Text as T

import DB
import Doc.DocStateData
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Types
import Happstack.Fields
import Kontra hiding (InternalError)
import Session.Model
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderFITupas

newtype StartFITupasEIDServiceTransactionResponse = StartFITupasEIDServiceTransactionResponse {
    sfiestAuthURL :: Text
  }

instance FromJSON StartFITupasEIDServiceTransactionResponse where
  parseJSON outer =
    StartFITupasEIDServiceTransactionResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: eidServiceFieldName)
          >>= withObject "object" (.: "authUrl")
          )
    where eidServiceFieldName = toEIDServiceProviderName provider <> "Auth"

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Text, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind doc sl = do
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
        , cestmProviderParams    = Nothing :: Maybe ()
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- sfiestAuthURL <$> startTransactionWithEIDService conf provider tid
  return (tid, turl, EIDServiceTransactionStatusStarted)

data FITupasEIDServiceCompletionData = FITupasEIDServiceCompletionData
  { eidtupasName :: !Text
  , eidtupasBirthDate :: !Text
  , eidtupasDistinguishedName :: !Text  -- may contain the personal number
  , eidtupasBank :: !(Maybe Text)  -- absent when using Mobile ID
  , eidtupasPid :: !(Maybe Text)
  -- ^ 'A fixed identifier for the user set in the E-Ident / FTN service.' (from
  -- the Nets documentation)
  , eidtupasSSN :: !(Maybe Text)  -- seems to be absent for 'legal persons'
  } deriving (Eq, Ord, Show)

instance FromJSON FITupasEIDServiceCompletionData where
  parseJSON outer = do
    let providerAuth =
          toEIDServiceProviderName EIDServiceTransactionProviderFITupas <> "Auth"
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              mbank       <- o .:? "bank"
              mpid        <- o .:? "pid"
              mssn        <- o .:? "ssn"
              (dob, name) <- o .: "profileData" >>= withObject
                "object"
                (\cd -> (,) <$> cd .: "birthdate" <*> cd .: "name")
              dn <- o .: "certificateData" >>= withObject "object"
                                                          (.: "distinguishedName")
              return FITupasEIDServiceCompletionData { eidtupasName              = name
                                                     , eidtupasBirthDate         = dob
                                                     , eidtupasDistinguishedName = dn
                                                     , eidtupasBank              = mbank
                                                     , eidtupasPid               = mpid
                                                     , eidtupasSSN               = mssn
                                                     }
            )

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToSign
  mtrans <- runMaybeT $ do
    Just estDB <- dbQuery
      $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    return (trans :: EIDServiceTransactionResponse FITupasEIDServiceCompletionData)
  case mtrans of
    Nothing -> return False
    Just trans ->
      return $ isSuccessFullTransaction trans && isJust (validateCompletionData sl trans)
  where
    isSuccessFullTransaction trans =
      estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess

validateCompletionData
  :: SignatoryLink
  -> EIDServiceTransactionResponse FITupasEIDServiceCompletionData
  -> Maybe FITupasEIDServiceCompletionData
validateCompletionData sl trans = case estRespCompletionData trans of
  Nothing -> Nothing
  Just cd ->
    if T.null (getPersonalNumber sl)
         || isNothing (eidtupasSSN cd)
         || (getPersonalNumber sl == fromMaybe "" (eidtupasSSN cd))
      then Just cd
      else Nothing
