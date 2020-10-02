module EID.Authentication.Model (
    EAuthentication(..)
  , AuthenticationProvider(..)
  -- from EID.CGI.GRP.Types
  , CGISEBankIDAuthentication(..)
  , MergeDocumentEidAuthentication(..)
  , GetDocumentEidAuthentication(..)
  , GetDocumentEidAuthenticationWithoutSession(..)
  , setEIDAuthentication
  , fetchEAuthentication
  , eidAuthenticationSelectors
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Int

import DB
import Doc.SignatoryLinkID
import Doc.Types.SignatoryLink
import EID.CGI.GRP.Types
import EID.EIDService.Types
import EID.Nets.Types
import Session.SessionID

-- If one more type of a signature is to be added, follow the
-- convention, i.e. make constructor name the same as signature
-- type, but with underscore at the end (it would be best to
-- have no underscore, but we also want to export all the
-- signature types from this module and ghc complains about
-- ambiguous exports in such case).

data EAuthentication
  = CGISEBankIDAuthentication_ !CGISEBankIDAuthentication
  | NetsNOBankIDAuthentication_ !NetsNOBankIDAuthentication
  | NetsDKNemIDAuthentication_ !NetsDKNemIDAuthentication
  | NetsFITupasAuthentication_ !NetsFITupasAuthentication
  | SMSPinAuthentication_ Text -- param is a phone number
  | EIDServiceVerimiAuthentication_ !EIDServiceVerimiAuthentication
  | EIDServiceIDINAuthentication_ !EIDServiceNLIDINAuthentication
  | EIDServiceNemIDAuthentication_ !EIDServiceDKNemIDAuthentication
  | EIDServiceNOBankIDAuthentication_ !EIDServiceNOBankIDAuthentication
  | EIDServiceFITupasAuthentication_ !EIDServiceFITupasAuthentication
  | EIDServiceSEBankIDAuthentication_ !EIDServiceSEBankIDAuthentication
  | EIDServiceOnfidoAuthentication_ !EIDServiceOnfidoAuthentication
  | EIDServiceSmsOtpAuthentication_ Text -- param is a phone number
    deriving (Show)

----------------------------------------

-- | Authentication provider. Used internally to distinguish between
-- authentications in the database. Should not be exported, as the
-- distinction between various signatures on the outside should
-- be made with pattern matching on 'EAuthentication' constructors.
data AuthenticationProvider
  = CgiGrpBankID
  | NetsNOBankID
  | NetsDKNemID
  | SMSPinAuth
  | SmsOtpAuth
  | NetsFITupas
  | VerimiAuth
  | IDINAuth
  | NemIDAuth
  | NOBankIDAuth
  | FITupasAuth
  | SEBankIDAuth
  | OnfidoAuth
    deriving (Eq, Ord, Show)

instance PQFormat AuthenticationProvider where
  pqFormat = pqFormat @Int16

instance FromSQL AuthenticationProvider where
  type PQBase AuthenticationProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return CgiGrpBankID
      2  -> return NetsNOBankID
      3  -> return NetsDKNemID
      4  -> return SMSPinAuth
      5  -> return NetsFITupas
      6  -> return VerimiAuth
      7  -> return IDINAuth
      8  -> return NemIDAuth
      9  -> return NOBankIDAuth
      10 -> return FITupasAuth
      11 -> return SEBankIDAuth
      12 -> return OnfidoAuth
      13 -> return SmsOtpAuth
      _  -> throwM RangeError { reRange = [(1, 13)], reValue = n }

instance ToSQL AuthenticationProvider where
  type PQDest AuthenticationProvider = PQDest Int16
  toSQL CgiGrpBankID = toSQL (1 :: Int16)
  toSQL NetsNOBankID = toSQL (2 :: Int16)
  toSQL NetsDKNemID  = toSQL (3 :: Int16)
  toSQL SMSPinAuth   = toSQL (4 :: Int16)
  toSQL NetsFITupas  = toSQL (5 :: Int16)
  toSQL VerimiAuth   = toSQL (6 :: Int16)
  toSQL IDINAuth     = toSQL (7 :: Int16)
  toSQL NemIDAuth    = toSQL (8 :: Int16)
  toSQL NOBankIDAuth = toSQL (9 :: Int16)
  toSQL FITupasAuth  = toSQL (10 :: Int16)
  toSQL SEBankIDAuth = toSQL (11 :: Int16)
  toSQL OnfidoAuth   = toSQL (12 :: Int16)
  toSQL SmsOtpAuth   = toSQL (13 :: Int16)

----------------------------------------

data MergeDocumentEidAuthentication = MergeDocumentEidAuthentication AuthenticationKind SessionID SignatoryLinkID  EAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeDocumentEidAuthentication () where
  dbUpdate (MergeDocumentEidAuthentication authKind sid slid eauth) = do
    runQuery_ . sqlInsert "eid_authentications" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id", "auth_kind"] . sqlUpdate "" $ do
        setFields
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "auth_kind"         authKind
        sqlSet "session_id"        sid
        setEIDAuthentication eauth

setEIDAuthentication :: (MonadState v m, SqlSet v) => EAuthentication -> m ()
setEIDAuthentication eauth = case eauth of
  CGISEBankIDAuthentication_ CGISEBankIDAuthentication {..} -> do
    sqlSet "provider"                  CgiGrpBankID
    sqlSet "signature"                 cgisebidaSignature
    sqlSet "signatory_name"            cgisebidaSignatoryName
    sqlSet "signatory_personal_number" cgisebidaSignatoryPersonalNumber
    sqlSet "ocsp_response"             cgisebidaOcspResponse
    sqlSet "signatory_ip"              cgisebidaSignatoryIP

  SMSPinAuthentication_ mobile -> do
    sqlSet "provider"               SMSPinAuth
    sqlSet "signatory_phone_number" mobile

  EIDServiceSmsOtpAuthentication_ phoneNumber -> do
    sqlSet "provider"               SmsOtpAuth
    sqlSet "signatory_phone_number" phoneNumber

  NetsFITupasAuthentication_ NetsFITupasAuthentication {..} -> do
    sqlSet "provider"                NetsFITupas
    sqlSet "signatory_name"          netsFITupasSignatoryName
    sqlSet "signatory_date_of_birth" netsFITupasDateOfBirth

  EIDServiceVerimiAuthentication_ EIDServiceVerimiAuthentication {..} -> do
    sqlSet "provider"               VerimiAuth
    sqlSet "signatory_name"         eidServiceVerimiName
    sqlSet "signatory_email"        eidServiceVerimiVerifiedEmail
    sqlSet "signatory_phone_number" eidServiceVerimiVerifiedPhone

  EIDServiceIDINAuthentication_ EIDServiceNLIDINAuthentication {..} -> do
    sqlSet "provider"                IDINAuth
    sqlSet "signatory_name"          eidServiceIDINName
    sqlSet "signatory_phone_number"  eidServiceIDINVerifiedPhone
    sqlSet "signatory_date_of_birth" eidServiceIDINBirthDate
    sqlSet "provider_customer_id"    eidServiceIDINCustomerID

  EIDServiceOnfidoAuthentication_ EIDServiceOnfidoAuthentication {..} -> do
    sqlSet "provider"                OnfidoAuth
    sqlSet "provider_method"         eidServiceOnfidoMethod
    sqlSet "signatory_name"          eidServiceOnfidoSignatoryName
    sqlSet "signatory_date_of_birth" eidServiceOnfidoDateOfBirth

  EIDServiceNemIDAuthentication_ EIDServiceDKNemIDAuthentication {..} -> do
    sqlSet "provider"                  NemIDAuth
    sqlSet "internal_provider"         eidServiceNemIDInternalProvider
    sqlSet "signatory_personal_number" eidServiceNemIDSignatoryPersonalOrCVRNumber
    sqlSet "signature"                 eidServiceNemIDCertificate
    sqlSet "signatory_name"            eidServiceNemIDSignatoryName
    sqlSet "signatory_date_of_birth"   eidServiceNemIDDateOfBirth

  EIDServiceNOBankIDAuthentication_ EIDServiceNOBankIDAuthentication {..} -> do
    sqlSet "provider"                NOBankIDAuth
    sqlSet "internal_provider"       eidServiceNOBankIDInternalProvider
    sqlSet "signature"               eidServiceNOBankIDCertificate
    sqlSet "signatory_name"          eidServiceNOBankIDSignatoryName
    sqlSet "signatory_date_of_birth" eidServiceNOBankIDDateOfBirth

  EIDServiceSEBankIDAuthentication_ EIDServiceSEBankIDAuthentication {..} -> do
    sqlSet "provider"                  SEBankIDAuth
    sqlSet "signatory_name"            eidServiceSEBankIDSignatoryName
    sqlSet "signatory_personal_number" eidServiceSEBankIDSignatoryPersonalNumber
    sqlSet "signatory_ip"              eidServiceSEBankIDSignatoryIP
    sqlSet "signature"                 eidServiceSEBankIDSignature
    sqlSet "ocsp_response"             eidServiceSEBankIDOcspResponse

  EIDServiceFITupasAuthentication_ EIDServiceFITupasAuthentication {..} -> do
    sqlSet "provider"                  FITupasAuth
    sqlSet "signatory_name"            eidServiceFITupasSignatoryName
    sqlSet "signatory_personal_number" eidServiceFITupasPersonalNumber
    sqlSet "signatory_date_of_birth"   eidServiceFITupasDateOfBirth

  NetsNOBankIDAuthentication_ _ ->
    unexpectedError
      "Tried to insert a NetsNOBankIDAuthentication, but Nets is no longer used for NOBankID!"

  NetsDKNemIDAuthentication_ _ ->
    unexpectedError
      "Tried to insert a NetsDKNemIDAuthentication, but Nets is no longer used for DKNemID!"

eidAuthenticationSelectors :: [SQL]
eidAuthenticationSelectors =
  [ "provider"
  , "internal_provider"
  , "provider_method"
  , "signature"
  , "signatory_name"
  , "signatory_personal_number"
  , "signatory_phone_number"
  , "signatory_date_of_birth"
  , "ocsp_response"
  , "signatory_ip"
  , "signatory_email"
  , "provider_customer_id"
  ]

getDocumentEidAuthenticationInternal
  :: (MonadThrow m, MonadDB m)
  => AuthenticationKind
  -> SignatoryLinkID
  -> Maybe SessionID
  -> m (Maybe EAuthentication)
getDocumentEidAuthenticationInternal authKind slid msid = do
  runQuery_ . sqlSelect "eid_authentications" $ do
    mapM_ sqlResult eidAuthenticationSelectors
    sqlWhereEq "signatory_link_id" slid
    sqlWhereEq "auth_kind"         authKind
    whenJust msid $ sqlWhereEq "session_id"
  fetchMaybe fetchEAuthentication

-- | Get signature for a given signatory. Used when generating evidence long after user has signed.
data GetDocumentEidAuthenticationWithoutSession = GetDocumentEidAuthenticationWithoutSession AuthenticationKind SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetDocumentEidAuthenticationWithoutSession (Maybe EAuthentication) where
  dbQuery (GetDocumentEidAuthenticationWithoutSession authKind slid) =
    getDocumentEidAuthenticationInternal authKind slid Nothing

-- | Get signature for a given signatory and session.
data GetDocumentEidAuthentication = GetDocumentEidAuthentication AuthenticationKind SessionID SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetDocumentEidAuthentication (Maybe EAuthentication) where
  dbQuery (GetDocumentEidAuthentication authKind sid slid) =
    getDocumentEidAuthenticationInternal authKind slid (Just sid)

fetchEAuthentication
  :: ( AuthenticationProvider
     , Maybe Int16
     , Maybe Text
     , Maybe ByteString
     , Maybe Text
     , Maybe Text
     , Maybe Text
     , Maybe Text
     , Maybe ByteString
     , Maybe Text
     , Maybe Text
     , Maybe Text
     )
  -> EAuthentication
fetchEAuthentication (provider, internal_provider, provider_method, msignature, msignatory_name, signatory_personal_number, signatory_phone_number, signatory_dob, ocsp_response, msignatory_ip, signatory_email, customer_id)
  = case provider of
    CgiGrpBankID -> CGISEBankIDAuthentication_ CGISEBankIDAuthentication
      { cgisebidaSignatoryName           = fromJust msignatory_name
      , cgisebidaSignatoryPersonalNumber = fromJust signatory_personal_number
      , cgisebidaSignatoryIP             = fromMaybe "" msignatory_ip
      , cgisebidaSignature               = fromJust msignature
      , cgisebidaOcspResponse            = fromJust ocsp_response
      }
    NetsNOBankID -> NetsNOBankIDAuthentication_ NetsNOBankIDAuthentication
      { netsNOBankIDInternalProvider = unsafeNetsNOBankIDInternalProviderFromInt16
                                         (fromJust internal_provider)
      , netsNOBankIDSignatoryName    = fromJust msignatory_name
      , netsNOBankIDPhoneNumber      = signatory_phone_number
      , netsNOBankIDDateOfBirth      = fromJust signatory_dob
      , netsNOBankIDCertificate      = fromJust msignature
      }
    NetsDKNemID -> NetsDKNemIDAuthentication_ NetsDKNemIDAuthentication
      { netsDKNemIDInternalProvider = unsafeNetsDKNemIDInternalProviderFromInt16
                                        (fromJust internal_provider)
      , netsDKNemIDSignatoryName    = fromJust msignatory_name
      , netsDKNemIDDateOfBirth      = fromJust signatory_dob
      , netsDKNemIDCertificate      = fromJust msignature
      }
    SMSPinAuth  -> SMSPinAuthentication_ (fromJust signatory_phone_number)
    NetsFITupas -> NetsFITupasAuthentication_ NetsFITupasAuthentication
      { netsFITupasSignatoryName = fromJust msignatory_name
      , netsFITupasDateOfBirth   = fromJust signatory_dob
      }
    VerimiAuth -> EIDServiceVerimiAuthentication_ EIDServiceVerimiAuthentication
      { eidServiceVerimiName          = fromJust msignatory_name
      , eidServiceVerimiVerifiedEmail = signatory_email
      , eidServiceVerimiVerifiedPhone = signatory_phone_number
      }
    IDINAuth -> EIDServiceIDINAuthentication_ EIDServiceNLIDINAuthentication
      { eidServiceIDINName          = fromJust msignatory_name
      , eidServiceIDINVerifiedPhone = signatory_phone_number
      , eidServiceIDINBirthDate     = signatory_dob
      , eidServiceIDINCustomerID    = customer_id
      }
    NemIDAuth -> EIDServiceNemIDAuthentication_ EIDServiceDKNemIDAuthentication
      { eidServiceNemIDInternalProvider = unsafeEIDServiceDKNemIDInternalProviderFromInt16
                                            $ fromJust internal_provider
      , eidServiceNemIDSignatoryPersonalOrCVRNumber = fromMaybe
                                                        ""
                                                        signatory_personal_number
      , eidServiceNemIDSignatoryName                = fromJust msignatory_name
      , eidServiceNemIDDateOfBirth                  = signatory_dob
      , eidServiceNemIDCertificate                  = fromJust msignature
      }
    NOBankIDAuth -> EIDServiceNOBankIDAuthentication_ EIDServiceNOBankIDAuthentication
      { eidServiceNOBankIDInternalProvider =
        unsafeEIDServiceNOBankIDInternalProviderFromInt16 $ fromJust internal_provider
      , eidServiceNOBankIDSignatoryName    = fromJust msignatory_name
      , eidServiceNOBankIDDateOfBirth      = fromJust signatory_dob
      , eidServiceNOBankIDCertificate      = msignature
      , eidServiceNOBankIDPhoneNumber      = signatory_phone_number
      }
    SEBankIDAuth -> EIDServiceSEBankIDAuthentication_ EIDServiceSEBankIDAuthentication
      { eidServiceSEBankIDSignatoryName           = fromJust msignatory_name
      , eidServiceSEBankIDSignatoryPersonalNumber = fromJust signatory_personal_number
      , eidServiceSEBankIDSignatoryIP             = fromJust msignatory_ip
      , eidServiceSEBankIDSignature               = fromJust msignature
      , eidServiceSEBankIDOcspResponse            = fromJust ocsp_response
      }
    FITupasAuth -> EIDServiceFITupasAuthentication_ EIDServiceFITupasAuthentication
      { eidServiceFITupasSignatoryName  = fromJust msignatory_name
      , eidServiceFITupasPersonalNumber = signatory_personal_number
      , eidServiceFITupasDateOfBirth    = signatory_dob
      }
    OnfidoAuth -> EIDServiceOnfidoAuthentication_ EIDServiceOnfidoAuthentication
      { eidServiceOnfidoSignatoryName = fromJust msignatory_name
      , eidServiceOnfidoDateOfBirth = fromJust signatory_dob
      , eidServiceOnfidoMethod = unsafeDecodeOnfidoMethod $ fromJust provider_method
      }
    SmsOtpAuth -> EIDServiceSmsOtpAuthentication_ (fromJust signatory_phone_number)
