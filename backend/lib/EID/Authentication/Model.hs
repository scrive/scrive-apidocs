module EID.Authentication.Model (
    EAuthentication(..)
  , AuthenticationProvider(..)
  -- from EID.CGI.GRP.Types
  , CGISEBankIDAuthentication(..)
  , MergeCGISEBankIDAuthentication(..)
  , MergeNetsFITupasAuthentication(..)
  , MergeSMSPinAuthentication(..)
  , MergeEIDServiceVerimiAuthentication(..)
  , MergeEIDServiceIDINAuthentication(..)
  , MergeEIDServiceNemIDAuthentication(..)
  , MergeEIDServiceNOBankIDAuthentication(..)
  , MergeEIDServiceSEBankIDAuthentication(..)
  , MergeEIDServiceFITupasAuthentication(..)
  , GetEAuthentication(..)
  , GetEAuthenticationWithoutSession(..)
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
  | NetsFITupas
  | VerimiAuth
  | IDINAuth
  | NemIDAuth
  | NOBankIDAuth
  | FITupasAuth
  | SEBankIDAuth
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
      _  -> throwM RangeError { reRange = [(1, 11)], reValue = n }

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

----------------------------------------

-- | General version of inserting some authentication for a given signatory or
-- replacing existing one.
--
-- Note: after document is signed, existing eid_authentication record will keep
-- being overwritten every time authenticate to view archived routine is
-- completed.
mergeAuthenticationInternal
  :: (MonadDB m, MonadMask m)
  => AuthenticationKind
  -> SessionID
  -> SignatoryLinkID
  -> (forall  v n . (MonadState v n, SqlSet v) => n ())
  -> m ()
mergeAuthenticationInternal authKind sid slid setDedicatedAuthFields = do
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
      setDedicatedAuthFields


-- | Insert bank id authentication for a given signatory or replace the existing one.
data MergeCGISEBankIDAuthentication = MergeCGISEBankIDAuthentication AuthenticationKind SessionID SignatoryLinkID CGISEBankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeCGISEBankIDAuthentication () where
  dbUpdate (MergeCGISEBankIDAuthentication authKind sid slid CGISEBankIDAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"                  CgiGrpBankID
        sqlSet "signature"                 cgisebidaSignature
        sqlSet "signatory_name"            cgisebidaSignatoryName
        sqlSet "signatory_personal_number" cgisebidaSignatoryPersonalNumber
        sqlSet "ocsp_response"             cgisebidaOcspResponse
        sqlSet "signatory_ip"              cgisebidaSignatoryIP

data MergeSMSPinAuthentication = MergeSMSPinAuthentication AuthenticationKind SessionID SignatoryLinkID Text
instance (MonadDB m, MonadMask m) => DBUpdate m MergeSMSPinAuthentication () where
  dbUpdate (MergeSMSPinAuthentication authKind sid slid mobile) = do
    mergeAuthenticationInternal authKind sid slid $ do
      sqlSet "provider"               SMSPinAuth
      sqlSet "signatory_phone_number" mobile

-- | Insert NemID authentication for a given signatory or replace the existing one.
data MergeNetsFITupasAuthentication = MergeNetsFITupasAuthentication AuthenticationKind SessionID SignatoryLinkID NetsFITupasAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsFITupasAuthentication () where
  dbUpdate (MergeNetsFITupasAuthentication authKind sid slid NetsFITupasAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"                NetsFITupas
        sqlSet "signatory_name"          netsFITupasSignatoryName
        sqlSet "signatory_date_of_birth" netsFITupasDateOfBirth

-- | Insert Verimi authentication for a given signatory or replace the existing one.
data MergeEIDServiceVerimiAuthentication = MergeEIDServiceVerimiAuthentication AuthenticationKind SessionID SignatoryLinkID EIDServiceVerimiAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceVerimiAuthentication () where
  dbUpdate (MergeEIDServiceVerimiAuthentication authKind sid slid EIDServiceVerimiAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"               VerimiAuth
        sqlSet "signatory_name"         eidServiceVerimiName
        sqlSet "signatory_email"        eidServiceVerimiVerifiedEmail
        sqlSet "signatory_phone_number" eidServiceVerimiVerifiedPhone

-- | Insert Verimi authentication for a given signatory or replace the existing one.
data MergeEIDServiceIDINAuthentication = MergeEIDServiceIDINAuthentication AuthenticationKind SessionID SignatoryLinkID EIDServiceNLIDINAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceIDINAuthentication () where
  dbUpdate (MergeEIDServiceIDINAuthentication authKind sid slid EIDServiceNLIDINAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"                IDINAuth
        sqlSet "signatory_name"          eidServiceIDINName
        sqlSet "signatory_phone_number"  eidServiceIDINVerifiedPhone
        sqlSet "signatory_date_of_birth" eidServiceIDINBirthDate
        sqlSet "provider_customer_id"    eidServiceIDINCustomerID

-- | Insert NemID authentication for a given signatory or replace the existing one.
data MergeEIDServiceNemIDAuthentication = MergeEIDServiceNemIDAuthentication AuthenticationKind SessionID SignatoryLinkID EIDServiceDKNemIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceNemIDAuthentication () where
  dbUpdate (MergeEIDServiceNemIDAuthentication authKind sid slid EIDServiceDKNemIDAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"                  NemIDAuth
        sqlSet "internal_provider"         eidServiceNemIDInternalProvider
        sqlSet "signatory_personal_number" eidServiceNemIDSignatoryPersonalOrCVRNumber
        sqlSet "signature"                 eidServiceNemIDCertificate
        sqlSet "signatory_name"            eidServiceNemIDSignatoryName
        sqlSet "signatory_date_of_birth"   eidServiceNemIDDateOfBirth

-- | Insert NOBankID authentication for a given signatory or replace the existing one.
data MergeEIDServiceNOBankIDAuthentication = MergeEIDServiceNOBankIDAuthentication AuthenticationKind SessionID SignatoryLinkID EIDServiceNOBankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceNOBankIDAuthentication () where
  dbUpdate (MergeEIDServiceNOBankIDAuthentication authKind sid slid EIDServiceNOBankIDAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"                NOBankIDAuth
        sqlSet "internal_provider"       eidServiceNOBankIDInternalProvider
        sqlSet "signature"               eidServiceNOBankIDCertificate
        sqlSet "signatory_name"          eidServiceNOBankIDSignatoryName
        sqlSet "signatory_date_of_birth" eidServiceNOBankIDDateOfBirth

-- | Insert SEBankID authentication for a given signatory or replace the existing one.
data MergeEIDServiceSEBankIDAuthentication = MergeEIDServiceSEBankIDAuthentication AuthenticationKind SessionID SignatoryLinkID EIDServiceSEBankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceSEBankIDAuthentication () where
  dbUpdate (MergeEIDServiceSEBankIDAuthentication authKind sid slid EIDServiceSEBankIDAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"                  SEBankIDAuth
        sqlSet "signatory_name"            eidServiceSEBankIDSignatoryName
        sqlSet "signatory_personal_number" eidServiceSEBankIDSignatoryPersonalNumber
        sqlSet "signatory_ip"              eidServiceSEBankIDSignatoryIP
        sqlSet "signature"                 eidServiceSEBankIDSignature
        sqlSet "ocsp_response"             eidServiceSEBankIDOcspResponse

-- | Insert FITupas authentication for a given signatory or replace the existing one.
data MergeEIDServiceFITupasAuthentication = MergeEIDServiceFITupasAuthentication AuthenticationKind SessionID SignatoryLinkID EIDServiceFITupasAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceFITupasAuthentication () where
  dbUpdate (MergeEIDServiceFITupasAuthentication authKind sid slid EIDServiceFITupasAuthentication {..})
    = do
      mergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider"                  FITupasAuth
        sqlSet "signatory_name"            eidServiceFITupasSignatoryName
        sqlSet "signatory_personal_number" eidServiceFITupasPersonalNumber
        sqlSet "signatory_date_of_birth"   eidServiceFITupasDateOfBirth

-- Get authentication - internal - just to unify code
data GetEAuthenticationInternal = GetEAuthenticationInternal AuthenticationKind SignatoryLinkID (Maybe SessionID)
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthenticationInternal (Maybe EAuthentication) where
  dbQuery (GetEAuthenticationInternal authKind slid msid) = do
    runQuery_ . sqlSelect "eid_authentications" $ do
      sqlResult "provider"
      sqlResult "internal_provider"
      sqlResult "signature"
      sqlResult "signatory_name"
      sqlResult "signatory_personal_number"
      sqlResult "signatory_phone_number"
      sqlResult "signatory_date_of_birth"
      sqlResult "ocsp_response"
      sqlResult "signatory_ip"
      sqlResult "signatory_email"
      sqlResult "provider_customer_id"
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "auth_kind"         authKind
      whenJust msid $ sqlWhereEq "session_id"
    fetchMaybe fetchEAuthentication

-- | Get signature for a given signatory. Used when generating evidence long after user has signed.
data GetEAuthenticationWithoutSession = GetEAuthenticationWithoutSession AuthenticationKind SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthenticationWithoutSession (Maybe EAuthentication) where
  dbQuery (GetEAuthenticationWithoutSession authKind slid) =
    dbQuery (GetEAuthenticationInternal authKind slid Nothing)

-- | Get signature for a given signatory and session.
data GetEAuthentication = GetEAuthentication AuthenticationKind SessionID SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthentication (Maybe EAuthentication) where
  dbQuery (GetEAuthentication authKind sid slid) =
    dbQuery (GetEAuthenticationInternal authKind slid (Just sid))

fetchEAuthentication
  :: ( AuthenticationProvider
     , Maybe Int16
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
fetchEAuthentication (provider, internal_provider, msignature, msignatory_name, signatory_personal_number, signatory_phone_number, signatory_dob, ocsp_response, msignatory_ip, signatory_email, customer_id)
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
      , eidServiceNemIDSignatoryPersonalOrCVRNumber = fromJust signatory_personal_number
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

