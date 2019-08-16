module EID.Authentication.Model (
    EAuthentication(..)
  , AuthenticationProvider(..)
  -- from EID.CGI.GRP.Types
  , CGISEBankIDAuthentication(..)
  , MergeCGISEBankIDAuthentication(..)
  , MergeNetsNOBankIDAuthentication(..)
  , MergeNetsDKNemIDAuthentication(..)
  , MergeNetsFITupasAuthentication(..)
  , MergeSMSPinAuthentication(..)
  , MergeEIDServiceVerimiAuthentication(..)
  , GetEAuthentication(..)
  , GetEAuthenticationWithoutSession(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Foldable as F
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
    deriving (Eq, Ord, Show)

instance PQFormat AuthenticationProvider where
  pqFormat = pqFormat @Int16

instance FromSQL AuthenticationProvider where
  type PQBase AuthenticationProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return CgiGrpBankID
      2 -> return NetsNOBankID
      3 -> return NetsDKNemID
      4 -> return SMSPinAuth
      5 -> return NetsFITupas
      6 -> return VerimiAuth
      _ -> throwM RangeError {
        reRange = [(1, 6)]
      , reValue = n
      }

instance ToSQL AuthenticationProvider where
  type PQDest AuthenticationProvider = PQDest Int16
  toSQL CgiGrpBankID = toSQL (1::Int16)
  toSQL NetsNOBankID = toSQL (2::Int16)
  toSQL NetsDKNemID  = toSQL (3::Int16)
  toSQL SMSPinAuth   = toSQL (4::Int16)
  toSQL NetsFITupas  = toSQL (5::Int16)
  toSQL VerimiAuth   = toSQL (6::Int16)

----------------------------------------

-- | General version of inserting some authentication for a given signatory or replacing existing one.
data MergeAuthenticationInternal = MergeAuthenticationInternal AuthenticationKind SessionID SignatoryLinkID (forall v n. (MonadState v n, SqlSet v) => n ())
instance (MonadDB m, MonadMask m) => DBUpdate m MergeAuthenticationInternal () where
  update (MergeAuthenticationInternal authKind sid slid setDedicatedAuthFields) = do
    -- Note: after document is signed, existing eid_authentication record will
    -- keep being overwritten every time authenticate to view archived routine
    -- is completed.
    loopOnUniqueViolation . withSavepoint "merge_authentication_internal" $ do
      success <- runQuery01 . sqlUpdate "eid_authentications" $ do
        setFields
        sqlWhereEq "signatory_link_id" slid
        sqlWhereEq "auth_kind" authKind
      unless success $
        runQuery_ . sqlInsertSelect "eid_authentications" "" $ do
          setFields
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "auth_kind" authKind
        sqlSet "session_id" sid
        setDedicatedAuthFields


-- | Insert bank id authentication for a given signatory or replace the existing one.
data MergeCGISEBankIDAuthentication = MergeCGISEBankIDAuthentication AuthenticationKind SessionID SignatoryLinkID CGISEBankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeCGISEBankIDAuthentication () where
  update (MergeCGISEBankIDAuthentication authKind sid slid CGISEBankIDAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider" CgiGrpBankID
        sqlSet "signature" cgisebidaSignature
        sqlSet "signatory_name" cgisebidaSignatoryName
        sqlSet "signatory_personal_number" cgisebidaSignatoryPersonalNumber
        sqlSet "ocsp_response" cgisebidaOcspResponse
        sqlSet "signatory_ip" cgisebidaSignatoryIP

-- | Insert bank id authentication for a given signatory or replace the existing one.
data MergeNetsNOBankIDAuthentication = MergeNetsNOBankIDAuthentication AuthenticationKind SessionID SignatoryLinkID NetsNOBankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsNOBankIDAuthentication () where
  update (MergeNetsNOBankIDAuthentication authKind sid slid NetsNOBankIDAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider" NetsNOBankID
        sqlSet "internal_provider" netsNOBankIDInternalProvider
        sqlSet "signature" netsNOBankIDCertificate
        sqlSet "signatory_name" netsNOBankIDSignatoryName
        sqlSet "signatory_phone_number" netsNOBankIDPhoneNumber
        sqlSet "signatory_date_of_birth" netsNOBankIDDateOfBirth

-- | Insert NemID authentication for a given signatory or replace the existing one.
data MergeNetsDKNemIDAuthentication = MergeNetsDKNemIDAuthentication AuthenticationKind SessionID SignatoryLinkID NetsDKNemIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsDKNemIDAuthentication () where
  update (MergeNetsDKNemIDAuthentication authKind sid slid NetsDKNemIDAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider" NetsDKNemID
        sqlSet "internal_provider" netsDKNemIDInternalProvider
        sqlSet "signature" netsDKNemIDCertificate
        sqlSet "signatory_name" netsDKNemIDSignatoryName
        sqlSet "signatory_date_of_birth" netsDKNemIDDateOfBirth

data MergeSMSPinAuthentication = MergeSMSPinAuthentication AuthenticationKind SessionID SignatoryLinkID Text
instance (MonadDB m, MonadMask m) => DBUpdate m MergeSMSPinAuthentication () where
  update (MergeSMSPinAuthentication authKind sid slid mobile) = do
    dbUpdate $ MergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider" SMSPinAuth
        sqlSet "signatory_phone_number" mobile

-- | Insert NemID authentication for a given signatory or replace the existing one.
data MergeNetsFITupasAuthentication = MergeNetsFITupasAuthentication AuthenticationKind SessionID SignatoryLinkID NetsFITupasAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsFITupasAuthentication () where
  update (MergeNetsFITupasAuthentication authKind sid slid NetsFITupasAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider" NetsFITupas
        sqlSet "signatory_name" netsFITupasSignatoryName
        sqlSet "signatory_date_of_birth" netsFITupasDateOfBirth

-- | Insert Verimi authentication for a given signatory or replace the existing one.
data MergeEIDServiceVerimiAuthentication = MergeEIDServiceVerimiAuthentication AuthenticationKind SessionID SignatoryLinkID EIDServiceVerimiAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceVerimiAuthentication () where
  update (MergeEIDServiceVerimiAuthentication authKind sid slid EIDServiceVerimiAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal authKind sid slid $ do
        sqlSet "provider" VerimiAuth
        sqlSet "signatory_name" eidServiceVerimiName
        sqlSet "signatory_email" eidServiceVerimiVerifiedEmail
        sqlSet "signatory_phone_number" eidServiceVerimiVerifiedPhone

-- Get authentication - internal - just to unify code
data GetEAuthenticationInternal = GetEAuthenticationInternal AuthenticationKind SignatoryLinkID (Maybe SessionID)
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthenticationInternal (Maybe EAuthentication) where
  query (GetEAuthenticationInternal authKind slid msid) = do
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
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "auth_kind" authKind
      F.forM_ msid $ sqlWhereEq "session_id"
    fetchMaybe fetchEAuthentication

-- | Get signature for a given signatory. Used when generating evidence long after user has signed.
data GetEAuthenticationWithoutSession = GetEAuthenticationWithoutSession AuthenticationKind SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthenticationWithoutSession (Maybe EAuthentication) where
  query (GetEAuthenticationWithoutSession authKind slid) = query (GetEAuthenticationInternal authKind slid Nothing)

-- | Get signature for a given signatory and session.
data GetEAuthentication = GetEAuthentication AuthenticationKind SessionID SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthentication (Maybe EAuthentication) where
  query (GetEAuthentication authKind sid slid) = query (GetEAuthenticationInternal authKind slid (Just sid) )

fetchEAuthentication :: (AuthenticationProvider, (Maybe Int16), Maybe ByteString, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe ByteString, Maybe Text, Maybe Text) -> EAuthentication
fetchEAuthentication (provider, internal_provider, msignature, msignatory_name, signatory_personal_number, signatory_phone_number, signatory_dob, ocsp_response, msignatory_ip, signatory_email) = case provider of
  CgiGrpBankID -> CGISEBankIDAuthentication_ CGISEBankIDAuthentication {
    cgisebidaSignatoryName = fromJust msignatory_name
  , cgisebidaSignatoryPersonalNumber = fromJust signatory_personal_number
  , cgisebidaSignatoryIP = fromMaybe "" msignatory_ip
  , cgisebidaSignature = fromJust msignature
  , cgisebidaOcspResponse = fromJust ocsp_response
  }
  NetsNOBankID -> NetsNOBankIDAuthentication_ NetsNOBankIDAuthentication {
    netsNOBankIDInternalProvider = unsafeNetsNOBankIDInternalProviderFromInt16 (fromJust internal_provider)
  , netsNOBankIDSignatoryName = fromJust msignatory_name
  , netsNOBankIDPhoneNumber   = signatory_phone_number
  , netsNOBankIDDateOfBirth   = fromJust signatory_dob
  , netsNOBankIDCertificate   = fromJust msignature
  }
  NetsDKNemID -> NetsDKNemIDAuthentication_ NetsDKNemIDAuthentication {
    netsDKNemIDInternalProvider = unsafeNetsDKNemIDInternalProviderFromInt16 (fromJust internal_provider)
  , netsDKNemIDSignatoryName = fromJust msignatory_name
  , netsDKNemIDDateOfBirth   = fromJust signatory_dob
  , netsDKNemIDCertificate   = fromJust msignature
  }
  SMSPinAuth -> SMSPinAuthentication_ (fromJust signatory_phone_number)
  NetsFITupas -> NetsFITupasAuthentication_ NetsFITupasAuthentication {
    netsFITupasSignatoryName = fromJust msignatory_name
  , netsFITupasDateOfBirth   = fromJust signatory_dob
  }
  VerimiAuth -> EIDServiceVerimiAuthentication_ EIDServiceVerimiAuthentication {
    eidServiceVerimiName           = fromJust msignatory_name
  , eidServiceVerimiVerifiedEmail  = signatory_email
  , eidServiceVerimiVerifiedPhone  = signatory_phone_number
  }
