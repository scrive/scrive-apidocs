module EID.Authentication.Model (
    EAuthentication(..)
  , AuthenticationProvider(..)
  -- from EID.CGI.GRP.Data
  , CGISEBankIDAuthentication(..)
  , MergeCGISEBankIDAuthentication(..)
  , MergeNetsNOBankIDAuthentication(..)
  , MergeNetsDKNemIDAuthentication(..)
  , GetEAuthentication(..)
  , GetEAuthenticationWithoutSession(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Foldable as F
import Data.Int
import Data.Time
import qualified Data.Text as T

import DB
import Doc.SignatoryLinkID
import EID.CGI.GRP.Data
import EID.Nets.Data
import KontraPrelude
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

----------------------------------------

-- | Authentication provider. Used internally to distinguish between
-- authentications in the database. Should not be exported, as the
-- distinction between various signatures on the outside should
-- be made with pattern matching on 'EAuthentication' constructors.
data AuthenticationProvider
  = CgiGrpBankID
  | NetsNOBankID
  | NetsDKNemID
    deriving (Eq, Ord, Show)

instance PQFormat AuthenticationProvider where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL AuthenticationProvider where
  type PQBase AuthenticationProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return CgiGrpBankID
      2 -> return NetsNOBankID
      3 -> return NetsDKNemID
      _ -> throwM RangeError {
        reRange = [(1, 3)]
      , reValue = n
      }

instance ToSQL AuthenticationProvider where
  type PQDest AuthenticationProvider = PQDest Int16
  toSQL CgiGrpBankID = toSQL (1::Int16)
  toSQL NetsNOBankID = toSQL (2::Int16)
  toSQL NetsDKNemID  = toSQL (3::Int16)

----------------------------------------

-- | General version of inserting some authentication for a given signatory or replacing existing one.
data MergeAuthenticationInternal = MergeAuthenticationInternal SessionID SignatoryLinkID (forall v n. (MonadState v n, SqlSet v) => n ())
instance (MonadDB m, MonadMask m) => DBUpdate m MergeAuthenticationInternal () where
  update (MergeAuthenticationInternal sid slid setDedicatedAuthFields) = do
    loopOnUniqueViolation . withSavepoint "merge_bank_id_authentication" $ do
      runQuery01_ selectSignatorySignTime
      msign_time :: Maybe UTCTime <- fetchOne runIdentity
      when (isJust msign_time) $ do
        $unexpectedErrorM "signatory already signed, can't merge authentication"
      success <- runQuery01 . sqlUpdate "eid_authentications" $ do
        setFields
        sqlWhereEq "signatory_link_id" slid
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
      when (not success) $ do
        runQuery_ . sqlInsertSelect "eid_authentications" "" $ do
          setFields
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "session_id" sid
        setDedicatedAuthFields


-- | Insert bank id authentication for a given signatory or replace the existing one.
data MergeCGISEBankIDAuthentication = MergeCGISEBankIDAuthentication SessionID SignatoryLinkID CGISEBankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeCGISEBankIDAuthentication () where
  update (MergeCGISEBankIDAuthentication sid slid CGISEBankIDAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal sid slid $ do
        sqlSet "provider" CgiGrpBankID
        sqlSet "signature" cgisebidaSignature
        sqlSet "signatory_name" cgisebidaSignatoryName
        sqlSet "signatory_personal_number" cgisebidaSignatoryPersonalNumber
        sqlSet "ocsp_response" cgisebidaOcspResponse
        sqlSet "signatory_ip" cgisebidaSignatoryIP

-- | Insert bank id authentication for a given signatory or replace the existing one.
data MergeNetsNOBankIDAuthentication = MergeNetsNOBankIDAuthentication SessionID SignatoryLinkID NetsNOBankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsNOBankIDAuthentication () where
  update (MergeNetsNOBankIDAuthentication sid slid NetsNOBankIDAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal sid slid $ do
        sqlSet "provider" NetsNOBankID
        sqlSet "internal_provider" netsNOBankIDInternalProvider
        sqlSet "signature" netsNOBankIDCertificate
        sqlSet "signatory_name" netsNOBankIDSignatoryName
        sqlSet "signatory_phone_number" netsNOBankIDPhoneNumber
        sqlSet "signatory_date_of_birth" netsNOBankIDDateOfBirth

-- | Insert NemID authentication for a given signatory or replace the existing one.
data MergeNetsDKNemIDAuthentication = MergeNetsDKNemIDAuthentication SessionID SignatoryLinkID NetsDKNemIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsDKNemIDAuthentication () where
  update (MergeNetsDKNemIDAuthentication sid slid NetsDKNemIDAuthentication{..}) = do
    dbUpdate $ MergeAuthenticationInternal sid slid $ do
        sqlSet "provider" NetsDKNemID
        sqlSet "internal_provider" netsDKNemIDInternalProvider
        sqlSet "signature" netsDKNemIDCertificate
        sqlSet "signatory_name" netsDKNemIDSignatoryName
        sqlSet "signatory_date_of_birth" netsDKNemIDDateOfBirth

-- Get authentication - internal - just to unify code
data GetEAuthenticationInternal = GetEAuthenticationInternal SignatoryLinkID (Maybe SessionID)
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthenticationInternal (Maybe EAuthentication) where
  query (GetEAuthenticationInternal slid msid) = do
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
      sqlWhereEq "signatory_link_id" slid
      F.forM_ msid $ sqlWhereEq "session_id"
    fetchMaybe fetchEAuthentication

-- | Get signature for a given signatory. Used when generating evidence long after user has signed.
data GetEAuthenticationWithoutSession = GetEAuthenticationWithoutSession SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthenticationWithoutSession (Maybe EAuthentication) where
  query (GetEAuthenticationWithoutSession slid) = query (GetEAuthenticationInternal slid Nothing)


-- | Get signature for a given signatory and session.
data GetEAuthentication = GetEAuthentication SessionID SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthentication (Maybe EAuthentication) where
  query (GetEAuthentication sid slid) = query (GetEAuthenticationInternal slid (Just sid) )

fetchEAuthentication :: (AuthenticationProvider, (Maybe Int16), ByteString, T.Text, Maybe T.Text, Maybe T.Text, Maybe T.Text, Maybe ByteString, Maybe T.Text) -> EAuthentication
fetchEAuthentication (provider, internal_provider, signature, signatory_name, signatory_personal_number, signatory_phone_number, signatory_dob, ocsp_response, msignatory_ip) = case provider of
  CgiGrpBankID -> CGISEBankIDAuthentication_ CGISEBankIDAuthentication {
    cgisebidaSignatoryName = signatory_name
  , cgisebidaSignatoryPersonalNumber = fromJust signatory_personal_number
  , cgisebidaSignatoryIP = fromMaybe "" msignatory_ip
  , cgisebidaSignature = signature
  , cgisebidaOcspResponse = fromJust ocsp_response
  }
  NetsNOBankID -> NetsNOBankIDAuthentication_ NetsNOBankIDAuthentication {
    netsNOBankIDInternalProvider = unsafeNetsNOBankIDInternalProviderFromInt16 (fromJust internal_provider)
  , netsNOBankIDSignatoryName = signatory_name
  , netsNOBankIDPhoneNumber   = signatory_phone_number
  , netsNOBankIDDateOfBirth   = fromJust signatory_dob
  , netsNOBankIDCertificate   = signature
  }
  NetsDKNemID -> NetsDKNemIDAuthentication_ NetsDKNemIDAuthentication {
    netsDKNemIDInternalProvider = unsafeNetsDKNemIDInternalProviderFromInt16 (fromJust internal_provider)
  , netsDKNemIDSignatoryName = signatory_name
  , netsDKNemIDDateOfBirth   = fromJust signatory_dob
  , netsDKNemIDCertificate   = signature
  }
