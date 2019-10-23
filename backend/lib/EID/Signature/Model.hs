module EID.Signature.Model (
    ESignature(..)
  , module EID.Signature.Legacy
  , SignatureProvider(..)
  -- from EID.CGI.GRP.Types
  , CGISEBankIDSignature(..)
  , MergeCGISEBankIDSignature(..)
  , GetESignature(..)
  -- from EID.Nets.Types
  , NetsNOBankIDSignature(..)
  , MergeNetsNOBankIDSignature(..)
  , NetsDKNemIDSignature(..)
  , MergeNetsDKNemIDSignature(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Int
import Data.Text.Encoding
import Data.Time

import DB
import Doc.SignatoryLinkID
import EID.CGI.GRP.Types
import EID.Nets.Types
import EID.Signature.Legacy

-- If one more type of a signature is to be added, follow the
-- convention, i.e. make constructor name the same as signature
-- type, but with underscore at the end (it would be best to
-- have no underscore, but we also want to export all the
-- signature types from this module and ghc complains about
-- ambiguous exports in such case).

data ESignature
  = LegacyBankIDSignature_ !LegacyBankIDSignature
  | LegacyTeliaSignature_ !LegacyTeliaSignature
  | LegacyNordeaSignature_ !LegacyNordeaSignature
  | LegacyMobileBankIDSignature_ !LegacyMobileBankIDSignature
  | CGISEBankIDSignature_ !CGISEBankIDSignature
  | NetsNOBankIDSignature_ !NetsNOBankIDSignature
  | NetsDKNemIDSignature_ !NetsDKNemIDSignature
  deriving (Eq, Ord, Show)

----------------------------------------

-- | Signature provider. Used internally to distinguish between
-- signatures in the database. Should not be exported, as the
-- distinction between various signatures on the outside should
-- be made with pattern matching on 'ESignature' constructors.
data SignatureProvider
  = LegacyBankID
  | LegacyTelia
  | LegacyNordea
  | LegacyMobileBankID
  | CgiGrpBankID
  | NetsNOBankID
  | NetsDKNemID
    deriving (Eq, Ord, Show)

instance PQFormat SignatureProvider where
  pqFormat = pqFormat @Int16

instance FromSQL SignatureProvider where
  type PQBase SignatureProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return LegacyBankID
      2 -> return LegacyTelia
      3 -> return LegacyNordea
      4 -> return LegacyMobileBankID
      5 -> return CgiGrpBankID
      6 -> return NetsNOBankID
      7 -> return NetsDKNemID
      _ -> throwM RangeError { reRange = [(1, 7)], reValue = n }

instance ToSQL SignatureProvider where
  type PQDest SignatureProvider = PQDest Int16
  toSQL LegacyBankID       = toSQL (1 :: Int16)
  toSQL LegacyTelia        = toSQL (2 :: Int16)
  toSQL LegacyNordea       = toSQL (3 :: Int16)
  toSQL LegacyMobileBankID = toSQL (4 :: Int16)
  toSQL CgiGrpBankID       = toSQL (5 :: Int16)
  toSQL NetsNOBankID       = toSQL (6 :: Int16)
  toSQL NetsDKNemID        = toSQL (7 :: Int16)


----------------------------------------

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeCGISEBankIDSignature = MergeCGISEBankIDSignature SignatoryLinkID CGISEBankIDSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeCGISEBankIDSignature () where
  update (MergeCGISEBankIDSignature slid CGISEBankIDSignature {..}) = do
    loopOnUniqueViolation . withSavepoint "merge_bank_id_signature" $ do
      runQuery01_ selectSignatorySignTime
      msign_time :: Maybe UTCTime <- fetchOne runIdentity
      when (isJust msign_time) $ do
        unexpectedError "signatory already signed, can't merge signature"
      success <- runQuery01 . sqlUpdate "eid_signatures" $ do
        setFields
        sqlWhereEq "signatory_link_id" slid
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
      unless success $ runQuery_ . sqlInsertSelect "eid_signatures" "" $ do
        setFields
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "provider"          CgiGrpBankID
        sqlSet "data"              cgisebidsSignedText
        sqlSet "signature"         cgisebidsSignature
        sqlSet "signatory_name"    cgisebidsSignatoryName
        sqlSet "signatory_personal_number" cgisebidsSignatoryPersonalNumber
        sqlSet "ocsp_response"     cgisebidsOcspResponse
        sqlSet "signatory_ip"      cgisebidsSignatoryIP

----------------------------------------

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeNetsNOBankIDSignature = MergeNetsNOBankIDSignature SignatoryLinkID NetsNOBankIDSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsNOBankIDSignature () where
  update (MergeNetsNOBankIDSignature slid NetsNOBankIDSignature {..}) = do
    loopOnUniqueViolation . withSavepoint "merge_bank_id_signature" $ do
      runQuery01_ selectSignatorySignTime
      msign_time :: Maybe UTCTime <- fetchOne runIdentity
      when (isJust msign_time) $ do
        unexpectedError "signatory already signed, can't merge signature"
      success <- runQuery01 . sqlUpdate "eid_signatures" $ do
        setFields
        sqlWhereEq "signatory_link_id" slid
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
      unless success $ do
        runQuery_ . sqlInsertSelect "eid_signatures" "" $ do
          setFields
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "provider"          NetsNOBankID
        sqlSet "data"              netsnoSignedText
        sqlSet "signature" . encodeUtf8 $ netsnoB64SDO
        sqlSet "signatory_name"            netsnoSignatoryName
        sqlSet "signatory_personal_number" netsnoSignatoryPID

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeNetsDKNemIDSignature = MergeNetsDKNemIDSignature SignatoryLinkID NetsDKNemIDSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeNetsDKNemIDSignature () where
  update (MergeNetsDKNemIDSignature slid NetsDKNemIDSignature {..}) = do
    loopOnUniqueViolation . withSavepoint "merge_bank_id_signature" $ do
      runQuery01_ selectSignatorySignTime
      msign_time :: Maybe UTCTime <- fetchOne runIdentity
      when (isJust msign_time) $ do
        unexpectedError "signatory already signed, can't merge signature"
      success <- runQuery01 . sqlUpdate "eid_signatures" $ do
        setFields
        sqlWhereEq "signatory_link_id" slid
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
      unless success $ do
        runQuery_ . sqlInsertSelect "eid_signatures" "" $ do
          setFields
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "provider"          NetsDKNemID
        sqlSet "data"              netsdkSignedText
        sqlSet "signature" . encodeUtf8 $ netsdkB64SDO
        sqlSet "signatory_name"            netsdkSignatoryName
        sqlSet "signatory_personal_number" netsdkSignatorySSN
        sqlSet "signatory_ip"              netsdkSignatoryIP

-- | Get signature for a given signatory.
data GetESignature = GetESignature SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetESignature (Maybe ESignature) where
  query (GetESignature slid) = do
    runQuery_ . sqlSelect "eid_signatures" $ do
      sqlResult "provider"
      sqlResult "data"
      sqlResult "signature"
      sqlResult "certificate"
      sqlResult "signatory_name"
      sqlResult "signatory_personal_number"
      sqlResult "ocsp_response"
      sqlResult "signatory_ip"
      sqlWhereEq "signatory_link_id" slid
    fetchMaybe fetchESignature

-- | Fetch e-signature.
fetchESignature
  :: ( SignatureProvider
     , Text
     , ByteString
     , Maybe ByteString
     , Maybe Text
     , Maybe Text
     , Maybe ByteString
     , Maybe Text
     )
  -> ESignature
fetchESignature (provider, sdata, signature, mcertificate, msignatory_name, msignatory_personal_number, mocsp_response, msignatory_ip)
  = case provider of
    LegacyBankID -> LegacyBankIDSignature_ LegacyBankIDSignature
      { lbidsSignedText  = sdata
      , lbidsSignature   = signature
      , lbidsCertificate = fromJust mcertificate
      }
    LegacyTelia -> LegacyTeliaSignature_ LegacyTeliaSignature
      { ltsSignedText  = sdata
      , ltsSignature   = signature
      , ltsCertificate = fromJust mcertificate
      }
    LegacyNordea -> LegacyNordeaSignature_ LegacyNordeaSignature
      { lnsSignedText  = sdata
      , lnsSignature   = signature
      , lnsCertificate = fromJust mcertificate
      }
    LegacyMobileBankID -> LegacyMobileBankIDSignature_ LegacyMobileBankIDSignature
      { lmbidsSignedText   = sdata
      , lmbidsSignature    = signature
      , lmbidsOcspResponse = fromJust mocsp_response
      }
    CgiGrpBankID -> CGISEBankIDSignature_ CGISEBankIDSignature
      { cgisebidsSignatoryName           = fromJust msignatory_name
      , cgisebidsSignatoryPersonalNumber = fromJust msignatory_personal_number
      , cgisebidsSignatoryIP             = fromMaybe "" msignatory_ip
      , cgisebidsSignedText              = sdata
      , cgisebidsSignature               = signature
      , cgisebidsOcspResponse            = fromJust mocsp_response
      }
    NetsNOBankID -> NetsNOBankIDSignature_ NetsNOBankIDSignature
      { netsnoSignedText    = sdata
      , netsnoB64SDO        = decodeUtf8 signature
      , netsnoSignatoryName = fromJust msignatory_name
      , netsnoSignatoryPID  = fromJust msignatory_personal_number
      }
    NetsDKNemID -> NetsDKNemIDSignature_ NetsDKNemIDSignature
      { netsdkSignedText    = sdata
      , netsdkB64SDO        = decodeUtf8 signature
      , netsdkSignatoryName = fromJust msignatory_name
      , netsdkSignatorySSN  = fromJust msignatory_personal_number
      , netsdkSignatoryIP   = fromMaybe "" msignatory_ip
      }
