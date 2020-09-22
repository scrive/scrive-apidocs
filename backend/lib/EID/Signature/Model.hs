module EID.Signature.Model (
    ESignature(..)
  , module EID.Signature.Legacy
  , SignatureProvider(..)
  , GetESignature(..)
  -- from EID.CGI.GRP.Types
  , CGISEBankIDSignature(..)
  , MergeCGISEBankIDSignature(..)
  -- from EID.Nets.Types
  , NetsNOBankIDSignature(..)
  , MergeNetsNOBankIDSignature(..)
  , NetsDKNemIDSignature(..)
  , MergeNetsDKNemIDSignature(..)
  -- from EID.EIDService.Types
  , EIDServiceNLIDINSignature(..)
  , MergeEIDServiceIDINSignature(..)
  , EIDServiceFITupasSignature(..)
  , MergeEIDServiceFITupasSignature(..)
  , EIDServiceOnfidoSignature(..)
  , MergeEIDServiceOnfidoSignature(..)
  , MergeEIDServiceNOBankIDSignature(..)
  , EIDServiceSEBankIDSignature(..)
  , MergeEIDServiceSEBankIDSignature(..)
  , MergeEIDServiceVerimiQesSignature(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text.Encoding
import Data.Time

import DB
import Doc.SignatoryLinkID
import EID.CGI.GRP.Types
import EID.EIDService.Types
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
  | EIDServiceIDINSignature_ !EIDServiceNLIDINSignature
  | EIDServiceFITupasSignature_ !EIDServiceFITupasSignature
  | EIDServiceOnfidoSignature_ !EIDServiceOnfidoSignature
  | EIDServiceNOBankIDSignature_ !EIDServiceNOBankIDSignature
  | EIDServiceSEBankIDSignature_ !EIDServiceSEBankIDSignature
  | EIDServiceVerimiQesSignature_ !EIDServiceVerimiQesSignature
  deriving (Eq, Ord, Show)

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
  | EIDServiceIDIN
  | EIDServiceTupas
  | EIDServiceOnfido
  | EIDServiceNOBankID
  | EIDServiceSEBankID
  | EIDServiceVerimi
    deriving (Eq, Ord, Show)

instance PQFormat SignatureProvider where
  pqFormat = pqFormat @Int16

instance FromSQL SignatureProvider where
  type PQBase SignatureProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return LegacyBankID
      2  -> return LegacyTelia
      3  -> return LegacyNordea
      4  -> return LegacyMobileBankID
      5  -> return CgiGrpBankID
      6  -> return NetsNOBankID
      7  -> return NetsDKNemID
      8  -> return EIDServiceIDIN
      9  -> return EIDServiceTupas
      10 -> return EIDServiceOnfido
      11 -> return EIDServiceNOBankID
      12 -> return EIDServiceSEBankID
      13 -> return EIDServiceVerimi
      _  -> throwM RangeError { reRange = [(1, 13)], reValue = n }

instance ToSQL SignatureProvider where
  type PQDest SignatureProvider = PQDest Int16
  toSQL LegacyBankID       = toSQL (1 :: Int16)
  toSQL LegacyTelia        = toSQL (2 :: Int16)
  toSQL LegacyNordea       = toSQL (3 :: Int16)
  toSQL LegacyMobileBankID = toSQL (4 :: Int16)
  toSQL CgiGrpBankID       = toSQL (5 :: Int16)
  toSQL NetsNOBankID       = toSQL (6 :: Int16)
  toSQL NetsDKNemID        = toSQL (7 :: Int16)
  toSQL EIDServiceIDIN     = toSQL (8 :: Int16)
  toSQL EIDServiceTupas    = toSQL (9 :: Int16)
  toSQL EIDServiceOnfido   = toSQL (10 :: Int16)
  toSQL EIDServiceNOBankID = toSQL (11 :: Int16)
  toSQL EIDServiceSEBankID = toSQL (12 :: Int16)
  toSQL EIDServiceVerimi   = toSQL (13 :: Int16)

----------------------------------------

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeCGISEBankIDSignature = MergeCGISEBankIDSignature SignatoryLinkID CGISEBankIDSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeCGISEBankIDSignature () where
  dbUpdate (MergeCGISEBankIDSignature slid CGISEBankIDSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
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
  dbUpdate (MergeNetsNOBankIDSignature slid NetsNOBankIDSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
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
  dbUpdate (MergeNetsDKNemIDSignature slid NetsDKNemIDSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
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

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeEIDServiceFITupasSignature = MergeEIDServiceFITupasSignature SignatoryLinkID EIDServiceFITupasSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceFITupasSignature () where
  dbUpdate (MergeEIDServiceFITupasSignature slid EIDServiceFITupasSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id"         slid
        sqlSet "provider"                  EIDServiceTupas
        sqlSet "signatory_name"            eidServiceFITupasSigSignatoryName
        sqlSet "signatory_personal_number" eidServiceFITupasSigPersonalNumber
        sqlSet "signatory_date_of_birth"   eidServiceFITupasSigDateOfBirth

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeEIDServiceNOBankIDSignature = MergeEIDServiceNOBankIDSignature SignatoryLinkID EIDServiceNOBankIDSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceNOBankIDSignature () where
  dbUpdate (MergeEIDServiceNOBankIDSignature slid EIDServiceNOBankIDSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id"       slid
        sqlSet "provider"                EIDServiceNOBankID
        sqlSet "signatory_name"          eidServiceNOBankIDSigSignatoryName
        sqlSet "signatory_personal_number" eidServiceNOBankIDSigPersonalNumber
        sqlSet "signatory_date_of_birth" eidServiceNOBankIDSigDateOfBirth
        sqlSet "data"                    eidServiceNOBankIDSigSignedText
        sqlSet "signature" . encodeUtf8 $ fromMaybe "" eidServiceNOBankIDSigCertificate

-- Note to Tom: please refactor these!
data MergeEIDServiceOnfidoSignature = MergeEIDServiceOnfidoSignature SignatoryLinkID EIDServiceOnfidoSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceOnfidoSignature () where
  dbUpdate (MergeEIDServiceOnfidoSignature slid EIDServiceOnfidoSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "provider"          EIDServiceOnfido
        sqlSet "data" $ encodeToLazyText eidServiceOnfidoSigMethod
        -- Well, this only slightly stretches the original purpose of the
        -- "data" field... Note that we can't use plain `encode`, since the
        -- ToSQL instance for ByteString spits out a hexadecimal representation.
        sqlSet "signatory_name"            eidServiceOnfidoSigSignatoryName
        sqlSet "signatory_date_of_birth"   eidServiceOnfidoSigDateOfBirth
        sqlSet "signatory_personal_number" ("" :: Text)  -- can't be null, but we don't actually set one. ugly?!

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeEIDServiceIDINSignature = MergeEIDServiceIDINSignature SignatoryLinkID EIDServiceNLIDINSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceIDINSignature () where
  dbUpdate (MergeEIDServiceIDINSignature slid EIDServiceNLIDINSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id"         slid
        sqlSet "provider"                  EIDServiceIDIN
        sqlSet "signatory_date_of_birth"   unEIDServiceIDINSigDateOfBirth
        sqlSet "signatory_personal_number" unEIDServiceIDINSigCustomerID
        sqlSet "signatory_name"            unEIDServiceIDINSigSignatoryName

data MergeEIDServiceSEBankIDSignature = MergeEIDServiceSEBankIDSignature SignatoryLinkID EIDServiceSEBankIDSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceSEBankIDSignature () where
  dbUpdate (MergeEIDServiceSEBankIDSignature slid EIDServiceSEBankIDSignature {..}) = do
    runQuery01_ selectSignatorySignTime
    msign_time :: Maybe UTCTime <- fetchOne runIdentity
    when (isJust msign_time) $ do
      unexpectedError "signatory already signed, can't merge signature"
    runQuery_ . sqlInsert "eid_signatures" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "provider"          EIDServiceSEBankID
        sqlSet "signatory_name"    eidServiceSEBankIDSigSignatoryName
        sqlSet "signatory_personal_number" eidServiceSEBankIDSigPersonalNumber
        sqlSet "signatory_ip"      eidServiceSEBankIDSigIP
        sqlSet "signature"         eidServiceSEBankIDSigSignature
        sqlSet "ocsp_response"     eidServiceSEBankIDSigOcspResponse
        sqlSet "data"              eidServiceSEBankIDSigSignedText

data MergeEIDServiceVerimiQesSignature = MergeEIDServiceVerimiQesSignature SignatoryLinkID EIDServiceVerimiQesSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeEIDServiceVerimiQesSignature () where
  dbUpdate (MergeEIDServiceVerimiQesSignature slid EIDServiceVerimiQesSignature {..}) =
    do
      runQuery01_ selectSignatorySignTime
      msign_time :: Maybe UTCTime <- fetchOne runIdentity
      when (isJust msign_time) $ do
        unexpectedError "signatory already signed, can't merge signature"
      runQuery_ . sqlInsert "eid_signatures" $ do
        setFields
        sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
          setFields
          -- replace the signature only if signatory hasn't signed yet
          sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id"      slid
        sqlSet "provider"               EIDServiceVerimi
        sqlSet "signatory_name"         eidServiceVerimiSigName
        sqlSet "signatory_email"        eidServiceVerimiSigVerifiedEmail
        sqlSet "signatory_phone_number" eidServiceVerimiSigVerifiedPhone

-- | Get signature for a given signatory.
newtype GetESignature = GetESignature SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetESignature (Maybe ESignature) where
  dbQuery (GetESignature slid) = do
    runQuery_ . sqlSelect "eid_signatures" $ do
      sqlResult "provider"
      sqlResult "data"
      sqlResult "signature"
      sqlResult "certificate"
      sqlResult "signatory_name"
      sqlResult "signatory_personal_number"
      sqlResult "ocsp_response"
      sqlResult "signatory_ip"
      sqlResult "signatory_date_of_birth"
      sqlResult "signatory_email"
      sqlResult "signatory_phone_number"
      sqlWhereEq "signatory_link_id" slid
    fetchMaybe fetchESignature

-- | Fetch e-signature.
fetchESignature
  :: ( SignatureProvider
     , Maybe Text
     , Maybe ByteString
     , Maybe ByteString
     , Maybe Text
     , Maybe Text
     , Maybe ByteString
     , Maybe Text
     , Maybe Text
     , Maybe Text
     , Maybe Text
     )
  -> ESignature
fetchESignature (provider, sdata, signature, mcertificate, msignatory_name, msignatory_personal_number, mocsp_response, msignatory_ip, msignatory_dob, msignatory_email, msignatory_phone_number)
  = case provider of
    LegacyBankID -> LegacyBankIDSignature_ LegacyBankIDSignature
      { lbidsSignedText  = fromJust sdata
      , lbidsSignature   = fromJust signature
      , lbidsCertificate = fromJust mcertificate
      }
    LegacyTelia -> LegacyTeliaSignature_ LegacyTeliaSignature
      { ltsSignedText  = fromJust sdata
      , ltsSignature   = fromJust signature
      , ltsCertificate = fromJust mcertificate
      }
    LegacyNordea -> LegacyNordeaSignature_ LegacyNordeaSignature
      { lnsSignedText  = fromJust sdata
      , lnsSignature   = fromJust signature
      , lnsCertificate = fromJust mcertificate
      }
    LegacyMobileBankID -> LegacyMobileBankIDSignature_ LegacyMobileBankIDSignature
      { lmbidsSignedText   = fromJust sdata
      , lmbidsSignature    = fromJust signature
      , lmbidsOcspResponse = fromJust mocsp_response
      }
    CgiGrpBankID -> CGISEBankIDSignature_ CGISEBankIDSignature
      { cgisebidsSignatoryName           = fromJust msignatory_name
      , cgisebidsSignatoryPersonalNumber = fromJust msignatory_personal_number
      , cgisebidsSignatoryIP             = fromMaybe "" msignatory_ip
      , cgisebidsSignedText              = fromJust sdata
      , cgisebidsSignature               = fromJust signature
      , cgisebidsOcspResponse            = fromJust mocsp_response
      }
    NetsNOBankID -> NetsNOBankIDSignature_ NetsNOBankIDSignature
      { netsnoSignedText    = fromJust sdata
      , netsnoB64SDO        = decodeUtf8 $ fromJust signature
      , netsnoSignatoryName = fromJust msignatory_name
      , netsnoSignatoryPID  = fromJust msignatory_personal_number
      }
    NetsDKNemID -> NetsDKNemIDSignature_ NetsDKNemIDSignature
      { netsdkSignedText    = fromJust sdata
      , netsdkB64SDO        = decodeUtf8 $ fromJust signature
      , netsdkSignatoryName = fromJust msignatory_name
      , netsdkSignatorySSN  = fromJust msignatory_personal_number
      , netsdkSignatoryIP   = fromMaybe "" msignatory_ip
      }
    EIDServiceIDIN -> EIDServiceIDINSignature_ $ EIDServiceNLIDINSignature
      { unEIDServiceIDINSigSignatoryName = fromJust msignatory_name
      , unEIDServiceIDINSigDateOfBirth   = fromJust msignatory_dob
      , unEIDServiceIDINSigCustomerID    = fromJust msignatory_personal_number
      }
    EIDServiceTupas -> EIDServiceFITupasSignature_ $ EIDServiceFITupasSignature
      { eidServiceFITupasSigSignatoryName  = fromJust msignatory_name
      , eidServiceFITupasSigPersonalNumber = msignatory_personal_number
      , eidServiceFITupasSigDateOfBirth    = msignatory_dob
      }
    EIDServiceOnfido -> EIDServiceOnfidoSignature_ $ EIDServiceOnfidoSignature
      { eidServiceOnfidoSigSignatoryName = fromJust msignatory_name
      , eidServiceOnfidoSigDateOfBirth   = fromJust msignatory_dob
      , eidServiceOnfidoSigMethod        = fromJust $ do
                                             dataText <- sdata
                                             decodeStrict $ encodeUtf8 dataText
      }
    EIDServiceNOBankID -> EIDServiceNOBankIDSignature_ $ EIDServiceNOBankIDSignature
      { eidServiceNOBankIDSigInternalProvider = EIDServiceNOBankIDStandard
      , eidServiceNOBankIDSigSignatoryName    = fromJust msignatory_name
      , eidServiceNOBankIDSigPhoneNumber      = Nothing
      -- make sure we insert empty string, not null for case
      -- where personal number is never provided (mobile bankid)
      , eidServiceNOBankIDSigPersonalNumber   = Just
                                                  $ fromMaybe "" msignatory_personal_number
      , eidServiceNOBankIDSigDateOfBirth      = msignatory_dob
      , eidServiceNOBankIDSigSignedText       = sdata
      , eidServiceNOBankIDSigCertificate      = decodeUtf8 <$> signature
      }
    EIDServiceSEBankID -> EIDServiceSEBankIDSignature_ $ EIDServiceSEBankIDSignature
      { eidServiceSEBankIDSigSignatoryName  = fromJust msignatory_name
      , eidServiceSEBankIDSigPersonalNumber = fromJust msignatory_personal_number
      , eidServiceSEBankIDSigIP             = fromJust msignatory_ip
      , eidServiceSEBankIDSigSignedText     = fromJust sdata
      , eidServiceSEBankIDSigSignature      = fromJust signature
      , eidServiceSEBankIDSigOcspResponse   = fromJust mocsp_response
      }
    EIDServiceVerimi -> EIDServiceVerimiQesSignature_ $ EIDServiceVerimiQesSignature
      { eidServiceVerimiSigName          = fromJust msignatory_name
      , eidServiceVerimiSigVerifiedEmail = msignatory_email
      , eidServiceVerimiSigVerifiedPhone = msignatory_phone_number
      }
