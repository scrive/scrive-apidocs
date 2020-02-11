module Chargeable.Model (
    ChargeableItem(..)
  , ChargeUserGroupForSMS(..)
  , ChargeUserGroupForSEBankIDSignature(..)
  , ChargeUserGroupForSEBankIDAuthentication(..)
  , ChargeUserGroupForNOBankIDAuthentication(..)
  , ChargeUserGroupForNOBankIDSignature(..)
  , ChargeUserGroupForDKNemIDAuthentication(..)
  , ChargeUserGroupForDKNemIDSignature(..)
  , ChargeUserGroupForFITupasAuthentication(..)
  , ChargeUserGroupForVerimiAuthentication(..)
  , ChargeUserGroupForIDINAuthentication(..)
  , ChargeUserGroupForIDINSignature(..)
  , ChargeUserGroupForStartingDocument(..)
  , ChargeUserGroupForClosingDocument(..)
  , ChargeUserGroupForClosingSignature(..)
  , ChargeUserGroupForShareableLink(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Data.Typeable

import DB
import Doc.DocumentID
import MinutesTime
import SMS.Types (SMSProvider(..))
import User.UserID
import UserGroup.Types

data ChargeableItem =
  CIStartingDocument       |
  CIClosingDocument        |
  CISMS                    |
  CISMSTelia               |
  CISEBankIDSignature      |
  CISEBankIDAuthentication |
  CINOBankIDAuthentication |
  CIDKNemIDAuthentication  |
  CIClosingSignature       |
  CINOBankIDSignature      |
  CIDKNemIDSignature       |
  CIFITupasAuthentication  |
  CIShareableLink          |
  CIVerimiAuthentication   |
  CIIDINAuthentication     |
  CIIDINSignature
  deriving (Eq, Ord, Show, Typeable)

instance PQFormat ChargeableItem where
  pqFormat = pqFormat @Int16

instance FromSQL ChargeableItem where
  type PQBase ChargeableItem = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      -- Note:
      -- If changing this, please also update `pure_sql/invoice_stats.sql`
      1  -> return CISMS
      2  -> return CISEBankIDSignature
      3  -> return CISEBankIDAuthentication
      4  -> return CINOBankIDAuthentication
      5  -> return CISMSTelia
      6  -> return CIStartingDocument
      7  -> return CIDKNemIDAuthentication
      8  -> return CIClosingDocument
      9  -> return CIClosingSignature
      10 -> return CINOBankIDSignature
      11 -> return CIDKNemIDSignature
      12 -> return CIFITupasAuthentication
      13 -> return CIShareableLink
      14 -> return CIVerimiAuthentication
      15 -> return CIIDINAuthentication
      16 -> return CIIDINSignature
      _  -> throwM RangeError { reRange = [(1, 16)], reValue = n }

instance ToSQL ChargeableItem where
  type PQDest ChargeableItem = PQDest Int16
  toSQL CISMS                    = toSQL (1 :: Int16)
  toSQL CISEBankIDSignature      = toSQL (2 :: Int16)
  toSQL CISEBankIDAuthentication = toSQL (3 :: Int16)
  toSQL CINOBankIDAuthentication = toSQL (4 :: Int16)
  toSQL CISMSTelia               = toSQL (5 :: Int16)
  toSQL CIStartingDocument       = toSQL (6 :: Int16)
  toSQL CIDKNemIDAuthentication  = toSQL (7 :: Int16)
  toSQL CIClosingDocument        = toSQL (8 :: Int16)
  toSQL CIClosingSignature       = toSQL (9 :: Int16)
  toSQL CINOBankIDSignature      = toSQL (10 :: Int16)
  toSQL CIDKNemIDSignature       = toSQL (11 :: Int16)
  toSQL CIFITupasAuthentication  = toSQL (12 :: Int16)
  toSQL CIShareableLink          = toSQL (13 :: Int16)
  toSQL CIVerimiAuthentication   = toSQL (14 :: Int16)
  toSQL CIIDINAuthentication     = toSQL (15 :: Int16)
  toSQL CIIDINSignature          = toSQL (16 :: Int16)

-- Note: We charge the user group of the author of the document
-- at a time of the event, therefore the user_group_id never
-- changes, even if the corresponding user moves to the other
-- user group.

-- | Charge user group of the author of the document for SMSes.
data ChargeUserGroupForSMS = ChargeUserGroupForSMS DocumentID SMSProvider Int32
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForSMS () where
  update (ChargeUserGroupForSMS document_id SMSDefault sms_count) =
    update (ChargeUserGroupFor CISMS sms_count document_id)
  update (ChargeUserGroupForSMS document_id SMSTeliaCallGuide sms_count) =
    update (ChargeUserGroupFor CISMSTelia sms_count document_id)

-- | Charge acting user group (normally that of the document author) for swedish bankid signature while signing.
data ChargeUserGroupForSEBankIDSignature = ChargeUserGroupForSEBankIDSignature UserGroupID DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForSEBankIDSignature () where
  update (ChargeUserGroupForSEBankIDSignature ugid document_id) =
    update (ChargeUserGroupExplicitlyFor CISEBankIDSignature 1 ugid document_id)

-- | Charge acting user group (normally that of the document author) for swedish authorization.
data ChargeUserGroupForSEBankIDAuthentication = ChargeUserGroupForSEBankIDAuthentication UserGroupID DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForSEBankIDAuthentication () where
  update (ChargeUserGroupForSEBankIDAuthentication ugid document_id) =
    update (ChargeUserGroupExplicitlyFor CISEBankIDAuthentication 1 ugid document_id)

-- | Charge user group of the author of the document for norwegian authorization
data ChargeUserGroupForNOBankIDAuthentication = ChargeUserGroupForNOBankIDAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForNOBankIDAuthentication () where
  update (ChargeUserGroupForNOBankIDAuthentication document_id) =
    update (ChargeUserGroupFor CINOBankIDAuthentication 1 document_id)

-- | Charge user group of the author of the document for norwegian bankid signature while signing.
data ChargeUserGroupForNOBankIDSignature = ChargeUserGroupForNOBankIDSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForNOBankIDSignature () where
  update (ChargeUserGroupForNOBankIDSignature document_id) =
    update (ChargeUserGroupFor CINOBankIDSignature 1 document_id)

-- | Charge user group of the author of the document for danish authentication
data ChargeUserGroupForDKNemIDAuthentication = ChargeUserGroupForDKNemIDAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForDKNemIDAuthentication () where
  update (ChargeUserGroupForDKNemIDAuthentication document_id) =
    update (ChargeUserGroupFor CIDKNemIDAuthentication 1 document_id)

-- | Charge user group of the author of the document for danish nemid signature
data ChargeUserGroupForDKNemIDSignature = ChargeUserGroupForDKNemIDSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForDKNemIDSignature () where
  update (ChargeUserGroupForDKNemIDSignature document_id) =
    update (ChargeUserGroupFor CIDKNemIDSignature 1 document_id)

-- | Charge user group of the author of the document for danish authentication
data ChargeUserGroupForFITupasAuthentication = ChargeUserGroupForFITupasAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForFITupasAuthentication () where
  update (ChargeUserGroupForFITupasAuthentication document_id) =
    update (ChargeUserGroupFor CIFITupasAuthentication 1 document_id)

-- | Charge user group of the author of the document for verimi authentication
data ChargeUserGroupForVerimiAuthentication = ChargeUserGroupForVerimiAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForVerimiAuthentication () where
  update (ChargeUserGroupForVerimiAuthentication document_id) =
    update (ChargeUserGroupFor CIVerimiAuthentication 1 document_id)

-- | Charge user group of the author of the document for iDIN authentication
data ChargeUserGroupForIDINAuthentication = ChargeUserGroupForIDINAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForIDINAuthentication () where
  update (ChargeUserGroupForIDINAuthentication document_id) =
    update (ChargeUserGroupFor CIIDINAuthentication 1 document_id)

-- | Charge user group of the author of the document for iDIN authentication
data ChargeUserGroupForIDINSignature = ChargeUserGroupForIDINSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForIDINSignature () where
  update (ChargeUserGroupForIDINSignature document_id) =
    update (ChargeUserGroupFor CIIDINSignature 1 document_id)

-- | Charge user group of the author of the document for creation of the document
data ChargeUserGroupForStartingDocument = ChargeUserGroupForStartingDocument DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForStartingDocument () where
  update (ChargeUserGroupForStartingDocument document_id) =
    update (ChargeUserGroupFor CIStartingDocument 1 document_id)

-- | Charge user group of the author of the document for closing of the document
data ChargeUserGroupForClosingDocument = ChargeUserGroupForClosingDocument DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForClosingDocument () where
  update (ChargeUserGroupForClosingDocument document_id) =
    update (ChargeUserGroupFor CIClosingDocument 1 document_id)

-- | Charge user group of the author of the document for closing a signature
data ChargeUserGroupForClosingSignature = ChargeUserGroupForClosingSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForClosingSignature () where
  update (ChargeUserGroupForClosingSignature document_id) =
    update (ChargeUserGroupFor CIClosingSignature 1 document_id)

-- | Charge user group of the author of the document for using a shareable link
data ChargeUserGroupForShareableLink = ChargeUserGroupForShareableLink DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupForShareableLink () where
  update (ChargeUserGroupForShareableLink document_id) =
    update (ChargeUserGroupFor CIShareableLink 1 document_id)

data ChargeUserGroupFor = ChargeUserGroupFor ChargeableItem Int32 DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupFor () where
  update (ChargeUserGroupFor item quantity document_id) = do
    now             <- currentTime
    (user_id, ugid) <- getAuthorAndAuthorsUserGroupIDs document_id
    runQuery_ . sqlInsert "chargeable_items" $ do
      sqlSet "time"          now
      sqlSet "type"          item
      sqlSet "user_id"       user_id
      sqlSet "document_id"   document_id
      sqlSet "quantity"      quantity
      sqlSet "user_group_id" ugid

-- Takes the charged user group as an explicit parameter rather than inferring it from the document.
data ChargeUserGroupExplicitlyFor = ChargeUserGroupExplicitlyFor ChargeableItem Int32 UserGroupID DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeUserGroupExplicitlyFor () where
  update (ChargeUserGroupExplicitlyFor item quantity user_group_id document_id) = do
    now          <- currentTime
    (user_id, _) <- getAuthorAndAuthorsUserGroupIDs document_id
    runQuery_ . sqlInsert "chargeable_items" $ do
      sqlSet "time"          now
      sqlSet "type"          item
      sqlSet "user_id"       user_id
      sqlSet "document_id"   document_id
      sqlSet "quantity"      quantity
      sqlSet "user_group_id" user_group_id

data GetTotalOfChargeableItemFromThisMonth = GetTotalOfChargeableItemFromThisMonth ChargeableItem UserGroupID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetTotalOfChargeableItemFromThisMonth Int64 where
  query (GetTotalOfChargeableItemFromThisMonth charge_type ugid) = do
    now <- currentTime
    let firstOfCurrentMonth = formatTime' "%Y-%m-01" now -- IGNORING TIME ZONE - DEFAULT ONE SHOULD BE FINE
    runQuery_ . sqlSelect "chargeable_items" $ do
      sqlWhereEq "user_group_id" ugid
      sqlWhereEq "type"          charge_type
      sqlWhere $ "time >= cast (" <?> firstOfCurrentMonth <+> " as timestamp)"
      sqlResult "COALESCE(sum(quantity),0)"
    fetchOne runIdentity

-- | Fetch id of the author of the document.
getAuthorAndAuthorsUserGroupIDs
  :: (MonadDB m, MonadThrow m) => DocumentID -> m (UserID, UserGroupID)
getAuthorAndAuthorsUserGroupIDs did = do
  runQuery_ . sqlSelect "documents d" $ do
    sqlJoinOn "users u" "d.author_user_id = u.id"
    sqlResult "u.id"
    sqlResult "u.user_group_id"
    sqlWhereEq "d.id" did
  fetchOne identity
