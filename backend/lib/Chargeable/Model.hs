module Chargeable.Model (
    ChargeableItem(..)
  , ChargeCompanyForSMS(..)
  , ChargeCompanyForSEBankIDSignature(..)
  , ChargeCompanyForSEBankIDAuthentication(..)
  , ChargeCompanyForNOBankIDAuthentication(..)
  , ChargeCompanyForNOBankIDSignature(..)
  , ChargeCompanyForDKNemIDAuthentication(..)
  , ChargeCompanyForStartingDocument(..)
  , ChargeCompanyForClosingDocument(..)
  , GetNumberOfDocumentsStartedThisMonth(..)
  , ChargeCompanyForClosingSignature(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Data.Typeable

import Company.CompanyID
import DB
import Doc.DocumentID
import MinutesTime
import SMS.Data (SMSProvider(..))
import User.UserID

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
  CINOBankIDSignature
  deriving (Eq, Ord, Show, Typeable)

instance PQFormat ChargeableItem where
  pqFormat = const $ pqFormat (undefined::Int16)

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
      _  -> throwM RangeError {
        reRange = [(1, 10)]
      , reValue = n
      }

instance ToSQL ChargeableItem where
  type PQDest ChargeableItem = PQDest Int16
  toSQL CISMS                    = toSQL ( 1::Int16)
  toSQL CISEBankIDSignature      = toSQL ( 2::Int16)
  toSQL CISEBankIDAuthentication = toSQL ( 3::Int16)
  toSQL CINOBankIDAuthentication = toSQL ( 4::Int16)
  toSQL CISMSTelia               = toSQL ( 5::Int16)
  toSQL CIStartingDocument       = toSQL ( 6::Int16)
  toSQL CIDKNemIDAuthentication  = toSQL ( 7::Int16)
  toSQL CIClosingDocument        = toSQL ( 8::Int16)
  toSQL CIClosingSignature       = toSQL ( 9::Int16)
  toSQL CINOBankIDSignature      = toSQL (10::Int16)

----------------------------------------

-- Note: We charge the company of the author of the document
-- at a time of the event, therefore the company id never
-- changes, even if the corresponding user moves to the other
-- company.

-- | Charge company of the author of the document for SMSes.
data ChargeCompanyForSMS = ChargeCompanyForSMS DocumentID SMSProvider Int32
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForSMS () where
  update (ChargeCompanyForSMS document_id SMSDefault sms_count)        = update (ChargeCompanyFor CISMS sms_count document_id)
  update (ChargeCompanyForSMS document_id SMSTeliaCallGuide sms_count) = update (ChargeCompanyFor CISMSTelia sms_count document_id)

-- | Charge company of the author of the document for swedish bankid signature while signing.
data ChargeCompanyForSEBankIDSignature = ChargeCompanyForSEBankIDSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForSEBankIDSignature () where
  update (ChargeCompanyForSEBankIDSignature document_id) = update (ChargeCompanyFor CISEBankIDSignature 1 document_id)

-- | Charge company of the author of the document for swedish authorization
data ChargeCompanyForSEBankIDAuthentication = ChargeCompanyForSEBankIDAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForSEBankIDAuthentication () where
  update (ChargeCompanyForSEBankIDAuthentication document_id) = update (ChargeCompanyFor CISEBankIDAuthentication 1 document_id)

-- | Charge company of the author of the document for norwegian authorization
data ChargeCompanyForNOBankIDAuthentication = ChargeCompanyForNOBankIDAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForNOBankIDAuthentication () where
  update (ChargeCompanyForNOBankIDAuthentication document_id) = update (ChargeCompanyFor CINOBankIDAuthentication 1 document_id)

-- | Charge company of the author of the document for norwegian bankid signature while signing.
data ChargeCompanyForNOBankIDSignature = ChargeCompanyForNOBankIDSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForNOBankIDSignature () where
  update (ChargeCompanyForNOBankIDSignature document_id) = update (ChargeCompanyFor CINOBankIDSignature 1 document_id)

-- | Charge company of the author of the document for danish authentication
data ChargeCompanyForDKNemIDAuthentication = ChargeCompanyForDKNemIDAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForDKNemIDAuthentication () where
  update (ChargeCompanyForDKNemIDAuthentication document_id) = update (ChargeCompanyFor CIDKNemIDAuthentication 1 document_id)

-- | Charge company of the author of the document for creation of the document
data ChargeCompanyForStartingDocument = ChargeCompanyForStartingDocument DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForStartingDocument () where
  update (ChargeCompanyForStartingDocument document_id) = update (ChargeCompanyFor CIStartingDocument 1 document_id)

-- | Charge company of the author of the document for closing of the document
data ChargeCompanyForClosingDocument = ChargeCompanyForClosingDocument DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForClosingDocument () where
  update (ChargeCompanyForClosingDocument document_id) = update (ChargeCompanyFor CIClosingDocument 1 document_id)

-- | Charge company of the author of the document for closing a signature
data ChargeCompanyForClosingSignature = ChargeCompanyForClosingSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForClosingSignature () where
  update (ChargeCompanyForClosingSignature document_id) = update (ChargeCompanyFor CIClosingSignature 1 document_id)

data ChargeCompanyFor = ChargeCompanyFor ChargeableItem Int32 DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyFor () where
  update (ChargeCompanyFor item quantity document_id) = do
    now <- currentTime
    (user_id,company_id) <- getAuthorAndAuthorsCompanyIDs document_id
    runQuery_ . sqlInsert "chargeable_items" $ do
      sqlSet "time" now
      sqlSet "type" item
      sqlSet "company_id" $ company_id
      sqlSet "user_id" user_id
      sqlSet "document_id" document_id
      sqlSet "quantity" quantity
----------------------------------------

data GetTotalOfChargeableItemFromThisMonth = GetTotalOfChargeableItemFromThisMonth ChargeableItem CompanyID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetTotalOfChargeableItemFromThisMonth Int64 where
  query (GetTotalOfChargeableItemFromThisMonth charge_type company_id) = do
    now <- currentTime
    let firstOfCurrentMonth = formatTime' "%Y-%m-01" now -- IGNORING TIME ZONE - DEFAULT ONE SHOULD BE FINE
    runQuery_ . sqlSelect "chargeable_items" $ do
      sqlWhereEq "company_id" company_id
      sqlWhereEq "type" charge_type
      sqlWhere $ "time >= cast ("<?> firstOfCurrentMonth <+>" as timestamp)"
      sqlResult "COALESCE(sum(quantity),0)"
    fetchOne runIdentity

data GetNumberOfDocumentsStartedThisMonth = GetNumberOfDocumentsStartedThisMonth CompanyID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetNumberOfDocumentsStartedThisMonth Int64 where
  query (GetNumberOfDocumentsStartedThisMonth company_id) = query $ GetTotalOfChargeableItemFromThisMonth CIStartingDocument company_id

-- | Fetch id of the author of the document.
getAuthorAndAuthorsCompanyIDs :: (MonadDB m, MonadThrow m) => DocumentID -> m (UserID, CompanyID)
getAuthorAndAuthorsCompanyIDs did = do
  runQuery_ . sqlSelect "documents d" $ do
    sqlJoinOn "users u" "d.author_user_id = u.id"
    sqlResult "u.id"
    sqlResult "u.company_id"
    sqlWhereEq "d.id" did
  fetchOne id
