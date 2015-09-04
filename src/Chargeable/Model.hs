module Chargeable.Model (
    ChargeCompanyForSMS(..)
  , ChargeCompanyForSEBankIDSignature(..)
  , ChargeCompanyForSEBankIDAuthentication(..)
  , ChargeCompanyForNOBankIDAuthentication(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Data.Typeable

import Company.CompanyID
import DB
import Doc.DocumentID
import KontraPrelude
import User.UserID

data ChargeableItem = SMS | SEBankIDSignature | SEBankIDAuthentication | NOBankIDAuthentication
  deriving (Eq, Ord, Show, Typeable)

instance PQFormat ChargeableItem where
  pqFormat = const $ pqFormat ($undefined::Int16)

instance FromSQL ChargeableItem where
  type PQBase ChargeableItem = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SMS
      2 -> return SEBankIDSignature
      3 -> return SEBankIDAuthentication
      4 -> return NOBankIDAuthentication
      _ -> throwM RangeError {
        reRange = [(1, 4)]
      , reValue = n
      }

instance ToSQL ChargeableItem where
  type PQDest ChargeableItem = PQDest Int16
  toSQL SMS                    = toSQL (1::Int16)
  toSQL SEBankIDSignature      = toSQL (2::Int16)
  toSQL SEBankIDAuthentication = toSQL (3::Int16)
  toSQL NOBankIDAuthentication = toSQL (4::Int16)

----------------------------------------

-- Note: We charge the company of the author of the document
-- at a time of the event, therefore the company id never
-- changes, even if the corresponding user moves to the other
-- company.

-- | Charge company of the author of the document for SMSes.
data ChargeCompanyForSMS = ChargeCompanyForSMS DocumentID Int32
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForSMS () where
  update (ChargeCompanyForSMS document_id sms_count) =  update (ChargeCompanyFor SMS sms_count document_id)

-- | Charge company of the author of the document for swedish bankid signature while signing.
data ChargeCompanyForSEBankIDSignature = ChargeCompanyForSEBankIDSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForSEBankIDSignature () where
  update (ChargeCompanyForSEBankIDSignature document_id) = update (ChargeCompanyFor SEBankIDSignature 1 document_id)

-- | Charge company of the author of the document for swedish authorization
data ChargeCompanyForSEBankIDAuthentication = ChargeCompanyForSEBankIDAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForSEBankIDAuthentication () where
  update (ChargeCompanyForSEBankIDAuthentication document_id) = update (ChargeCompanyFor SEBankIDAuthentication 1 document_id)

-- | Charge company of the author of the document for norwegian authorization
data ChargeCompanyForNOBankIDAuthentication = ChargeCompanyForNOBankIDAuthentication DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForNOBankIDAuthentication () where
  update (ChargeCompanyForNOBankIDAuthentication document_id) = update (ChargeCompanyFor NOBankIDAuthentication 1 document_id)

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

-- | Fetch id of the author of the document.
getAuthorAndAuthorsCompanyIDs :: (MonadDB m, MonadThrow m) => DocumentID -> m (UserID,CompanyID)
getAuthorAndAuthorsCompanyIDs document_id = do
  runQuery_ . sqlSelect "signatory_links as sl, users as u" $ do
    sqlResult "sl.user_id"
    sqlResult "u.company_id"
    sqlWhereEq "sl.document_id" document_id
    sqlWhere "sl.is_author"
    sqlWhere "u.id = sl.user_id"
  fetchOne id
