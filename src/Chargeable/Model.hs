module Chargeable.Model (
    ChargeCompanyForSMS(..)
  , ChargeCompanyForElegSignature(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Typeable

import Company.CompanyID
import Data.Time.Monad
import DB
import Doc.DocumentID
import KontraPrelude
import User.UserID

data ChargeableItem = SMS | ELegSignature
  deriving (Eq, Ord, Show, Typeable)

instance PQFormat ChargeableItem where
  pqFormat = const $ pqFormat ($undefined::Int16)

instance FromSQL ChargeableItem where
  type PQBase ChargeableItem = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SMS
      2 -> return ELegSignature
      _ -> throwM RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL ChargeableItem where
  type PQDest ChargeableItem = PQDest Int16
  toSQL SMS           = toSQL (1::Int16)
  toSQL ELegSignature = toSQL (2::Int16)

----------------------------------------

-- Note: We charge the company of the author of the document
-- at a time of the event, therefore the company id never
-- changes, even if the corresponding user moves to the other
-- company.

-- | Charge company of the author of the document for SMSes.
data ChargeCompanyForSMS = ChargeCompanyForSMS DocumentID Int32
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForSMS () where
  update (ChargeCompanyForSMS document_id sms_count) = do
    now <- currentTime
    (user_id,company_id) <- getAuthorAndAuthorsCompanyIDs document_id
    runQuery_ . sqlInsert "chargeable_items" $ do
      sqlSet "time" now
      sqlSet "type" SMS
      sqlSet "company_id" $ company_id
      sqlSet "user_id" user_id
      sqlSet "document_id" document_id
      sqlSet "quantity" sms_count

-- | Charge company of the author of the document for e-leg signature.
data ChargeCompanyForElegSignature = ChargeCompanyForElegSignature DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m ChargeCompanyForElegSignature () where
  update (ChargeCompanyForElegSignature document_id) = do
    now <- currentTime
    (user_id,company_id) <- getAuthorAndAuthorsCompanyIDs document_id
    runQuery_ . sqlInsert "chargeable_items" $ do
      sqlSet "time" now
      sqlSet "type" ELegSignature
      sqlSet "company_id" $ company_id
      sqlSet "user_id" user_id
      sqlSet "document_id" document_id
      sqlSet "quantity" (1::Int32)

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
