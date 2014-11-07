module Chargeable.Model (
    ChargeCompanyForSMS(..)
  , ChargeCompanyForElegSignature(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Typeable

import DB
import Doc.DocumentID
import User.UserID

data ChargeableItem = SMS | ELegSignature
  deriving (Eq, Ord, Show, Typeable)

instance PQFormat ChargeableItem where
  pqFormat _ = pqFormat (undefined::Int16)

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
instance (MonadDB m, MonadThrow m) => DBUpdate m ChargeCompanyForSMS () where
  update (ChargeCompanyForSMS document_id sms_count) = do
    user_id <- getAuthorID document_id
    runQuery_ . sqlInsert "chargeable_items" $ do
      sqlSetCmd "time" "now()"
      sqlSetCmd "company_id" $ userCompanyID user_id
      sqlSet "type" SMS
      sqlSet "user_id" user_id
      sqlSet "document_id" document_id
      sqlSet "quantity" sms_count

-- | Charge company of the author of the document for e-leg signature.
data ChargeCompanyForElegSignature = ChargeCompanyForElegSignature DocumentID
instance (MonadDB m, MonadThrow m) => DBUpdate m ChargeCompanyForElegSignature () where
  update (ChargeCompanyForElegSignature document_id) = do
    user_id <- getAuthorID document_id
    runQuery_ . sqlInsert "chargeable_items" $ do
      sqlSetCmd "time" "now()"
      sqlSetCmd "company_id" $ userCompanyID user_id
      sqlSet "type" ELegSignature
      sqlSet "user_id" user_id
      sqlSet "document_id" document_id
      sqlSet "quantity" (1::Int32)

----------------------------------------

-- | Fetch id of the author of the document.
getAuthorID :: (MonadDB m, MonadThrow m) => DocumentID -> m UserID
getAuthorID document_id = do
  runQuery_ . sqlSelect "signatory_links" $ do
    sqlResult "user_id"
    sqlWhereEq "document_id" document_id
    sqlWhere "is_author"
  fetchOne unSingle

-- | SQL for querying id of the company the user belongs to.
userCompanyID :: UserID -> SQL
userCompanyID user_id = parenthesize . toSQLCommand . sqlSelect "users" $ do
  sqlResult "company_id"
  sqlWhereEq "id" user_id
