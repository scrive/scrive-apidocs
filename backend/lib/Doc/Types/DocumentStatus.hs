module Doc.Types.DocumentStatus (
    DocumentStatus(..)
  ) where

import Control.Monad.Catch
import Data.Int

import DB

{-
   Document start in Preparation state.

   Meaning:
   * Preparation: Only author can see it. He's still editing.
   * Pending: People can sign document. Could be timed out.
   * Closed: Everybody signed. This is final state.
   * Canceled: Author has canceled the document.
   * Timedout: This works as autocancel and has exactly same
     properties.

   Transitions:
   * Preparation to Pending: When invitations are sent.
   * Preparation to Cancel: mail about cancel to
     all who have signed it already is sent.
     TODO: Should other parties get an email?
   * Preparation to Timedout: mail about timeout to
     all who have signed it already is sent.
   * Pending to Closed: When everyone has signed.
     Info about closed deal is sent to everybody involved.
   * Pending to Cancel: Send no emails.
   * Pending to Timeout: TODO: No action?

   Allowed actions:
   * Preparation: change document, change title, add/rem signatories
   * Pending: change email of a signatory, signatory can sign
   * Closed: nothing
   * Canceled: edit back to Preparation
   * Timedout: edit back to Preparation
 -}

data DocumentStatus
  = Preparation
  | Pending
  | Closed
  | Canceled
  | Timedout
  | Rejected
  | DocumentError
    deriving (Eq, Ord)

instance PQFormat DocumentStatus where
  pqFormat = pqFormat @Int16

instance FromSQL DocumentStatus where
  type PQBase DocumentStatus = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Preparation
      2 -> return Pending
      3 -> return Closed
      4 -> return Canceled
      5 -> return Timedout
      6 -> return Rejected
      7 -> return DocumentError
      _ -> throwM RangeError { reRange = [(1, 7)], reValue = n }

instance ToSQL DocumentStatus where
  type PQDest DocumentStatus = PQDest Int16
  toSQL Preparation   = toSQL (1 :: Int16)
  toSQL Pending       = toSQL (2 :: Int16)
  toSQL Closed        = toSQL (3 :: Int16)
  toSQL Canceled      = toSQL (4 :: Int16)
  toSQL Timedout      = toSQL (5 :: Int16)
  toSQL Rejected      = toSQL (6 :: Int16)
  toSQL DocumentError = toSQL (7 :: Int16)

-- | Used by API (FIXME: it shouldn't be).
instance Show DocumentStatus where
  show Preparation   = "Preparation"
  show Pending       = "Pending"
  show Closed        = "Closed"
  show Canceled      = "Canceled"
  show Timedout      = "Timedout"
  show Rejected      = "Rejected"
  show DocumentError = "DocumentError"
