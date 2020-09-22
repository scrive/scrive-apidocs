module DigitalSignatureMethod (
    DigitalSignatureMethod(..)
  , digitalSignatureMethodText
  , digitalSignatureMethodFromText
  ) where

import Control.Monad.Catch
import Data.Int (Int16)
import Database.PostgreSQL.PQTypes

-- | DigitalSignatureMethod describes, which digital signature method is used to
-- digitally sign a document. 'Signature' might be a bit of a misnomer for
-- GuardTime, since that is really just a timestamp.
data DigitalSignatureMethod
    -- | The default method uses keyless signature infrastructure provided by Guardtime.
    = Guardtime
    -- | Pades method uses PKI-based signing.
    | Pades
  deriving (Eq, Ord, Show)

instance PQFormat DigitalSignatureMethod where
  pqFormat = pqFormat @Int16

instance FromSQL DigitalSignatureMethod where
  type PQBase DigitalSignatureMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Guardtime
      2 -> return Pades
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL DigitalSignatureMethod where
  type PQDest DigitalSignatureMethod = PQDest Int16
  toSQL Guardtime = toSQL (1 :: Int16)
  toSQL Pades     = toSQL (2 :: Int16)

digitalSignatureMethodText :: DigitalSignatureMethod -> Text
digitalSignatureMethodText Guardtime = "guardtime"
digitalSignatureMethodText Pades     = "pades"

digitalSignatureMethodFromText :: Text -> Maybe DigitalSignatureMethod
digitalSignatureMethodFromText s =
  find (\p -> s == digitalSignatureMethodText p) [Guardtime, Pades]
