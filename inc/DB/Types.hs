module DB.Types (
    Binary(..)
  ) where

import Data.Convertible
import Database.HDBC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

import DB.Derive

-- | Used for serializing binary data (as bytea type in postgres) for convenience
-- since normally pgsql expects/returns bytea in some fucked up format, we need to
-- use encode(x, 'base64') while SELECTing to/decode(x, 'base64') while INSERTing
-- from db and then convert it to/from base64 implicitly using fromSql/toSql.
newtype Binary = Binary { unBinary :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''Binary)

instance Convertible Binary SqlValue where
  safeConvert = safeConvert . B64.encode . unBinary

-- decode is too strict for PostgreSQL taste, so we use decodeLenient instead
instance Convertible SqlValue Binary where
  safeConvert = either Left (Right . Binary . B64.decodeLenient) . safeConvert
