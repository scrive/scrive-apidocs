{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB.Types (
    Binary(..)
  ) where

import Control.Monad (liftM)
import Data.Convertible
import Data.Int
import Data.Word
import Database.HDBC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

import DB.Derive
import Misc

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

-- IP addresses are currently cast to signed Int32 in DB
instance Convertible IPAddress SqlValue where
  safeConvert = safeConvert . (\(IPAddress a) -> fromIntegral a :: Int32)

instance Convertible SqlValue IPAddress where
  safeConvert = liftM (IPAddress . (fromIntegral :: Int32 -> Word32)) . safeConvert

