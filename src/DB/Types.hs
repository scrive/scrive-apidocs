module DB.Types (
    MagicHash(..)
  , Binary(..)
  ) where

import Control.Applicative
import Control.Arrow
import Data.Convertible
import Data.Data
import Data.Int
import Data.Word
import Database.HDBC
import Happstack.Server
import Happstack.State
import Happstack.Util.Common
import Numeric
import System.Random
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

import DB.Derive
import Misc

-- | Opaque 'Int64' type, used as authentication token. Underlying type needs
-- to be signed because pgsql doesn't support unsigned ones and protests if we
-- feed it with unsigned value which can be properly represented in binary form
-- as signed one, but that doesn't matter for application as we just cast it to
-- unsigned one in Show instance and readHex works fine with negative numbers.
newtype MagicHash = MagicHash Int64
  deriving (Data, Eq, Ord, Random, Typeable)
$(newtypeDeriveConvertible ''MagicHash)

deriving instance Serialize MagicHash
instance Version MagicHash

instance Show MagicHash where
  showsPrec _prec (MagicHash x) =
    (++) (pad0 16 $ showHex (fromIntegral x :: Word64) "")

instance Read MagicHash where
  readsPrec _ s = first MagicHash <$> readHex s

instance FromReqURI MagicHash where
  fromReqURI = readM

-- | Used for serializing binary data (as bytea type in postgres) for convenience
-- since normally pgsql expects/returns bytea in some fucked up format, we need to
-- use encode(x, 'base64') while SELECTing to/decode(x, 'base64') while INSERTing
-- from db and then convert it to/from base64 implicitly using fromSql/toSql.
newtype Binary = Binary { unBinary :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''Binary)

instance Convertible Binary SqlValue where
  safeConvert = safeConvert . B64.encode . unBinary

instance Convertible SqlValue Binary where
  safeConvert v = case safeConvert v of
    Right s -> case B64.decode s of
      Right ds -> Right $ Binary ds
      Left e   -> Left $ ConvertError {
          convSourceValue = show s
        , convSourceType = "ByteString"
        , convDestType = "Binary"
        , convErrorMessage = "Couldn't convert from base64: " ++ e
      }
    Left e -> Left e
