module DB.Types (
    MagicHash(..)
  , Binary(..)
  ) where

import Control.Applicative
import Control.Arrow
import Data.Convertible
import Data.Data
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

-- | Opaque 'Word64' type, used as authentication token.
newtype MagicHash = MagicHash { unMagicHash :: Word64 }
  deriving (Data, Eq, Ord, Random, Typeable)
$(newtypeDeriveConvertible ''MagicHash)

deriving instance Serialize MagicHash
instance Version MagicHash

instance Show MagicHash where
  showsPrec _prec (MagicHash x) = (++) (pad0 16 (showHex x ""))

instance Read MagicHash where
  readsPrec _ s = first MagicHash <$> readHex s

instance FromReqURI MagicHash where
  fromReqURI = readM

-- | Used for serializing binary data (as bytea type in postgres) for convenience
-- since HDBC serializes bytestrings as bytea properly, but it returns unescaped
-- strings in some fucked up format from columns of type bytea, so we need to use
-- encode(x, 'base64') while SELECTing x from db and then convert it from base64 to
-- normal string and we want to run that last conversion implicitly by fromSql.
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
