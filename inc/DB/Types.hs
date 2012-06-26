module DB.Types (
    Binary(..)
  ) where

import Data.Convertible
import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as B16

import DB.Derive

-- | Used for serializing binary data (as bytea type in postgres) for convenience
newtype Binary = Binary { unBinary :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''Binary)

instance Convertible Binary SqlValue where
  safeConvert = safeConvert . BS.append (BS.pack "\\x") . B16.encode . unBinary

instance Convertible SqlValue Binary where
  safeConvert v = case safeConvert v of
    Left err -> Left err
    Right s
      | BS.take 2 s == BS.pack "\\x" -> case B16.decode $ BS.drop 2 s of
        result@(b16decoded, rest)
          | rest == BS.empty -> Right $ Binary b16decoded
          | otherwise -> Left err { convErrorMessage = "Conversion from base16 encoded string failed: result is " ++ show result }
      | otherwise -> Left err { convErrorMessage = "Two first bytes are incorrect (should be '\\x')" }
      where
        err = ConvertError {
            convSourceValue = show s
          , convSourceType = "ByteString"
          , convDestType = "Binary"
          , convErrorMessage = ""
        }
