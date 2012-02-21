{-# LANGUAGE StandaloneDeriving #-}

-- | Support for cryptographically secure hashes
module MagicHash
  ( MagicHash
  , unsafeMagicHash
  , unMagicHash
  ) where


import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.Word (Word64)
import Happstack.Data(Version, Serialize)
import Happstack.Server (FromReqURI(..))
import Happstack.Util.Common (readM)
import Numeric (showHex, readHex)

import Crypto.RNG(Random)
import DB.Derive(newtypeDeriveConvertible)
import Misc (pad0)

-- | Hash value that is produced by cryptographically secure random
-- generators for authentication purposes.
--
-- Underlying type (Int64) needs to be signed because pgsql doesn't
-- support unsigned ones and protests if we feed it with unsigned
-- value which can be properly represented in binary form as signed
-- one, but that doesn't matter for application as we just cast it to
-- unsigned one in Show instance and readHex works fine with negative
-- numbers.
newtype MagicHash = MagicHash Int64
  deriving (Data, Eq, Ord, Typeable, Random)
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

-- | Construct a magic hash manually.  It is up to the caller to
-- reason about the consequences if the argument is not generated from
-- a secure random source.
unsafeMagicHash :: Int64 -> MagicHash
unsafeMagicHash = MagicHash

-- | Deconstruct a magic hash manually.  It is up to the caller to
-- reason about the consequences of using the internal number.
unMagicHash :: MagicHash -> Int64
unMagicHash (MagicHash value) = value
