-- | Support for cryptographically secure hashes
module Auth.MagicHash
  ( MagicHash
  , unsafeMagicHash
  , unMagicHash
  , showOnlyLastFourCharacters
  ) where

import Control.Arrow
import Crypto.RNG
import Data.Int
import Data.Unjson
import Data.Word
import Database.PostgreSQL.PQTypes
import Happstack.Server
import Numeric
import qualified Data.Text as T

import Auth.Utils

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
  deriving (Eq, Ord)

instance PQFormat MagicHash where
  pqFormat = pqFormat @Int64

instance Random MagicHash where
  random = MagicHash <$> random

instance Show MagicHash where
  show (MagicHash x) = pad0 16 $ showHex (fromIntegral x :: Word64) ""

instance Read MagicHash where
  readsPrec _ s = first MagicHash <$> readHex s

instance FromReqURI MagicHash where
  fromReqURI = maybeRead . T.pack

instance Unjson MagicHash where
  unjsonDef = unjsonInvmapR
    (maybe (fail "Can't parse access token") return . maybeRead . T.pack)
    show
    unjsonDef

instance FromSQL MagicHash where
  type PQBase MagicHash = PQBase Int64
  fromSQL mbase = MagicHash <$> fromSQL mbase

instance ToSQL MagicHash where
  type PQDest MagicHash = PQDest Int64
  toSQL (MagicHash n) = toSQL n

-- | Construct a magic hash manually.  It is up to the caller to
-- reason about the consequences if the argument is not generated from
-- a secure random source.
unsafeMagicHash :: Int64 -> MagicHash
unsafeMagicHash = MagicHash

-- | Deconstruct a magic hash manually.  It is up to the caller to
-- reason about the consequences of using the internal number.
unMagicHash :: MagicHash -> Int64
unMagicHash (MagicHash mh) = mh

showOnlyLastFourCharacters :: MagicHash -> String
showOnlyLastFourCharacters mh =
  let asString = show mh
      len      = length asString
  in  zipWith (\i c -> if i <= 4 then c else '*') [len, (len - 1) .. 1] asString
