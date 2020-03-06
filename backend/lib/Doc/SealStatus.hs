
module Doc.SealStatus
  ( SealStatus(..)
  , isSealed
  , isGuardtime
  ) where

import Data.Function (on)
import Data.Int
import Data.Typeable (Typeable)
import Database.PostgreSQL.PQTypes
import GHC.Generics
import qualified Control.Exception.Lifted as E

data SealStatus
  -- | File's digital signature status has not been determined
  -- State: unused
  = UnknownSealStatus
  -- | File lacks any form of digital signature
  | Missing
  -- | File has a TrustWeaver signature
  -- State: obsolete
  | TrustWeaver
  -- | File has a Guardtime signature
  | Guardtime {
      extended :: Bool -- ^ The signature has been extended
    , private  :: Bool -- ^ The signature was created using Scrive's own gateway
  }
  -- | File has a Pades signature
  | Pades
  deriving (Eq, Show, Generic, Typeable)

isSealed :: SealStatus -> Bool
isSealed s = isGuardtime s || s == Pades

isGuardtime :: SealStatus -> Bool
isGuardtime = \case
  Guardtime{} -> True
  _           -> False

instance Enum SealStatus where
  toEnum (-1) = UnknownSealStatus
  toEnum 0    = Missing
  toEnum 1    = TrustWeaver
  toEnum 2    = Guardtime { extended = False, private = False }
  toEnum 3    = Guardtime { extended = True, private = False }
  toEnum 4    = Guardtime { extended = False, private = True }
  toEnum 5    = Guardtime { extended = True, private = True }
  toEnum 6    = Pades
  toEnum i    = unexpectedError $ "invalid value:" <+> (showt i)

  fromEnum UnknownSealStatus = -1
  fromEnum Missing           = 0
  fromEnum TrustWeaver       = 1
  fromEnum Guardtime { extended = False, private = False } = 2
  fromEnum Guardtime { extended = True, private = False } = 3
  fromEnum Guardtime { extended = False, private = True } = 4
  fromEnum Guardtime { extended = True, private = True } = 5
  fromEnum Pades             = 6

instance Ord SealStatus where
  compare = compare `on` fromEnum

instance PQFormat SealStatus where
  pqFormat = pqFormat @Int16

instance FromSQL SealStatus where
  type PQBase SealStatus = PQBase Int16
  fromSQL mbase = do
    n :: Int16 <- fromSQL mbase
    if n < -1 || n > 6
      then E.throwIO $ RangeError { reRange = [(-1, 6)], reValue = n }
      else return . toEnum . fromIntegral $ n

instance ToSQL SealStatus where
  type PQDest SealStatus = PQDest Int16
  toSQL ss = toSQL (fromIntegral (fromEnum ss) :: Int16)
