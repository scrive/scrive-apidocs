
module Doc.SealStatus
  ( SealStatus(..)
  , HasGuardtimeSignature(..)
  ) where

import Data.Function (on)
import Data.Int
import Data.Typeable (Typeable)
import Database.PostgreSQL.PQTypes
import GHC.Generics
import qualified Control.Exception.Lifted as E

data SealStatus
 -- | File's digital signature status has not been determined
 = UnknownSealStatus
 -- | The file lacks any form of digital signature
 | Missing
 -- | The file has a TrustWeaver signature
 | TrustWeaver
 -- | The file has a Guardtime signature
 | Guardtime {
    extended :: Bool -- ^ The signature has been extended
  , private  :: Bool -- ^ The signature was created using Scrive's own gateway
  }
  deriving (Eq, Show, Generic, Typeable)

class HasGuardtimeSignature a where
  hasGuardtimeSignature :: a -> Bool

-- How can 'SealStatus' "have" digital signature?
instance HasGuardtimeSignature SealStatus where
  hasGuardtimeSignature Guardtime{} = True
  hasGuardtimeSignature _           = False

instance Enum SealStatus where
  toEnum (-1) = UnknownSealStatus
  toEnum 0    = Missing
  toEnum 1    = TrustWeaver
  toEnum 2    = Guardtime { extended = False, private = False }
  toEnum 3    = Guardtime { extended = True, private = False }
  toEnum 4    = Guardtime { extended = False, private = True }
  toEnum 5    = Guardtime { extended = True, private = True }
  toEnum i    = unexpectedError $ "invalid value:" <+> (showt i)

  fromEnum UnknownSealStatus = -1
  fromEnum Missing           = 0
  fromEnum TrustWeaver       = 1
  fromEnum Guardtime { extended = False, private = False } = 2
  fromEnum Guardtime { extended = True, private = False } = 3
  fromEnum Guardtime { extended = False, private = True } = 4
  fromEnum Guardtime { extended = True, private = True } = 5

instance Ord SealStatus where
  compare = compare `on` fromEnum

instance PQFormat SealStatus where
  pqFormat = pqFormat @Int16

instance FromSQL SealStatus where
  type PQBase SealStatus = PQBase Int16
  fromSQL mbase = do
    n :: Int16 <- fromSQL mbase
    if n < -1 || n > 5
      then E.throwIO $ RangeError { reRange = [(-1, 5)], reValue = n }
      else return . toEnum . fromIntegral $ n

instance ToSQL SealStatus where
  type PQDest SealStatus = PQDest Int16
  toSQL ss = toSQL (fromIntegral (fromEnum ss) :: Int16)
