
module Doc.DigitalSignatureStatus
  ( DigitalSignatureStatus(..)
  , isDigitallySigned
  , isGuardtime
  ) where

import Data.Function (on)
import Data.Int
import Data.Typeable (Typeable)
import Database.PostgreSQL.PQTypes
import GHC.Generics
import qualified Control.Exception.Lifted as E

data DigitalSignatureStatus
  -- | File's digital signature status has not been determined
  -- State: unused
  = UnknownDigitalSignatureStatus
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

isDigitallySigned :: DigitalSignatureStatus -> Bool
isDigitallySigned s = isGuardtime s || s == Pades

isGuardtime :: DigitalSignatureStatus -> Bool
isGuardtime = \case
  Guardtime{} -> True
  _           -> False

instance Enum DigitalSignatureStatus where
  toEnum (-1) = UnknownDigitalSignatureStatus
  toEnum 0    = Missing
  toEnum 1    = TrustWeaver
  toEnum 2    = Guardtime { extended = False, private = False }
  toEnum 3    = Guardtime { extended = True, private = False }
  toEnum 4    = Guardtime { extended = False, private = True }
  toEnum 5    = Guardtime { extended = True, private = True }
  toEnum 6    = Pades
  toEnum i    = unexpectedError $ "invalid value:" <+> showt i

  fromEnum UnknownDigitalSignatureStatus = -1
  fromEnum Missing     = 0
  fromEnum TrustWeaver = 1
  fromEnum Guardtime { extended = False, private = False } = 2
  fromEnum Guardtime { extended = True, private = False } = 3
  fromEnum Guardtime { extended = False, private = True } = 4
  fromEnum Guardtime { extended = True, private = True } = 5
  fromEnum Pades       = 6

instance Ord DigitalSignatureStatus where
  compare = compare `on` fromEnum

instance PQFormat DigitalSignatureStatus where
  pqFormat = pqFormat @Int16

instance FromSQL DigitalSignatureStatus where
  type PQBase DigitalSignatureStatus = PQBase Int16
  fromSQL mbase = do
    n :: Int16 <- fromSQL mbase
    if n < -1 || n > 6
      then E.throwIO $ RangeError { reRange = [(-1, 6)], reValue = n }
      else return . toEnum . fromIntegral $ n

instance ToSQL DigitalSignatureStatus where
  type PQDest DigitalSignatureStatus = PQDest Int16
  toSQL ss = toSQL (fromIntegral (fromEnum ss) :: Int16)
