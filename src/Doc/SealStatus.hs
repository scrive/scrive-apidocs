module Doc.SealStatus
  ( SealStatus(..)
  ) where

import Database.HDBC (SqlValue)
import Data.Convertible (Convertible(..), convError)
import Data.Function (on)
import Data.Typeable (Typeable)

data SealStatus =
   Missing             -- ^ The document lacks any form of digital signature

 | TrustWeaver         -- ^ The document has a TrustWeaver signature

 | Guardtime           -- ^ The document has a Guardtime signature
    { extended :: Bool -- ^ The signature has been extended
    , private  :: Bool -- ^ The signature was created using Scrive's own gateway
    }
  deriving (Eq, Show, Typeable)

instance Enum SealStatus where
  toEnum 0 = Missing
  toEnum 1 = TrustWeaver
  toEnum 2 = Guardtime{ extended = False, private = False }
  toEnum 3 = Guardtime{ extended = True,  private = False }
  toEnum 4 = Guardtime{ extended = False, private = True }
  toEnum 5 = Guardtime{ extended = True,  private = True }
  toEnum i = error $ "SealStatus.toEnum: " ++ show i

  fromEnum Missing                                        = 0
  fromEnum TrustWeaver                                    = 1
  fromEnum Guardtime{ extended = False, private = False } = 2
  fromEnum Guardtime{ extended = True,  private = False } = 3
  fromEnum Guardtime{ extended = False, private = True }  = 4
  fromEnum Guardtime{ extended = True,  private = True }  = 5

instance Ord SealStatus where
  compare = compare `on` fromEnum

instance Convertible SqlValue SealStatus where
  safeConvert s = do
    i :: Int <- safeConvert s
    if i < 0 || i > 5 then convError "Doc.SealStatus" i
                      else return (toEnum i)

instance Convertible SealStatus SqlValue where
  safeConvert = safeConvert . fromEnum
