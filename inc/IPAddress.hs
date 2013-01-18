module IPAddress (
    IPAddress
  , unsafeIPAddress
  , noIP
  , formatIP
  ) where

import Data.Bits
import Data.Convertible
import Data.Int
import Data.List
import Data.Word
import Database.HDBC
import Data.Binary

newtype IPAddress = IPAddress Word32
  deriving (Eq, Ord)

instance Binary IPAddress where
  put (IPAddress w32) = put w32
  get = IPAddress `fmap` get

-- IP addresses are currently cast to signed Int32 in DB
instance Convertible IPAddress SqlValue where
  safeConvert = safeConvert . (\(IPAddress a) -> fromIntegral a :: Int32)

instance Convertible SqlValue IPAddress where
  safeConvert = fmap (IPAddress . (fromIntegral :: Int32 -> Word32)) . safeConvert

instance Show IPAddress where
  show (IPAddress n) = intercalate "." [
      show $ (n `shiftR` 0)  .&. 255
    , show $ (n `shiftR` 8)  .&. 255
    , show $ (n `shiftR` 16) .&. 255
    , show $ (n `shiftR` 24) .&. 255
    ]

unsafeIPAddress :: Word32 -> IPAddress
unsafeIPAddress = IPAddress

noIP :: IPAddress
noIP = IPAddress 0

formatIP :: IPAddress -> String
formatIP ip = "(IP: " ++ show ip ++ ")"
