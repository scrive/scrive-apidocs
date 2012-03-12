module IPAddress where

import Data.Bits
import Data.Convertible
import Data.Int
import Data.List
import Data.Word
import Database.HDBC

newtype IPAddress = IPAddress Word32
  deriving (Eq, Ord)

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

unknownIPAddress :: IPAddress
unknownIPAddress = IPAddress 0

formatIP :: IPAddress -> String
formatIP ip = "(IP: " ++ show ip ++ ")"
