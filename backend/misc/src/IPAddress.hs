module IPAddress (
    IPAddress
  , unsafeIPAddress
  , noIP
  , formatIP
  , IPAddressWithMask
  , unsafeIPAddressWithMask
  , ipAddressIsInNetwork
  ) where

import Data.Binary as B
import Data.Bits
import Data.Char
import Data.Int
import Database.PostgreSQL.PQTypes
import Numeric

newtype IPAddress = IPAddress Word32
  deriving (Eq, Ord)

instance Binary IPAddress where
  put (IPAddress w32) = put w32
  get = IPAddress `fmap` B.get

-- IP addresses are currently cast to signed Int32 in DB
instance PQFormat IPAddress where
  pqFormat = pqFormat @Int32
instance FromSQL IPAddress where
  type PQBase IPAddress = PQBase Int32
  fromSQL mbase = do
    n :: Int32 <- fromSQL mbase
    return . IPAddress . fromIntegral $ n
instance ToSQL IPAddress where
  type PQDest IPAddress = PQDest Int32
  toSQL (IPAddress n) = toSQL (fromIntegral n :: Int32)

instance Show IPAddress where
  show (IPAddress n) = intercalate "." [
      show $ (n `shiftR` 0)  .&. 255
    , show $ (n `shiftR` 8)  .&. 255
    , show $ (n `shiftR` 16) .&. 255
    , show $ (n `shiftR` 24) .&. 255
    ]

instance Read IPAddress where
  readsPrec _ str = do
    (a, r1) <- readDec (dropWhile isSpace str)
    guard $ a >=0 && a <=255
    '.' : r2 <- return r1
    (b, r3) <- readDec r2
    guard $ b >=0 && b <=255
    '.' : r4 <- return r3
    (c, r5) <- readDec r4
    guard $ c >=0 && c <=255
    '.' : r6 <- return r5
    (d, r7) <- readDec r6
    guard $ d >=0 && d <=255
    return (unsafeIPAddress ((a `shiftL` 0) .|.
                             (b `shiftL` 8) .|.
                             (c `shiftL` 16) .|.
                             (d `shiftL` 24)), r7)

unsafeIPAddress :: Word32 -> IPAddress
unsafeIPAddress = IPAddress

noIP :: IPAddress
noIP = IPAddress 0

formatIP :: IPAddress -> String
formatIP ip = "(IP: " ++ show ip ++ ")"


data IPAddressWithMask = IPAddressWithMask IPAddress Word8
  deriving (Eq, Ord)

instance Show IPAddressWithMask where
  show (IPAddressWithMask addr 32) = show addr
  show (IPAddressWithMask addr mask) = show addr ++ "/" ++ show mask

instance Read IPAddressWithMask where
  readsPrec _ str = do
    (addr, r1) <- reads str
    case r1 of
      '/' : r2 -> do
              (mask, r3) <- readDec r2
              guard $ mask >=0 && mask <= 32
              return (IPAddressWithMask addr mask, r3)
      r2 -> return (IPAddressWithMask addr 32, r2)

unsafeIPAddressWithMask :: IPAddress -> Word8 -> IPAddressWithMask
unsafeIPAddressWithMask = IPAddressWithMask

ipAddressIsInNetwork :: IPAddress -> IPAddressWithMask -> Bool
ipAddressIsInNetwork (IPAddress ipaddr) (IPAddressWithMask (IPAddress netaddr) bits) =
  (addr_le .&. mask) == (subnet_le .&. mask)
  where
    a_addr = (ipaddr `shiftR` 0)  .&. 255
    b_addr = (ipaddr `shiftR` 8)  .&. 255
    c_addr = (ipaddr `shiftR` 16) .&. 255
    d_addr = (ipaddr `shiftR` 24) .&. 255
    addr_le = (d_addr `shiftL` 0) .|.
              (c_addr `shiftL` 8) .|.
              (b_addr `shiftL` 16) .|.
              (a_addr `shiftL` 24)
    a_subnet = (netaddr `shiftR` 0)  .&. 255
    b_subnet = (netaddr `shiftR` 8)  .&. 255
    c_subnet = (netaddr `shiftR` 16) .&. 255
    d_subnet = (netaddr `shiftR` 24) .&. 255
    subnet_le = (d_subnet `shiftL` 0) .|.
              (c_subnet `shiftL` 8) .|.
              (b_subnet `shiftL` 16) .|.
              (a_subnet `shiftL` 24)
    mask = 0xFFFFFFFF `shiftL` (32 - fromIntegral bits)
