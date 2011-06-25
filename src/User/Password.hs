module User.Password
    ( Password(..)
    , createPassword
    , verifyPassword

) where
import Codec.Utils (Octet)
import Data.Data
import Data.Digest.SHA256 (hash)
import Happstack.Data
import System.Random
import qualified Data.ByteString as BS

data Password = Password [Octet] [Octet] | NoPassword
    deriving (Eq, Ord, Typeable)

deriving instance Show Password
instance Version Password

createPassword :: BS.ByteString -> IO Password
createPassword password = do
  salt <- makeSalt
  return $ Password salt (hashPassword password salt)
  
randomOctets :: Int -> IO [Octet]
randomOctets n = do
  randomGen <- newStdGen
  return $ take n $ map fromIntegral (randoms randomGen :: [Int])

makeSalt :: IO [Octet]
makeSalt = randomOctets 10

hashPassword :: BS.ByteString -> [Octet] -> [Octet]
hashPassword password salt =
  hash (salt ++ (BS.unpack password))

verifyPassword :: Password -> BS.ByteString -> Bool
verifyPassword (Password salt hash1) password = hash1 == (hashPassword password salt)
verifyPassword _ _ = False        

$(deriveSerializeFor [ ''Password  ])


