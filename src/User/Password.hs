{-# OPTIONS_GHC -Wall #-}
module User.Password
    ( Password(..)
    , createPassword
    , verifyPassword

) where
import Happstack.Data
import Happstack.State
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify,MonadState(..))
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (unlines) 
import Happstack.Data.IxSet as IxSet
import Data.Maybe(isJust,fromJust,maybe)
import Misc
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Codec.Utils (Octet)
import Data.Digest.SHA256 (hash)
import System.Random
import Data.List
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Control.Applicative
import MinutesTime
import qualified Payments.PaymentsState as Payments
import Data.Data

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


