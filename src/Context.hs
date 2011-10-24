module Context (
      Context(..)
    ) where

import Control.Concurrent.MVar
import Data.Word
import Database.HDBC.PostgreSQL
import File.FileID
import Doc.JpegPages
import MinutesTime
import User.Model
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import Templates.Templates
import qualified TrustWeaver as TW
import ELegitimation.ELeg
import qualified MemCache
import FlashMessage
import API.Service.Model
import Company.Model
import DB.Types

data Context = Context {
      ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxhostpart            :: String -- ^ The hostname of the URL for the request.
    , ctxflashmessages       :: [FlashMessage] -- ^ The flash messages for the NEXT request.
    , ctxtime                :: MinutesTime -- ^ The time of the request.
    , ctxnormalizeddocuments :: MVar (Map.Map FileID JpegPages) -- ^
    , ctxipnumber            :: Word32 -- ^ The ip number of the client.
    , ctxdbconn              :: Connection -- ^ PostgreSQL database connection
    , ctxdbconnclose         :: Bool -- ^ Indicates whether we want to close connection explicitly or let it be closed by GC
    , ctxdbconnstring        :: String -- ^ sometimes we need to make connection to postgress again
    , ctxdocstore            :: FilePath -- ^ The temporary document directory.
    , ctxs3action            :: AWS.S3Action -- ^
    , ctxgscmd               :: String -- ^
    , ctxproduction          :: Bool -- ^ Is this server the production server?
    , ctxbackdooropen        :: Bool -- ^ Whether the testing backdoor is open?
    , ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text
    , ctxtemplatesforlocale  :: Locale -> KontrakcjaTemplates -- ^ Templates by locale.
    , ctxesenforcer          :: MVar () -- ^
    , ctxtwconf              :: TW.TrustWeaverConf -- ^ TrustWeaver configuration
    , ctxelegtransactions    :: [ELegTransaction] -- ^ Transactions for connections to the Logica server
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxcompany             :: Maybe Company -- ^
    , ctxservice             :: Maybe Service -- ^
    , ctxlocation            :: String -- ^
    , ctxadminaccounts       :: [Email] -- ^
    , ctxuserlocale          :: Locale
    , ctxdoclocale           :: Maybe Locale
}

{- |
    If there is a doc locale then we want to use it, but otherwise we
    use the user's locale
-}
instance HasLocale Context where
  getLocale Context{ctxuserlocale} = ctxuserlocale
