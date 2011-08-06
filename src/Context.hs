module Context (
      Context(..)
    ) where

import Control.Concurrent.MVar
import Data.Word
import Database.HDBC.PostgreSQL
import Doc.DocState
import MinutesTime
import User.UserState
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import Templates.Templates
import qualified TrustWeaver as TW
import ELegitimation.ELeg
import qualified MemCache
import FlashMessage
import API.Service.ServiceState
import Company.CompanyState
import DB.Types

data Context = Context {
      ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxhostpart            :: String -- ^ The hostname of the URL for the request.
    , ctxflashmessages       :: [FlashMessage] -- ^ The flash messages for the NEXT request.
    , ctxtime                :: MinutesTime -- ^ The time of the request.
    , ctxnormalizeddocuments :: MVar (Map.Map FileID JpegPages) -- ^ 
    , ctxipnumber            :: Word32 -- ^ The ip number of the client.
    , ctxdbconn              :: Connection -- ^ PostgreSQL database connection
    , ctxdocstore            :: FilePath -- ^ The temporary document directory.
    , ctxs3action            :: AWS.S3Action -- ^ 
    , ctxgscmd               :: String -- ^ 
    , ctxproduction          :: Bool -- ^ Is this server the production server?
    , ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text
    , ctxesenforcer          :: MVar () -- ^ 
    , ctxtwconf              :: TW.TrustWeaverConf -- ^ TrustWeaver configuration
    , ctxelegtransactions    :: [ELegTransaction] -- ^ Transactions for connections to the Logica server
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxcompany             :: Maybe Company -- ^ 
    , ctxservice             :: Maybe Service -- ^
    , ctxlocation            :: String -- ^ 
    , ctxadminaccounts       :: [Email] -- ^
}
