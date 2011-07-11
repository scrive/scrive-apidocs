module Context (
      Context(..)
    ) where

import Control.Concurrent.MVar
import Data.Word
import Doc.DocState
import MinutesTime
import Misc
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

data Context = Context {
      ctxmaybeuser           :: Maybe User
    , ctxhostpart            :: String
    , ctxflashmessages       :: [FlashMessage]
    , ctxtime                :: MinutesTime
    , ctxnormalizeddocuments :: MVar (Map.Map FileID JpegPages)
    , ctxipnumber            :: Word32
    , ctxdocstore            :: FilePath
    , ctxs3action            :: AWS.S3Action
    , ctxgscmd               :: String
    , ctxproduction          :: Bool
    , ctxtemplates           :: KontrakcjaTemplates
    , ctxesenforcer          :: MVar ()
    , ctxtwconf              :: TW.TrustWeaverConf
    , ctxelegtransactions    :: [ELegTransaction]
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString
    , ctxxtoken              :: MagicHash
    , ctxcompany             :: Maybe Company
    , ctxservice             :: Maybe Service
    , ctxlocation            :: String
    , ctxadminaccounts       :: [Email]
}
