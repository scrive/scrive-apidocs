{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module KontraLink where

import UserState
import DocState
import Session
import Happstack.Server
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX
import HSP
import qualified Data.ByteString.UTF8 as BS
import Control.Monad
import Data.Maybe
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Object.Json as Json
import System.Log.Logger
import Control.Monad.State
import MinutesTime

data KontraLink
    = LinkAbout
    | LinkLogin
    | LinkIssue
    | LinkMain
    | LinkAccount
    | LinkLandpageSaved Document SignatoryLinkID
    | LinkSignDoc Document SignatoryLinkID
    | LinkIssueDoc Document
    | LinkIssueDocPDF Document {- Which file? -}
    | LinkSubaccount

instance Show KontraLink where
    showsPrec _ LinkAbout = (++) "/about"
    showsPrec _ LinkLogin = (++) "/login"
    showsPrec _ LinkIssue = (++) "/issue"
    showsPrec _ LinkMain = (++) "/"
    showsPrec _ LinkAccount = (++) "/account"
    showsPrec _ LinkSubaccount = (++) "/account/subaccount"
    showsPrec _ (LinkLandpageSaved document signatorylinkid) = 
        (++) $ "/landpage/saved/" ++ show (documentid document) ++ "/" ++ show signatorylinkid
    showsPrec _ (LinkIssueDoc document) = 
        (++) $ "/issue/" ++ show (documentid document)
    showsPrec _ (LinkIssueDocPDF document) = 
        (++) $ "/issue/" ++ show (documentid document) ++ "/" ++ BS.toString (documenttitle document) ++ ".pdf"
    showsPrec _ (LinkSignDoc document signatorylinkid) = 
        (++) $ "/sign/" ++ show (documentid document) ++ "/" ++ show signatorylinkid

{-
instance (EmbedAsAttr m String) => (EmbedAsAttr m KontraLink) where
    asAttr = asAttr . show

instance (HSX.XMLGen m,EmbedAsAttr m String) => (EmbedAsAttr m (Attr String KontraLink)) where
    asAttr = asAttr . show
-}

instance (EmbedAsChild m String) => (EmbedAsChild m KontraLink) where
    asChild = asChild . show

instance Monad m => IsAttrValue m KontraLink where
    toAttrValue = toAttrValue . show

