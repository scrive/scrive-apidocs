{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module KontraLink(KontraLink(..), sendRedirect ) where

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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Object.Json as Json
import MinutesTime
import Happstack.Server.HSP.HTML (webHSP)
import qualified HSX.XMLGenerator as HSX (XML)
import HSP
import Session (SessionId)
import Misc
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import Control.Monad.Trans(liftIO, MonadIO,lift)

seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>


data KontraLink
    = LinkAbout
    | LinkLogin
    | LinkLogout
    | LinkSignup
    | LinkSignupDone
    | LinkForgotPassword
    | LinkForgotPasswordDone
    | LinkIssue
    | LinkMain
    | LinkAccount
    | LinkAccountPassword
    | LinkLandpageSaved Document SignatoryLink
    | LinkSignDoc Document SignatoryLink
    | LinkIssueDoc DocumentID
    | LinkIssueDocPDF Document {- Which file? -}
    | LinkSubaccount
    | LinkRemind Document SignatoryLink
    | LinkCancel Document
    | LinkRestart DocumentID
    | LinkSigned DocumentID SignatoryLinkID 
    | LinkRejected DocumentID SignatoryLinkID 
    | LinkSignInvite DocumentID
    | LinkAcceptTOS
    | LinkAdminOnly
    | LinkAdminOnlyIndexDB
    | LinkStats
    | LinkPaymentsAdmin
    | LinkUserAdmin (Maybe UserID)
    | LinkPasswordChange SessionId MagicHash
    | LoopBack
    | BackToReferer
    
instance Show KontraLink where
    showsPrec _ LinkAbout = (++) "/about"
    showsPrec _ LinkLogin = (++) "/login"
    showsPrec _ LinkLogout = (++) "/logout"
    showsPrec _ LinkSignup = (++) "/signup"
    showsPrec _ LinkSignupDone = (++) "/signupdone"
    showsPrec _ LinkForgotPassword = (++) "/amnesia"
    showsPrec _ LinkForgotPasswordDone = (++) "/amnesiadone"
    showsPrec _ LinkIssue = (++) "/d"
    showsPrec _ LinkMain = (++) "/"
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ LinkAccount = (++) "/account"
    showsPrec _ LinkAccountPassword = (++) "/account/password"
    showsPrec _ LinkSubaccount = (++) "/account/subaccount"
    showsPrec _ (LinkLandpageSaved document signatorylink) = 
        (++) $ "/landpage/signedsave/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink)
    showsPrec _ (LinkIssueDoc documentid) = 
        (++) $ "/d/" ++ show documentid
    showsPrec _ (LinkIssueDocPDF document) = 
        (++) $ "/d/" ++ show (documentid document) ++ "/" ++ BS.toString (documenttitle document) ++ ".pdf"
    showsPrec _ (LinkSignDoc document signatorylink) = 
        (++) $ "/s/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++ 
                 "/" ++ show (signatorymagichash signatorylink)
    showsPrec _ (LinkRemind document signlink) = (++) $ "/resend/"++(show $ documentid document)++"/"++(show $ signatorylinkid signlink)   
    showsPrec _ (LinkCancel document) = (++) $ "/cancel/"++(show $ documentid document)
    showsPrec _ (LinkRestart documentid) = (++) $ "/restart/"++(show  documentid)
    showsPrec _ (LinkSigned documentid signatorylinkid) = (++) $ "/landpage/signed/" ++ show documentid ++ "/" ++ show signatorylinkid
    showsPrec _ (LinkRejected documentid signatorylinkid) = (++) $ "/landpage/rejected/" ++ show documentid ++ "/" ++ show signatorylinkid
    showsPrec _ (LinkSignInvite documentid) = (++) $ "/landpage/signinvite/" ++ show documentid ++ "/"
    showsPrec _ LinkAdminOnly = (++) $ "/adminonly/"
    showsPrec _ LinkAdminOnlyIndexDB = (++) $ "/adminonly/db"
    showsPrec _ LinkStats = (++) $ "/stats"
    showsPrec _ (LinkPaymentsAdmin ) = (++) $ "/adminonly/advpayments"
    showsPrec _ (LinkUserAdmin Nothing) = (++) $ "/adminonly/useradmin"
    showsPrec _ (LinkUserAdmin (Just userId)) = (++) $ "/adminonly/useradmin/"++show userId
    showsPrec _ (LinkPasswordChange sid mh) = (++) $ "/changepassword/"++show sid++"/"++show mh
    showsPrec _ LoopBack = (++) $ "/" -- this should be never used
    showsPrec _ BackToReferer = (++) $ "/" -- this should be never used
    
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

--sendRedirect :: KontraLink -> Kontra Response
sendRedirect LoopBack = do
                         ref <- fmap (fmap BS.toString) $ getHeaderM "referer"
                         let link = fromMaybe (show LinkMain) $ ref 
                         response <- webHSP (seeOtherXML link)
                         seeOther link response
sendRedirect BackToReferer    = do
                         ref <- getField "referer"
                         let link = fromMaybe (show LinkMain) $ ref
                         response <- webHSP (seeOtherXML link)
                         seeOther link response

sendRedirect LinkLogin = do
                         curr <- fmap rqUri askRq 
                         let link =  (show LinkLogin) ++"?referer=" ++ (URL.encode $ UTF.encode curr) 
                         response <- webHSP (seeOtherXML $ link)
                         seeOther link response

sendRedirect link = do  
  response <- webHSP (seeOtherXML $ show link)
  seeOther (show link) response