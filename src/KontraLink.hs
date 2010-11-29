{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module KontraLink(KontraLink(..), hpost0, hpost1, hpost2, hpost3, hpost4, sendRedirect ) where

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


hpost0 action = methodM POST >> do
                  (link :: KontraLink) <- action
                  sendRedirect link

hpost1 action = path $ \a1 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1
                  sendRedirect link

hpost2 action = path $ \a1 -> path $ \a2 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1 a2
                  sendRedirect link

hpost3 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1 a2 a3
                  sendRedirect link

hpost4 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> path $ \a4 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1 a2 a3 a4
                  sendRedirect link

--sendRedirect :: KontraLink -> Kontra Response
sendRedirect link = do  
  response <- webHSP (seeOtherXML $ show link)
  seeOther (show link) response