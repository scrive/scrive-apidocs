{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module KontraLink(KontraLink(..), sendRedirect ) where

import User.UserState
import Doc.DocState
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

{- |
   All the links available for responses
-}
data KontraLink
    = LinkAbout
    | LinkLogin
    | LinkLogout
    | LinkSignup
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
    | LinkUnloggedUserAction SessionId MagicHash String String -- email / username
    | LinkChangeSignatoryEmail DocumentID SignatoryLinkID 
    | LinkWithdrawn DocumentID 
    | LoopBack
    | BackToReferer
    | LinkDaveDocument DocumentID
    | LinkFile FileID BS.ByteString
    | LinkRequestAccount
    | LinkAskQuestion
    | LinkInvite
   
{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
    showsPrec _ LinkAbout = (++) "/about"
    showsPrec _ LinkLogin = (++) "/login"
    showsPrec _ LinkLogout = (++) "/logout"
    showsPrec _ LinkSignup = (++) "/signup"
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
    showsPrec _ (LinkFile fileid filename) = 
        (++) $ "/df/" ++ show fileid ++ "/" ++ BS.toString filename
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
    showsPrec _ (LinkUnloggedUserAction sid mh email username) = (++) $ "/accountsetup/"++show sid++"/"++show mh ++ 
                                                                                       "?" ++ "email=" ++ email ++
                                                                                       "&" ++ "name="++ username
    showsPrec _ (LinkChangeSignatoryEmail did slid ) = (++) $ "/changeemail/"++show did++"/"++show slid
    showsPrec _ (LinkWithdrawn did ) = (++) $ "/withdrawn/"++show did
    showsPrec _ LoopBack = (++) $ "/" -- this should never be used
    showsPrec _ BackToReferer = (++) $ "/" -- this should never be used
    showsPrec _ (LinkDaveDocument docid) = (++) ("/dave/document/" ++ show docid)
    showsPrec _ (LinkAskQuestion) = (++) ("/question")
    showsPrec _ (LinkRequestAccount) = (++) "/requestaccount"
    showsPrec _ (LinkInvite) = (++) "/invite"
    
-- type class instances used for xml'ing the KontraLinks

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

{-|
   Redirects to the url relevant to the KontraLink.
-}
--sendRedirect :: KontraLink -> Kontra Response
sendRedirect LoopBack = do
                         ref <- fmap (fmap BS.toString) $ getHeaderM "referer"
                         let link = fromMaybe (show LinkMain) $ ref 
                         response <- webHSP (seeOtherXML link)
                         seeOther link response
sendRedirect BackToReferer    = do
                         ref <- getField "referer"
                         let link' = fromMaybe (show LinkMain) $ ref
                         let link = if (null link') then (show LinkMain) else link'
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
