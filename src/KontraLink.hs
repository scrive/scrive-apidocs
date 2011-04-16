{-# OPTIONS_GHC -Wall #-}
module KontraLink(KontraLink(..), LoginRedirectReason(..), DesignStep(..), DesignStep2Flag(..)) where

import Doc.DocState
import HSP
import Misc
import ActionSchedulerState (ActionID)
import User.UserState
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS
import PayEx.PayExState
import ListUtil
{- |
   Defines the reason why we are redirected to login page
-}
data LoginRedirectReason = LoginTry
                         | NotLogged
                         | NotLoggedAsSuperUser
                         | InvalidLoginInfo String -- email

data DesignStep2Flag = AfterCSVUpload
type Person = Int

data DesignStep = DesignStep1 
                | DesignStep2 DocumentID (Maybe Person) (Maybe DesignStep2Flag) 
                | DesignStep3 DocumentID   

{- |
   All the links available for responses
-}
data KontraLink
    = LinkAbout
    | LinkLogin LoginRedirectReason
    | LinkLogout
    | LinkSignup
    | LinkForgotPassword
    | LinkContracts ListParams
    | LinkTemplates ListParams
    | LinkOffers ListParams
    | LinkMain 
    | LinkNew (Maybe DocumentType) ListParams
    | LinkAjaxTemplates DocumentType ListParams
    | LinkAccount
    | LinkSecurity
    | LinkLandpageSaved Document SignatoryLink
    | LinkSignDoc Document SignatoryLink
    | LinkIssueDoc DocumentID
    | LinkDesignDoc DesignStep
    | LinkIssueDocPDF (Maybe SignatoryLink) Document {- Which file? -}
    | LinkSubaccount ListParams
    | LinkSharing ListParams
    | LinkRemind Document SignatoryLink
    | LinkCancel Document
    | LinkRestart DocumentID
    | LinkAcceptTOS
    | LinkAdminOnly
    | LinkAdminOnlyIndexDB
    | LinkStats
    | LinkPaymentsAdmin
    | LinkUserAdmin (Maybe UserID)
    | LinkPasswordReminder ActionID MagicHash
    | LinkViralInvitationSent ActionID MagicHash
    | LinkAccountCreated ActionID MagicHash String -- email
    | LinkAccountCreatedBySigning ActionID MagicHash
    | LinkAccountRemoval ActionID MagicHash
    | LinkChangeSignatoryEmail DocumentID SignatoryLinkID 
    | LinkWithdrawn DocumentID 
    | LoopBack
    | BackToReferer
    | LinkDaveDocument DocumentID
    | LinkFile FileID BS.ByteString
    | LinkRequestAccount
    | LinkAskQuestion
    | LinkInvite
    | LinkPayExView (Maybe PaymentId)
    | LinkSignCanceledDataMismatch DocumentID SignatoryLinkID

{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
    showsPrec _ LinkAbout = (++) "/about"
    showsPrec _ (LinkLogin LoginTry) = (++) "/login"
    showsPrec _ (LinkLogin (InvalidLoginInfo email)) = (++) $ "/?logging&email=" ++ (URL.encode . UTF.encode $ email)
    showsPrec _ (LinkLogin _) = (++) "/?logging"
    showsPrec _ LinkLogout = (++) "/logout"
    showsPrec _ LinkSignup = (++) "/signup"
    showsPrec _ LinkForgotPassword = (++) "/amnesia"
    showsPrec _ (LinkContracts params) = (++) $ "/d" ++ "?" ++ show params
    showsPrec _ (LinkTemplates params) = (++) $ "/t" ++ "?" ++ show params
    showsPrec _ (LinkOffers params) = (++) $ "/o" ++ "?" ++ show params
    showsPrec _ LinkMain = (++) "/"
    showsPrec _ (LinkNew mdoctype params) = (++) $ "/" ++ "?showTemplates=Yes&" ++ "doctype="++ (maybe "" show mdoctype) ++"&"++ show params
    showsPrec _ (LinkAjaxTemplates doctype params) = (++) $ "/templates?" ++ "doctype="++ show doctype ++"&"++ show params
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ LinkAccount = (++) "/account"
    showsPrec _ (LinkSubaccount params) = (++) $ "/account/subaccount" ++ "?" ++ show params
    showsPrec _ (LinkSharing params) = (++) $ "/account/sharing" ++ "?" ++ show params
    showsPrec _ LinkSecurity = (++) "/account/security"
    showsPrec _ (LinkLandpageSaved document signatorylink) = 
        (++) $ "/landpage/signedsave/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink)
    showsPrec _ (LinkIssueDoc documentid) = 
        (++) $ "/d/" ++ show documentid
    showsPrec _ (LinkDesignDoc DesignStep1) =  (++) $ "/"
    showsPrec _ (LinkDesignDoc (DesignStep2 documentid Nothing _)) = (++) $ "/d/" ++ show documentid ++ "?step2"
    showsPrec _ (LinkDesignDoc (DesignStep2 documentid (Just person) Nothing)) = (++) $ "/d/" ++ show documentid ++ "?step2&person=" ++ show person
    showsPrec _ (LinkDesignDoc (DesignStep2 documentid (Just person) (Just AfterCSVUpload))) = (++) $ "/d/" ++ show documentid ++ "?step2&person=" ++ show person ++ "&aftercsvupload"
    showsPrec _ (LinkDesignDoc (DesignStep3 documentid)) = (++) $ "/d/" ++ show documentid ++ "?step3"
    showsPrec _ (LinkIssueDocPDF Nothing document) = 
        (++) $ "/d/" ++ show (documentid document) ++ "/" ++ BS.toString (documenttitle document) ++ ".pdf"
    showsPrec _ (LinkIssueDocPDF (Just SignatoryLink{signatorylinkid, signatorymagichash}) document) = 
        (++) $ "/d/" ++ show (documentid document) ++ "/" ++ show signatorylinkid ++ "/" ++ show signatorymagichash ++ "/" ++ BS.toString (documenttitle document) ++ ".pdf"
    showsPrec _ (LinkFile fileid filename) = 
        (++) $ "/df/" ++ show fileid ++ "/" ++ BS.toString filename
    showsPrec _ (LinkSignDoc document signatorylink) = 
        (++) $ "/s/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++ 
                 "/" ++ show (signatorymagichash signatorylink)
    showsPrec _ (LinkRemind document signlink) = (++) $ "/resend/"++(show $ documentid document)++"/"++(show $ signatorylinkid signlink)   
    showsPrec _ (LinkCancel document) = (++) $ "/cancel/"++(show $ documentid document)
    showsPrec _ (LinkRestart documentid) = (++) $ "/restart/"++(show  documentid)
    showsPrec _ LinkAdminOnly = (++) $ "/adminonly/"
    showsPrec _ LinkAdminOnlyIndexDB = (++) $ "/adminonly/db"
    showsPrec _ LinkStats = (++) $ "/stats"
    showsPrec _ (LinkPaymentsAdmin ) = (++) $ "/adminonly/advpayments"
    showsPrec _ (LinkUserAdmin Nothing) = (++) $ "/adminonly/useradmin"
    showsPrec _ (LinkUserAdmin (Just userId)) = (++) $ "/adminonly/useradmin/"++show userId
    showsPrec _ (LinkPasswordReminder aid hash) = (++) $ "/amnesia/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkViralInvitationSent aid hash) = (++) $ "/accountsetup/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccountCreated aid hash email) = (++) $ "/accountsetup/" ++ show aid ++ "/" ++ show hash ++ "?email=" ++ email
    showsPrec _ (LinkAccountCreatedBySigning aid hash) = (++) $ "/accountsetup/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccountRemoval aid hash) = (++) $ "/accountremoval/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkChangeSignatoryEmail did slid ) = (++) $ "/changeemail/"++show did++"/"++show slid
    showsPrec _ (LinkWithdrawn did ) = (++) $ "/withdrawn/"++show did
    showsPrec _ LoopBack = (++) $ "/" -- this should never be used
    showsPrec _ BackToReferer = (++) $ "/" -- this should never be used
    showsPrec _ (LinkDaveDocument docid) = (++) ("/dave/document/" ++ show docid)
    showsPrec _ (LinkAskQuestion) = (++) ("/question")
    showsPrec _ (LinkRequestAccount) = (++) "/requestaccount"
    showsPrec _ (LinkInvite) = (++) "/invite"
    showsPrec _ (LinkPayExView Nothing) = (++) $ "/payex"
    showsPrec _ (LinkPayExView (Just pid)) = (++) $ "/payex/" ++ show pid
    showsPrec _ (LinkSignCanceledDataMismatch docid sigid) = (++) $ "/landpage/signcanceleddatamismatch/" ++ show docid ++ "/" ++ show sigid
    
    
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
