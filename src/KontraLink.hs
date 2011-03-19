{-# OPTIONS_GHC -Wall #-}
module KontraLink(KontraLink(..), LoginRedirectReason(..), DesignStep(..)) where

import Doc.DocState
import HSP
import Misc
import Session (SessionId)
import User.UserState
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS
import PayEx.PayExState
import ListUtil
{- |
   Defines the reason why we are redirected to login page
-}
data LoginRedirectReason = NoReason
                         | NotLogged
                         | NotLoggedAsSuperUser
                         | InvalidEmail
                         | InvalidPassword String -- ^ correct email

data DesignStep = DesignStep1 | DesignStep2 DocumentID | DesignStep3 DocumentID   

{- |
   All the links available for responses
-}
data KontraLink
    = LinkAbout
    | LinkLogin LoginRedirectReason
    | LinkLogout
    | LinkSignup
    | LinkForgotPassword
    | LinkForgotPasswordDone
    | LinkContracts ListParams
    | LinkTemplates ListParams
    | LinkMain
    | LinkAccount
    | LinkLandpageSaved Document SignatoryLink
    | LinkSignDoc Document SignatoryLink
    | LinkIssueDoc DocumentID
    | LinkDesignDoc DesignStep
    | LinkIssueDocPDF Document {- Which file? -}
    | LinkSubaccount ListParams
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
    | LinkPayExView (Maybe PaymentId)
   
{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
    showsPrec _ LinkAbout = (++) "/about"
    showsPrec _ (LinkLogin (InvalidPassword email)) = (++) $ "/login/?email=" ++ (URL.encode . UTF.encode $ email) ++ "&"
    showsPrec _ (LinkLogin _) = (++) "/login/?"
    showsPrec _ LinkLogout = (++) "/logout"
    showsPrec _ LinkSignup = (++) "/signup"
    showsPrec _ LinkForgotPassword = (++) "/amnesia"
    showsPrec _ LinkForgotPasswordDone = (++) "/amnesiadone"
    showsPrec _ (LinkContracts params) = (++) $ "/d" ++ "?" ++ show params
    showsPrec _ (LinkTemplates params) = (++) $ "/t" ++ "?" ++ show params
    showsPrec _ LinkMain = (++) "/"
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ LinkAccount = (++) "/account"
    showsPrec _ (LinkSubaccount params) = (++) $ "/account/subaccount" ++ "?" ++ show params
    showsPrec _ (LinkLandpageSaved document signatorylink) = 
        (++) $ "/landpage/signedsave/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink)
    showsPrec _ (LinkIssueDoc documentid) = 
        (++) $ "/d/" ++ show documentid
    showsPrec _ (LinkDesignDoc DesignStep1) =  (++) $ "/"
    showsPrec _ (LinkDesignDoc (DesignStep2 documentid)) = (++) $ "/d/" ++ show documentid ++ "?step2"
    showsPrec _ (LinkDesignDoc (DesignStep3 documentid)) = (++) $ "/d/" ++ show documentid ++ "?step3"
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
    showsPrec _ (LinkPayExView Nothing) = (++) $ "/payex"
    showsPrec _ (LinkPayExView (Just pid)) = (++) $ "/payex/" ++ show pid
    
    
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
