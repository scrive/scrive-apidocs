module KontraLink(KontraLink(..), LoginRedirectReason(..), DesignStep(..), DesignStep2Flag(..)) where

import DB.Types
import Doc.DocState
import Misc
import ActionSchedulerState (ActionID)
import User.Model
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS
import ListUtil
import Session
import API.Service.Model
import Company.Model
import File.FileID

{- |
   Defines the reason why we are redirected to login page
-}
data LoginRedirectReason = LoginTry
                         | NotLogged
                         | NotLoggedAsSuperUser
                         | InvalidLoginInfo String -- email
    deriving (Eq)
data DesignStep2Flag = AfterCSVUpload  deriving (Eq)
type Person = Int

data DesignStep = DesignStep1
                | DesignStep2 DocumentID (Maybe Person) (Maybe DesignStep2Flag) SignLast
                | DesignStep3 DocumentID SignLast
    deriving (Eq)
type SignLast = Bool

instance Show DesignStep where
    show DesignStep1 =  ""
    show (DesignStep2 documentid Nothing _ sl) = "d/" ++ show documentid ++ "?step2" ++ (if sl then "&authorsignlast" else "")
    show (DesignStep2 documentid (Just person) Nothing sl) = "d/" ++ show documentid ++ "?step2&person=" ++ show person ++ (if sl then "&authorsignlast" else "")
    show (DesignStep2 documentid (Just person) (Just AfterCSVUpload) sl) =  "d/" ++ show documentid ++ "?step2&person=" ++ show person ++ "&aftercsvupload" ++ (if sl then "&authorsignlast" else "")
    show (DesignStep3 documentid sl) ="d/" ++ show documentid ++ "?step3" ++ (if sl then "&authorsignlast" else "")

{- |
   All the links available for responses
-}
data KontraLink
    = LinkHome Locale
    | LinkPriceplan Locale
    | LinkSecurity Locale
    | LinkLegal Locale
    | LinkPrivacyPolicy Locale
    | LinkTerms Locale
    | LinkAbout Locale
    | LinkPartners Locale
    | LinkClients Locale
    | LinkContactUs Locale
    | LinkAPIPage Locale
    | LinkScriveByMailPage Locale
    | LinkLogin Locale LoginRedirectReason
    | LinkLogout
    | LinkSignup
    | LinkForgotPassword
    | LinkUpload
    | LinkLocaleSwitch
    | LinkContracts
    | LinkTemplates
    | LinkOffers
    | LinkOrders
    | LinkAttachments
    | LinkRubbishBin
    | LinkAccount Bool -- show create company modal?
    | LinkChangeUserEmail ActionID MagicHash
    | LinkAccountSecurity
    | LinkUserMailAPI
    | LinkSignDoc Document SignatoryLink
    | LinkAccountFromSign Document SignatoryLink ActionID MagicHash
    | LinkIssueDoc DocumentID
    | LinkDesignDoc DesignStep
    | LinkRenameAttachment DocumentID
    | LinkIssueDocPDF (Maybe SignatoryLink) Document {- Which file? -}
    | LinkCompanyAccounts ListParams
    | LinkCompanyTakeover CompanyID
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
    | LinkCompanyAdmin (Maybe CompanyID)
    | LinkCompanyUserAdmin CompanyID
    | LinkAdminServices
    | LinkAdminQuarantine
    | LinkPasswordReminder ActionID MagicHash
    | LinkViralInvitationSent ActionID MagicHash String --email
    | LinkAccountCreated ActionID MagicHash String -- email
    | LinkAccountCreatedBySigning ActionID MagicHash
    | LinkAccountRemoval ActionID MagicHash
    | LinkChangeSignatoryEmail DocumentID SignatoryLinkID
    | LinkWithdrawn DocumentID
    | LoopBack
    | BackToReferer
    | LinkDaveDocument DocumentID
    | LinkFile FileID BS.ByteString
    | LinkAskQuestion
    | LinkRequestPhoneCall
    | LinkInvite
    | LinkSignCanceledDataMismatch DocumentID SignatoryLinkID
    | LinkConnectUserSession ServiceID UserID SessionId KontraLink
    | LinkConnectCompanySession ServiceID CompanyID SessionId KontraLink
    | LinkAttachmentForAuthor DocumentID FileID
    | LinkAttachmentForViewer DocumentID SignatoryLinkID MagicHash FileID
    | LinkServiceLogo ServiceID
    | LinkServiceButtonsBody ServiceID
    | LinkServiceButtonsRest ServiceID
    | LinkCSVLandPage Int
    | LinkDocumentPreview DocumentID (Maybe SignatoryLink) FileID
    | LinkAPIDocumentMetadata DocumentID
    | LinkAPIDocumentSignatoryAttachment DocumentID SignatoryLinkID String
    deriving (Eq)

localeFolder :: Locale -> String
localeFolder locale = "/" ++ (codeFromRegion $ getRegion locale) ++ "/" ++ (codeFromLang $ getLang locale)

{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
    showsPrec _ (LinkHome locale) = (++) $ localeFolder locale
    showsPrec _ (LinkPriceplan locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/priser"
      | otherwise = (++) $ localeFolder locale ++ "/pricing"
    showsPrec _ (LinkSecurity locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/sakerhet"
      | otherwise = (++) $ localeFolder locale ++ "/security"
    showsPrec _ (LinkLegal locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/juridik"
      | otherwise = (++) $ localeFolder locale ++ "/legal"
    showsPrec _ (LinkPrivacyPolicy locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/sekretesspolicy"
      | otherwise = (++) $ localeFolder locale ++ "/privacy-policy"
    showsPrec _ (LinkTerms locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/allmana-villkor"
      | otherwise = (++) $ localeFolder locale ++ "/terms"
    showsPrec _ (LinkAbout locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/om-scrive"
      | otherwise = (++) $ localeFolder locale ++ "/about"
    showsPrec _ (LinkPartners locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/partners"
      | otherwise = (++) $ localeFolder locale ++ "/partners"
    showsPrec _ (LinkClients locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/kunder"
      | otherwise = (++) $ localeFolder locale ++ "/clients"
    showsPrec _ (LinkContactUs locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/kontakta"
      | otherwise = (++) $ localeFolder locale ++ "/contact"
    showsPrec _ (LinkAPIPage locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/scriveapi"
      | otherwise = (++) $ localeFolder locale ++ "/scriveapi"
    showsPrec _ (LinkScriveByMailPage locale)
      | getLang locale == LANG_SE = (++) $ localeFolder locale ++ "/scrivebymail"
      | otherwise = (++) $ localeFolder locale ++ "/scrivebymail"
    showsPrec _ (LinkLogin locale LoginTry) = (++) $ localeFolder locale ++ "/login"
    showsPrec _ (LinkLogin locale (InvalidLoginInfo email)) = (++) $ localeFolder locale ++ "/?logging&email=" ++ (URL.encode . UTF.encode $ email)
    showsPrec _ (LinkLogin locale _) = (++) $ localeFolder locale ++ "/?logging"
    showsPrec _ LinkLogout = (++) "/logout"
    showsPrec _ LinkSignup = (++) "/signup"
    showsPrec _ LinkForgotPassword = (++) "/amnesia"
    showsPrec _ LinkUpload = (++) "/upload"
    showsPrec _ LinkLocaleSwitch = (++) "/locale"
    showsPrec _ (LinkContracts) = (++) $ "/d"
    showsPrec _ (LinkTemplates) = (++) $ "/t"
    showsPrec _ (LinkOffers) = (++) $ "/o"
    showsPrec _ (LinkOrders) = (++) $ "/or"
    showsPrec _ (LinkAttachments) = (++) $ "/a"
    showsPrec _ (LinkRubbishBin) = (++) $ "/r"
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ (LinkAccount False) = (++) "/account"
    showsPrec _ (LinkAccount True) = (++) "/account/?createcompany"
    showsPrec _ (LinkChangeUserEmail actionid magichash) =
        (++) $ "/account/" ++ show actionid ++  "/" ++ show magichash
    showsPrec _ (LinkCompanyAccounts params) = (++) $ "/account/companyaccounts" ++ "?" ++ show params
    showsPrec _ (LinkCompanyTakeover companyid) = (++) $ "/companyaccounts/join/" ++ show companyid
    showsPrec _ (LinkSharing params) = (++) $ "/account/sharing" ++ "?" ++ show params
    showsPrec _ LinkAccountSecurity = (++) "/account/security"
    showsPrec _ LinkUserMailAPI = (++) "/account/mailapi"
    showsPrec _ (LinkIssueDoc documentid) =
        (++) $ "/d/" ++ show documentid
    showsPrec _ (LinkDesignDoc designstep) =  (++) $ "/" ++ show designstep
    showsPrec _ (LinkRenameAttachment documentid) = (++) $ "/a/rename/" ++ show documentid
    showsPrec _ (LinkIssueDocPDF Nothing document) =
        (++) $ "/d/" ++ show (documentid document) ++ "/" ++ BS.toString (documenttitle document) ++ ".pdf"
    showsPrec _ (LinkIssueDocPDF (Just SignatoryLink{signatorylinkid, signatorymagichash}) document) =
        (++) $ "/d/" ++ show (documentid document) ++ "/" ++ show signatorylinkid ++ "/" ++ show signatorymagichash ++ "/" ++ BS.toString (documenttitle document) ++ ".pdf"
    showsPrec _ (LinkFile fileid filename) =
        (++) $ "/df/" ++ show fileid ++ "/" ++ BS.toString filename
    showsPrec _ (LinkSignDoc document signatorylink) =
        (++) $ "/s/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++
                 "?" ++ "magichash="++ show (signatorymagichash signatorylink)
    showsPrec _ (LinkAccountFromSign document signatorylink actionid magichash) =
        (++) $ "/s/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++
                 "/" ++ show (signatorymagichash signatorylink) ++
                 "/" ++ show actionid ++
                 "/" ++ show magichash
    showsPrec _ (LinkRemind document signlink) = (++) $ "/resend/"++(show $ documentid document)++"/"++(show $ signatorylinkid signlink)
    showsPrec _ (LinkCancel document) = (++) $ "/cancel/"++(show $ documentid document)
    showsPrec _ (LinkRestart documentid) = (++) $ "/restart/"++(show  documentid)
    showsPrec _ LinkAdminOnly = (++) $ "/adminonly/"
    showsPrec _ LinkAdminOnlyIndexDB = (++) $ "/adminonly/db"
    showsPrec _ LinkStats = (++) $ "/stats"
    showsPrec _ (LinkPaymentsAdmin ) = (++) $ "/adminonly/advpayments"
    showsPrec _ (LinkUserAdmin Nothing) = (++) $ "/adminonly/useradmin"
    showsPrec _ (LinkUserAdmin (Just userId)) = (++) $ "/adminonly/useradmin/"++show userId
    showsPrec _ (LinkCompanyAdmin Nothing) = (++) $ "/adminonly/companyadmin"
    showsPrec _ (LinkCompanyAdmin (Just companyid)) = (++) $ "/adminonly/companyadmin/" ++ show companyid
    showsPrec _ (LinkCompanyUserAdmin companyid) = (++) $ "/adminonly/companyadmin/users/" ++ show companyid
    showsPrec _ (LinkAdminServices) = (++) $ "/adminonly/services"
    showsPrec _ (LinkAdminQuarantine) = (++) $ "/adminonly/quarantine"
    showsPrec _ (LinkPasswordReminder aid hash) = (++) $ "/amnesia/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkViralInvitationSent aid hash email) = (++) $ "/accountsetup/" ++ show aid ++ "/" ++ show hash ++ "?email=" ++ email
    showsPrec _ (LinkAccountCreated aid hash email) = (++) $ "/accountsetup/" ++ show aid ++ "/" ++ show hash ++ "?email=" ++ email
    showsPrec _ (LinkAccountCreatedBySigning aid hash) = (++) $ "/accountsetup/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccountRemoval aid hash) = (++) $ "/accountremoval/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkChangeSignatoryEmail did slid ) = (++) $ "/changeemail/"++show did++"/"++show slid
    showsPrec _ (LinkWithdrawn did ) = (++) $ "/withdrawn/"++show did
    showsPrec _ LoopBack = (++) $ "/" -- this should never be used
    showsPrec _ BackToReferer = (++) $ "/" -- this should never be used
    showsPrec _ (LinkDaveDocument docid) = (++) ("/dave/document/" ++ show docid)
    showsPrec _ (LinkAskQuestion) = (++) ("/question")
    showsPrec _ (LinkRequestPhoneCall) = (++) "/phone"
    showsPrec _ (LinkInvite) = (++) "/invite"
    showsPrec _ (LinkSignCanceledDataMismatch docid sigid) = (++) $ "/landpage/signcanceleddatamismatch/" ++ show docid ++ "/" ++ show sigid
    showsPrec _ (LinkConnectUserSession sid uid ssid referer) = (++) $ "/integration/connectuser/" ++ encodeForURL sid ++ "/" ++ show uid  ++ "/" ++ show ssid
                                                                        ++ "?referer=" ++ (URL.encode $ UTF.encode  $ show referer)
    showsPrec _ (LinkConnectCompanySession sid cid ssid referer) = (++) $ "/integration/connectcompany/" ++ encodeForURL sid ++ "/" ++ show cid  ++ "/" ++ show ssid
                                                                        ++ "?referer=" ++ (URL.encode $ UTF.encode  $ show referer)
    showsPrec _ (LinkAttachmentForAuthor did fid) = (++) $ "/download/" ++ show did ++ "/" ++ show fid ++ "/" ++ "file.pdf"
    showsPrec _ (LinkAttachmentForViewer did sid mh fid) = (++) $ "/download/" ++ show did ++ "/" ++ show fid ++ "/file.pdf?signatorylinkid=" ++ show sid ++ "&magichash=" ++ show mh 
    showsPrec _ (LinkServiceLogo sid) = (++) $ "/services/logo/" ++ encodeForURL sid
    showsPrec _ (LinkServiceButtonsBody sid) = (++) $ "/services/buttons_body/" ++ encodeForURL sid
    showsPrec _ (LinkServiceButtonsRest sid) = (++) $ "/services/buttons_rest/" ++ encodeForURL sid
    showsPrec _ (LinkCSVLandPage c) = (++) ("/csvlandpage/" ++ show c)
    showsPrec _ (LinkDocumentPreview did (Just sl) fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show (signatorylinkid sl) ++
                 "/" ++ show (signatorymagichash sl) ++
                 "/" ++ show fid)
    showsPrec _ (LinkDocumentPreview did Nothing fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show fid)
    showsPrec _ (LinkAPIDocumentMetadata did) = (++) ("/api/document/" ++ show did ++ "/metadata")
    showsPrec _ (LinkAPIDocumentSignatoryAttachment did sid name) =
      (++) ("/api/document/" ++ show did ++ "/signatory/" ++ show sid ++ "/attachment/" ++ name)
