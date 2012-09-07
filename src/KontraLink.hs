module KontraLink(KontraLink(..), LoginRedirectReason(..), getHomeOrUploadLink) where

import Data.Int

import Doc.DocStateData
import MagicHash
import Utils.List
import User.Model
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import ListUtil
import Company.Model
import File.FileID
import OAuth.Model
import Network.URI
import Network.HTTP
import KontraMonad
import Context
import Attachment.AttachmentID

{- |
   Defines the reason why we are redirected to login page
-}
data LoginRedirectReason = LoginTry
                         | NotLogged
                         | NotLoggedAsSuperUser
                         | InvalidLoginInfo String -- email
    deriving (Eq)

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
    | LinkArchive
    | LinkAccount
    | LinkAccountCompany (Maybe CompanyID)
    | LinkCompanyLogo CompanyID
    | LinkChangeUserEmail UserID MagicHash
    | LinkAccountSecurity
    | LinkUserMailAPI
    | LinkSignDoc Document SignatoryLink
    | LinkSignDocNoMagicHash DocumentID SignatoryLinkID
    | LinkIssueDoc DocumentID
    | LinkDesignDoc DocumentID
    | LinkRenameAttachment AttachmentID
    | LinkCompanyAccounts ListParams
    | LinkCompanyTakeover CompanyID
    | LinkRemind Document SignatoryLink
    | LinkCancel Document
    | LinkRestart DocumentID
    | LinkAcceptTOS
    | LinkAdminOnly
    | LinkUserAdmin (Maybe UserID)
    | LinkCompanyAdmin (Maybe CompanyID)
    | LinkCompanyUserAdmin CompanyID
    | LinkAdminServices
    | LinkAdminQuarantine
    | LinkPasswordReminder UserID MagicHash
    | LinkAccountCreated UserID MagicHash -- email
    | LinkChangeSignatoryEmail DocumentID SignatoryLinkID
    | LinkWithdrawn DocumentID
    | LoopBack
    | BackToReferer
    | LinkDaveDocument DocumentID
    | LinkFile FileID String
    | LinkAskQuestion
    | LinkSignCanceledDataMismatch DocumentID SignatoryLinkID
    | LinkAttachmentView AttachmentID
    | LinkCSVLandPage Int
    | LinkEnableCookies
    | LinkDocumentPreview DocumentID (Maybe SignatoryLink) FileID
    | LinkAPIDocumentSignatoryAttachment DocumentID SignatoryLinkID String
    | LinkPadDeviceArchive 
    | LinkPadDeviceView
    | LinkMailAPIDelayConfirmation String Int64 MagicHash
    | LinkOAuthAuthorization APIToken
    | LinkOAuthCallback URI APIToken (Maybe MagicHash)
    | LinkOAuthDashboard
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
    showsPrec _ (LinkArchive) = (++) $ "/d"
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ (LinkAccount) = (++) "/account"
    showsPrec _ (LinkAccountCompany Nothing) = (++) "/account/company"
    showsPrec _ (LinkAccountCompany (Just cid)) = (++) $ "/adminonly/companyadmin/branding/" ++ show cid
    showsPrec _ (LinkCompanyLogo cid) = (++) $ "/account/company/" ++ show cid
    showsPrec _ (LinkChangeUserEmail actionid magichash) =
        (++) $ "/account/" ++ show actionid ++  "/" ++ show magichash
    showsPrec _ (LinkCompanyAccounts params) = (++) $ "/account/companyaccounts" ++ "?" ++ show params
    showsPrec _ (LinkCompanyTakeover companyid) = (++) $ "/companyaccounts/join/" ++ show companyid
    showsPrec _ LinkAccountSecurity = (++) "/account/security"
    showsPrec _ LinkUserMailAPI = (++) "/account/mailapi"
    showsPrec _ (LinkIssueDoc documentid) =
        (++) $ "/d/" ++ show documentid
    showsPrec _ (LinkDesignDoc did) =  (++) $ "/" ++ show did
    showsPrec _ (LinkRenameAttachment documentid) = (++) $ "/a/rename/" ++ show documentid
    showsPrec _ (LinkFile fileid filename) =
        (++) $ "/df/" ++ show fileid ++ "/" ++ filename
    showsPrec _ (LinkSignDoc document signatorylink) =
        (++) $ "/s/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++
                 "/"++ show (signatorymagichash signatorylink)
    showsPrec _ (LinkSignDocNoMagicHash documentid signatorylinkid) =
        (++) $ "/s/" ++ show documentid ++ "/" ++ show signatorylinkid
    showsPrec _ (LinkRemind document signlink) = (++) $ "/resend/"++(show $ documentid document)++"/"++(show $ signatorylinkid signlink)
    showsPrec _ (LinkCancel document) = (++) $ "/cancel/"++(show $ documentid document)
    showsPrec _ (LinkRestart documentid) = (++) $ "/restart/"++(show  documentid)
    showsPrec _ LinkAdminOnly = (++) $ "/adminonly/"
    showsPrec _ (LinkUserAdmin Nothing) = (++) $ "/adminonly/useradmin"
    showsPrec _ (LinkUserAdmin (Just userId)) = (++) $ "/adminonly/useradmin/"++show userId
    showsPrec _ (LinkCompanyAdmin Nothing) = (++) $ "/adminonly/companyadmin"
    showsPrec _ (LinkCompanyAdmin (Just companyid)) = (++) $ "/adminonly/companyadmin/" ++ show companyid
    showsPrec _ (LinkCompanyUserAdmin companyid) = (++) $ "/adminonly/companyadmin/users/" ++ show companyid
    showsPrec _ (LinkAdminServices) = (++) $ "/adminonly/services"
    showsPrec _ (LinkAdminQuarantine) = (++) $ "/adminonly/quarantine"
    showsPrec _ (LinkPasswordReminder aid hash) = (++) $ "/amnesia/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccountCreated uid hash) = (++) $ "/accountsetup/" ++ show uid ++ "/" ++ show hash
    showsPrec _ (LinkChangeSignatoryEmail did slid ) = (++) $ "/changeemail/"++show did++"/"++show slid
    showsPrec _ (LinkWithdrawn did ) = (++) $ "/withdrawn/"++show did
    showsPrec _ LoopBack = (++) $ "/" -- this should never be used
    showsPrec _ BackToReferer = (++) $ "/" -- this should never be used
    showsPrec _ (LinkDaveDocument docid) = (++) ("/dave/document/" ++ show docid ++"/")
    showsPrec _ (LinkAskQuestion) = (++) ("/question")
    showsPrec _ (LinkSignCanceledDataMismatch docid sigid) = (++) $ "/landpage/signcanceleddatamismatch/" ++ show docid ++ "/" ++ show sigid
    showsPrec _ (LinkCSVLandPage c) = (++) ("/csvlandpage/" ++ show c)
    showsPrec _ (LinkEnableCookies) = (++) ("/enable-cookies/enable-cookies.html")
    showsPrec _ (LinkDocumentPreview did (Just sl) fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show (signatorylinkid sl) ++
                 "/" ++ show (signatorymagichash sl) ++
                 "/" ++ show fid)
    showsPrec _ (LinkDocumentPreview did Nothing fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show fid)
    showsPrec _ (LinkAPIDocumentSignatoryAttachment did sid name) =
      (++) ("/api/document/" ++ show did ++ "/signatory/" ++ show sid ++ "/attachment/" ++ name)
    showsPrec _ (LinkPadDeviceArchive) =
      (++) ("/padqueue/archive")
    showsPrec _ (LinkPadDeviceView) =
      (++) ("/padqueue")
    showsPrec _ (LinkMailAPIDelayConfirmation email delayid key) = (++) ("/mailapi/confirmdelay/" ++ (URL.encode $ UTF.encode email) ++ "/" ++ show delayid ++ "/" ++ show key)
    showsPrec _ (LinkOAuthAuthorization token) = (++) ("/oauth/authorization?oauth_token=" ++ show token)
    showsPrec _ (LinkOAuthCallback url token (Just verifier)) = 
      (++) (show $ setParams url [("oauth_token", show token), ("oauth_verifier", show verifier)])
    showsPrec _ (LinkOAuthCallback url token Nothing) = 
      (++) (show $ setParams url [("oauth_token", show token), ("denied", "true")])
    showsPrec _ LinkOAuthDashboard = (++) ("/oauth/dashboard")
    showsPrec _ (LinkAttachmentView attid) = (++) ("/a/" ++ show attid)

setParams :: URI -> [(String, String)] -> URI
setParams uri params = uri { uriQuery = "?" ++ vars }
  where
    mvars = urlDecodeVars $ uriQuery uri
    vars = urlEncodeVars $ maybe params (++ params) mvars
    urlDecodeVars :: String -> Maybe [(String, String)]
    urlDecodeVars ('?':s) = urlDecodeVars s
    urlDecodeVars s = makeKV (splitOver "&" s) []
      where
        makeKV [] a = Just a
        makeKV (kv:ks) a = case break (== '=') kv of
          (k, '=':v) -> makeKV ks ((urlDecode k, urlDecode v):a)
          (k, "") -> makeKV ks ((urlDecode k, ""):a)
          _ -> Nothing

getHomeOrUploadLink :: KontraMonad m => m KontraLink
getHomeOrUploadLink = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Just _ -> return LinkUpload
    Nothing -> return $ LinkHome (ctxlocale ctx)
