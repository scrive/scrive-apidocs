module KontraLink(KontraLink(..), LoginRedirectReason(..), getHomeOrDesignViewLink) where

import Data.Int

import Doc.DocStateData
import MagicHash
import Utils.List
import User.Model
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
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
    = LinkHome Lang
    | LinkPriceplan Lang
    | LinkSecurity Lang
    | LinkLegal Lang
    | LinkPrivacyPolicy Lang
    | LinkTerms Lang
    | LinkJobs Lang
    | LinkAbout Lang
    | LinkPartners Lang
    | LinkClients Lang
    | LinkContactUs Lang
    | LinkAPIPage Lang
    | LinkScriveByMailPage Lang
    | LinkFeaturesPage Lang
    | LinkLogin Lang LoginRedirectReason
    | LinkLogout
    | LinkSignup
    | LinkForgotPassword
    | LinkLangSwitch
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
    | LinkCompanyAccounts
    | LinkCompanyTakeover CompanyID
    | LinkAcceptTOS
    | LinkAdminOnly
    | LinkUserAdmin (Maybe UserID)
    | LinkCompanyAdmin (Maybe CompanyID)
    | LinkCompanyUserAdmin CompanyID
    | LinkAdminStatistics
    | LinkAdminStatsByDay
    | LinkAdminStatsByMonth
    | LinkPasswordReminder UserID MagicHash
    | LinkAccountCreated UserID MagicHash -- email
    | LoopBack
    | BackToReferer
    | LinkDaveDocument DocumentID
    | LinkFile FileID String
    | LinkAskQuestion
    | LinkAttachmentView AttachmentID
    | LinkEnableCookies
    | LinkDocumentPreview DocumentID (Maybe SignatoryLink) FileID
    | LinkMailAPIDelayConfirmation String Int64 MagicHash
    | LinkOAuthAuthorization APIToken
    | LinkOAuthCallback URI APIToken (Maybe MagicHash)
    | LinkCompanyAdminPayments CompanyID
    | LinkUserAdminPayments UserID
    | LinkExternal String
    | LinkDesignView 
    deriving (Eq)

langFolder :: Lang -> String
langFolder lang = "/" ++ (codeFromLang lang)

{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
    showsPrec _ (LinkHome lang) = (++) $ langFolder lang
    showsPrec _ (LinkPriceplan lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/priser"
      | otherwise = (++) $ langFolder lang ++ "/pricing"
    showsPrec _ (LinkSecurity lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/sakerhet"
      | otherwise = (++) $ langFolder lang ++ "/security"
    showsPrec _ (LinkLegal lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/juridik"
      | otherwise = (++) $ langFolder lang ++ "/legal"
    showsPrec _ (LinkPrivacyPolicy lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/sekretesspolicy"
      | otherwise = (++) $ langFolder lang ++ "/privacy-policy"
    showsPrec _ (LinkTerms lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/allmana-villkor"
      | otherwise = (++) $ langFolder lang ++ "/terms"
    showsPrec _ (LinkJobs lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/jobb"
      | otherwise = (++) $ langFolder lang ++ "/jobs"
    showsPrec _ (LinkAbout lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/om-scrive"
      | otherwise = (++) $ langFolder lang ++ "/about"
    showsPrec _ (LinkPartners lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/partners"
      | otherwise = (++) $ langFolder lang ++ "/partners"
    showsPrec _ (LinkClients lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/kunder"
      | otherwise = (++) $ langFolder lang ++ "/clients"
    showsPrec _ (LinkContactUs lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/kontakta"
      | otherwise = (++) $ langFolder lang ++ "/contact"
    showsPrec _ (LinkAPIPage lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/scriveapi"
      | otherwise = (++) $ langFolder lang ++ "/scriveapi"
    showsPrec _ (LinkScriveByMailPage lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/scrivebymail"
      | otherwise = (++) $ langFolder lang ++ "/scrivebymail"
    showsPrec _ (LinkFeaturesPage lang)
      | getLang lang == LANG_SE = (++) $ langFolder lang ++ "/funktioner"
      | otherwise = (++) $ langFolder lang ++ "/features"
    showsPrec _ (LinkLogin lang LoginTry) = (++) $ langFolder lang ++ "/login"
    showsPrec _ (LinkLogin lang (InvalidLoginInfo email)) = (++) $ langFolder lang ++ "/?logging&email=" ++ (URL.encode . UTF.encode $ email)
    showsPrec _ (LinkLogin lang _) = (++) $ langFolder lang ++ "/?logging"
    showsPrec _ LinkLogout = (++) "/logout"
    showsPrec _ LinkSignup = (++) "/signup"
    showsPrec _ LinkForgotPassword = (++) "/amnesia"
    showsPrec _ LinkLangSwitch = (++) "/lang"
    showsPrec _ (LinkArchive) = (++) $ "/d"
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ (LinkAccount) = (++) "/account"
    showsPrec _ (LinkAccountCompany Nothing) = (++) "/account#company"
    showsPrec _ (LinkAccountCompany (Just cid)) = (++) $ "/adminonly/companyadmin/branding/" ++ show cid
    showsPrec _ (LinkCompanyLogo cid) = (++) $ "/account/company/" ++ show cid
    showsPrec _ (LinkChangeUserEmail actionid magichash) =
        (++) $ "/account/" ++ show actionid ++  "/" ++ show magichash
    showsPrec _ (LinkCompanyAccounts) = (++) $ "/account#users"
    showsPrec _ (LinkCompanyTakeover companyid) = (++) $ "/companyaccounts/join/" ++ show companyid
    showsPrec _ LinkAccountSecurity = (++) "/account#security"
    showsPrec _ LinkUserMailAPI = (++) "/account#mailapi"
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
    showsPrec _ LinkAdminOnly = (++) $ "/adminonly/"
    showsPrec _ (LinkUserAdmin Nothing) = (++) $ "/adminonly/useradmin"
    showsPrec _ (LinkUserAdmin (Just userId)) = (++) $ "/adminonly/useradmin/"++show userId
    showsPrec _ (LinkCompanyAdmin Nothing) = (++) $ "/adminonly/companyadmin"
    showsPrec _ (LinkCompanyAdmin (Just companyid)) = (++) $ "/adminonly/companyadmin/" ++ show companyid
    showsPrec _ (LinkCompanyUserAdmin companyid) = (++) $ "/adminonly/companyadmin/users/" ++ show companyid
    showsPrec _ (LinkAdminStatistics) = (++) $ "/adminonly/statistics"
    showsPrec _ (LinkAdminStatsByDay) = (++) $ "/adminonly/statsbyday"
    showsPrec _ (LinkAdminStatsByMonth) = (++) $ "/adminonly/statsbymonth"
    showsPrec _ (LinkPasswordReminder aid hash) = (++) $ "/amnesia/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccountCreated uid hash) = (++) $ "/accountsetup/" ++ show uid ++ "/" ++ show hash
    showsPrec _ LoopBack = (++) $ "/" -- this should never be used
    showsPrec _ BackToReferer = (++) $ "/" -- this should never be used
    showsPrec _ (LinkDaveDocument docid) = (++) ("/dave/document/" ++ show docid ++"/")
    showsPrec _ (LinkAskQuestion) = (++) ("/question")
    showsPrec _ (LinkEnableCookies) = (++) ("/enable-cookies/enable-cookies.html")
    showsPrec _ (LinkDocumentPreview did (Just sl) fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show (signatorylinkid sl) ++
                 "/" ++ show (signatorymagichash sl) ++
                 "/" ++ show fid)
    showsPrec _ (LinkDocumentPreview did Nothing fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show fid)
    showsPrec _ (LinkMailAPIDelayConfirmation email delayid key) = (++) ("/mailapi/confirmdelay/" ++ (URL.encode $ UTF.encode email) ++ "/" ++ show delayid ++ "/" ++ show key)
    showsPrec _ (LinkOAuthAuthorization token) = (++) ("/oauth/authorization?oauth_token=" ++ show token)
    showsPrec _ (LinkOAuthCallback url token (Just verifier)) =
      (++) (show $ setParams url [("oauth_token", show token), ("oauth_verifier", show verifier)])
    showsPrec _ (LinkOAuthCallback url token Nothing) =
      (++) (show $ setParams url [("oauth_token", show token), ("denied", "true")])
    showsPrec _ (LinkAttachmentView attid) = (++) ("/a/" ++ show attid)
    showsPrec _ (LinkCompanyAdminPayments cid) =
      (++) ("/adminonly/companyadmin/payments/" ++ show cid)
    showsPrec _ (LinkUserAdminPayments uid) =
      (++) ("/adminonly/useradmin/payments/" ++ show uid)
    showsPrec _ (LinkDesignView) = (++) "/newdocumentorlatestdraft"
    showsPrec _ (LinkExternal s) = (++) s


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

getHomeOrDesignViewLink :: KontraMonad m => m KontraLink
getHomeOrDesignViewLink = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Just _ -> return LinkDesignView
    Nothing -> return $ LinkHome (ctxlang ctx)
