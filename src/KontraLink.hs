module KontraLink(KontraLink(..), LoginRedirectReason(..), getHomeOrDesignViewLink) where

import qualified Data.ByteString.Char8 as BSC
import Doc.DocStateData
import Doc.SignatoryLinkID
import MagicHash
import Doc.DocumentID
import Utils.List
import User.Model
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
    | LinkLogin Lang LoginRedirectReason
    | LinkLoginDirect
    | LinkLogout
    | LinkSignup Lang
    | LinkArchive
    | LinkAccount
    | LinkAccountCompany (Maybe CompanyID)
    | LinkCompanySignViewLogo CompanyID
    | LinkCompanyCustomLogo CompanyID
    | LinkCompanyEmailLogo CompanyID
    | LinkChangeUserEmail UserID MagicHash
    | LinkUserMailAPI
    | LinkSignDoc Document SignatoryLink
    | LinkMainFile Document SignatoryLink
    | LinkSignDocNoMagicHash DocumentID SignatoryLinkID
    | LinkIssueDoc DocumentID
    | LinkEvidenceAttachment DocumentID BSC.ByteString
    | LinkCompanyAccounts
    | LinkCompanyTakeover CompanyID
    | LinkAcceptTOS
    | LinkUserAdmin UserID
    | LinkCompanyAdmin CompanyID
    | LinkPasswordReminder UserID MagicHash
    | LinkAccessNewAccount UserID MagicHash
    | LinkAccountCreated Lang UserID MagicHash SignupMethod -- email
    | LoopBack
    | LinkDaveDocument DocumentID
    | LinkDaveFile FileID String
    | LinkAttachmentView AttachmentID
    | LinkEnableCookies
    | LinkDocumentPreview DocumentID (Maybe SignatoryLink) FileID
    | LinkOAuthAuthorization APIToken
    | LinkOAuthCallback URI APIToken (Maybe MagicHash)
    | LinkExternal String
    | LinkDesignView
    deriving (Eq)

langFolder :: Lang -> String
langFolder lang = "/" ++ (codeFromLang lang)

{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
    showsPrec _ (LinkHome lang) = (++) $ langFolder lang ++ "/"
    showsPrec _ (LinkLogin lang _) = (++) $ langFolder lang ++ "/login"
    showsPrec _ LinkLoginDirect = (++) $ "/login"
    showsPrec _ LinkLogout = (++) "/logout"
    showsPrec _ (LinkSignup lang) = (++) $ langFolder lang ++ "/signup"
    showsPrec _ (LinkArchive) = (++) $ "/d"
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ (LinkAccount) = (++) "/account"
    showsPrec _ (LinkAccountCompany Nothing) = (++) "/account#company"
    showsPrec _ (LinkAccountCompany (Just cid)) = (++) $ "/adminonly/companyadmin/" ++ show cid
    showsPrec _ (LinkCompanySignViewLogo cid) = (++) $ "/account/company/signview/" ++ show cid
    showsPrec _ (LinkCompanyCustomLogo cid) = (++) $ "/account/company/custom/" ++ show cid
    showsPrec _ (LinkCompanyEmailLogo cid) = (++) $ "/account/company/email/" ++ show cid
    showsPrec _ (LinkChangeUserEmail actionid magichash) =
        (++) $ "/account/" ++ show actionid ++  "/" ++ show magichash
    showsPrec _ (LinkCompanyAccounts) = (++) $ "/account#users"
    showsPrec _ (LinkCompanyTakeover companyid) = (++) $ "/companyaccounts/join/" ++ show companyid
    showsPrec _ LinkUserMailAPI = (++) "/account#mailapi"
    showsPrec _ (LinkIssueDoc documentid) =
        (++) $ "/d/" ++ show documentid
    showsPrec _ (LinkEvidenceAttachment did filename) =  (++) $ "/d/evidenceattachment/" ++ show did ++ "/" ++ BSC.unpack filename
    showsPrec _ (LinkSignDoc document signatorylink) =
        (++) $ "/s/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++
                 "/"++ show (signatorymagichash signatorylink)
    showsPrec _ (LinkMainFile document signatorylink) =
        (++) $ "/download/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++
                 "/"++ show (signatorymagichash signatorylink) ++ "/"++ documenttitle document ++ ".pdf"
    showsPrec _ (LinkSignDocNoMagicHash documentid signatorylinkid) =
        (++) $ "/s/" ++ show documentid ++ "/" ++ show signatorylinkid
    showsPrec _ (LinkUserAdmin userId) = (++) $ "/adminonly/useradmin/"++show userId
    showsPrec _ (LinkCompanyAdmin companyid) = (++) $ "/adminonly/companyadmin/" ++ show companyid
    showsPrec _ (LinkPasswordReminder aid hash) = (++) $ "/amnesia/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccessNewAccount aid hash) = (++) $ "/mynewaccount/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccountCreated lang uid hash sm) = (++) $ langFolder lang  ++ "/accountsetup/" ++ show uid ++ "/" ++ show hash ++ "/" ++ show sm
    showsPrec _ LoopBack = (++) $ "/" -- this should never be used
    showsPrec _ (LinkDaveDocument docid) = (++) ("/dave/document/" ++ show docid ++"/")
    showsPrec _ (LinkDaveFile fileid filename) =  (++) $ "/dave/file/" ++ show fileid ++ "/" ++ filename
    showsPrec _ (LinkEnableCookies) = (++) ("/enable-cookies/enable-cookies.html")
    showsPrec _ (LinkDocumentPreview did (Just sl) fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show (signatorylinkid sl) ++
                 "/" ++ show (signatorymagichash sl) ++
                 "/" ++ show fid)
    showsPrec _ (LinkDocumentPreview did Nothing fid) = (++) ("/preview/" ++ show did ++
                 "/" ++ show fid)
    showsPrec _ (LinkOAuthAuthorization token) = (++) ("/oauth/authorization?oauth_token=" ++ show token)
    showsPrec _ (LinkOAuthCallback url token (Just verifier)) =
      (++) (show $ setParams url [("oauth_token", show token), ("oauth_verifier", show verifier)])
    showsPrec _ (LinkOAuthCallback url token Nothing) =
      (++) (show $ setParams url [("oauth_token", show token), ("denied", "true")])
    showsPrec _ (LinkAttachmentView attid) = (++) ("/a/" ++ show attid)
    showsPrec _ (LinkDesignView) = (++) "/newdocument"
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
