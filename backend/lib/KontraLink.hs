module KontraLink(
    KontraLink(..)
  , getHomeOrDesignViewLink
  ) where

import Data.List.Split
import Network.HTTP
import Network.URI

import Context
import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID
import File.FileID
import KontraMonad
import MagicHash
import OAuth.Model
import User.Model
import UserGroup.Types

{- |
   All the links available for responses
-}
data KontraLink
    = LinkHome Lang
    | LinkLogin Lang
    | LinkLoginDirect Lang
    | LinkSignup Lang
    | LinkArchive
    | LinkAccount
    | LinkChangeUserEmail UserID MagicHash
    | LinkSignDocPad DocumentID SignatoryLinkID
    | LinkMainFile Document SignatoryLink MagicHash
    | LinkSignDocNoMagicHash DocumentID SignatoryLinkID
    | LinkSignDocMagicHash DocumentID SignatoryLinkID MagicHash
    | LinkIssueDoc DocumentID
    | LinkEvidenceAttachment DocumentID String
    | LinkCompanyTakeover UserGroupID
    | LinkAcceptTOS
    | LinkPasswordReminder UserID MagicHash
    | LinkAccountCreated Lang UserID MagicHash SignupMethod -- email
    | LoopBack
    | LinkDaveDocument DocumentID
    | LinkDaveFile FileID String
    | LinkEnableCookies
    | LinkDocumentPreview DocumentID (Maybe (SignatoryLink, Maybe MagicHash)) FileID Int
    | LinkOAuthAuthorization APIToken
    | LinkOAuthCallback URI APIToken (Maybe MagicHash)
    | LinkExternal String
    | LinkDesignView
    | LinkPadList
    | LinkPreviewLockedImage
    | LinkPermanentRedirect String
    | LinkTemplateShareableLink DocumentID MagicHash

langFolder :: Lang -> String
langFolder lang = "/" ++ (codeFromLang lang)

{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
    showsPrec _ (LinkHome lang) = (++) $ langFolder lang ++ "/"
    showsPrec _ (LinkLogin lang) = (++) $ langFolder lang ++ "/enter#log-in" -- THIS ONE IS NOT USED. CHECK sendRedirect
    showsPrec _ (LinkLoginDirect lang) = (++) $ langFolder lang ++ "/enter#log-in"  -- THIS ONE IS NOT USED. CHECK sendRedirect
    showsPrec _ (LinkSignup lang) = (++) $ langFolder lang ++ "/enter#sign-up"
    showsPrec _ (LinkArchive) = (++) $ "/d"
    showsPrec _ LinkAcceptTOS = (++) "/accepttos"
    showsPrec _ (LinkAccount) = (++) "/account"
    showsPrec _ (LinkChangeUserEmail actionid magichash) =
        (++) $ "/account/" ++ show actionid ++  "/" ++ show magichash
    showsPrec _ (LinkCompanyTakeover ugid) = (++) $ "/companyaccounts/join/" ++ show ugid
    showsPrec _ (LinkIssueDoc documentid) =
        (++) $ "/d/" ++ show documentid
    showsPrec _ (LinkEvidenceAttachment did filename) =  (++) $ "/d/evidenceattachment/" ++ show did ++ "/" ++ filename
    showsPrec _ (LinkSignDocPad did slid) =
        (++) $ "/sp/" ++ show did ++ "/" ++ show slid
    showsPrec _ (LinkMainFile document signatorylink mh) =
        (++) $ "/download/" ++ show (documentid document) ++ "/" ++ show (signatorylinkid signatorylink) ++
                 "/"++ show mh ++ "/"++ urlEncode (documenttitle document) ++ ".pdf"
    showsPrec _ (LinkSignDocNoMagicHash documentid signatorylinkid) =
        (++) $ "/s/" ++ show documentid ++ "/" ++ show signatorylinkid
    showsPrec _ (LinkSignDocMagicHash documentid signatorylinkid mh) =
        (++) $ "/s/" ++ show documentid ++ "/" ++ show signatorylinkid ++ "/" ++ show mh
    showsPrec _ (LinkPasswordReminder aid hash) = (++) $ "/amnesia/" ++ show aid ++ "/" ++ show hash
    showsPrec _ (LinkAccountCreated lang uid hash sm) = (++) $ langFolder lang  ++ "/accountsetup/" ++ show uid ++ "/" ++ show hash ++ "/" ++ show sm
    showsPrec _ LoopBack = (++) $ "/" -- this should never be used
    showsPrec _ (LinkDaveDocument docid) = (++) ("/dave/document/" ++ show docid ++"/")
    showsPrec _ (LinkDaveFile fileid filename) =  (++) $ "/dave/file/" ++ show fileid ++ "/" ++ filename
    showsPrec _ (LinkEnableCookies) = (++) ("/enable-cookies/enable-cookies.html")
    showsPrec _ (LinkDocumentPreview did (Just (sl, Just mh)) fid width) =
      (++) ("/preview/" ++ show did
      ++ "/" ++ show (signatorylinkid sl)
      ++ "/" ++ show mh
      ++ "/" ++ show fid
      ++ "?pixelwidth=" ++ show width)
    showsPrec _ (LinkDocumentPreview did (Just (sl, Nothing)) fid width) =
      (++) ("/preview/" ++ show did
      ++ "/" ++ show (signatorylinkid sl)
      ++ "/" ++ show fid
      ++ "?pixelwidth=" ++ show width)
    showsPrec _ (LinkDocumentPreview did Nothing fid width) = (++) ("/preview/" ++ show did ++
                 "/" ++ show fid ++
                 "?pixelwidth=" ++ show width)
    showsPrec _ (LinkOAuthAuthorization token) = (++) ("/oauth/authorization?oauth_token=" ++ show token)
    showsPrec _ (LinkOAuthCallback url token (Just verifier)) =
      (++) (show $ setParams url [("oauth_token", show token), ("oauth_verifier", show verifier)])
    showsPrec _ (LinkOAuthCallback url token Nothing) =
      (++) (show $ setParams url [("oauth_token", show token), ("denied", "true")])
    showsPrec _ (LinkDesignView) = (++) "/newdocument"
    showsPrec _ (LinkPadList) = (++) "/to-sign"
    showsPrec _ (LinkExternal s) = (++) s
    showsPrec _ (LinkPreviewLockedImage) = (++) "/img/preview_locked.png"
    showsPrec _ (LinkPermanentRedirect s) = (++) s
    showsPrec _ (LinkTemplateShareableLink did mh) =
      (++) ("/t/" ++ show did ++ "/" ++ show mh)

setParams :: URI -> [(String, String)] -> URI
setParams uri params = uri { uriQuery = "?" ++ vars }
  where
    mvars = urlDecodeVars $ uriQuery uri
    vars = urlEncodeVars $ maybe params (++ params) mvars
    urlDecodeVars :: String -> Maybe [(String, String)]
    urlDecodeVars ('?':s) = urlDecodeVars s
    urlDecodeVars s = makeKV (splitOn "&" s) []
      where
        makeKV [] a = Just a
        makeKV (kv:ks) a = case break (== '=') kv of
          (k, '=':v) -> makeKV ks ((urlDecode k, urlDecode v):a)
          (k, "") -> makeKV ks ((urlDecode k, ""):a)
          _ -> Nothing

getHomeOrDesignViewLink :: KontraMonad m => m KontraLink
getHomeOrDesignViewLink = do
  ctx <- getContext
  case get ctxmaybeuser ctx of
    Just _ -> return LinkDesignView
    Nothing -> return $ LinkHome (get ctxlang ctx)
