module KontraLink (
    KontraLink(..)
  , getHomeOrDesignViewLink
  ) where

import Data.List.Split
import Network.HTTP
import Network.URI
import qualified Data.Text as T

import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID
import File.FileID
import KontraMonad
import MagicHash
import MinutesTime
import OAuth.Model
import User.Email
import User.Model
import UserGroup.Types
import qualified Util.SMSLinkShortening as SMSLinkShortening

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
    | LinkSignDocMagicHashShort SignatoryLinkID MagicHash
    | LinkIssueDoc DocumentID
    | LinkEvidenceAttachment DocumentID Text
    | LinkCompanyTakeover UserGroupID
    | LinkAcceptTOS
    | LinkPasswordReminder UserID MagicHash
    | LinkAccountCreated Lang UserID MagicHash SignupMethod -- email
    | LoopBack
    | LinkDaveDocument DocumentID
    | LinkDaveFile FileID Text
    | LinkEnableCookies
    | LinkDocumentPreview DocumentID (Maybe (SignatoryLink, Maybe MagicHash)) FileID Int
    | LinkOAuthAuthorization APIToken
    | LinkOAuthCallback URI APIToken (Maybe MagicHash)
    | LinkExternal Text
    | LinkDesignView
    | LinkPadList
    | LinkPreviewLockedImage
    | LinkPermanentRedirect Text
    | LinkTemplateShareableLink DocumentID MagicHash
    | LinkPortalInviteWithAccount Text Email
    | LinkPortalInviteWithoutAccount Text Email MagicHash UTCTime

langFolder :: Lang -> Text
langFolder lang = "/" <> (codeFromLang lang)

{- |
   Shows each link as a relative url
-}
instance Show KontraLink where
  showsPrec _ (LinkHome  lang) = (<>) $ (T.unpack $ langFolder lang) <> "/"
  showsPrec _ (LinkLogin lang) = (<>) $ (T.unpack $ langFolder lang) <> "/enter#log-in" -- THIS ONE IS NOT USED. CHECK sendRedirect
  showsPrec _ (LinkLoginDirect lang) =
    (<>) $ (T.unpack $ langFolder lang) <> "/enter#log-in"  -- THIS ONE IS NOT USED. CHECK sendRedirect
  showsPrec _ (LinkSignup lang) = (<>) $ (T.unpack $ langFolder lang) <> "/enter#sign-up"
  showsPrec _ (LinkArchive    ) = (<>) $ "/d"
  showsPrec _ LinkAcceptTOS     = (<>) "/accepttos"
  showsPrec _ (LinkAccount)     = (<>) "/account"
  showsPrec _ (LinkChangeUserEmail actionid magichash) =
    (<>) $ "/account/" <> show actionid <> "/" <> show magichash
  showsPrec _ (LinkCompanyTakeover ugid) = (<>) $ "/companyaccounts/join/" <> show ugid
  showsPrec _ (LinkIssueDoc documentid) = (<>) $ "/d/" <> show documentid
  showsPrec _ (LinkEvidenceAttachment did filename) =
    (<>) $ "/d/evidenceattachment/" <> show did <> "/" <> (T.unpack filename)
  showsPrec _ (LinkSignDocPad did slid) = (<>) $ "/sp/" <> show did <> "/" <> show slid
  showsPrec _ (LinkMainFile document signatorylink mh) =
    (<>)
      $  "/download/"
      <> show (documentid document)
      <> "/"
      <> show (signatorylinkid signatorylink)
      <> "/"
      <> show mh
      <> "/"
      <> urlEncode (T.unpack $ documenttitle document)
      <> ".pdf"
  showsPrec _ (LinkSignDocNoMagicHash documentid signatorylinkid) =
    (<>) $ "/s/" <> show documentid <> "/" <> show signatorylinkid
  showsPrec _ (LinkSignDocMagicHash documentid signatorylinkid mh) =
    (<>) $ "/s/" <> show documentid <> "/" <> show signatorylinkid <> "/" <> show mh
  showsPrec _ (LinkSignDocMagicHashShort signatorylinkid mh) =
    (<>) $ "/z/" <> T.unpack (SMSLinkShortening.short (signatorylinkid, mh))
  showsPrec _ (LinkPasswordReminder aid hash) =
    (<>) $ "/amnesia/" <> show aid <> "/" <> show hash
  showsPrec _ (LinkAccountCreated lang uid hash sm) =
    (<>)
      $  (T.unpack $ langFolder lang)
      <> "/accountsetup/"
      <> show uid
      <> "/"
      <> show hash
      <> "/"
      <> show sm
  showsPrec _ LoopBack                 = (<>) $ "/" -- this should never be used
  showsPrec _ (LinkDaveDocument docid) = (<>) ("/dave/document/" <> show docid <> "/")
  showsPrec _ (LinkDaveFile fileid filename) =
    (<>) $ "/dave/file/" <> show fileid <> "/" <> (T.unpack filename)
  showsPrec _ (LinkEnableCookies) = (<>) ("/enable-cookies/enable-cookies.html")
  showsPrec _ (LinkDocumentPreview did (Just (sl, Just mh)) fid width) = (<>)
    (  "/preview/"
    <> show did
    <> "/"
    <> show (signatorylinkid sl)
    <> "/"
    <> show mh
    <> "/"
    <> show fid
    <> "?pixelwidth="
    <> show width
    )
  showsPrec _ (LinkDocumentPreview did (Just (sl, Nothing)) fid width) = (<>)
    (  "/preview/"
    <> show did
    <> "/"
    <> show (signatorylinkid sl)
    <> "/"
    <> show fid
    <> "?pixelwidth="
    <> show width
    )
  showsPrec _ (LinkDocumentPreview did Nothing fid width) =
    (<>) ("/preview/" <> show did <> "/" <> show fid <> "?pixelwidth=" <> show width)
  showsPrec _ (LinkOAuthAuthorization token) =
    (<>) ("/oauth/authorization?oauth_token=" <> show token)
  showsPrec _ (LinkOAuthCallback url token (Just verifier)) = (<>)
    ( show
    $ setParams url [("oauth_token", showt token), ("oauth_verifier", showt verifier)]
    )
  showsPrec _ (LinkOAuthCallback url token Nothing) =
    (<>) (show $ setParams url [("oauth_token", showt token), ("denied", "true")])
  showsPrec _ (LinkDesignView         ) = (<>) "/newdocument"
  showsPrec _ (LinkPadList            ) = (<>) "/to-sign"
  showsPrec _ (LinkExternal s         ) = (<>) (T.unpack s)
  showsPrec _ (LinkPreviewLockedImage ) = (<>) "/img/preview_locked.png"
  showsPrec _ (LinkPermanentRedirect s) = (<>) (T.unpack s)
  showsPrec _ (LinkTemplateShareableLink did mh) =
    (<>) ("/t/" <> show did <> "/" <> show mh)
  showsPrec _ (LinkPortalInviteWithAccount portalUrl email) = (<>)
    (T.unpack portalUrl <> "/portal_invite/exists?email=" <> urlEncode
      (T.unpack $ unEmail email)
    )
  showsPrec _ (LinkPortalInviteWithoutAccount portalUrl email token utctime) = (<>)
    (  T.unpack portalUrl
    <> "/portal_invite/new?email="
    <> urlEncode (T.unpack $ unEmail email)
    <> "&token="
    <> show token
    <> "&expires="
    <> urlEncode (formatTimeISO utctime)
    )

setParams :: URI -> [(Text, Text)] -> URI
setParams uri params = uri { uriQuery = "?" <> vars }
  where
    params' :: [(String, String)]
    params' = fmap (\(t1, t2) -> (T.unpack t1, T.unpack t2)) params

    mvars   = urlDecodeVars $ uriQuery uri
    vars    = urlEncodeVars $ maybe params' (<> params') mvars
    urlDecodeVars :: String -> Maybe [(String, String)]
    urlDecodeVars ('?' : s) = urlDecodeVars s
    urlDecodeVars s         = makeKV (splitOn "&" s) []
      where
        makeKV []        a = Just a
        makeKV (kv : ks) a = case break (== '=') kv of
          (k, '=' : v) -> makeKV ks ((urlDecode k, urlDecode v) : a)
          (k, ""     ) -> makeKV ks ((urlDecode k, "") : a)
          _            -> Nothing

getHomeOrDesignViewLink :: KontraMonad m => m KontraLink
getHomeOrDesignViewLink = do
  ctx <- getContext
  case ctx ^. #maybeUser of
    Just _  -> return LinkDesignView
    Nothing -> return $ LinkHome (ctx ^. #lang)
