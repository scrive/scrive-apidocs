module API.V2.User (
    getAPIUser
  , getAPIUserWithAnyPrivileges
  , getAPIUserWithPad
  , getMagicHashForSignatoryAction
  , getDocumentSignatoryMagicHash
) where

import Happstack.Server
import Log

import API.V2.Errors
import API.V2.MonadUtils
import Cookies
import DB
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import Kontra
import MagicHash (MagicHash)
import OAuth.Model
import OAuth.Util
import Session.Cookies
import User.Model
import Util.Actor
import Util.SignatoryLinkUtils

-- | Same as `getAPIUserWithPrivileges` but for only one `APIPrivilege`
getAPIUser :: Kontrakcja m => APIPrivilege -> m (User, Actor)
getAPIUser priv = getAPIUserWithPrivileges [priv]

-- | Get the User and Actor for the API, as long as any privileges are granted
-- Same behaviour as `getAPIUserWithPrivileges`
getAPIUserWithAnyPrivileges :: Kontrakcja m => m (User, Actor)
getAPIUserWithAnyPrivileges = getAPIUserWithPrivileges allPrivileges

-- | Get the User and Actor for a API call
-- Either through:
-- 1. OAuth using the Authorization header
-- 2. Session for AJAX client (only if the Authorization header is empty)
--
-- Only returns if *any* of the privileges in privs are issued.
getAPIUserWithPrivileges :: Kontrakcja m => [APIPrivilege] -> m (User, Actor)
getAPIUserWithPrivileges privs = getAPIUserWith (get ctxmaybeuser) privs

getAPIUserWithPad :: Kontrakcja m => APIPrivilege -> m (User, Actor)
getAPIUserWithPad priv = getAPIUserWith getContextUser [priv]

-- | Get the `MagicHash` from the session for the `DocumentID` and
-- `SignatoryLinkID`
--
-- If no matching session then try with `getApiUser APIPersonal` and get the
-- signatory's `MagicHash` using `getMagicHashForDocumentSignatoryWithUser`
-- that checks if the User is the author
--
-- Used to return `m (MagicHash, Maybe User)` but we don't seem to need the
-- `User`, can be reintroduced if really needed
getMagicHashForSignatoryAction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m MagicHash
getMagicHashForSignatoryAction did slid = do
  mSessionMagicHash <- dbQuery $ GetDocumentSessionToken slid
  case mSessionMagicHash of
    Just mh -> return mh
    Nothing -> do
      (user, _) <- getAPIUser APIPersonal
      mUserMagicHash <- getMagicHashForDocumentSignatoryWithUser did slid user
      case mUserMagicHash of
        Nothing -> apiError documentActionForbidden
        Just mh -> return mh

-- | Get the `SignatoryLink` based on document session token for the given
-- `DocumentID` and `SignatoryLinkID`
--
-- Will give a `Nothing` if there is no matching session
getDocumentSignatoryMagicHash :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m (Maybe SignatoryLink)
getDocumentSignatoryMagicHash did slid = do
  mMagicHash <- dbQuery $ GetDocumentSessionToken slid
  case (mMagicHash) of
    Nothing -> return Nothing
    Just mh -> withDocumentID did $ do
      sl <- apiGuardJustM (documentNotFound did) $ getSigLinkFor slid <$> theDocument
      if mh == signatorymagichash sl
          then return $ Just sl
          else apiError $ documentActionForbidden

-- * Internal functions

getAPIUserWith :: Kontrakcja m => (Context -> Maybe User) -> [APIPrivilege] -> m (User, Actor)
getAPIUserWith ctxUser privs = do
  moauthuser <- getOAuthUser privs
  case moauthuser of
    Just (Left msg) -> apiError $ invalidAuthorizationWithMsg msg
    Just (Right (user, actor)) -> return (user, actor)
    Nothing -> do
      msessionuser <- do
        ctx <- getContext
        case ctxUser ctx of
          Nothing -> return Nothing
          Just user -> return $ Just (user, authorActor ctx user)
      case msessionuser of
        Just (user, actor) -> return (user, actor)
        Nothing -> do
          sesids <- (lookCookieValues cookieNameSessionID . rqHeaders) <$> askRq
          auth <- (lookCookieValues "authorization" . rqHeaders) <$> askRq
          cookies <- (lookCookieNames . rqHeaders) <$> askRq
          logInfo "Could not find user session" $ object [ "session_id_cookies" .= sesids
                                                         , "authorization" .= auth
                                                         , "cookie names" .= show cookies
                                                         ]
          apiError $ invalidAuthorization
