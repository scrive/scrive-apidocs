module API.V2.User (
    getAPIUser
  , getMaybeAPIUser
  , getAPIUserWithAnyPrivileges
  , getAPIUserWithPrivileges
  , getAPIUserWithPad
  , getAPIUserWithAPIPersonal
) where

import Happstack.Server
import Log

import API.V2.Errors
import API.V2.MonadUtils
import Cookies
import Doc.Model ()
import Kontra
import OAuth.Model
import OAuth.Util
import Session.Cookies
import User.Model
import Util.Actor

-- | Same as `getAPIUserWithPrivileges` but for only one `APIPrivilege`
getAPIUser :: Kontrakcja m => APIPrivilege -> m (User, Actor)
getAPIUser priv = getAPIUserWithPrivileges [priv]

getMaybeAPIUser :: Kontrakcja m => APIPrivilege -> m (Maybe (User, Actor))
getMaybeAPIUser priv = getMaybeAPIUserWith (view #maybeUser) [priv]

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
getAPIUserWithPrivileges = getAPIUserWith (view #maybeUser)

getAPIUserWithPad :: Kontrakcja m => APIPrivilege -> m (User, Actor)
getAPIUserWithPad priv = getAPIUserWith contextUser [priv]

getMaybeAPIUserWith
  :: Kontrakcja m => (Context -> Maybe User) -> [APIPrivilege] -> m (Maybe (User, Actor))
getMaybeAPIUserWith ctxUser privs = do
  moauthuser <- getOAuthUser privs
  case moauthuser of
    Just (Left  msg          ) -> apiError $ invalidAuthorizationWithMsg msg
    Just (Right (user, actor)) -> return $ Just (user, actor)
    Nothing                    -> do
      ctx <- getContext
      case ctxUser ctx of
        Nothing   -> return Nothing
        Just user -> return $ Just (user, authorActor ctx user)

-- * Internal functions
getAPIUserWith
  :: Kontrakcja m => (Context -> Maybe User) -> [APIPrivilege] -> m (User, Actor)
getAPIUserWith ctxUser privs = do
  mApiOrSessionUser <- getMaybeAPIUserWith ctxUser privs
  case mApiOrSessionUser of
    Just (user, actor) -> return (user, actor)
    Nothing            -> do
      sesids  <- lookCookieValues cookieNameSessionID . rqHeaders <$> askRq
      auth    <- lookCookieValues "authorization" . rqHeaders <$> askRq
      xtoken  <- lookCookieValues cookieNameXToken . rqHeaders <$> askRq
      cookies <- lookCookieNames . rqHeaders <$> askRq
      logInfo "Could not find user session" $ object
        [ "session_id_cookies" .= sesids
        , "authorization" .= auth
        , "cookie names" .= show cookies
        , "xtoken" .= xtoken
        ]
      apiError invalidAuthorization

getAPIUserWithAPIPersonal :: Kontrakcja m => m User
getAPIUserWithAPIPersonal = do
  fst <$> getAPIUser APIPersonal
