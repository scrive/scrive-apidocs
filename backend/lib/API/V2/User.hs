module API.V2.User (
    getAPIUser
  , getAPIUserWithAnyPrivileges
  , getAPIUserWithPrivileges
  , getAPIUserWithPad
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
getAPIUserWithPrivileges privs = getAPIUserWith (view #maybeUser) privs

getAPIUserWithPad :: Kontrakcja m => APIPrivilege -> m (User, Actor)
getAPIUserWithPad priv = getAPIUserWith contextUser [priv]


-- * Internal functions
getAPIUserWith
  :: Kontrakcja m => (Context -> Maybe User) -> [APIPrivilege] -> m (User, Actor)
getAPIUserWith ctxUser privs = do
  moauthuser <- getOAuthUser privs
  case moauthuser of
    Just (Left  msg          ) -> apiError $ invalidAuthorizationWithMsg msg
    Just (Right (user, actor)) -> return (user, actor)
    Nothing                    -> do
      msessionuser <- do
        ctx <- getContext
        case ctxUser ctx of
          Nothing   -> return Nothing
          Just user -> return $ Just (user, authorActor ctx user)
      case msessionuser of
        Just (user, actor) -> return (user, actor)
        Nothing            -> do
          sesids  <- (lookCookieValues cookieNameSessionID . rqHeaders) <$> askRq
          auth    <- (lookCookieValues "authorization" . rqHeaders) <$> askRq
          xtoken  <- (lookCookieValues cookieNameXToken . rqHeaders) <$> askRq
          cookies <- (lookCookieNames . rqHeaders) <$> askRq
          logInfo "Could not find user session" $ object
            [ "session_id_cookies" .= sesids
            , "authorization" .= auth
            , "cookie names" .= show cookies
            , "xtoken" .= xtoken
            ]
          apiError $ invalidAuthorization
