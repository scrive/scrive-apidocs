module Auth.Model 
       (
         GetAccessTokenAuthorization(..),
         GetGrantedPrivileges(..),
         GetAPITokens(..),
         CreateAPIToken(..),
         CreateAccessToken(..),
         GrantPrivilege(..),
         RevokePrivilege(..),
         RevokeAllPrivileges(..),
         NewAPISecret(..)
       )
       
       where

import Database.HDBC hiding (originalQuery)

import DB.Derive

import Auth.Internal
import DB.Utils
import DB.Classes
import Data.Maybe
import Misc
import MinutesTime
import User.Model
import Auth.Tables
import Control.Applicative
import Random
import Data.List

import qualified Control.Exception as E



-- | Given an AccessToken, return an Authorization representing those privileges
data GetAccessTokenAuthorization = GetAccessTokenAuthorization AccessToken
instance DBQuery GetAccessTokenAuthorization (Maybe AccessTokenAuthorization) where
  dbQuery (GetAccessTokenAuthorization tok) = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT at.expires, at.user_id, au.privilege FROM auth_access_token at, auth_authorization au WHERE at.user_id = au.user_id AND at.api_token = au.api_token AND at.access_token = ?"
    _ <- execute st [toSql tok]
    packageAccessTokenAuthorization st
    
{-- | Users my grant privileges to other users through their
      API Tokens
  
      What privileges have been granted to which users?

      Given a UserID Joe, what other UserID's have privileges to
      act on behalf of Joe; and what are those Privileges?
--}
data GetGrantedPrivileges = GetGrantedPrivileges UserID
instance DBQuery GetGrantedPrivileges [(UserID, [Privilege])] where
  dbQuery (GetGrantedPrivileges uid) = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT ap.user_id, au.privilege FROM auth_api_token ap, auth_authorization au WHERE ap.api_token = au.api_token AND au.user_id = ?"
    _ <- execute st [toSql uid]
    packagePrivileges st
    
-- | Each User has a collection of APITokens he can access the API with.
--   Returns the APITokens for a specific user.
data GetAPITokens = GetAPITokens UserID
instance DBQuery GetAPITokens [(APIToken, APISecret, APITokenStatus)] where
  dbQuery (GetAPITokens uid) = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT api_token, api_secret, status FROM auth_api_token WHERE user_id = ?"
    _ <- execute st [toSql uid]
    fetchAPITokens st

-- | Create a new APIToken for a UserID; Returns the APIToken and APISecret
data CreateAPIToken = CreateAPIToken UserID
instance DBUpdate CreateAPIToken (APIToken, APISecret) where
  dbUpdate (CreateAPIToken uid) = do
    wrapDB $ \conn -> runRaw conn "LOCK TABLE auth_api_token IN ACCESS EXCLUSIVE MODE"
    apitoken <- APIToken  <$> getUniqueIDField tableAPIToken "api_token"
    secret   <- APISecret <$> (wrapDB $ \_ -> randomRIO (0, maxBound))
    n <- wrapDB $ \conn ->
      run conn "INSERT INTO auth_api_token (api_token, user_id, api_secret, status) VALUES (?, ?, ?, ?)"
      [toSql apitoken,
       toSql uid,
       toSql secret,
       toSql $ fromSafeEnum APITokenActive]
    case n of
      1 -> return (apitoken, secret)
      _ -> E.throw TooManyObjects { originalQuery = "",
                                    tmoExpected = 1,
                                    tmoGiven = n }
      
-- | Create a new APISecret for an APIToken; Returns the APIToken and APISecret
data NewAPISecret = NewAPISecret APIToken
instance DBUpdate NewAPISecret (APIToken, APISecret) where
  dbUpdate (NewAPISecret tok) = do
    secret <- APISecret <$> (wrapDB $ \_ -> randomRIO (0, maxBound))
    n <- wrapDB $ \conn ->
      run conn "UPDATE auth_api_token set api_secret = ?"
      [toSql secret]
    case n of
      1 -> return (tok, secret)
      _ -> E.throw TooManyObjects { originalQuery = "",
                                    tmoExpected = 1,
                                    tmoGiven = n }

-- | Create a limited-time access token; destroys all previous tokens
-- for the apitoken/userid pair
data CreateAccessToken = CreateAccessToken APIToken UserID MinutesTime
instance DBUpdate CreateAccessToken AccessToken where
  dbUpdate (CreateAccessToken token uid expires) = do
    wrapDB $ \conn -> runRaw conn "LOCK TABLE auth_access_token IN ACCESS EXCLUSIVE MODE"
    wrapDB $ \conn -> do
      _ <- run conn "DELETE FROM auth_access_token WHERE user_id = ? AND api_token = ? AND status = ?"
           [toSql uid,
            toSql token,
            toSql $ fromSafeEnum APITokenActive]
      return ()
    accesstoken <- AccessToken <$> getUniqueIDField tableAccessToken "access_token"
    wrapDB $ \conn -> do
      n <- run conn ("INSERT INTO auth_access_token (access_token, user_id, api_token, expires)"
                     ++ " VALUES (?, ?, ?, ?)" )
           [toSql accesstoken,
            toSql uid,
            toSql token,
            toSql expires]
      case n of
        1 -> return accesstoken
        _ -> E.throw TooManyObjects { originalQuery = "",
                                      tmoExpected = 1,
                                      tmoGiven = n }
      
-- | An User may grant privileges to a specific APIToken
data GrantPrivilege = GrantPrivilege APIToken UserID Privilege
instance DBUpdate GrantPrivilege Bool where
  dbUpdate (GrantPrivilege token uid priv) = wrapDB $ \conn -> do
    runRaw conn "LOCK TABLE auth_authorization IN ACCESS EXCLUSIVE MODE"
    st <- prepare conn $ "SELECT au.api_token, au.user_id, au.privilege FROM auth_authorization au WHERE au.user_id = ? AND au.api_token = ? AND au.privilege = ?"
    _ <- execute st [toSql token,
                     toSql uid,
                     toSql $ fromSafeEnum priv]
    privs <- fetchPrivs st
    case privs of
      [] -> do        
        st2 <- prepare conn $ "INSERT INTO auth_authorization ("
               ++ "  user_id"
               ++ ", api_token"
               ++ ", privilege"
               ++ ") VALUES (?, ?, ?)"
        r <- execute st2 [toSql $ uid
                         ,toSql $ token
                         ,toSql $ fromSafeEnum priv]
        oneRowAffectedGuard r
      _ -> return False
      
-- | Revoke a privilege from a User
-- the first UserID is the one who was granted access
-- the second UserID is the owner of the resource
data RevokePrivilege = RevokePrivilege UserID UserID Privilege
instance DBUpdate RevokePrivilege Bool where
  dbUpdate (RevokePrivilege uidClient uidOwner priv) = wrapDB $ \conn -> do
    oneRowAffectedGuard =<< run conn "DELETE FROM auth_authorization au WHERE au.user_id = ? AND au.privilege = ? AND au.api_token in (SELECT ap.api_token FROM auth_api_token ap WHERE ap.user_id = ?)"
      [toSql uidOwner,
       toSql $ fromSafeEnum priv,
       toSql uidClient]

-- | Revoke all privileges from a User
-- the first UserID is the one who was granted access
-- the second UserID is the owner of the resource
data RevokeAllPrivileges = RevokeAllPrivileges UserID UserID
instance DBUpdate RevokeAllPrivileges Bool where
  dbUpdate (RevokeAllPrivileges uidClient uidOwner) = wrapDB $ \conn -> do
    n <- run conn "DELETE FROM auth_authorization au WHERE au.user_id = ? AND au.api_token in (SELECT ap.api_token FROM auth_api_token ap WHERE ap.user_id = ?)"
         [toSql uidOwner,
          toSql uidClient]
    case n of
      0 -> return False
      _ -> return True

fetchPrivs :: Statement -> IO [(APIToken, UserID, Privilege)]
fetchPrivs st = do
  catMaybes <$> fetchValues st decoder
  where decoder tok uid pr = (do case toSafeEnum pr of                              
                                   Nothing -> return Nothing
                                   Just pr' -> return $ Just (tok, uid, pr')) :: Either DBException (Maybe (APIToken, UserID, Privilege))

fetchAPITokens :: Statement -> IO [(APIToken, APISecret, APITokenStatus)]
fetchAPITokens st = do
  catMaybes <$> fetchValues st decoder
  where decoder tok sec as = (do case toSafeEnum as of                              
                                   Nothing -> return Nothing
                                   Just as' -> return $ Just (tok, sec, as')) :: Either DBException (Maybe (APIToken, APISecret, APITokenStatus))

packageAccessTokenAuthorization :: Statement -> IO (Maybe AccessTokenAuthorization)
packageAccessTokenAuthorization st = do
  ps' <- fetchValues st decoder
  case ps' of
    [] -> return Nothing
    prs@((ex, uid, _):_) -> 
      return $ Just $ AccessTokenAuthorization ex uid (privs prs)
  where decoder ex uid pr = (return (ex, uid, pr)) :: Either DBException (MinutesTime, UserID, Integer)
        privs prs = catMaybes $ map (\(_,_,x) -> toSafeEnum x) prs

packagePrivileges :: Statement -> IO [(UserID, [Privilege])]
packagePrivileges st = do
  merge <$> groupBy (\(a,_) (b,_) -> a == b) <$> catMaybes <$> fetchValues st decoder
  where decoder uid pr = (do case toSafeEnum pr of
                               Nothing -> return Nothing
                               Just pr' -> return $ Just (uid, pr')) :: Either DBException (Maybe (UserID, Privilege))
        merge = map merge'
        merge' ps@((uid, _):_) = (uid, map snd ps)
        merge' [] = error "This is impossible"


$(newtypeDeriveConvertible ''AccessToken)
$(newtypeDeriveUnderlyingReadShow ''AccessToken)

$(newtypeDeriveConvertible ''APIToken)
$(newtypeDeriveUnderlyingReadShow ''APIToken)

$(newtypeDeriveConvertible ''APISecret)
$(newtypeDeriveUnderlyingReadShow ''APISecret)
