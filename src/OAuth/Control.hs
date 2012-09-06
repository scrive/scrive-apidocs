module OAuth.Control where

import Kontra
import API.Monad
import Routing
import OAuth.Model
import DB
import Happstack.Fields
import Utils.List
import Utils.Read
import KontraLink
import Happstack.StaticRouting(Route, choice, dir)
import User.Utils
import Util.HasSomeUserInfo
import Util.MonadUtils
import OAuth.View
import OAuth.Util
import Login
import User.Model
import AppView
import qualified Log
import Stats.Control
import Util.FlashUtil
import User.UserView
import ListUtil

import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import Text.JSON

import Happstack.Server.RqData
import Control.Monad.Trans
import Control.Applicative
import Happstack.Server.Types
import Data.Maybe
import Control.Monad.Error
import Data.Map (singleton)

oauthAPI :: Route (KontraPlus Response)
oauthAPI = choice [
  dir "oauth" $ dir "temporarycredentials" $ hGet  $ toK0 $ tempCredRequest,
  dir "oauth" $ dir "authorization"        $ hGet  $ toK0 $ authorization,
  dir "oauth" $ dir "authorizationconfirm" $ hPost $ toK0 $ authorizationGranted,
  dir "oauth" $ dir "authorizationconfirmlogin"   $ hPostNoXToken $ toK0 $ authorizationGrantedLogin,
  dir "oauth" $ dir "authorizationconfirmnewuser" $ hPostNoXToken $ toK0 $ authorizationGrantedNewUser,  
  dir "oauth" $ dir "authorizationdeny"    $ hPost $ toK0 $ authorizationDenied,
  dir "oauth" $ dir "tokencredentials"     $ hGet  $ toK0 $ tokenCredRequest,
  dir "oauth" $ dir "createapitoken"       $ hPost $ toK0 $ createAPIToken,
  dir "oauth" $ dir "deleteapitoken"       $ hPost $ toK0 $ deleteAPIToken,
  dir "oauth" $ dir "createpersonaltoken"  $ hPost $ toK0 $ createPersonalToken,
  dir "oauth" $ dir "deletepersonaltoken"  $ hPost $ toK0 $ deletePersonalToken,
  dir "oauth" $ dir "deleteprivilege"      $ hPost $ toK0 $ deletePrivilege,
  dir "oauth" $ dir "dashboard"            $ hGet  $ toK0 $ apiDashboard,
  dir "oauth" $ dir "dashboard" $ dir "personaltoken"     $ hGet $ toK0 $ apiDashboardPersonalTokens,
  dir "oauth" $ dir "dashboard" $ dir "apitokens"         $ hGet $ toK0 $ apiDashboardAPITokens,
  dir "oauth" $ dir "dashboard" $ dir "grantedprivileges" $ hGet $ toK0 $ apiDashboardGrantedPrivileges
  ]

-- OAuth Flow

tempCredRequest :: Kontrakcja m => m Response
tempCredRequest = api $ do
  time <- ctxtime <$> getContext

  etcr <- lift $ getTempCredRequest
  case etcr of
    Left errors -> throwError $ badInput errors
    Right tcr -> do
      Log.debug $ "TempCredRequest: " ++ show tcr
      (temptoken, tempsecret) <- apiGuardL' $ dbUpdate $ RequestTempCredentials tcr time
      return $ FormEncoded [("oauth_token", show temptoken),
                            ("oauth_token_secret", show tempsecret),
                            ("oauth_callback_confirmed", "true")]

authorization :: Kontrakcja m => m (Either KontraLink String)
authorization = do
  muser  <- ctxmaybeuser <$> getContext
  time   <- ctxtime      <$> getContext
  locale <- ctxlocale    <$> getContext

  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk
  
  mprivs <- dbQuery $ GetRequestedPrivileges token time

  case mprivs of
    Just (companyname, p:ps) ->
      case muser of
        Just user -> Right <$> pagePrivilegesConfirm       (p:ps) (getEmail user) companyname token
        _         -> Right <$> pagePrivilegesConfirmNoUser (p:ps) companyname token
    _ -> -- no privileges recorded? we just take the traffic
      return $ Left $ LinkHome locale

authorizationDenied :: Kontrakcja m => m KontraLink
authorizationDenied = do
  time   <- ctxtime <$> getContext
  locale <- ctxlocale <$> getContext
  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk
  murl <- dbUpdate $ DenyCredentials token time
  case murl of
    Nothing -> return $ LinkHome locale
    Just url -> do
      -- here we redirect to callback with denied=true
      return $ LinkOAuthCallback url token Nothing

authorizationGranted :: Kontrakcja m => m KontraLink
authorizationGranted = do
  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk
  muser <- ctxmaybeuser <$> getContext
  time <- ctxtime <$> getContext
  case muser of
    Nothing -> 
      return $ LinkOAuthAuthorization token
    Just user -> do
      (url, verifier) <- guardJustM $ dbUpdate $ VerifyCredentials token (userid user) time
      _ <- addUserStatAPIGrantAccess (userid user) time (usercompany user) Nothing 
      return $ LinkOAuthCallback url token $ Just verifier

authorizationGrantedLogin :: Kontrakcja m => m KontraLink
authorizationGrantedLogin = do
  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk
  muser' <- ctxmaybeuser <$> getContext
  when_ (isNothing muser') handleLoginPost
  muser <- ctxmaybeuser <$> getContext  
  time <- ctxtime <$> getContext
  case muser of
    Nothing -> do
      addFlashM $ flashMessageLoginRedirectReason $ InvalidLoginInfo undefined
      return $ LinkOAuthAuthorization token
    Just user -> do
      (url, verifier) <- guardJustM $ dbUpdate $ VerifyCredentials token (userid user) time
      _ <- addUserStatAPIGrantAccess (userid user) time (usercompany user) Nothing       
      return $ LinkOAuthCallback url token $ Just verifier

authorizationGrantedNewUser :: Kontrakcja m => m KontraLink
authorizationGrantedNewUser = do
  time <- ctxtime <$> getContext
  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk
  meu <- handleSignup
  case meu of
    Just (email, Just uid) -> do
      void $ addUserStatAPINewUser uid time Nothing Nothing 
      addFlashM $ modalUserSignupDone $ email
    _ -> return ()
  return $ LinkOAuthAuthorization token

tokenCredRequest :: Kontrakcja m => m Response
tokenCredRequest = api $ do
  time <- ctxtime <$> getContext
  etr <- lift $ getTokenRequest
  case etr of
    Left errors -> throwError $ badInput errors
    Right tr -> do
      (accesstoken, accesssecret) <- apiGuardL' $ dbUpdate $ RequestAccessToken tr time
      return $ FormEncoded [("oauth_token",        show accesstoken)
                           ,("oauth_token_secret", show accesssecret)
                           ]

-- Show API Dashboard
    
apiDashboard :: Kontrakcja m => m (Either KontraLink Response)
apiDashboard = checkUserTOSGet $ do
  Just user <- ctxmaybeuser <$> getContext -- safe because we check user
  showAPIDashboard user >>= renderFromBody kontrakcja

apiDashboardPersonalTokens :: Kontrakcja m => m JSValue
apiDashboardPersonalTokens = do
  Context{..} <- getContext
  user <- guardJust ctxmaybeuser
  ls <- map jsonFromPersonalToken <$> maybeToList <$> (dbQuery $ GetPersonalToken (userid user))
  return $ runJSONGen $ do
    J.objects "list" $ map (J.value "fields") ls
    J.value "paging" $ pagingParamsJSON $ PagedList {list = ls, pageSize = 100, params = emptyListParams}
  
apiDashboardAPITokens :: Kontrakcja m => m JSValue
apiDashboardAPITokens = do
  Context{..} <- getContext
  user <- guardJust ctxmaybeuser
  ls <- map jsonFromAPIToken <$> (dbQuery $ GetAPITokensForUser (userid user))
  return $ runJSONGen $ do
    J.objects "list" $ map (J.value "fields") ls
    J.value "paging" $ pagingParamsJSON $ PagedList {list = ls, pageSize = 100, params = emptyListParams}
      
apiDashboardGrantedPrivileges :: Kontrakcja m => m JSValue
apiDashboardGrantedPrivileges = do
  Context{..} <- getContext
  user <- guardJust ctxmaybeuser
  ds <- mapassocM privilegeDescription [APIDocCreate]
  ls <- concatMap (\p->jsonFromGrantedPrivilege p ds) <$> (dbQuery $ GetGrantedPrivileges (userid user))
  return $ runJSONGen $ do
    J.objects "list" $ map (J.value "fields") ls
    J.value "paging" $ pagingParamsJSON $ PagedList {list = ls, pageSize = 100, params = emptyListParams}

-- Manipulate dashboard stuff

createAPIToken :: Kontrakcja m => m JSValue
createAPIToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  _success <- dbUpdate $ CreateAPIToken (userid user)
  return $ toJSValue $ singleton "status" "success"
  
deleteAPIToken :: Kontrakcja m => m JSValue
deleteAPIToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  mtk <- getDataFn' (look "apitoken")
  case maybeRead =<< mtk of
    Nothing -> return ()
    Just token -> void $ dbUpdate $ DeleteAPIToken (userid user) token
  return $ toJSValue $ singleton "status" "success"
      
createPersonalToken :: Kontrakcja m => m JSValue
createPersonalToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  _success <- dbUpdate $ CreatePersonalToken (userid user)
  return $ toJSValue $ singleton "status" "success"
  
deletePersonalToken :: Kontrakcja m => m JSValue
deletePersonalToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  _success <- dbUpdate $ DeletePersonalToken (userid user)
  return $ toJSValue $ singleton "status" "success"

deletePrivilege :: Kontrakcja m => m JSValue
deletePrivilege = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  mtk <- getDataFn' (look "tokenid")
  case maybeRead =<< mtk of
    Nothing -> return ()
    Just tokenid -> do
      mpr <- getDataFn' (look "privilege")
      case maybeRead =<< mpr of
        Nothing -> void $ dbUpdate $ DeletePrivileges (userid user) tokenid
        Just pr -> void $ dbUpdate $ DeletePrivilege  (userid user) tokenid pr
  return $ toJSValue $ singleton "status" "success"
