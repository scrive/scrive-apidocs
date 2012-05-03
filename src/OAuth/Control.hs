module OAuth.Control where

import Kontra
import API.Monad
import Routing
import OAuth.Model
import DB
import Misc
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


import Happstack.Server.RqData
import Control.Monad.Trans
import Control.Applicative
import Happstack.Server.Types
import Data.Maybe
import Control.Monad.Error

--import qualified Log

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
  dir "oauth" $ dir "dashboard"            $ hGet  $ toK0 $ apiDashboard
  ]

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
        Just user -> do
          let email = getEmail user
          Right <$> pagePrivilegesConfirm       (p:ps) email companyname token
        _ ->
          Right <$> pagePrivilegesConfirmNoUser (p:ps) companyname token
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
    Nothing ->
      return $ LinkOAuthAuthorization token
    Just user -> do
      (url, verifier) <- guardJustM $ dbUpdate $ VerifyCredentials token (userid user) time
      return $ LinkOAuthCallback url token $ Just verifier

authorizationGrantedNewUser :: Kontrakcja m => m KontraLink
authorizationGrantedNewUser = do
  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk
  _ <- signupPagePost
  -- flash message one by signup page
  
  return $ LinkOAuthAuthorization token

tokenCredRequest :: Kontrakcja m => m Response
tokenCredRequest = api $ do
  time <- ctxtime <$> getContext
  etr <- lift $ getTokenRequest
  case etr of
    Left errors -> throwError $ badInput errors
    Right tr -> do
      (accesstoken, accesssecret) <- apiGuardL' $ dbUpdate $ RequestAccessToken (trAPIToken tr) (trAPISecret tr) (trTempToken tr) (trTempSecret tr) (trVerifier tr) time
      return $ FormEncoded [("oauth_token",        show accesstoken)
                           ,("oauth_token_secret", show accesssecret)
                           ]
    
apiDashboard :: Kontrakcja m => m (Either KontraLink Response)
apiDashboard = checkUserTOSGet $ do
  Just user <- ctxmaybeuser <$> getContext -- safe because we check user
  apitokens      <- dbQuery $ GetAPITokensForUser  (userid user)
  apiprivileges  <- dbQuery $ GetGrantedPrivileges (userid user)
  mpersonaltoken <- dbQuery $ GetPersonalToken     (userid user)
  showAPIDashboard user apitokens apiprivileges mpersonaltoken >>= renderFromBody kontrakcja
  
createAPIToken :: Kontrakcja m => m KontraLink
createAPIToken = withUserPost $ do
  Just user <- ctxmaybeuser <$> getContext -- safe because we check user
  _success <- dbUpdate $ CreateAPIToken (userid user)
  return LinkOAuthDashboard
  
deleteAPIToken :: Kontrakcja m => m KontraLink
deleteAPIToken = withUserPost $ do
  Just user <- ctxmaybeuser <$> getContext -- safe because we check user
  mtk <- getDataFn' (look "apitoken")
  case maybeRead =<< mtk of
    Nothing -> return LinkOAuthDashboard
    Just token -> do
      _success <- dbUpdate $ DeleteAPIToken (userid user) token
      return LinkOAuthDashboard
      
createPersonalToken :: Kontrakcja m => m KontraLink
createPersonalToken = withUserPost $ do
  Just user <- ctxmaybeuser <$> getContext -- safe because we check user
  _success <- dbUpdate $ CreatePersonalToken (userid user)
  return LinkOAuthDashboard
  
deletePersonalToken :: Kontrakcja m => m KontraLink
deletePersonalToken = withUserPost $ do
  Just user <- ctxmaybeuser <$> getContext -- safe because we check user
  _success <- dbUpdate $ DeletePersonalToken (userid user)
  return LinkOAuthDashboard

deletePrivilege :: Kontrakcja m => m KontraLink
deletePrivilege = withUserPost $ do
  Just user <- ctxmaybeuser <$> getContext -- safe because we check user
  mtk <- getDataFn' (look "tokenid")
  case maybeRead =<< mtk of
    Nothing -> return ()
    Just tokenid -> do
      mpr <- getDataFn' (look "privilege")
      case maybeRead =<< mpr of
        Nothing -> ignore $ dbUpdate $ DeletePrivileges (userid user) tokenid
        Just pr -> ignore $ dbUpdate $ DeletePrivilege  (userid user) tokenid pr
  return LinkOAuthDashboard
