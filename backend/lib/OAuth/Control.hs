{-# LANGUAGE ExtendedDefaultRules #-}
module OAuth.Control(oauth) where

import Control.Monad.Catch
import Data.Aeson
import Data.Map (singleton)
import Happstack.Server.RqData
import Happstack.Server.Types
import Happstack.StaticRouting (Route, choice, dir)
import Log
import Network.HTTP.Base (urlEncodeVars)
import Text.JSON
import qualified Happstack.Server.Response as Web
import qualified Text.JSON.Gen as J

import API.Monad.V1
import DB
import Happstack.Fields
import Kontra
import KontraLink
import KontraPrelude
import OAuth.Model
import OAuth.Util
import OAuth.View
import Routing
import User.Model
import Util.MonadUtils
import Utils.List

oauth :: Route (Kontra Response)
oauth = choice [
  dir "oauth" $ dir "temporarycredentials" $ hGet  $ toK0 $ tempCredRequest,
  dir "oauth" $ dir "authorization"        $ hGet  $ toK0 $ authorization,
  dir "oauth" $ dir "authorizationconfirm" $ hPost $ toK0 $ authorizationGranted,
  dir "oauth" $ dir "authorizationdeny"    $ hPost $ toK0 $ authorizationDenied,
  dir "oauth" $ dir "tokencredentials"     $ hGet  $ toK0 $ tokenCredRequest,
  dir "oauth" $ dir "createapitoken"       $ hPost $ toK0 $ createAPIToken,
  dir "oauth" $ dir "deleteapitoken"       $ hPost $ toK0 $ deleteAPIToken,
  dir "oauth" $ dir "createpersonaltoken"  $ hPost $ toK0 $ createPersonalToken,
  dir "oauth" $ dir "deletepersonaltoken"  $ hPost $ toK0 $ deletePersonalToken,
  dir "oauth" $ dir "deleteprivilege"      $ hPost $ toK0 $ deletePrivilege,
  dir "oauth" $ dir "dashboard" $ dir "personaltoken"     $ hGet $ toK0 $ apiDashboardPersonalTokens,
  dir "oauth" $ dir "dashboard" $ dir "apitokens"         $ hGet $ toK0 $ apiDashboardAPITokens,
  dir "oauth" $ dir "dashboard" $ dir "grantedprivileges" $ hGet $ toK0 $ apiDashboardGrantedPrivileges
  ]

-- OAuth Flow

tempCredRequest :: Kontrakcja m => m Response
tempCredRequest = api $ do
  time <- ctxtime <$> getContext

  etcr <- getTempCredRequest
  case etcr of
    Left errors -> (throwM . SomeDBExtraException) $ badInput errors
    Right tcr -> do
      logInfo "TempCredRequest got successfully" $ Log.object [
          "temp_cred_request" .= show tcr
        ]
      (temptoken, tempsecret) <- apiGuardL' $ dbUpdate $ RequestTempCredentials tcr time
      return $ setHeader "Content-Type" "application/x-www-form-urlencoded" $
                  Web.toResponse $ urlEncodeVars [
                    ("oauth_token", show temptoken),
                    ("oauth_token_secret", show tempsecret),
                    ("oauth_callback_confirmed", "true")
                  ]

authorization :: Kontrakcja m => m (Either KontraLink Response)
authorization = do
  ctx <- getContext
  time   <- ctxtime      <$> getContext
  lang <- ctxlang        <$> getContext

  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk

  mprivs <- dbQuery $ GetRequestedPrivileges token time

  case mprivs of
    Just (companyname, p:ps) -> Right <$> pagePrivilegesConfirm ctx (p:ps) companyname token
    _ -> return $ Left $ LinkHome lang -- no privileges recorded? we just take the traffic


authorizationDenied :: Kontrakcja m => m KontraLink
authorizationDenied = do
  time   <- ctxtime <$> getContext
  lang   <- ctxlang <$> getContext
  mtk <- getDataFn' (look "oauth_token")
  token <- guardJust $ maybeRead =<< mtk
  murl <- dbUpdate $ DenyCredentials token time
  case murl of
    Nothing -> return $ LinkHome lang
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

tokenCredRequest :: Kontrakcja m => m Response
tokenCredRequest = api $ do
  time <- ctxtime <$> getContext
  etr <- getTokenRequest
  case etr of
    Left errors -> (throwM . SomeDBExtraException) $ badInput errors
    Right tr -> do
      (accesstoken, accesssecret) <- apiGuardL' $ dbUpdate $ RequestAccessToken tr time
      return $ setHeader "Content-Type" "application/x-www-form-urlencoded" $
                  Web.toResponse $ urlEncodeVars [
                    ("oauth_token",        show accesstoken),
                    ("oauth_token_secret", show accesssecret)
                  ]

apiDashboardPersonalTokens :: Kontrakcja m => m JSValue
apiDashboardPersonalTokens = do
  Context{..} <- getContext
  user <- guardJust ctxmaybeuser
  ls <- map jsonFromPersonalToken <$> maybeToList <$> (dbQuery $ GetPersonalToken (userid user))
  return $ J.runJSONGen $ do
    J.value "personal_tokens" $ ls

apiDashboardAPITokens :: Kontrakcja m => m JSValue
apiDashboardAPITokens = do
  Context{..} <- getContext
  user <- guardJust ctxmaybeuser
  ls <- map jsonFromAPIToken <$> (dbQuery $ GetAPITokensForUser (userid user))
  return $ J.runJSONGen $ do
    J.value "api_tokens" $  ls

apiDashboardGrantedPrivileges :: Kontrakcja m => m JSValue
apiDashboardGrantedPrivileges = do
  Context{..} <- getContext
  user <- guardJust ctxmaybeuser
  ds <- mapKeepM privilegeDescription [APIDocCreate, APIDocSend, APIDocCheck]
  ls <- concatMap (\p->jsonFromGrantedPrivilege p ds) <$> (dbQuery $ GetGrantedPrivileges (userid user))
  return $ J.runJSONGen $ do
    J.value "granted_privileges" $ ls

-- Manipulate dashboard stuff

success :: JSValue
success = J.toJSValue $ singleton ("status" :: String) ("success" :: String)

createAPIToken :: Kontrakcja m => m JSValue
createAPIToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  _success <- dbUpdate $ CreateAPIToken (userid user)
  return success

deleteAPIToken :: Kontrakcja m => m JSValue
deleteAPIToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  mtk <- getDataFn' (look "apitoken")
  case maybeRead =<< mtk of
    Nothing -> return ()
    Just token -> void $ dbUpdate $ DeleteAPIToken (userid user) token
  return success

createPersonalToken :: Kontrakcja m => m JSValue
createPersonalToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  _success <- dbUpdate $ CreatePersonalToken (userid user)
  return success

deletePersonalToken :: Kontrakcja m => m JSValue
deletePersonalToken = do
  muser <- ctxmaybeuser <$> getContext
  user <- guardJust muser
  _success <- dbUpdate $ DeletePersonalToken (userid user)
  return success

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
  return success
