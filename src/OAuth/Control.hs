module OAuth.Control where

import Kontra
import API.Monad
import Routing
import OAuth.Model
import DB.Classes
import Misc
import KontraLink
import Redirect
import Happstack.StaticRouting(Route, choice, dir)
import Util.HasSomeUserInfo
import Util.MonadUtils
import OAuth.View
import OAuth.Parse

import Happstack.Server.RqData
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Codec.Binary.UTF8.String as UTF
import qualified Codec.Binary.Url as URL
import Control.Monad.Trans
import Control.Applicative
--import Happstack.Server.RqData
import Happstack.Server.Types
import Happstack.Server.Monads
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Error
import Network.URI

import qualified Log

oauthAPI :: Route (Kontra Response)
oauthAPI = choice [
  dir "oauth" $ dir "temporarycredentials" $ hGet $ toK0 $ tempCredRequest,
  dir "oauth" $ dir "authorization" $ hGet $ toK0 $ authorization,
  dir "oauth" $ dir "authorizationconfirm" $ hPost $ toK0 $ authorizationGranted,
  dir "oauth" $ dir "authorizationdeny" $ hPost $ toK0 $ authorizationDenied,
  dir "oauth" $ dir "tokencredentials" $ hGet $ toK0 $ tokenCredRequest
  ]

tempCredRequest :: Kontrakcja m => m Response
tempCredRequest = api $ do
  time <- ctxtime <$> getContext
  rq <- lift askRq
  let headers = rqHeaders rq
  Log.debug $ "Got headers: " ++ show headers

  HeaderPair _ auths <- apiGuard' BadInput $ Map.lookup (BS.fromString "authorization") headers

  Log.debug $ "Got Authorization: headers: " ++ show auths

  auth <- apiGuard' BadInput $ BS.toString <$> listToMaybe auths

  -- pull the data out of Authorization
  let params = splitAuthorization auth

  Log.debug $ "Split Authorization header into params: " ++ show params

  sigtype <- apiGuard' BadInput $ maybeRead =<< lookup "oauth_signature_method" params
  when (sigtype /= "PLAINTEXT") $ throwError BadInput

  (mapisecret, _) <- apiGuard' BadInput $ splitSignature =<< maybeRead =<< lookup "oauth_signature" params
  apisecret <- apiGuard' Forbidden mapisecret

  Log.debug $ "Got api secret: " ++ show apisecret
     
  callback <- apiGuard' BadInput $ UTF.decode <$> (URL.decode =<< maybeRead =<< lookup "oauth_callback" params)

  Log.debug $ "Got callback: " ++ show callback

  apitoken <- apiGuard' BadInput $ maybeRead =<< maybeRead =<< lookup "oauth_consumer_key" params

  Log.debug $ "Got token: " ++ show apitoken

  privilegesstring <- apiGuardL' BadInput $ getDataFn' (look "privileges")

  privileges <- apiGuard' BadInput $ readPrivileges privilegesstring

  Log.debug $ "Got privileges: " ++ show privileges

  email <- apiGuardL' BadInput $ getDataFn' (look "useremail")

  Log.debug $ "Got email: " ++ show email

  (temptoken, tempsecret) <- apiGuardL' Forbidden $ runDBUpdate $ RequestTempCredentials apitoken apisecret email privileges callback time

  Log.debug $ "Got temp stuff: " ++ show (temptoken, tempsecret)

  return $ FormEncoded [("oauth_token", show temptoken),
                        ("oauth_token_secret", show tempsecret),
                        ("oauth_callback_confirmed", "true")]

authorization :: Kontrakcja m => m (Either KontraLink String)
authorization = do
  muser <- ctxmaybeuser <$> getContext
  time <- ctxtime <$> getContext
  locale <- ctxlocale <$> getContext
  case muser of
    Nothing -> return $ Left $ LinkLogin locale NotLogged
    Just user -> do
      let email = BS.toString $ getEmail user
      mtk <- getDataFn' (look "oauth_token")
      token <- guardJust $ maybeRead =<< mtk

      Log.debug $ "token: " ++ show token

      privileges <- runDBQuery $ GetRequestedPrivileges token email time

      case privileges of
        [] -> return $ Left $ LinkHome locale
        _  -> Right <$> pagePrivilegesConfirm privileges "Scrive, Inc." token

authorizationDenied :: Kontrakcja m => m Response
authorizationDenied = do
  muser  <- ctxmaybeuser <$> getContext
  time   <- ctxtime <$> getContext
  locale <- ctxlocale <$> getContext
  case muser of
    Nothing -> sendRedirect $ LinkLogin locale NotLogged
    Just user -> do
      let email = BS.toString $ getEmail user
      mtk <- getDataFn' (look "oauth_token")
      token <- guardJust $ maybeRead =<< mtk

      Log.debug $ "token: " ++ show token

      _ <- runDBUpdate $ DenyCredentials token email time

      sendRedirect $ LinkHome locale

authorizationGranted :: Kontrakcja m => m Response
authorizationGranted = do
  muser <- ctxmaybeuser <$> getContext
  time <- ctxtime <$> getContext
  locale <- ctxlocale <$> getContext
  case muser of
    Nothing -> sendRedirect $ LinkLogin locale NotLogged
    Just user -> do
      let email = BS.toString $ getEmail user
      mtk <- getDataFn' (look "oauth_token")
      token <- guardJust $ maybeRead =<< mtk

      Log.debug $ "token: " ++ show token

      (callback, verifier) <- guardJustM $ runDBUpdate $ VerifyCredentials token email time

      Log.debug $ "got callback + verifier: " ++ show (callback, verifier)

      url <- guardJust $ parseURI callback

      Log.debug $ "url: " ++ show url

      sendRedirect $ LinkOAuthCallback url token verifier

tokenCredRequest :: Kontrakcja m => m Response
tokenCredRequest = api $ do
  time <- ctxtime <$> getContext
  rq <- lift askRq
  let headers = rqHeaders rq
  Log.debug $ "Got headers: " ++ show headers

  HeaderPair _ auths <- apiGuard' BadInput $ Map.lookup (BS.fromString "authorization") headers

  Log.debug $ "Got Authorization: headers: " ++ show auths

  auth <- apiGuard' BadInput $ BS.toString <$> listToMaybe auths

  -- pull the data out of Authorization
  let params = splitAuthorization auth                 

  Log.debug $ "Split Authorization header into params: " ++ show params

  sigtype <- apiGuard' BadInput $ maybeRead =<< lookup "oauth_signature_method" params
  when (sigtype /= "PLAINTEXT") $ throwError BadInput

  (mapisecret, mtoksecret) <- apiGuard' BadInput $ splitSignature =<< maybeRead =<< lookup "oauth_signature" params
  apisecret <- apiGuard' Forbidden mapisecret
  tokensecret <- apiGuard' Forbidden mtoksecret

  Log.debug $ "Got api secret: " ++ show apisecret
     
  apitoken <- apiGuard' BadInput $ maybeRead =<< maybeRead =<< lookup "oauth_consumer_key" params

  temptoken <- apiGuard' BadInput $ maybeRead =<< maybeRead =<< lookup "oauth_token" params
  verifier  <- apiGuard' BadInput $ maybeRead =<< maybeRead =<< lookup "oauth_verifier" params

  Log.debug $ "Got token: " ++ show apitoken

  (accesstoken, accesssecret) <- apiGuardL' Forbidden $ runDBUpdate $ RequestAccessToken apitoken apisecret temptoken tokensecret verifier time

  Log.debug $ "Got temp stuff: " ++ show (accesstoken, accesssecret)

  return $ FormEncoded [("oauth_token",        show accesstoken)
                       ,("oauth_token_secret", show accesssecret)
                       ]

