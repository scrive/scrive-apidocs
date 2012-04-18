module OAuth.Control where

import Kontra
import API.Monad
import Routing
import OAuth.Model
import DB
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

--import qualified Log

oauthAPI :: Route (KontraPlus Response)
oauthAPI = choice [
  dir "oauth" $ dir "temporarycredentials" $ hGet  $ toK0 $ tempCredRequest,
  dir "oauth" $ dir "authorization"        $ hGet  $ toK0 $ authorization,
  dir "oauth" $ dir "authorizationconfirm" $ hPost $ toK0 $ authorizationGranted,
  dir "oauth" $ dir "authorizationdeny"    $ hPost $ toK0 $ authorizationDenied,
  dir "oauth" $ dir "tokencredentials"     $ hGet  $ toK0 $ tokenCredRequest
  ]

tempCredRequest :: Kontrakcja m => m Response
tempCredRequest = api $ do
  time <- ctxtime <$> getContext
  rq <- lift askRq
  let headers = rqHeaders rq

  HeaderPair _ auths <- apiGuard (badInput "Authorization header is required.") $ Map.lookup (BS.fromString "authorization") headers

  auth <- apiGuard' $ BS.toString <$> listToMaybe auths

  -- pull the data out of Authorization
  let params = splitAuthorization auth

  sigtype <- apiGuard (badInput "oauth_signature_method is required") $ maybeRead =<< lookup "oauth_signature_method" params
  when (sigtype /= "PLAINTEXT") $ throwError $ badInput "oauth_signature_method must be 'PLAINTEXT'."

  (mapisecret, _) <- apiGuard (badInput "oauth_signature was missing or in bad format.") $ splitSignature =<< maybeRead =<< lookup "oauth_signature" params
  apisecret <- apiGuard (badInput "API Secret is missing or in bad format.") mapisecret

  callback <- apiGuard (badInput "oauth_callback is required and must be a valid URL.") $ UTF.decode <$> (URL.decode =<< maybeRead =<< lookup "oauth_callback" params)

  apitoken <- apiGuard (badInput "oauth_consumer_key is missing or is invalid.") $ maybeRead =<< maybeRead =<< lookup "oauth_consumer_key" params

  privilegesstring <- apiGuardL (badInput "'privileges' parameter must exist.") $ getDataFn' (look "privileges")

  privileges <- apiGuard (badInput "'privileges' parameter is invalid.") $ readPrivileges privilegesstring

  email <- apiGuardL (badInput "useremail is missing.") $ getDataFn' (look "useremail")

  (temptoken, tempsecret) <- apiGuardL' $ dbUpdate $ RequestTempCredentials apitoken apisecret email privileges callback time

  return $ FormEncoded [("oauth_token", show temptoken),
                        ("oauth_token_secret", show tempsecret),
                        ("oauth_callback_confirmed", "true")]

authorization :: Kontrakcja m => m (Either KontraLink String)
authorization = do
  muser  <- ctxmaybeuser <$> getContext
  time   <- ctxtime      <$> getContext
  locale <- ctxlocale    <$> getContext
  case muser of
    -- soon this should be custom page, not login
    Nothing -> return $ Left $ LinkLogin locale LoginTry
    Just user -> do
      let email = getEmail user
      mtk <- getDataFn' (look "oauth_token")
      token <- guardJust $ maybeRead =<< mtk

      mprivs <- dbQuery $ GetRequestedPrivileges token email time

      case mprivs of
        Just (companyname, p:ps) -> Right <$> pagePrivilegesConfirm (p:ps) companyname token
        _ -> return $ Left $ LinkHome locale

authorizationDenied :: Kontrakcja m => m Response
authorizationDenied = do
  muser  <- ctxmaybeuser <$> getContext
  time   <- ctxtime <$> getContext
  locale <- ctxlocale <$> getContext
  case muser of
    Nothing -> sendRedirect $ LinkLogin locale NotLogged
    Just user -> do
      let email = getEmail user
      mtk <- getDataFn' (look "oauth_token")
      token <- guardJust $ maybeRead =<< mtk

      _ <- dbUpdate $ DenyCredentials token email time

      sendRedirect $ LinkHome locale

authorizationGranted :: Kontrakcja m => m Response
authorizationGranted = do
  muser <- ctxmaybeuser <$> getContext
  time <- ctxtime <$> getContext
  locale <- ctxlocale <$> getContext
  case muser of
    Nothing -> sendRedirect $ LinkLogin locale NotLogged
    Just user -> do
      let email = getEmail user
      mtk <- getDataFn' (look "oauth_token")
      token <- guardJust $ maybeRead =<< mtk

      (callback, verifier) <- guardJustM $ dbUpdate $ VerifyCredentials token email time

      url <- guardJust $ parseURI callback

      sendRedirect $ LinkOAuthCallback url token verifier

tokenCredRequest :: Kontrakcja m => m Response
tokenCredRequest = api $ do
  time <- ctxtime <$> getContext
  rq <- lift askRq
  let headers = rqHeaders rq

  HeaderPair _ auths <- apiGuard (badInput "Authorization header is required.") $ Map.lookup (BS.fromString "authorization") headers

  auth <- apiGuard (badInput "Authorization header is required.") $ BS.toString <$> listToMaybe auths

  -- pull the data out of Authorization
  let params = splitAuthorization auth

  sigtype <- apiGuard (badInput "oauth_signature_method is required") $ maybeRead =<< lookup "oauth_signature_method" params
  when (sigtype /= "PLAINTEXT") $ throwError $ badInput "oauth_signature_method must be PLAINTEXT."

  (mapisecret, mtoksecret) <- apiGuard (badInput "oauth_signature is required.") $ splitSignature =<< maybeRead =<< lookup "oauth_signature" params
  apisecret <- apiGuard (badInput "API Secret is in bad format.") mapisecret
  tokensecret <- apiGuard (badInput "Token Secret is in bad format.") mtoksecret

  apitoken <- apiGuard (badInput "oauth_consumer_key is required.") $ maybeRead =<< maybeRead =<< lookup "oauth_consumer_key" params

  temptoken <- apiGuard (badInput "oauth_token is required.") $ maybeRead =<< maybeRead =<< lookup "oauth_token" params
  verifier  <- apiGuard (badInput "oauth_verifier is required.") $ maybeRead =<< maybeRead =<< lookup "oauth_verifier" params

  (accesstoken, accesssecret) <- apiGuardL' $ dbUpdate $ RequestAccessToken apitoken apisecret temptoken tokensecret verifier time

  return $ FormEncoded [("oauth_token",        show accesstoken)
                       ,("oauth_token_secret", show accesssecret)
                       ]

