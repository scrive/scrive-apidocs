module OAuth.Control where

import Kontra
import API.Monad
import Routing
import OAuth.Model
import DB.Classes
import Misc

import Happstack.StaticRouting(Route, choice, dir)

import Data.String.Utils
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

import qualified Log

oauthAPI :: Route (Kontra Response)
oauthAPI = choice [
  dir "oauth" $ dir "temporarycredentials" $ hPostNoXToken $ toK0 $ tempCredRequest
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

  privileges <- apiGuard' BadInput $ readPrivileges =<< maybeRead =<< lookup "privileges" params

  Log.debug $ "Got privileges: " ++ show privileges

  email <- apiGuard' BadInput $ maybeRead =<< lookup "useremail" params

  Log.debug $ "Got email: " ++ show email

  (temptoken, tempsecret) <- apiGuardL' Forbidden $ runDBUpdate $ RequestTempCredentials apitoken apisecret email privileges callback time

  Log.debug $ "Got temp stuff: " ++ show (temptoken, tempsecret)

  return $ FormEncoded [("oauth_token", show temptoken),
                        ("oauth_token_secret", show tempsecret),
                        ("oauth_callback_confirmed", "true")]



splitAuthorization :: String -> [(String, String)]
splitAuthorization s = 
  catMaybes $ map makeKV $ splitOver "," s
  where makeKV kv = case break (== '=') kv of
          (k, '=':v) -> Just (strip k, strip v)
          _ -> Nothing

splitSignature :: String -> Maybe (Maybe APISecret, Maybe APISecret)
splitSignature s = case break (== '&') s of
  ("",'&':"") -> Just (Nothing, Nothing)
  ("",'&':sc) -> case maybeRead sc of
    Just secret -> Just (Nothing, Just secret)
    Nothing -> Nothing
  (sc,'&':"") -> case maybeRead sc of
    Just secret -> Just (Just secret, Nothing)
    Nothing -> Nothing
  (s1,'&':s2) -> case (maybeRead s1, maybeRead s2) of
    (Just sc1, Just sc2) -> Just (Just sc1, Just sc2)
    _ -> Nothing
  _ -> Nothing

readPrivileges :: String -> Maybe [APIPrivilege]
readPrivileges s = privs (splitOver "+" s) []
  where privs [] a = Just a
        privs (p:pp) a = case maybeRead p of
          Just priv -> privs pp (priv:a)
          Nothing -> Nothing
