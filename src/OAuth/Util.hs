module OAuth.Util(
    getTempCredRequest
  , getTokenRequest
  , getAuthorization
) where

import Control.Conditional ((<|), (|>))
import Happstack.Server.Monads
import Happstack.Server.RqData
import Happstack.Server.Types
import Network.URI
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map

import Happstack.Fields
import Kontra
import KontraPrelude
import OAuth.Model
import OAuth.Parse
import Utils.Read

getAuthorizationHeader :: Kontrakcja m => m (Maybe [(String, String)])
getAuthorizationHeader = do
  rq <- askRq
  case Map.lookup (BS.fromString "authorization") $ rqHeaders rq of
    Nothing -> return Nothing
    Just (HeaderPair _ auths) -> do
      case BS.toString <$> listToMaybe auths of
        Nothing -> return $ Just []
        Just auth -> return $ Just $ splitAuthorization auth

getTempCredRequest :: Kontrakcja m => m (Either String OAuthTempCredRequest)
getTempCredRequest = do
  eparams <- getAuthorizationHeader
  case eparams of
    Nothing -> return $ Left "Authorization header is required."
    Just [] -> return $ Left "Authorization header does not have a valid format and could not be parsed."
    Just params -> do
      mprivilegesstring <- getDataFn' (look "privileges")
      let msigtype          = lookupAndRead "oauth_signature_method" params
          mapisecret        = splitSignature =<< lookupAndRead "oauth_signature" params
          mcallback         = parseURI =<< UTF.decode <$> (URL.decode =<< lookupAndRead "oauth_callback" params)
          mapitoken         = lookupAndReadString "oauth_consumer_key" params
          mprivileges       = readPrivileges =<< mprivilegesstring
          errors            = intercalate "; "
                   (["oauth_signature_method must be 'PLAINTEXT'"]         <| Just "PLAINTEXT" /= msigtype   |> [] ++
                    ["oauth_signature was missing or in bad format"]       <| isNothing mapisecret           |> [] ++
                    ["oauth_signature api secret (first param) is missing"]    <| isNothing (fst =<< mapisecret) |> [] ++
                    ["oauth_callback is required and must be a valid URL"] <| isNothing mcallback            |> [] ++
                    ["oauth_consumer_key is missing or is invalid"]        <| isNothing mapitoken            |> [] ++
                    ["'privileges' parameter must exist"]                  <| isNothing mprivilegesstring    |> [] ++
                    ["'privileges' parameter is invalid"]                  <| isNothing mprivileges          |> [] )
      if not $ null errors
        then return $ Left errors
        else return $ Right $ OAuthTempCredRequest { tcCallback   = $fromJust mcallback
                                                   , tcAPIToken   = $fromJust mapitoken
                                                   , tcAPISecret  = $fromJust $ fst $ $fromJust mapisecret
                                                   , tcPrivileges = $fromJust mprivileges
                                                   }

getTokenRequest :: Kontrakcja m => m (Either String OAuthTokenRequest)
getTokenRequest = do
  eparams <- getAuthorizationHeader
  case eparams of
    Nothing -> return $ Left "Authorization header is required."
    Just params -> do
      let msigtype          = lookupAndRead "oauth_signature_method" params
          mapisecret        = splitSignature =<< lookupAndRead "oauth_signature" params
          mapitoken         = lookupAndReadString "oauth_consumer_key" params
          mtemptoken        = lookupAndReadString "oauth_token" params
          mverifier         = lookupAndReadString "oauth_verifier" params
          errors            = intercalate "; "
                   (["oauth_signature_method must be 'PLAINTEXT'"]             <| Just "PLAINTEXT" /= msigtype   |> [] ++
                    ["oauth_signature was missing or in bad format"]           <| isNothing mapisecret           |> [] ++
                    ["oauth_signature api secret (first param) is missing"]    <| isNothing (fst =<< mapisecret) |> [] ++
                    ["oauth_signature token secret (second param) is missing"] <| isNothing (snd =<< mapisecret) |> [] ++
                    ["oauth_consumer_key is missing or is invalid"]            <| isNothing mapitoken            |> [] ++
                    ["oauth_token is required"]                                <| isNothing mtemptoken           |> [] ++
                    ["oauth_verifier is required"]                             <| isNothing mverifier            |> [])
      if not $ null errors
        then return $ Left errors
        else return $ Right $ OAuthTokenRequest { trAPIToken   = $fromJust mapitoken
                                                , trAPISecret  = $fromJust $ fst $ $fromJust mapisecret
                                                , trTempToken  = $fromJust mtemptoken
                                                , trTempSecret = $fromJust $ snd $ $fromJust mapisecret
                                                , trVerifier   = $fromJust mverifier
                                                }

-- Read authorization header for oauth. Returns Nothing if 'authorization' header is missing.
getAuthorization :: Kontrakcja m => m (Maybe (Either String OAuthAuthorization))
getAuthorization = do
  eparams <- getAuthorizationHeader
  case eparams of
    Nothing -> return $ Nothing
    Just params -> do
      let msigtype          = lookupAndRead "oauth_signature_method" params
          mapisecret        = splitSignature =<< lookupAndRead "oauth_signature" params
          mapitoken         = lookupAndReadString "oauth_consumer_key" params
          macctoken         = lookupAndReadString "oauth_token" params
          errors            = intercalate "; "
                   (["oauth_signature_method must be 'PLAINTEXT'"]             <| Just "PLAINTEXT" /= msigtype   |> [] ++
                    ["oauth_signature was missing or in bad format"]           <| isNothing mapisecret           |> [] ++
                    ["oauth_signature api secret (first param) is missing"]    <| isNothing (fst =<< mapisecret) |> [] ++
                    ["oauth_signature token secret (second param) is missing"] <| isNothing (snd =<< mapisecret) |> [] ++
                    ["oauth_consumer_key is missing or is invalid"]            <| isNothing mapitoken            |> [] ++
                    ["oauth_token is required"]                                <| isNothing macctoken            |> [])
      if not $ null errors
        then return $ Just $ Left errors
        else return $ Just $ Right $ OAuthAuthorization { oaAPIToken     = $fromJust mapitoken
                                                 , oaAPISecret    = $fromJust $ fst $ $fromJust mapisecret
                                                 , oaAccessToken  = $fromJust macctoken
                                                 , oaAccessSecret = $fromJust $ snd $ $fromJust mapisecret
                                                 }
