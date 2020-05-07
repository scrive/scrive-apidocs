module OAuth.Util (
    getTempCredRequest
  , getTokenRequest
  , getAuthorization
  , getOAuthUser
  , getUserFromOAuthWithAnyPrivileges
  , getMaybeAPIUserWithAnyPrivileges
) where

import Control.Conditional ((<|), (|>))
import Happstack.Server.Monads
import Happstack.Server.RqData
import Happstack.Server.Types
import Network.HTTP.Base (urlDecode)
import Network.URI
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Text as T

import Auth.OAuth
import Auth.Parse
import DB
import Happstack.Fields
import Kontra
import OAuth.Model
import User.Model.Query
import User.Types.User
import Util.Actor
import Utils.Read

getAuthorizationHeader :: Kontrakcja m => m (Maybe [(Text, Text)])
getAuthorizationHeader = do
  rq <- askRq
  case Map.lookup (BS.fromString "authorization") $ rqHeaders rq of
    Nothing                   -> return Nothing
    Just (HeaderPair _ auths) -> do
      case BS.toString <$> listToMaybe auths of
        Nothing   -> return $ Just []
        Just auth -> return . Just $ splitAuthorization (T.pack auth)

readPrivileges :: Text -> Maybe [APIPrivilege]
readPrivileges s = privs (T.splitOn "+" s) []
  where
    privs []       a = Just a
    privs (p : pp) a = case maybeRead p of
      Just priv -> privs pp (priv : a)
      Nothing   -> Nothing

getTempCredRequest :: Kontrakcja m => m (Either Text OAuthTempCredRequest)
getTempCredRequest = do
  eparams <- getAuthorizationHeader
  case eparams of
    Nothing -> return $ Left "Authorization header is required."
    Just [] ->
      return $ Left
        "Authorization header does not have a valid format and could not be parsed."
    Just params -> do
      mprivilegesstring <- getDataFn' (look "privileges")
      let customParseURI s = case parseURI s of
            Just URI { uriScheme = "data:" } -> Nothing
            Just URI { uriScheme = "javascript:" } -> Nothing
            _ -> parseURI s
      let
        msigtype   = lookupAndRead "oauth_signature_method" params
        mapisecret = splitSignature =<< lookupAndRead "oauth_signature" params
        mcallback =
          customParseURI =<< (urlDecode <$> lookupAndRead "oauth_callback" params)
        mapitoken   = lookupAndReadString "oauth_consumer_key" params
        mprivileges = readPrivileges =<< (T.pack <$> mprivilegesstring)
        errors      = T.intercalate
          "; "
          (  ["oauth_signature_method must be 'PLAINTEXT'"]
          <| Just "PLAINTEXT"
          /= msigtype
          |> []
          <> ["oauth_signature was missing or in bad format"]
          <| isNothing mapisecret
          |> []
          <> ["oauth_signature api secret (first param) is missing"]
          <| isNothing (fst =<< mapisecret)
          |> []
          <> ["oauth_callback is required and must be a valid URL"]
          <| isNothing mcallback
          |> []
          <> ["oauth_consumer_key is missing or is invalid"]
          <| isNothing mapitoken
          |> []
          <> ["'privileges' parameter must exist"]
          <| isNothing mprivilegesstring
          |> []
          <> ["'privileges' parameter is invalid"]
          <| isNothing mprivileges
          |> []
          )
      if not $ T.null errors
        then return $ Left errors
        else return . Right $ OAuthTempCredRequest
          { tcCallback   = fromJust mcallback
          , tcAPIToken   = fromJust mapitoken
          , tcAPISecret  = fromJust . fst $ fromJust mapisecret
          , tcPrivileges = fromJust mprivileges
          }

getTokenRequest :: Kontrakcja m => m (Either Text OAuthTokenRequest)
getTokenRequest = do
  eparams <- getAuthorizationHeader
  case eparams of
    Nothing     -> return $ Left "Authorization header is required."
    Just params -> do
      let msigtype   = lookupAndRead "oauth_signature_method" params
          mapisecret = splitSignature =<< lookupAndRead "oauth_signature" params
          mapitoken  = lookupAndReadString "oauth_consumer_key" params
          mtemptoken = lookupAndReadString "oauth_token" params
          mverifier  = lookupAndReadString "oauth_verifier" params
          errors     = T.intercalate
            "; "
            (  ["oauth_signature_method must be 'PLAINTEXT'"]
            <| Just "PLAINTEXT"
            /= msigtype
            |> []
            <> ["oauth_signature was missing or in bad format"]
            <| isNothing mapisecret
            |> []
            <> ["oauth_signature api secret (first param) is missing"]
            <| isNothing (fst =<< mapisecret)
            |> []
            <> ["oauth_signature token secret (second param) is missing"]
            <| isNothing (snd =<< mapisecret)
            |> []
            <> ["oauth_consumer_key is missing or is invalid"]
            <| isNothing mapitoken
            |> []
            <> ["oauth_token is required"]
            <| isNothing mtemptoken
            |> []
            <> ["oauth_verifier is required"]
            <| isNothing mverifier
            |> []
            )
      if not $ T.null errors
        then return $ Left errors
        else return . Right $ OAuthTokenRequest
          { trAPIToken   = fromJust mapitoken
          , trAPISecret  = fromJust . fst $ fromJust mapisecret
          , trTempToken  = fromJust mtemptoken
          , trTempSecret = fromJust . snd $ fromJust mapisecret
          , trVerifier   = fromJust mverifier
          }

getAuthorization :: Kontrakcja m => m (Maybe (Either Text OAuthAuthorization))
getAuthorization = fmap parseParams <$> getAuthorizationHeader

getOAuthUser :: Kontrakcja m => [APIPrivilege] -> m (Maybe (Either Text (User, Actor)))
getOAuthUser privs = do
  eauth <- getAuthorization
  case eauth of
    Nothing           -> return Nothing
    Just (Left  l   ) -> return . Just $ Left ("OAuth headers could not be parsed: " <> l)
    Just (Right auth) -> Just <$> getUserFromOAuth auth privs

getUserFromOAuth
  :: Kontrakcja m => OAuthAuthorization -> [APIPrivilege] -> m (Either Text (User, Actor))
getUserFromOAuth OAuthAuthorization {..} privs = do
  ctx <- getContext
  uap <- dbQuery $ GetUserIDForAPIWithPrivilege oaAPIToken
                                                oaAPISecret
                                                oaAccessToken
                                                oaAccessSecret
                                                privs
  case uap of
    Nothing -> return
      $ Left "OAuth credentials are invalid or they may not have sufficient privileges."
    Just (userid, apistring) -> dbQuery (GetUserByID userid) >>= \case
      Nothing ->
        return
          $ Left
              "OAuth credentials are valid but the user account for those credentials does not exist."
      Just user -> return $ Right (user, apiActor ctx user (T.pack apistring))

getUserFromOAuthWithAnyPrivileges
  :: Kontrakcja m => OAuthAuthorization -> m (Either Text (User, Actor))
getUserFromOAuthWithAnyPrivileges oauth = getUserFromOAuth oauth allPrivileges

getMaybeAPIUserWithAnyPrivileges :: Kontrakcja m => m (Maybe User)
getMaybeAPIUserWithAnyPrivileges = getOAuthUser allPrivileges >>= \case
  Just (Left  _        ) -> return Nothing
  Just (Right (user, _)) -> return $ Just user
  Nothing                -> view #maybeUser <$> getContext
