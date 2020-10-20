module Auth.OAuth
  ( APIPrivilege(..)
  , APIToken(..)
  , OAuthAuthorization(..)
  , allPrivileges
  , parseParams
  , splitAuthorization
  , splitSignature
  , unjsonAPIToken
  )
  where

import Control.Conditional ((<|), (|>))
import Control.Monad.Catch
import Data.Aeson
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Network.HTTP (urlDecode)
import qualified Data.Text as T

import Auth.MagicHash
import Auth.Utils

data APIPrivilege = APIDocCreate
                  | APIDocCheck
                  | APIDocSend
                  -- OAuth version of personal access token
                  -- (also used as fallback for the other privileges)
                  | APIFullAccess
                  | APIPersonal  -- used only for personal access token
  deriving (Eq, Enum, Ord)

allPrivileges :: [APIPrivilege]
allPrivileges = [toEnum 0 ..]

instance Read APIPrivilege where
  readsPrec _ "DOC_CREATE"  = [(APIDocCreate, "")]
  readsPrec _ "DOC_CHECK"   = [(APIDocCheck, "")]
  readsPrec _ "DOC_SEND"    = [(APIDocSend, "")]
  readsPrec _ "FULL_ACCESS" = [(APIFullAccess, "")]
  readsPrec _ _             = [] -- we should never read APIPersonal

instance Show APIPrivilege where
  showsPrec _ APIDocCreate  = (++) "DOC_CREATE"
  showsPrec _ APIDocCheck   = (++) "DOC_CHECK"
  showsPrec _ APIDocSend    = (++) "DOC_SEND"
  showsPrec _ APIFullAccess = (++) "FULL_ACCESS"
  showsPrec _ APIPersonal   = (++) "PERSONAL"

instance PQFormat APIPrivilege where
  pqFormat = pqFormat @Int16

instance FromSQL APIPrivilege where
  type PQBase APIPrivilege = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      0 -> return APIPersonal
      1 -> return APIDocCreate
      2 -> return APIDocCheck
      3 -> return APIDocSend
      4 -> return APIFullAccess
      _ -> throwM $ RangeError { reRange = [(0, 4)], reValue = n }

instance ToSQL APIPrivilege where
  type PQDest APIPrivilege = PQDest Int16
  toSQL APIPersonal   = toSQL (0 :: Int16)
  toSQL APIDocCreate  = toSQL (1 :: Int16)
  toSQL APIDocCheck   = toSQL (2 :: Int16)
  toSQL APIDocSend    = toSQL (3 :: Int16)
  toSQL APIFullAccess = toSQL (4 :: Int16)

data APIToken = APIToken
  { atID    :: Int64     -- autoincrement for uniqueness
  , atToken :: MagicHash -- random part for security
  } deriving (Eq, Ord)

instance Show APIToken where
  showsPrec _ token = (++) $ show (atToken token) ++ "_" ++ show (atID token)

instance Read APIToken where
  readsPrec p s = case break (== '_') s of
    (ts, '_' : is) ->
      [ (APIToken { atID = i, atToken = read $ T.pack ts }, v)
      | (i, v) <- readsPrec p is
      ]
    _ -> []

unjsonAPIToken :: UnjsonDef APIToken
unjsonAPIToken = unjsonInvmapR
  (\s -> case reads s of
    [(apitoken, [])] -> pure apitoken
    _                -> fail "cannot parse APIToken"
  )
  show
  unjsonDef

instance ToJSON APIToken where
  toJSON = unjsonToJSON unjsonAPIToken

data OAuthAuthorization = OAuthAuthorization
  { oaAPIToken     :: APIToken
  , oaAPISecret    :: MagicHash
  , oaAccessToken  :: APIToken
  , oaAccessSecret :: MagicHash
  } deriving (Show, Eq)

-- Read authorization header for oauth.
parseParams :: [(Text, Text)] -> Either Text OAuthAuthorization
parseParams params =
  let msigtype   = lookupAndRead "oauth_signature_method" params :: Maybe Text
      mapisecret = splitSignature =<< lookupAndRead "oauth_signature" params
      mapitoken  = lookupAndReadString "oauth_consumer_key" params
      macctoken  = lookupAndReadString "oauth_token" params
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
        <| isNothing macctoken
        |> []
        )
  in  if not $ T.null errors
        then Left errors
        else Right $ OAuthAuthorization
          { oaAPIToken     = fromJust mapitoken
          , oaAPISecret    = fromJust . fst $ fromJust mapisecret
          , oaAccessToken  = fromJust macctoken
          , oaAccessSecret = fromJust . snd $ fromJust mapisecret
          }

splitAuthorization :: Text -> [(Text, Text)]
splitAuthorization s = mapMaybe makeKV (splitParts over_)
  where
        -- By default we split on commas, but if no comma is there, we
        -- split on spaces. This is a fix for some headers that cant
        -- send commas in header.
    splitParts o = if T.isInfixOf "," o then T.splitOn "," o else T.splitOn " " o
    over_ = if "OAuth" `T.isPrefixOf` s then T.drop 5 s else s
    makeKV kv = case T.breakOn "=" kv of
      (_, "") -> Nothing
      (k, v ) -> Just (T.strip k, T.strip $ T.drop 1 v) -- drop prefix "=" from value

splitSignature :: Text -> Maybe (Maybe MagicHash, Maybe MagicHash)
splitSignature s = case break (== '&') $ urlDecode (T.unpack s) of
  ("", '&' : "") -> Just (Nothing, Nothing)
  ("", '&' : sc) -> case maybeRead $ T.pack sc of
    Just secret -> Just (Nothing, Just secret)
    Nothing     -> Nothing
  (sc, '&' : "") -> case maybeRead $ T.pack sc of
    Just secret -> Just (Just secret, Nothing)
    Nothing     -> Nothing
  (s1, '&' : s2) -> case (maybeRead $ T.pack s1, maybeRead $ T.pack s2) of
    (Just sc1, Just sc2) -> Just (Just sc1, Just sc2)
    _                    -> Nothing
  _ -> Nothing

