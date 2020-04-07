module OAuth.Parse (
                    splitAuthorization
                   ,splitSignature
                   ,readPrivileges
                   ) where

import Network.HTTP (urlDecode)
import qualified Data.Text as T

import MagicHash
import OAuth.Model

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

readPrivileges :: Text -> Maybe [APIPrivilege]
readPrivileges s = privs (T.splitOn "+" s) []
  where
    privs []       a = Just a
    privs (p : pp) a = case maybeRead p of
      Just priv -> privs pp (priv : a)
      Nothing   -> Nothing
