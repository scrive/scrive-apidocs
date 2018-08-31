module OAuth.Parse (
                    splitAuthorization
                   ,splitSignature
                   ,readPrivileges
                   ) where

import Data.List.Split
import Data.String.Utils hiding (maybeRead)
import Network.HTTP (urlDecode)

import MagicHash
import OAuth.Model

splitAuthorization :: String -> [(String, String)]
splitAuthorization s =
  catMaybes $ map makeKV $ splitParts $ over
  where
        -- By default we split on commas, but if no comma is there, we
        -- split on spaces. This is a fix for some headers that cant
        -- send commas in header.
        splitParts o = if (',' `elem` o)
                          then splitOn "," o
                          else splitOn " " o
        over = if "OAuth" `isPrefixOf` s
               then drop 5 s
               else s
        makeKV kv = case break (== '=') kv of
          (k, '=':v) -> Just (strip k, strip v)
          _ -> Nothing

splitSignature :: String -> Maybe (Maybe MagicHash, Maybe MagicHash)
splitSignature s = case break (== '&') $ urlDecode s of
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
readPrivileges s = privs (splitOn "+" s) []
  where privs [] a = Just a
        privs (p:pp) a = case maybeRead p of
          Just priv -> privs pp (priv:a)
          Nothing -> Nothing
