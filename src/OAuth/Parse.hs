module OAuth.Parse (
                    splitAuthorization
                   ,splitSignature
                   ,readPrivileges
                   ) where

import OAuth.Model
import Misc

import Data.String.Utils
import Data.Maybe


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
