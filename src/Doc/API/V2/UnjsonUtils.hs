{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.UnjsonUtils (unjsonEnumBy, unjsonEnum, nothingToNullDef, unjsonDefWithNull) where


import KontraPrelude
import Data.Unjson
import Data.Text
import qualified Data.Aeson as Aeson

unjsonEnumBy :: (Eq a) => Text -> [(a,Text)] -> UnjsonDef a
unjsonEnumBy desc enumDef =
  unjsonEnum desc (parseEnum enumDef) (printEnum enumDef)
  where

    parseEnum :: [(a,Text)] -> Text -> Maybe a
    parseEnum ((m,t):ms) v = if (t == v)
                            then Just m
                            else parseEnum ms v
    parseEnum _ _ = Nothing

    printEnum :: (Eq a) => [(a,Text)] -> a -> Text
    printEnum ((m,t):ms) a =  if (a == m)
                            then t
                            else printEnum ms a
    printEnum _ _ = $unexpectedError $ unpack ("Incompleate printEnum definition for " `append` desc)

unjsonEnum :: Text -> (Text -> Maybe a) -> (a -> Text) -> UnjsonDef a
unjsonEnum desc parseEnum printEnum  =
  SimpleUnjsonDef desc parseFromEnumFromAeson printEnumToAeson
  where
    printEnumToAeson a = Aeson.String $ printEnum a
    parseFromEnumFromAeson (Aeson.String s) = case (parseEnum s) of
                                                Just a -> Result a []
                                                _ -> fail $ unpack $ "cannot parse enum " `append` desc `append` " from " `append` s
    parseFromEnumFromAeson _ = fail $ unpack $ "cannot parse enum " `append` desc `append` " from not string"


nothingToNullDef :: UnjsonDef (Maybe a) ->  UnjsonDef (Maybe a)
nothingToNullDef def = SimpleUnjsonDef "ReadOnly"  (parse def) $ \mv ->
  case mv of
    Nothing -> Aeson.Null
    _ -> unjsonToJSON def mv

unjsonDefWithNull :: Unjson a => UnjsonDef (Maybe a)
unjsonDefWithNull = SimpleUnjsonDef "ReadOnly"  (\v -> Just <$> parse unjsonDef v) $ \mv ->
  case mv of
    Nothing -> Aeson.Null
    Just v -> unjsonToJSON unjsonDef v

