module Transifex.Utils
  ( TranslationResource(..)
  , allResources, readResource, translationFile
  , allLangs, allTargetLangs, sourceLang
  , encodeTranslationJSON, textsToJSON, textsFromStringJSON, textsFromJSON
  , Change(..), compareTranslations, parsePushResponse )
where

import Control.Monad.Identity
import Data.List
import Data.Maybe
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen

data TranslationResource = Texts | Events | Signview deriving Show

allResources :: [TranslationResource]
allResources = [Texts, Events, Signview]

readResource :: String -> Maybe TranslationResource
readResource "texts"    = Just Texts
readResource "events"   = Just Events
readResource "signview" = Just Signview
readResource _          = Nothing


translationFile :: String -> TranslationResource -> String
translationFile lang Texts    = "texts/" ++ lang ++ "/texts.json"
translationFile lang Events   = "texts/" ++ lang ++ "/events.json"
translationFile lang Signview = "texts/" ++ lang ++ "/signview.json"


allLangs :: [String]
allLangs =
  [ "en"
  , "sv"
  , "de"
  , "fr"
  , "it"
  , "es"
  , "pt"
  , "cs"
  , "pl"
  , "nl"
  , "da"
  , "no"
  , "el"
  , "fi"
  , "is"
  , "et"
  , "lv"
  , "lt"
  , "hu"
  ]

sourceLang :: String
sourceLang = "en"

allTargetLangs :: [String]
allTargetLangs = delete sourceLang allLangs

encodeTranslationJSON :: JSValue -> String
encodeTranslationJSON (JSObject jso) =
  "{ \n"
    ++ intercalate ",\n"
                   (map (\(s, js) -> encode s ++ ":" ++ encode js) (fromJSObject jso))
    ++ "\n}\n"
encodeTranslationJSON e = encode e

textsFromJSON :: JSValue -> [(String, String)]
textsFromJSON (JSObject jso) =
  map (\(a, JSString js) -> (a, fromJSString js)) (fromJSObject jso)
textsFromJSON _ = error "While decoding JSON with translations"


textsFromStringJSON :: Bool -> JSValue -> [(String, String)]
textsFromStringJSON acceptNotReviewed js =
  fromJust . runIdentity . withJSValue js . fromJSValueCustomMany $ do
    mk         <- fromJSValueField "key"
    mv         <- fromJSValueField "translation"
    isReviewed <- fromMaybe False <$> fromJSValueField "reviewed"
    case (mk, mv, acceptNotReviewed || isReviewed) of
      (Just k, Just v, True) -> return $ Just (k, v)
      (Just k, Just _v, False) -> return $ Just (k, "")
      _ -> return Nothing

textsToJSON :: [(String, String)] -> JSValue
textsToJSON s = runJSONGen $ textsToJSON_ s
  where
    textsToJSON_ :: [(String, String)] -> JSONGen ()
    textsToJSON_ ((n, v) : ss) = value n v >> textsToJSON_ ss
    textsToJSON_ []            = return ()


data Change =   Remove String String
              | Change String String String
              | Add String String


instance Show Change where
  show (Remove n v    ) = "--- \"" ++ n ++ "\" " ++ "\"" ++ v ++ "\""
  show (Add    n v    ) = "+++ \"" ++ n ++ "\" " ++ "\"" ++ v ++ "\""
  show (Change n v1 v2) = show (Remove n v1) ++ "\n" ++ show (Add n v2)

compareTranslations :: [(String, String)] -> [(String, String)] -> [Change]
compareTranslations [] ts = map (uncurry Add) ts
compareTranslations ts [] = map (uncurry Remove) ts
compareTranslations (t1 : ts1) (t2 : ts2)
  | fst t1 == fst t2 = if snd t1 == snd t2
    then compareTranslations ts1 ts2
    else uncurry Change t1 (snd t2) : compareTranslations ts1 ts2
  | fst t1 < fst t2 = uncurry Remove t1 : compareTranslations ts1 (t2 : ts2)
  | otherwise = uncurry Add t2 : compareTranslations (t1 : ts1) ts2

parsePushResponse :: String -> Maybe (Int, Int, Int)
parsePushResponse s = case decode s of
  Ok js -> runIdentity . withJSValue js $ do
    md <- fromJSValueField "strings_delete"
    mu <- fromJSValueField "strings_updated"
    ma <- fromJSValueField "strings_added"
    case (md, mu, ma) of
      (Just d, Just u, Just a) -> return $ Just (d, u, a)
      _ -> return Nothing
  _ -> Nothing
