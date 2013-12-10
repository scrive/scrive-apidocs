module Transifex.Utils (TranslationResource(..), translationFile, sourceLang , encodeTranslationJSON, textsToJSON,textsFromStringJSON, textsFromJSON, Change(..), compareTranslations,parsePushResponse, applyChange) where


import Data.CSV (csvFile)
import Data.Char (isSpace, isControl)
import Data.List (isSuffixOf)
import System.IO
import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec (parse)
import Text.JSON.Gen
import Text.JSON
import Text.JSON.Pretty
import Text.PrettyPrint.HughesPJ
import Data.List
import Text.JSON.FromJSValue
import Control.Monad.Identity
import Data.Maybe


data TranslationResource = Texts | Events deriving Show

translationFile:: String -> TranslationResource -> String
translationFile lang Texts = "texts/" ++ lang ++ "/texts.json"
translationFile lang Events = "texts/" ++ lang ++ "/events.json"

sourceLang :: String
sourceLang = "en"

encodeTranslationJSON :: JSValue -> String
encodeTranslationJSON  (JSObject jso) = "{ \n" ++ (intercalate ",\n" $ map (\(s,js) -> encode s ++ ":" ++ encode js) (fromJSObject jso)) ++ "\n}"
encodeTranslationJSON  e = encode e

textsFromJSON :: JSValue -> [(String,String)]
textsFromJSON (JSObject jso) = map (\(a,JSString js) -> (a, fromJSString js)) (fromJSObject jso)
textsFromJSON _ = error "While decoding JSON with translations"


textsFromStringJSON  :: JSValue -> [(String,String)]
textsFromStringJSON js =  fromJust $ runIdentity $ withJSValue js $ fromJSValueCustomMany $ do
                                mk <- fromJSValueField "key"
                                mv <- fromJSValueField "translation"
                                case (mk,mv) of
                                     (Just k, Just v) -> return $ Just (k,v)
                                     _ -> return Nothing

textsToJSON :: [(String,String)] -> JSValue
textsToJSON s = runJSONGen $ textsToJSON_ s
  where
    textsToJSON_ :: [(String,String)] -> JSONGen ()
    textsToJSON_ ((n,v):ss) = value n v >> textsToJSON_ ss
    textsToJSON_ [] = return ()




data Change =   Remove String String
              | Change String String String
              | Add String String


instance Show Change where
  show (Remove n v) = "--- \"" ++ n ++ "\" " ++ "\"" ++ v ++ "\""
  show (Add n v) = "+++ \"" ++ n ++ "\" " ++ "\"" ++ v ++ "\""
  show (Change n v1 v2 ) = show (Remove n v1) ++ "\n" ++ show (Add n v2)


applyChange :: Change -> [(String,String)] -> [(String,String)]
applyChange (Remove n v) list = filter (\(n',_) -> n' /= n) list
applyChange (Add n v)    list = sort $ (n,v) : list
applyChange (Change n v1 v2)    list = (applyChange (Add n v2)) $ (applyChange (Remove n v1)) $ list



compareTranslations :: [(String,String)] -> [(String,String)] -> [Change]
compareTranslations [] ts = map (\t -> (Add (fst t) (snd t))) ts
compareTranslations ts [] = map (\t -> (Remove (fst t) (snd t))) ts
compareTranslations (t1:ts1) (t2:ts2)  = if (fst t1 == fst t2)
                                           then if (snd t1 == snd t2)
                                                  then compareTranslations ts1 ts2
                                                  else (Change (fst t1) (snd t1) (snd t2)) : (compareTranslations ts1 ts2)
                                           else if fst t1 < fst t2
                                                  then (Remove (fst t1) (snd t1)) : (compareTranslations ts1 (t2:ts2))
                                                  else (Add (fst t2) (snd t2)) : (compareTranslations (t1:ts1) ts2)



parsePushResponse :: String -> Maybe (Int,Int,Int)
parsePushResponse  s = case decode s of
     Ok js -> runIdentity $ withJSValue js $ do
        md <- fromJSValueField "strings_delete"
        mu <- fromJSValueField "strings_updated"
        ma <- fromJSValueField "strings_added"
        case (md,mu,ma) of
          (Just d,Just u,Just a) -> return $ Just (d,u,a)
          _ -> return Nothing
     _ -> Nothing
