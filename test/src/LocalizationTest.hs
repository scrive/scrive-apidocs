module LocalizationTest (localizationTest) where

import Data.List
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, assertBool, Assertion)
import Control.Monad

import Text.JSON
import User.Lang
import Utils.Enum
import TestKontra
import Text.StringTemplate
import Text.StringTemplates.TextTemplates
import Data.Map ((!))
import Data.String.Utils
import Data.Maybe
--import qualified Log as Log
import Control.Applicative

localizationTest :: TestEnvSt -> Test
localizationTest _ = testGroup "Localization Test"
    [ testGroup "static checks"
      [
          testCase "tests if translations are valid JSONS and are ordered" testTranslationFilesAreJSONSAndSorted
        , testCase "text templates have same structure" testTranslationsHaveSameStructure
      ]
    ]

testTranslationFilesAreJSONSAndSorted :: Assertion
testTranslationFilesAreJSONSAndSorted = do
  forM_ (allValues :: [Lang]) $ \l -> do
    mjson <- readFile $ "texts/" ++ codeFromLang l ++ "/texts.json"
    case decode mjson of
      Ok js -> isSorted js
      _ -> assertFailure $ "Translation file is not a valid JSON for lang " ++ codeFromLang l


isSorted :: JSValue -> Assertion
isSorted (JSObject jso) = assertBool "Translations jsons are not sorted. Use './transifex.sh fix' to fix it" $ (map fst $ fromJSObject jso) == (sort (map fst $ fromJSObject jso))
isSorted _ = assertFailure  "Illegal structures in translation file for lang"


testTranslationsHaveSameStructure :: Assertion
testTranslationsHaveSameStructure = do
  templates <- getTextTemplates "texts"
  let sourceTemplates = sort $ templates ! "en"
  lErrors <- forM (allValues :: [Lang]) $ \l -> do
    let translationTemplates = sort $ templates ! (codeFromLang l)
    let errors = catMaybes $ checkTexts sourceTemplates translationTemplates
    case errors of
       [] -> return Nothing
       errs -> return $ Just $ "For lang " ++ show (codeFromLang l) ++ "\n" ++ concat errs
  case catMaybes lErrors of
       [] -> return ()
       _lErrs -> return () --assertFailure $ "Some translation texts had different structure then base texts\n" ++ concat lErrs

checkTexts :: [(String,String)] -> [(String,String)] ->  [Maybe String]
checkTexts _ [] = []
checkTexts [] _ = []
checkTexts ((sn,sv):ss) ((tn,tv):tt)
  | (sn < tn) = checkTexts ss ((tn,tv):tt)
  | (tn > sn) = checkTexts ss ((sn,sv):tt)
  | (strip tv == "") = checkTexts ss tt
  | otherwise = ((\s -> "In " ++ tn ++ " " ++ s ++ "\n") <$> compareTranslations sv tv) : (checkTexts ss tt)


compareTranslations:: String -> String -> Maybe String
compareTranslations s t =
    let
     (Nothing,msv,mst) = checkTemplate $ (newSTMP s :: StringTemplate String)
     (Nothing,mtv,mtt) = checkTemplate $ (newSTMP t :: StringTemplate String)
    in
     if ((sort <$> msv) /= (sort <$> mtv))
      then Just $ "Variables dont match : " ++ show msv ++ " vs." ++ show mtv
      else if ((sort <$> mst) /= (sort <$> mtt))
            then Just $ "Subtemplates dont match : " ++ show mst ++ " vs." ++ show mtt
            else Nothing