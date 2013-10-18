module LocalizationTest (localizationTest) where

import Data.List
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, assertBool, Assertion)
import Control.Monad

import Text.JSON
import User.Lang
import Utils.Enum

localizationTest :: Test
localizationTest = testGroup "Localization Test"
    [ testGroup "static checks"
    [ testCase "tests if translations are valid JSONS and are ordered" testTranslationFilesAreJSONSAndSorted]
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
