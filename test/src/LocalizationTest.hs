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
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types

localizationTest :: TestEnvSt -> Test
localizationTest _ = testGroup "Localization Test"
    [ testGroup "static checks"
      [
          testCase "tests if translations are valid JSONS and are ordered" testTranslationFilesAreJSONSAndSorted
        , testCase "text templates have same structure" testTranslationsHaveSameStructure
        , testCase "text templates have same html structure" testTranslationsHaveSameHtmlStructure
        , testCase "text templates have same number of excaped \\\" " testTranslationsHaveSameEscapeSequences
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
       lErrs -> assertFailure $ "Some translation texts had different structure then base texts\n" ++ concat lErrs

checkTexts :: [(String,String)] -> [(String,String)] ->  [Maybe String]
checkTexts _ [] = []
checkTexts [] _ = []
checkTexts src@((sn,sv):ss) tar@((tn,tv):tt)
  | (sn < tn) = checkTexts ss tar
  | (sn > tn) = checkTexts src tt
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


testTranslationsHaveSameHtmlStructure :: Assertion
testTranslationsHaveSameHtmlStructure = do
  templates <- getTextTemplates "texts"
  let sourceTemplates = sort $ templates ! "en"
  lErrors <- forM (allValues :: [Lang]) $ \l -> do
    let translationTemplates = sort $ templates ! (codeFromLang l)
    let errors = catMaybes $ checkHtmlStructureTexts sourceTemplates translationTemplates
    case errors of
       [] -> return Nothing
       errs -> return $ Just $ "For lang " ++ show (codeFromLang l) ++ "\n" ++ concat errs
  case catMaybes lErrors of
       [] -> return ()
       lErrs -> assertFailure $ "Some translation texts had different structure then base texts\n" ++ concat lErrs

checkHtmlStructureTexts :: [(String,String)] -> [(String,String)] ->  [Maybe String]
checkHtmlStructureTexts _ [] = []
checkHtmlStructureTexts [] _ = []
checkHtmlStructureTexts src@((sn,sv):ss) tar@((tn,tv):tt)
  | (sn < tn) = checkHtmlStructureTexts ss tar
  | (sn > tn) = checkHtmlStructureTexts src tt
  | (strip tv == "") = checkHtmlStructureTexts ss tt
  | otherwise = ((\s -> "In " ++ tn ++ " " ++ s ++ "\n") <$> compareHTMLStructure sv tv) : (checkHtmlStructureTexts ss tt)



compareHTMLStructure:: String -> String -> Maybe String
compareHTMLStructure s t =
     case (parseStringAsXML s,parseStringAsXML t) of
       (Just (Document _ _ se _),Just (Document _ _ te _)) -> matchElement se te
       _ -> Just "Can't parse HTML"

parseStringAsXML :: String -> Maybe (Document Posn)
parseStringAsXML rawtxt =
  let preparedtxt = "<template>\n" ++ rawtxt ++ "\n</template>"
  in case xmlParse' "Translation text" preparedtxt of
    Left _ -> Nothing
    Right r -> Just r

matchElement :: Element Posn -> Element Posn -> Maybe String
matchElement (Elem n1 a1 c1) (Elem n2 a2 c2) =
  if (n1 /= n2)
     then Just $ "Can't match " ++ show n1 ++ " with " ++ show n2
     else if (not $ matchAttributes a1 a2)
      then Just $ "Can't match attributes for " ++ show (n1,a1) ++ " with " ++ show (n2,a2)
      else matchContent c1 c2

matchAttributes :: [(QName, AttValue)] -> [(QName, AttValue)] -> Bool
matchAttributes ((n1,v1):a1s) ((n2,v2):a2s) = (n1 == n2) && (v1 == v2) && matchAttributes a1s a2s
matchAttributes [] [] =  True
matchAttributes _ _  =  False

matchContent  :: [Content Posn] -> [Content Posn] -> Maybe String
matchContent ((CElem e1 _) : e1s) ((CElem e2 _) : e2s) = (matchElement e1 e2) `mplus` (matchContent e1s e2s)
matchContent ((CElem e1 p1) : e1s) (_ : e2s) = matchContent ((CElem e1 p1) : e1s) e2s
matchContent ((CElem (Elem n1 _ _) _) : _) [] = Just $ "Can match " ++ show n1 ++ " from source"
matchContent (_ : e1s) ((CElem e2 p2) : e2s) = matchContent e1s ((CElem e2 p2) : e2s)
matchContent [] ((CElem (Elem n2 _ _) _) : _) = Just $ "Can match " ++ show n2 ++ " from translation"
matchContent (_ : e1s) e2s = matchContent e1s e2s
matchContent [] (_ : e2s) = matchContent [] e2s
matchContent [] [] = Nothing

testTranslationsHaveSameEscapeSequences:: Assertion
testTranslationsHaveSameEscapeSequences = do
  templates <- getTextTemplates "texts"
  let sourceTemplates = sort $ templates ! "en"
  lErrors <- forM (allValues :: [Lang]) $ \l -> do
    let translationTemplates = sort $ templates ! (codeFromLang l)
    let errors = catMaybes $ checkEscapeSequencesTexts sourceTemplates translationTemplates
    case errors of
       [] -> return Nothing
       errs -> return $ Just $ "For lang " ++ show (codeFromLang l) ++ "\n" ++ concat errs
  case catMaybes lErrors of
       [] -> return ()
       lErrs -> assertFailure $ "Some translation texts had different escapes structure then base texts\n" ++ concat lErrs

checkEscapeSequencesTexts :: [(String,String)] -> [(String,String)] ->  [Maybe String]
checkEscapeSequencesTexts _ [] = []
checkEscapeSequencesTexts [] _ = []
checkEscapeSequencesTexts src@((sn,sv):ss) tar@((tn,tv):tt)
  | (sn < tn) = checkEscapeSequencesTexts ss tar
  | (sn > tn) = checkEscapeSequencesTexts src tt
  | (strip tv == "") = checkEscapeSequencesTexts ss tt
  | otherwise = ((\s -> "In " ++ tn ++ " " ++ s ++ "\n") <$> compareEscapeSequeces sv tv) : (checkEscapeSequencesTexts ss tt)


-- We need to extend this one day with " and '
compareEscapeSequeces:: String -> String -> Maybe String
compareEscapeSequeces s t =
  if (countEscapes1 s /= countEscapes1 t || countEscapes2 s /= countEscapes2 t || countEscapes3 s /= countEscapes3 t)
    then Just "Escaped sequences don't match"
    else Nothing
  where
    countEscapes1 str = length $ split "\\\"" str
    countEscapes2 str = length $ split "\\\\\"" str
    countEscapes3 str = length $ split "\\\\\\\"" str
