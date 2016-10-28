{-# OPTIONS_GHC -fno-warn-orphans #-}
module LocalizationTest (localizationTest) where

import Data.Function
import Data.Map ((!))
import Data.String.Utils
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool)
import Text.JSON
import Text.StringTemplate
import Text.StringTemplates.TextTemplates
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types

import KontraPrelude
import TestingUtil (assertFailure)
import TestKontra
import User.Lang
import Utils.Enum

localizationTest :: TestEnvSt -> Test
localizationTest _env = testGroup "Localization Test"
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
    mjson1 <- readFile $ "texts/" ++ codeFromLang l ++ "/texts.json"
    mjson2 <- readFile $ "texts/" ++ codeFromLang l ++ "/events.json"
    mjson3 <- readFile $ "texts/" ++ codeFromLang l ++ "/signview.json"
    case (decode mjson1,decode mjson2,decode mjson3) of
      (Ok js1,Ok js2,Ok js3) -> isSorted js1 >> isSorted js2 >> isSorted js3
      _ -> assertFailure $ "Translation file is not a valid JSON for lang " ++ codeFromLang l


isSorted :: JSValue -> Assertion
isSorted (JSObject jso) = assertBool "Translations jsons are not sorted. Use './transifex/transifex.sh fix' to fix it" $ (map fst $ fromJSObject jso) == (sort (map fst $ fromJSObject jso))
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

instance Ord Posn where
    compare = compare `on` show
deriving instance Ord Reference
deriving instance Ord Misc
deriving instance Ord AttValue
deriving instance Ord (Element Posn)
deriving instance Ord (Content Posn)

matchElement :: Element Posn -> Element Posn -> Maybe String
matchElement (Elem n1 a1 c1) (Elem n2 a2 c2) =
  if (n1 /= n2)
     then Just $ "Can't match " ++ show n1 ++ " with " ++ show n2
     else if (not $ matchAttributes a1 a2)
      then Just $ "Can't match attributes for " ++ show (n1,a1) ++ " with " ++ show (n2,a2)
      else matchContent (sort c1) (sort c2)

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
    let cmpFunc = case l of
                    LANG_LT -> compareEscapeSequecesForLatvian
                    _ -> compareEscapeSequeces
    let errors = catMaybes $ checkEscapeSequencesTexts cmpFunc sourceTemplates translationTemplates
    case errors of
       [] -> return Nothing
       errs -> return $ Just $ "For lang " ++ show (codeFromLang l) ++ "\n" ++ concat errs
  case catMaybes lErrors of
       [] -> return ()
       lErrs -> assertFailure $ "Some translation texts had different escapes structure then base texts\n" ++ concat lErrs

checkEscapeSequencesTexts :: (String -> String -> Maybe String) -> [(String,String)] -> [(String,String)] ->  [Maybe String]
checkEscapeSequencesTexts _ _ [] = []
checkEscapeSequencesTexts _ [] _ = []
checkEscapeSequencesTexts cmp src@((sn,sv):ss) tar@((tn,tv):tt)
  | (sn < tn) = checkEscapeSequencesTexts cmp ss tar
  | (sn > tn) = checkEscapeSequencesTexts cmp src tt
  | (strip tv == "") = checkEscapeSequencesTexts cmp ss tt
  | otherwise = ((\s -> "In " ++ tn ++ " " ++ s ++ "\n") <$> cmp sv tv) : (checkEscapeSequencesTexts cmp ss tt)


-- We need to extend this one day with " and '
compareEscapeSequeces:: String -> String -> Maybe String
compareEscapeSequeces s t =
  if (countEscapes0 s /= countEscapes0 t ||
      countEscapes1 s /= countEscapes1 t ||
      countEscapes2 s /= countEscapes2 t ||
      countEscapes3 s /= countEscapes3 t ||
      countEOLs s /= countEOLs t         ||
      countGTs s /=  countGTs t          ||
      countLTs s /=  countLTs t)
    then Just "Escaped sequences don't match"
    else Nothing

-- Latvian has different aproach for quotations
-- 1) It often uses '„' and '“' chars instead of '"'
-- 2) It uses quotation for every foreign name. So there will be many more quotations

compareEscapeSequecesForLatvian:: String -> String -> Maybe String
compareEscapeSequecesForLatvian s t =
  if (countEscapes0 s > countEscapes0 t + countLatvianExtraQuotes t ||
      countEscapes1 s > countEscapes1 t + countLatvianExtraQuotes t ||
      countEscapes2 s > countEscapes2 t + countLatvianExtraQuotes t ||
      countEscapes3 s > countEscapes3 t + countLatvianExtraQuotes t ||
      countEOLs s /= countEOLs t         ||
      countGTs s /=  countGTs t          ||
      countLTs s /=  countLTs t)
    then Just $ "Escaped sequences don't match for Latvia"
    else Nothing


countEscapes0 :: String -> Int
countEscapes0 = count "\""

countEscapes1 :: String -> Int
countEscapes1 = count "\\\""

countEscapes2 :: String -> Int
countEscapes2 = count "\\\\\""

countEscapes3 :: String -> Int
countEscapes3 = count "\\\\\\\""

countLatvianExtraQuotes :: String -> Int
countLatvianExtraQuotes s = count "„" s + count "“" s

countEOLs :: String -> Int
countEOLs = count "\n"

countGTs :: String -> Int
countGTs = count ">"

countLTs :: String -> Int
countLTs = count "<"

count :: String -> String -> Int
count p s = (length $ split p s) - 1
