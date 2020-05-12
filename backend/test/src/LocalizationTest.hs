{-# OPTIONS_GHC -fno-warn-orphans #-}
module LocalizationTest (localizationTest) where

import Data.List.Extra
import Data.Map ((!), Map)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool)
import Text.JSON
import Text.StringTemplate
import Text.StringTemplates.TextTemplates
import Text.XML (Element(..), Node(..), def)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML as XML

import TestingUtil (assertFailure)
import TestKontra
import User.Lang
import Utils.Enum

localizationTest :: TestEnvSt -> Test
localizationTest _env = testGroup
  "Localization Test"
  [ testGroup
      "static checks"
      [ testCase "tests if translations are valid JSONS and are ordered"
                 testTranslationFilesAreJSONSAndSorted
      , testCase "text templates have same structure" testTranslationsHaveSameStructure
      , testCase "text templates have same html structure"
                 testTranslationsHaveSameHtmlStructure
      , testCase "text templates have same number of escaped \\\" "
                 testTranslationsHaveSameEscapeSequences
      ]
  ]

testTranslationFilesAreJSONSAndSorted :: Assertion
testTranslationFilesAreJSONSAndSorted = do
  forM_ (allValues :: [Lang]) $ \l -> do
    mjson1 <- readFile $ "texts/" <> T.unpack (codeFromLang l) <> "/texts.json"
    mjson2 <- readFile $ "texts/" <> T.unpack (codeFromLang l) <> "/events.json"
    mjson3 <- readFile $ "texts/" <> T.unpack (codeFromLang l) <> "/signview.json"
    case (decode mjson1, decode mjson2, decode mjson3) of
      (Ok js1, Ok js2, Ok js3) -> isSorted js1 >> isSorted js2 >> isSorted js3
      _ -> assertFailure $ "Translation file is not a valid JSON for lang " <> T.unpack
        (codeFromLang l)


isSorted :: JSValue -> Assertion
isSorted (JSObject jso) =
  assertBool
      (  "Translation JSONs are not sorted. "
      <> "Use './shake.sh transifex-fix' to fix this."
      )
    $  map fst (fromJSObject jso)
    == sort (map fst $ fromJSObject jso)
isSorted _ = assertFailure "Illegal structures in translation file for lang"


testTranslationsHaveSameStructure :: Assertion
testTranslationsHaveSameStructure = do
  templates <- getTextTemplates "texts"
  let sourceTemplates = sort $ templates ! "en"
  lErrors <- forM (allValues :: [Lang]) $ \l -> do
    let translationTemplates = sort $ templates ! T.unpack (codeFromLang l)
    let errors               = catMaybes $ checkTexts sourceTemplates translationTemplates
    case errors of
      []   -> return Nothing
      errs -> return . Just $ "For lang " <> show (codeFromLang l) <> "\n" <> concat errs
  case catMaybes lErrors of
    [] -> return ()
    lErrs ->
      assertFailure
        $  "Some translation texts had different structure then base texts\n"
        <> concat lErrs

checkTexts :: [(String, String)] -> [(String, String)] -> [Maybe String]
checkTexts _  [] = []
checkTexts [] _  = []
checkTexts src@((sn, sv) : ss) tar@((tn, tv) : tt)
  | sn < tn
  = checkTexts ss tar
  | sn > tn
  = checkTexts src tt
  | trim tv == ""
  = checkTexts ss tt
  | otherwise
  = ((\s -> "In " <> tn <> " " <> s <> "\n") <$> compareTranslations sv tv)
    : checkTexts ss tt


compareTranslations :: String -> String -> Maybe String
compareTranslations s t =
  let (Nothing, msv, mst) = checkTemplate (newSTMP s :: StringTemplate String)
      (Nothing, mtv, mtt) = checkTemplate (newSTMP t :: StringTemplate String)
  in  if (sort <$> msv) /= (sort <$> mtv)
        then Just $ "Variables dont match : " <> show msv <> " vs." <> show mtv
        else if (sort <$> mst) /= (sort <$> mtt)
          then Just $ "Subtemplates dont match : " <> show mst <> " vs." <> show mtt
          else Nothing


testTranslationsHaveSameHtmlStructure :: Assertion
testTranslationsHaveSameHtmlStructure = do
  templates <- getTextTemplates "texts"
  let sourceTemplates = sort $ templates ! "en"
  lErrors <- forM (allValues :: [Lang]) $ \l -> do
    let translationTemplates = sort $ templates ! T.unpack (codeFromLang l)
    let errors = catMaybes $ checkHtmlStructureTexts sourceTemplates translationTemplates
    case errors of
      []   -> return Nothing
      errs -> return . Just $ "For lang " <> show (codeFromLang l) <> "\n" <> concat errs
  case catMaybes lErrors of
    [] -> return ()
    lErrs ->
      assertFailure
        $  "Some translation texts had different structure then base texts\n"
        <> concat lErrs

checkHtmlStructureTexts :: [(String, String)] -> [(String, String)] -> [Maybe String]
checkHtmlStructureTexts _  [] = []
checkHtmlStructureTexts [] _  = []
checkHtmlStructureTexts src@((sn, sv) : ss) tar@((tn, tv) : tt)
  | sn < tn
  = checkHtmlStructureTexts ss tar
  | sn > tn
  = checkHtmlStructureTexts src tt
  | trim tv == ""
  = checkHtmlStructureTexts ss tt
  | otherwise
  = ((\s -> "In " <> tn <> " " <> s <> "\n") <$> compareHTMLStructure sv tv)
    : checkHtmlStructureTexts ss tt


compareHTMLStructure :: String -> String -> Maybe String
compareHTMLStructure s t = case (parseStringAsXML s, parseStringAsXML t) of
  (Just doc1, Just doc2) -> matchElement (XML.documentRoot doc1) (XML.documentRoot doc2)
  _ -> Just "Can't parse HTML"

parseStringAsXML :: String -> Maybe XML.Document
parseStringAsXML rawtxt =
  let preparedtxt = "<template>\n" <> fixup rawtxt <> "\n</template>"
  in  case XML.parseText def (TL.pack preparedtxt) of
        Left  _ -> Nothing
        Right r -> Just r
  where
    -- HACK: Some strings contain unescaped '&' characters, which we
    -- want to allow, because changing them to '&amp;' breaks
    -- frontend.
    --
    -- See
    -- https://github.com/scrive/kontrakcja/pull/811#issuecomment-380458720.
        fixup = replace "& " "&amp; "

matchElement :: XML.Element -> XML.Element -> Maybe String
matchElement (Element n1 a1 c1) (Element n2 a2 c2)
  | n1 /= n2
  = Just $ "Can't match " <> show n1 <> " with " <> show n2
  | not $ matchAttributes a1 a2
  = Just $ "Can't match attributes for " <> show (n1, a1) <> " with " <> show (n2, a2)
  | otherwise
  = matchContent (sort c1) (sort c2)

matchAttributes :: Map XML.Name Text -> Map XML.Name Text -> Bool
matchAttributes attrs1 attrs2 = attrs1 == attrs2

matchContent :: [XML.Node] -> [XML.Node] -> Maybe String
matchContent ((NodeElement e1) : e1s) ((NodeElement e2) : e2s) =
  matchElement e1 e2 `mplus` matchContent e1s e2s
matchContent ((NodeElement e1) : e1s) (_ : e2s) = matchContent (NodeElement e1 : e1s) e2s
matchContent ((NodeElement (Element n1 _ _)) : _) [] =
  Just $ "Can match " <> show (XML.nameLocalName n1) <> " from source"
matchContent (_ : e1s) ((NodeElement e2) : e2s) = matchContent e1s (NodeElement e2 : e2s)
matchContent [] ((NodeElement (Element n2 _ _)) : _) =
  Just $ "Can match " <> show (XML.nameLocalName n2) <> " from translation"
matchContent (_ : e1s) e2s       = matchContent e1s e2s
matchContent []        (_ : e2s) = matchContent [] e2s
matchContent []        []        = Nothing

testTranslationsHaveSameEscapeSequences :: Assertion
testTranslationsHaveSameEscapeSequences = do
  templates <- getTextTemplates "texts"
  let sourceTemplates = sort $ templates ! "en"
  lErrors <- forM (allValues :: [Lang]) $ \l -> do
    let translationTemplates = sort $ templates ! T.unpack (codeFromLang l)
    let cmpFunc :: String -> String -> Maybe String
        cmpFunc = case l of
          LANG_LT -> compareEscapeSequecesForLithuanian
          _       -> compareEscapeSequeces
    let errors = catMaybes
          $ checkEscapeSequencesTexts cmpFunc sourceTemplates translationTemplates
    case errors of
      []   -> return Nothing
      errs -> return . Just $ "For lang " <> show (codeFromLang l) <> "\n" <> concat errs
  case catMaybes lErrors of
    [] -> return ()
    lErrs ->
      assertFailure
        $  "Some translation texts had different escapes structure then base texts\n"
        <> concat lErrs

checkEscapeSequencesTexts
  :: (String -> String -> Maybe String)
  -> [(String, String)]
  -> [(String, String)]
  -> [Maybe String]
checkEscapeSequencesTexts _ _  [] = []
checkEscapeSequencesTexts _ [] _  = []
checkEscapeSequencesTexts cmp src@((sn, sv) : ss) tar@((tn, tv) : tt)
  | sn < tn
  = checkEscapeSequencesTexts cmp ss tar
  | sn > tn
  = checkEscapeSequencesTexts cmp src tt
  | trim tv == ""
  = checkEscapeSequencesTexts cmp ss tt
  | otherwise
  = ((\s -> "In " <> tn <> " " <> s <> "\n") <$> cmp sv tv)
    : checkEscapeSequencesTexts cmp ss tt

-- We need to extend this one day with " and '
compareEscapeSequeces :: String -> String -> Maybe String
compareEscapeSequeces s t =
  if (countEscapes0 s /= countEscapes0 t)
       || (countEscapes1 s /= countEscapes1 t)
       || (countEscapes2 s /= countEscapes2 t)
       || (countEscapes3 s /= countEscapes3 t)
       || (countEOLs s /= countEOLs t)
       || (countGTs s /= countGTs t)
       || (countLTs s /= countLTs t)
    then Just "Escaped sequences don't match"
    else Nothing

-- Lithuanian has different aproach for quotations
-- 1) It often uses '„' and '“' chars instead of '"'
-- 2) It uses quotation for every foreign name. So there will be many more quotations

compareEscapeSequecesForLithuanian :: String -> String -> Maybe String
compareEscapeSequecesForLithuanian s t =
  if (countEscapes0 s > countEscapes0 t + countLatvianExtraQuotes t)
       || (countEscapes1 s > countEscapes1 t + countLatvianExtraQuotes t)
       || (countEscapes2 s > countEscapes2 t + countLatvianExtraQuotes t)
       || (countEscapes3 s > countEscapes3 t + countLatvianExtraQuotes t)
       || (countEOLs s /= countEOLs t)
       || (countGTs s /= countGTs t)
       || (countLTs s /= countLTs t)
    then Just "Escaped sequences don't match for Lithuania"
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
count p s = length (splitOn p s) - 1
