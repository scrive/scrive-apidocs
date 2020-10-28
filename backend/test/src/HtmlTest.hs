module HtmlTest (htmlTests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.StringTemplates.TemplatesLoader (renderTemplateMain)
import Text.XML
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Templates
import TestingUtil
import TestKontra
import User.Lang

htmlTests :: TestEnvSt -> Test
htmlTests _ = testGroup
  "HTML"
  [ testGroup
      "static checks"
      [ testCase "templates make valid xml"                     testValidXml
      , testCase "no unnecessary double divs" testNoUnnecessaryDoubleDivs
      , testCase "no nested p tags when templates are rendered" testNoNestedP
      ]
  ]

excludedTemplates :: [Text]
excludedTemplates =
  [ "paymentsadminpagesuperuser"
  , "javascriptLocalisation"
  , "htmlopentag"
  , "htmlclosingtag"
  , "wrapperclosingtag"
  , "wrapperopentag"
  , "analyticsLoaderBase"
  ]

isIncluded :: (Text, Text) -> Bool
isIncluded (name, _) = name `notElem` excludedTemplates

testValidXml :: Assertion
testValidXml = do
  ts <- getAllTemplates
  mapM_ assertTemplateIsValidXML . filter isIncluded $ ts

assertTemplateIsValidXML :: (Text, Text) -> Assertion
assertTemplateIsValidXML = assertRight . parseTemplateAsXML

testNoUnnecessaryDoubleDivs :: Assertion
testNoUnnecessaryDoubleDivs = do
  templates <- getAllTemplates
  mapM_ assertNoUnnecessaryDoubleDivs $ filter isIncluded templates

testNoNestedP :: Assertion
testNoNestedP = do
  langtemplates    <- readGlobalTemplates
  alltemplatenames <- map fst <$> getAllTemplates
  forM_ allLangs $ \lang -> do
    let templates = localizedVersion lang langtemplates
    assertNoNestedP alltemplatenames templates

assertNoNestedP :: [Text] -> KontrakcjaTemplates -> Assertion
assertNoNestedP tnames templates = do
  forM_ (filter (not . flip elem excludedTemplates) tnames) $ \n -> do
    let t = renderTemplateMain templates (T.unpack n) ([] :: [(String, String)]) identity
    case parseStringAsXML (n, removeScripts . removeDocTypeDeclaration $ T.pack t) of
      Left  msg -> assertFailure $ T.unpack msg
      Right doc -> checkXMLForNestedP n (NodeElement $ documentRoot doc)

checkXMLForNestedP :: Text -> Node -> Assertion
checkXMLForNestedP templatename elt = assertBool msg (not isPAndHasP elt)
  where
    msg = T.unpack
      (  "nested <p> tags in template "
      <> templatename
      <> ":\n"
      <> (T.pack . renderMarkup . toMarkup $ elt)
      )

isPOrHasP :: Node -> Bool
isPOrHasP (NodeElement (Element tag _attrs children)) =
  ((T.toLower . nameLocalName $ tag) == "p") || any isPOrHasP children
isPOrHasP _ = False

isPAndHasP :: Node -> Bool
isPAndHasP (NodeElement (Element tag _attrs children)) =
  if (T.toLower . nameLocalName $ tag) == "p"
    then any isPOrHasP children
    else any isPAndHasP children
isPAndHasP _ = False

assertNoUnnecessaryDoubleDivs :: (Text, Text) -> Assertion
assertNoUnnecessaryDoubleDivs t@(name, _) = case parseTemplateAsXML t of
  Left  msg -> assertFailure $ T.unpack msg
  Right doc -> checkXMLForUnnecessaryDoubleDivs name (NodeElement $ documentRoot doc)

checkXMLForUnnecessaryDoubleDivs :: Text -> Node -> Assertion
checkXMLForUnnecessaryDoubleDivs templatename e@(NodeElement (Element _ _ children)) =
  let isDiv            = isDivElem e
      isSingleChild    = length children == 1
      isSingleChildDiv = isSingleChild && isDivElem (head children)
      isUnnecessaryDiv = isDiv && isSingleChildDiv
  in  if isUnnecessaryDiv
        then assertFailure $ T.unpack
          ("unnecessary double divs in template " <> templatename <> ":\n" <> T.pack
            (renderMarkup . toMarkup $ e)
          )
        else do
          mapM_ (checkXMLForUnnecessaryDoubleDivs templatename) children
  where
    isDivElem :: Node -> Bool
    isDivElem (NodeElement (Element n _ _)) = (T.toLower . nameLocalName $ n) == "div"
    isDivElem _ = False
checkXMLForUnnecessaryDoubleDivs _ _ = assertSuccess

parseStringAsXML :: (Text, Text) -> Either Text Document
parseStringAsXML (name, rawtxt) =
  let preparedtxt = "<template>\n" <> rawtxt <> "\n</template>"
      prettyprinttxt =
          T.unlines . zipWith mklinewithno ([1 ..] :: [Int]) $ T.lines preparedtxt
      mklinewithno no line | -- okay, i did indenting in a horrible way,
                           -- it's just a test!
                             no < 10   = showt no <> ".    |" <> line
                           | no < 100  = showt no <> ".   |" <> line
                           | no < 1000 = showt no <> ".  |" <> line
                           | otherwise = showt no <> ". |" <> line
  in  case parseText def (TL.fromStrict preparedtxt) of
        Left  exc -> Left $ name <> ":" <> showt exc <> "\n" <> prettyprinttxt
        Right doc -> Right doc

parseTemplateAsXML :: (Text, Text) -> Either Text Document
parseTemplateAsXML (name, rawtxt) = parseStringAsXML (name, clearTemplating rawtxt)

clearTemplating :: Text -> Text
clearTemplating =
  T.pack
    . clearTemplating' NotTag NotTemplateCode
    . T.unpack
    . removeScripts
    . removeDocTypeDeclaration

removeDocTypeDeclaration :: Text -> Text
removeDocTypeDeclaration s =
  if "<!DOCTYPE" `T.isPrefixOf` s then T.tail $ T.dropWhile (/= '>') s else s

removeScripts :: Text -> Text
removeScripts = T.pack . removeScripts' NotScript . T.unpack

data ScriptState = NotScript | ScriptStartTag | InScript

removeScripts' :: ScriptState -> String -> String
removeScripts' _ [] = []
removeScripts' NotScript ('<' : 's' : 'c' : 'r' : 'i' : 'p' : 't' : xs) =
  "<script" <> removeScripts' ScriptStartTag xs
removeScripts' ScriptStartTag ('>' : xs) = '>' : removeScripts' InScript xs
removeScripts' InScript ('<' : '/' : 's' : 'c' : 'r' : 'i' : 'p' : 't' : '>' : xs) =
  "</script>" <> removeScripts' NotScript xs
removeScripts' InScript (_ : xs) = removeScripts' InScript xs
removeScripts' s        (x : xs) = x : removeScripts' s xs

{-
  this is wrong.  but it pretty much seems to work.  i really really really need to make nicer :-(  but then again, this is just a test.
-}

data TagState = NotTag | Tag
data TemplateCodeState = NotTemplateCode | TemplateCode

clearTemplating' :: TagState -> TemplateCodeState -> String -> String
clearTemplating' _  _   []                = []
clearTemplating' ts tcs ('\\' : '$' : xs) = '$' : clearTemplating' ts tcs xs
clearTemplating' NotTag NotTemplateCode ('<' : xs) =
  '<' : clearTemplating' Tag NotTemplateCode xs
clearTemplating' NotTag NotTemplateCode ('$' : 'i' : 'f' : xs) =
  "<template-if>" <> clearTemplating' NotTag TemplateCode xs
clearTemplating' NotTag NotTemplateCode ('$' : 'e' : 'l' : 's' : 'e' : xs) =
  "</template-if><template-if>" <> clearTemplating' NotTag TemplateCode xs
clearTemplating' NotTag NotTemplateCode ('$' : 'e' : 'n' : 'd' : 'i' : 'f' : '$' : xs) =
  "</template-if>" <> clearTemplating' NotTag NotTemplateCode xs
clearTemplating' NotTag TemplateCode ('|' : xs) =
  "<template-loop>" <> clearTemplating' NotTag NotTemplateCode xs
clearTemplating' NotTag NotTemplateCode ('}' : ';' : xs) =
  "</template-loop>" <> clearTemplating' NotTag TemplateCode xs
clearTemplating' NotTag NotTemplateCode ('}' : ':' : xs) =
  "</template-loop>" <> clearTemplating' NotTag TemplateCode xs
clearTemplating' NotTag NotTemplateCode ('}' : '$' : xs) =
  "</template-loop>" <> clearTemplating' NotTag NotTemplateCode xs
clearTemplating' Tag NotTemplateCode ('>' : xs) =
  '>' : clearTemplating' NotTag NotTemplateCode xs
clearTemplating' ts NotTemplateCode ('$' : xs) = clearTemplating' ts TemplateCode xs
clearTemplating' ts NotTemplateCode ('}' : ';' : xs) =
  clearTemplating' ts TemplateCode xs
clearTemplating' ts NotTemplateCode ('}' : ':' : xs) =
  clearTemplating' ts TemplateCode xs
clearTemplating' ts NotTemplateCode ('}' : '$' : xs) =
  clearTemplating' ts NotTemplateCode xs
clearTemplating' ts TemplateCode ('|' : xs) = clearTemplating' ts NotTemplateCode xs
clearTemplating' ts TemplateCode ('$' : xs) = clearTemplating' ts NotTemplateCode xs
clearTemplating' ts TemplateCode (_   : xs) = clearTemplating' ts TemplateCode xs
clearTemplating' ts tcs          (x   : xs) = x : clearTemplating' ts tcs xs

