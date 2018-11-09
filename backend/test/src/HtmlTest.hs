module HtmlTest (htmlTests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertFailure)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.StringTemplates.TemplatesLoader (renderTemplateMain)
import Text.XML
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Templates
import TestKontra
import User.Lang

htmlTests :: TestEnvSt -> Test
htmlTests _ = testGroup "HTML"
    [ testGroup "static checks"
        [ testCase "templates make valid xml" testValidXml
        , testCase "no unnecessary double divs" testNoUnnecessaryDoubleDivs
        , testCase "no nested p tags when templates are rendered" testNoNestedP
        ]
    ]

excludedTemplates :: [String]
excludedTemplates = [ "paymentsadminpagesuperuser"
                    , "javascriptLocalisation"
                    , "htmlopentag"
                    , "htmlclosingtag"
                    , "wrapperclosingtag"
                    , "wrapperopentag"
                    , "analyticsLoaderBase"
                    ]

isIncluded :: (String, String) -> Bool
isIncluded (name, _) = not $ name `elem` excludedTemplates

testValidXml :: Assertion
testValidXml = do
  ts <- getAllTemplates
  mapM_ assertTemplateIsValidXML . filter isIncluded $ ts
  assertSuccess

assertTemplateIsValidXML :: (String, String) -> Assertion
assertTemplateIsValidXML t =
  case parseTemplateAsXML t of
    Left  msg -> assertFailure msg
    Right   _ -> assertSuccess

testNoUnnecessaryDoubleDivs :: Assertion
testNoUnnecessaryDoubleDivs = do
  templates <- getAllTemplates
  mapM_ assertNoUnnecessaryDoubleDivs $ filter isIncluded templates
  assertSuccess

testNoNestedP :: Assertion
testNoNestedP = do
  langtemplates    <- readGlobalTemplates
  alltemplatenames <- map fst <$> getAllTemplates
  forM_ allLangs $ \lang -> do
    let templates = localizedVersion lang langtemplates
    assertNoNestedP alltemplatenames templates
  assertSuccess

assertNoNestedP :: [String] -> KontrakcjaTemplates -> Assertion
assertNoNestedP tnames templates = do
  forM_ (filter (not . (flip elem) excludedTemplates) tnames) $ \n -> do
    let t = renderTemplateMain templates n ([]::[(String, String)]) id
    case parseStringAsXML (n, removeScripts $ removeDocTypeDeclaration t) of
      Left  msg -> assertFailure msg
      Right doc -> checkXMLForNestedP n (NodeElement $ documentRoot doc)
  assertSuccess

checkXMLForNestedP :: String -> Node -> Assertion
checkXMLForNestedP templatename elt =
  if isPAndHasP elt
  then assertFailure $ "nested <p> tags in template " ++ templatename ++ ":\n" ++
                       (renderMarkup . toMarkup $ elt)
  else assertSuccess

isPOrHasP :: Node -> Bool
isPOrHasP (NodeElement (Element tag _attrs children)) =
  if (T.toLower . nameLocalName $ tag) == "p"
  then True
  else any isPOrHasP children
isPOrHasP _ = False

isPAndHasP :: Node -> Bool
isPAndHasP (NodeElement (Element tag _attrs children)) =
  if (T.toLower . nameLocalName $ tag) == "p"
  then any isPOrHasP children
  else any isPAndHasP children
isPAndHasP _ = False

assertNoUnnecessaryDoubleDivs :: (String, String) -> Assertion
assertNoUnnecessaryDoubleDivs t@(name,_) =
  case parseTemplateAsXML t of
    Left msg  -> assertFailure msg
    Right doc -> checkXMLForUnnecessaryDoubleDivs name
                 (NodeElement $ documentRoot doc)

checkXMLForUnnecessaryDoubleDivs :: String -> Node -> Assertion
checkXMLForUnnecessaryDoubleDivs templatename
                                 e@(NodeElement (Element _ _ children)) =
  let isDiv            = isDivElem e
      isSingleChild    = length children == 1
      isSingleChildDiv = isSingleChild && isDivElem (head children)
      isUnnecessaryDiv = isDiv && isSingleChildDiv in
  if isUnnecessaryDiv
    then assertFailure $ "unnecessary double divs in template " ++
                         templatename ++ ":\n" ++
                         (renderMarkup . toMarkup $ e)
    else do
      mapM_ (checkXMLForUnnecessaryDoubleDivs templatename) children
      assertSuccess
  where isDivElem :: Node -> Bool
        isDivElem (NodeElement (Element n _ _)) =
          (T.toLower . nameLocalName $ n) == "div"
        isDivElem _                             = False
checkXMLForUnnecessaryDoubleDivs _ _ = assertSuccess

parseStringAsXML :: (String, String) -> Either String Document
parseStringAsXML (name, rawtxt) =
  let preparedtxt    = "<template>\n" ++ rawtxt ++ "\n</template>"
      prettyprinttxt = unlines . zipWith mklinewithno ([1..]::[Int])
                       $ lines preparedtxt
      mklinewithno no line -- okay, i did indenting in a horrible way,
                           -- it's just a test!
        | no<10     = (show no) ++ ".    |" ++ line
        | no<100    = (show no) ++  ".   |" ++ line
        | no<1000   = (show no) ++   ".  |" ++ line
        | otherwise = (show no) ++    ". |" ++ line
  in case parseText def (TL.pack preparedtxt) of
    Left  exc -> Left $ name ++ ":" ++ show exc ++ "\n" ++ prettyprinttxt
    Right doc -> Right doc

parseTemplateAsXML :: (String, String) -> Either String Document
parseTemplateAsXML (name, rawtxt) =
  parseStringAsXML (name, clearTemplating rawtxt)

clearTemplating :: String -> String
clearTemplating = clearTemplating' NotTag NotTemplateCode . removeScripts . removeDocTypeDeclaration

removeDocTypeDeclaration :: String -> String
removeDocTypeDeclaration s =
  if "<!DOCTYPE" `isPrefixOf` s
    then tail $ dropWhile (/= '>') s
    else s

removeScripts :: String -> String
removeScripts = removeScripts' NotScript

data ScriptState = NotScript | ScriptStartTag | InScript

removeScripts' :: ScriptState -> String -> String
removeScripts' _ [] = []
removeScripts' NotScript ('<':'s':'c':'r':'i':'p':'t':xs) = "<script" ++ removeScripts' ScriptStartTag xs
removeScripts' ScriptStartTag ('>':xs) = '>' : removeScripts' InScript xs
removeScripts' InScript ('<':'/':'s':'c':'r':'i':'p':'t':'>':xs) = "</script>" ++ removeScripts' NotScript xs
removeScripts' InScript (_:xs) = removeScripts' InScript xs
removeScripts' s (x:xs) = x : removeScripts' s xs

{-
  this is wrong.  but it pretty much seems to work.  i really really really need to make nicer :-(  but then again, this is just a test.
-}

data TagState = NotTag | Tag
data TemplateCodeState = NotTemplateCode | TemplateCode

clearTemplating'  :: TagState -> TemplateCodeState -> String -> String
clearTemplating' _ _ [] = []
clearTemplating' ts tcs ('\\':'$':xs) = '$' : clearTemplating' ts tcs xs
clearTemplating' NotTag NotTemplateCode ('<':xs) = '<' : clearTemplating' Tag NotTemplateCode xs
clearTemplating' NotTag NotTemplateCode ('$':'i':'f':xs) = "<template-if>" ++ (clearTemplating' NotTag TemplateCode xs)
clearTemplating' NotTag NotTemplateCode ('$':'e':'l':'s':'e':xs) = "</template-if><template-if>" ++ clearTemplating' NotTag TemplateCode xs
clearTemplating' NotTag NotTemplateCode ('$':'e':'n':'d':'i':'f':'$':xs) = "</template-if>" ++ clearTemplating' NotTag NotTemplateCode xs
clearTemplating' NotTag TemplateCode ('|':xs) = "<template-loop>" ++ clearTemplating' NotTag NotTemplateCode xs
clearTemplating' NotTag NotTemplateCode ('}':';':xs) = "</template-loop>" ++ clearTemplating' NotTag TemplateCode xs
clearTemplating' NotTag NotTemplateCode ('}':':':xs) = "</template-loop>" ++ clearTemplating' NotTag TemplateCode xs
clearTemplating' NotTag NotTemplateCode ('}':'$':xs) = "</template-loop>" ++ clearTemplating' NotTag NotTemplateCode xs
clearTemplating' Tag NotTemplateCode ('>':xs) = '>' : clearTemplating' NotTag NotTemplateCode xs
clearTemplating' ts NotTemplateCode ('$':xs) = clearTemplating' ts TemplateCode xs
clearTemplating' ts NotTemplateCode ('}':';':xs) = clearTemplating' ts TemplateCode xs
clearTemplating' ts NotTemplateCode ('}':':':xs) = clearTemplating' ts TemplateCode xs
clearTemplating' ts NotTemplateCode ('}':'$':xs) = clearTemplating' ts NotTemplateCode xs
clearTemplating' ts TemplateCode ('|':xs) = clearTemplating' ts NotTemplateCode xs
clearTemplating' ts TemplateCode ('$':xs) = clearTemplating' ts NotTemplateCode xs
clearTemplating' ts TemplateCode (_:xs) = clearTemplating' ts TemplateCode xs
clearTemplating' ts tcs (x:xs) = x : clearTemplating' ts tcs xs

assertSuccess :: Assertion
assertSuccess = assertBool "not success?!" True
