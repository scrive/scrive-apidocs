module HtmlTest (htmlTests) where

import Data.Char
import Data.List
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, assertBool, Assertion)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Html.Pretty
import Text.XML.HaXml.Types
import Control.Monad

import Templates
import Text.StringTemplates.TemplatesLoader (renderTemplateMain)
import User.Lang
import Static.Resources as SR
import TestingUtil (assertRight)

htmlTests :: Test
htmlTests = testGroup "HTML"
    [ testGroup "static checks"
        [ testCase "templates make valid xml" testValidXml,
          testCase "no unecssary double divs" testNoUnecessaryDoubleDivs ,
          testCase "no nested p tags when templates are rendered" testNoNestedP,
          testCase "testing static resource generation" testStaticResources
          ]
    ]

excludedTemplates :: [String]
excludedTemplates = ["paymentsadminpagesuperuser", "javascriptLocalisation"]

isIncluded :: (String, String) -> Bool
isIncluded (name, _) = not $ name `elem` excludedTemplates

testValidXml :: Assertion
testValidXml = do
  ts <- getAllTemplates
  _ <- mapM assertTemplateIsValidXML . filter isIncluded $ ts
  assertSuccess

assertTemplateIsValidXML :: (String, String) -> Assertion
assertTemplateIsValidXML t =
  case parseTemplateAsXML t of
    Left msg -> assertFailure msg
    Right _ -> assertSuccess

testNoUnecessaryDoubleDivs :: Assertion
testNoUnecessaryDoubleDivs = do
  templates <- getAllTemplates
  _ <- mapM assertNoUnecessaryDoubleDivs $ filter isIncluded templates
  assertSuccess

testNoNestedP :: Assertion
testNoNestedP = do
  langtemplates <- readGlobalTemplates
  ts <- getAllTemplates
  let alltemplatenames = map fst ts
  _ <- forM allLangs $ \lang -> do
    let templates = localizedVersion lang langtemplates
    --ts <- getTextTemplates lang
    assertNoNestedP alltemplatenames templates
  assertSuccess

assertNoNestedP :: [String] -> KontrakcjaTemplates -> Assertion
assertNoNestedP tnames templates = do
  _ <- forM (filter (not . (flip elem) excludedTemplates) tnames) $ \n -> do
    let t = renderTemplateMain templates n ([]::[(String, String)]) id
    case parseStringAsXML (n, removeScripts t) of
      Left msg -> assertFailure msg
      Right (Document _ _ root _) -> checkXMLForNestedP n $ CElem root undefined
  assertSuccess

checkXMLForNestedP :: String -> Content Posn -> Assertion
checkXMLForNestedP templatename e =
  if isPAndHasP e
  then assertFailure $ "nested <p> tags in template " ++ templatename ++ ":\n" ++
                         (show $ content e)
  else assertSuccess

isPOrHasP :: Content Posn -> Bool
isPOrHasP (CElem (Elem (N tag) _ children) _) =
  if map toLower tag == "p"
  then True
  else any isPOrHasP children
isPOrHasP _ = False

isPAndHasP :: Content Posn -> Bool
isPAndHasP (CElem (Elem (N tag) _ children) _) =
  if map toLower tag == "p"
  then any isPOrHasP children
  else any isPAndHasP children
isPAndHasP _ = False

assertNoUnecessaryDoubleDivs :: (String, String) -> Assertion
assertNoUnecessaryDoubleDivs t@(name,_) =
  case parseTemplateAsXML t of
    Left msg -> assertFailure msg
    Right (Document _ _ root _) -> checkXMLForUnecessaryDoubleDivs name $ CElem root undefined

checkXMLForUnecessaryDoubleDivs :: String -> Content Posn -> Assertion
checkXMLForUnecessaryDoubleDivs templatename e@(CElem (Elem _ _ children) _) =
  let isDiv = isDivElem e
      isSingleChild = length children == 1
      isSingleChildDiv = isSingleChild && isDivElem (head children)
      isUnecessaryDiv = isDiv && isSingleChildDiv in
  if isUnecessaryDiv
    then assertFailure $ "unecesary double divs in template " ++ templatename ++ ":\n" ++
                         (show $ content e)
    else do
      _ <- mapM (checkXMLForUnecessaryDoubleDivs templatename) children
      assertSuccess
  where isDivElem :: Content Posn -> Bool
        isDivElem (CElem (Elem (N n) _ _) _) = map toLower n == "div"
        isDivElem _ = False
checkXMLForUnecessaryDoubleDivs _ _ = assertSuccess

parseStringAsXML :: (String, String) -> Either String (Document Posn)
parseStringAsXML (name, rawtxt) =
  let preparedtxt = "<template>\n" ++ rawtxt ++ "\n</template>"
      prettyprinttxt = unlines . zipWith mklinewithno ([1..]::[Int]) $ lines preparedtxt
      mklinewithno no line --okay, i did indenting in a horrible way, it's just a test!
        | no<10  = (show no) ++ ".    |" ++ line
        | no<100  = (show no) ++ ".   |" ++ line
        | no<1000  = (show no) ++ ".  |" ++ line
        | otherwise = (show no) ++ ". |" ++ line
  in case xmlParse' name preparedtxt of
    Left msg -> Left $ msg ++ "\n" ++ prettyprinttxt
    r@(Right _) -> r


parseTemplateAsXML :: (String, String) -> Either String (Document Posn)
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

-- Library testing if all js and css are listed and if config file is valid
testStaticResources :: Assertion
testStaticResources = do
    v <- SR.getResourceSetsForImport SR.Development "public/resources.spec" ""
    assertRight $ v
