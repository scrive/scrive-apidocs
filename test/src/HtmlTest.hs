{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates
-fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}

module HtmlTest where

import Data.Char
import Data.List
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, assertBool, Assertion)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Html.Pretty
import Text.XML.HaXml.Types
import System.IO

import Misc
import Templates.Langs
import Templates.TemplatesFiles

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    defaultMain tests

tests :: [Test]
tests = [ testGroup "Html" htmlTests
        ]


htmlTests :: [Test]
htmlTests = 
    [ testGroup "static checks"
        [ testCase "templates make valid xml" testValidXml,
          testCase "no unecssary double divs" testNoUnecessaryDoubleDivs ]
    ]

excludedTemplates :: [String]
excludedTemplates = ["paymentsadminpagesuperuser"]

isIncluded :: (String, String) -> Bool
isIncluded (name, _) = not $ name `elem` excludedTemplates

testValidXml :: Assertion
testValidXml = do
  ts <- mapM getTemplates templatesFilesPath
  texts <- mapM getTextTemplates allValues
  _ <- mapM assertTemplateIsValidXML . filter isIncluded $ concat ts ++ concat texts
  assertSuccess

assertTemplateIsValidXML :: (String, String) -> Assertion
assertTemplateIsValidXML t =
  case parseTemplateAsXML t of
    Left msg -> assertFailure msg
    Right _ -> assertSuccess
    
testNoUnecessaryDoubleDivs :: Assertion
testNoUnecessaryDoubleDivs = do
  templates <- mapM getTemplates templatesFilesPath
  _ <- mapM assertNoUnecessaryDoubleDivs . filter isIncluded $ concat templates
  assertSuccess

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
        isDivElem (CElem (Elem n _ _) _) | map toLower n == "div" = True
        isDivElem _ = False
checkXMLForUnecessaryDoubleDivs _ _ = assertSuccess

parseTemplateAsXML :: (String, String) -> Either String (Document Posn)
parseTemplateAsXML (name, rawtxt) =
  let preparedtxt = "<template>\n" ++ (clearTemplating rawtxt) ++ "\n</template>"
      prettyprinttxt = unlines . zipWith mklinewithno ([1..]::[Int]) $ lines preparedtxt
      mklinewithno no line --okay, i did indenting in a horrible way, it's just a test!
        | no<10  = (show no) ++ ".    |" ++ line
        | no<100  = (show no) ++ ".   |" ++ line
        | no<1000  = (show no) ++ ".  |" ++ line
        | otherwise = (show no) ++ ". |" ++ line
  in case xmlParse' name preparedtxt of
    Left msg -> Left $ msg ++ "\n" ++ prettyprinttxt
    r@(Right _) -> r

clearTemplating :: String -> String
clearTemplating = clearTemplating' NotTag NotTemplateCode . removeDocTypeDeclaration

removeDocTypeDeclaration :: String -> String
removeDocTypeDeclaration s =
  if "<!DOCTYPE" `isPrefixOf` s
    then tail $ dropWhile (/= '>') s
    else s

{-
  this is kind of wrong.  but it pretty much seems to work.  i really really really need to make nicer :-(
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
