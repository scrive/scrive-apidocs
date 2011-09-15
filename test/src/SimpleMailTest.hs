module SimpleMailTest where

import API.MailAPI

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)

import Text.JSON.String

simpleMailTests :: Test
simpleMailTests = testGroup "Simple Mail Tests" [
  testCase "Testing test framework" (return ()),
  --testCase "Testing failing" (error "oops"),
  blankEmailTest
  ,singleMinimalSignatory
  ]
-- TestName
-- Assertion

blankEmailTest :: Test
blankEmailTest = testCase "Blank Email Test" $ do
   case parseSimpleEmail "" "" of
     Left _ -> return ()
     Right _ -> error "Blank email did not fail"
        
singleMinimalSignatory :: Test
singleMinimalSignatory = testCase "Single Minimal Signatory" $ do
  case parseSimpleEmail "Contract Title" 
       "First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se" of
    Left msg -> error msg
    a | a == runGetJSON readJSValue ("{\"title\":\"Contract Title\"," ++
        "\"involved\":[{\"fstname\":\"Mariusz\"," ++
        "\"sndname\":\"Rak\"," ++
        "\"email\":\"mariusz@skrivapa.se\"}]}") -> return ()
    _ -> error "Did not return correct json"
