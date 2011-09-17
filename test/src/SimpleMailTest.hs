module SimpleMailTest where

import API.MailAPI

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)

import Text.JSON.String
import Data.List

simpleMailTests :: Test
simpleMailTests = testGroup "Simple Mail Tests" [
  blankEmailTest
  ,singleMinimalSignatory
  ,doubleMinimalSignatory
  ,doubleOptionalFieldsSignatory
  ,doubleOptionalFieldsBadFieldSignatory
  ,doubleOptionalFieldsNoFirstNameSignatory
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

doubleMinimalSignatory :: Test
doubleMinimalSignatory = testCase "Double Minimal Signatory" $ do
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\n\n" ++ 
        "First name: Eric\nLast name: Normand\nemail: eric@skrivapa.se") of
    Left msg -> error msg
    a | a == runGetJSON readJSValue ("{\"title\":\"Contract Title\"," ++
        "\"involved\":[{\"fstname\":\"Mariusz\"," ++
        "\"sndname\":\"Rak\"," ++
        "\"email\":\"mariusz@skrivapa.se\"}," ++
        "{\"fstname\":\"Eric\"," ++
        "\"sndname\":\"Normand\"," ++
        "\"email\":\"eric@skrivapa.se\"}" ++
        "]}") -> return ()
    _ -> error "Did not return correct json"

doubleOptionalFieldsSignatory :: Test
doubleOptionalFieldsSignatory = testCase "Double Optional Fields Signatory" $ do
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\nOrganization number : 78765554\n \n" ++ 
        "personal number: 78676545464  \nFirs name: Eric\nLast name: Normand\nemail: eric@skrivapa.se\ncompany  :Hello\n\n\n  \n") of
    Left msg -> error msg
    a | a == runGetJSON readJSValue ("{\"title\":\"Contract Title\"," ++
        "\"involved\":[{\"fstname\":\"Mariusz\"," ++
        "\"sndname\":\"Rak\"," ++
        "\"email\":\"mariusz@skrivapa.se\"," ++
        "\"companynr\":\"78765554\"" ++
        "}," ++
        "{" ++
        "\"fstname\":\"Eric\"," ++
        "\"sndname\":\"Normand\"," ++
        "\"email\":\"eric@skrivapa.se\"," ++
        "\"company\":\"Hello\"," ++
        "\"personalnr\": \"78676545464\"}" ++
        "]}") -> return ()
    _ -> error "Did not return correct json"

doubleOptionalFieldsBadFieldSignatory :: Test
doubleOptionalFieldsBadFieldSignatory = testCase "Double Optional Fields Bad Field Signatory" $ do
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\nOrganization number : 78765554\nJones : 9090 \n \n" ++ 
        "personal number: 78676545464  \nFirst name: Eric\nLast name: Normand\nemail: eric@skrivapa.se\ncompany  :Hello\n\n\n  \n") of
    Left msg | "Jones" `isInfixOf` msg -> return ()
    Left _ -> error "Did not talk about bad field name: Jones"
    _ -> error "Did not fail with bad field name"
    
doubleOptionalFieldsNoFirstNameSignatory :: Test
doubleOptionalFieldsNoFirstNameSignatory = testCase "Double Optional Fields Bad Field Signatory" $ do
  case parseSimpleEmail "Contract Title"
       ("Last name: Rak\nemail: mariusz@skrivapa.se\nOrganization number : 78765554\n\n \n" ++ 
        "personal number: 78676545464  \nFirst name: Eric\nLast name: Normand\nemail: eric@skrivapa.se\ncompany  :Hello\n\n\n  \n") of
    Left msg | "First name" `isInfixOf` msg -> return ()
    Left _ -> error "Did not talk about missing field: First name"
    _ -> error "Did not fail with mssing field "


