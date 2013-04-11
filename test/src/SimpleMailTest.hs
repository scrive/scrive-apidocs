module SimpleMailTest where

import ScriveByMail.Parse

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import qualified Log
import Utils.String

import Data.Maybe

import Data.List
import Doc.DocStateData

simpleMailTests :: Test
simpleMailTests = testGroup "Simple Mail Tests" [
  blankEmailTest
  ,singleMinimalSignatory
  ,doubleMinimalSignatory
  ,doubleOptionalFieldsSignatory
  ,doubleOptionalFieldsBadFieldSignatory
  ,doubleOptionalFieldsNoFirstNameSignatory
  ,stupidEmailSignature
  ,looksLikeSignature
  ,doubleOptionalFieldsWeirdSignatory
  ,testMinimumDistance
  ,testWackySignature
  ,testExtraSpaces
  --,testExtraSpaces2
  ]

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
    Right (title, [sig]) |
      title == "Contract Title" &&
      sig == SignatoryDetails { signatorysignorder = SignOrder 0,
                                signatoryfields = [ SignatoryField FirstNameFT "Mariusz" True False []
                                                  , SignatoryField LastNameFT  "Rak" True False []
                                                  , SignatoryField EmailFT     "mariusz@skrivapa.se" True False []],
                                signatoryisauthor = False,
                                signatoryispartner = False
                              }
      -> return ()
    _ -> error "Did not return correct details"

doubleMinimalSignatory :: Test
doubleMinimalSignatory = testCase "Double Minimal Signatory" $ do
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\n\n" ++
        "First name: Eric\nLast name: Normand\nemail: eric@skrivapa.se") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Contract Title" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Mariusz" True False []
                                              , SignatoryField LastNameFT  "Rak" True False []
                                              , SignatoryField EmailFT     "mariusz@skrivapa.se" True False []] False False,
               SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Eric" True False []
                                              , SignatoryField LastNameFT  "Normand" True False []
                                              , SignatoryField EmailFT     "eric@skrivapa.se" True False []] False False]
      -> return ()
    _ -> error "Did not return correct json"

doubleOptionalFieldsSignatory :: Test
doubleOptionalFieldsSignatory = testCase "Double Optional Fields Signatory" $ do
  case parseSimpleEmail "Contract Title2"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\nOrganization number : 78765554\n \n" ++
        "personal number: 78676545464  \nFirs name: Eric\nLast name: Normand\nemail: eric@skrivapa.se\ncompany  :Hello\n\n\n  \n") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Contract Title2" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Mariusz" True False []
                                              , SignatoryField LastNameFT  "Rak" True False []
                                              , SignatoryField EmailFT     "mariusz@skrivapa.se" True False []
                                              , SignatoryField CompanyNumberFT "78765554" True False []] False False,
               SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Eric" True False []
                                              , SignatoryField LastNameFT  "Normand" True False []
                                              , SignatoryField EmailFT     "eric@skrivapa.se" True False []
                                              , SignatoryField CompanyFT   "Hello" True False []
                                              , SignatoryField PersonalNumberFT "78676545464" True False []] False False]
      -> return ()
    a -> do
      Log.debug $ "JSON returned from parse: " ++ show a
      error "Did not return correct json"

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

stupidEmailSignature :: Test
stupidEmailSignature = testCase "Stupid email signature" $ do
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\n\n \n" ++
        "First name: Eric\nLast name: Normand\nemail: eric@skrivapa.se\n"
        ++"________________________________________\n"
        ++"\n"
        ++"Lukas Duczko, CEO\n"
        ++"\n"
        ++"SkrivaP=E5 CM AB\n"
        ++"Phone: +46 (0)70 456 04 04\n"
        ++"E-mail: lukas@skrivapa.se\n"
        ++"Homepage: https://skrivapa.se\n"
        ++"________________________________________\n") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Contract Title" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Mariusz" True False []
                                              , SignatoryField LastNameFT  "Rak" True False []
                                              , SignatoryField EmailFT     "mariusz@skrivapa.se" True False []] False False,
               SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Eric" True False []
                                              , SignatoryField LastNameFT  "Normand" True False []
                                              , SignatoryField EmailFT     "eric@skrivapa.se" True False []] False False]
      -> return ()
    _ -> error "Did not return correct json"

looksLikeSignature :: Test
looksLikeSignature = testCase "Stupid email looks like signatory signature" $ do
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\n\n \n" ++
        "First name: Eric\nLast name: Normand\nemail: eric@skrivapa.se\n\n\n"
        ++"________________________________________\n"
        ++"Phone: +46 (0)70 456 04 04\n"
        ++"E-mail: lukas@skrivapa.se\n"
        ++"Homepage: https://skrivapa.se\n"
        ++"________________________________________\n") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Contract Title" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Mariusz" True False []
                                              , SignatoryField LastNameFT  "Rak" True False []
                                              , SignatoryField EmailFT     "mariusz@skrivapa.se" True False []] False False,
               SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Eric" True False []
                                              , SignatoryField LastNameFT  "Normand" True False []
                                              , SignatoryField EmailFT     "eric@skrivapa.se" True False []] False False]
      -> return ()
    _ -> error "Did not return correct values"

doubleOptionalFieldsWeirdSignatory :: Test
doubleOptionalFieldsWeirdSignatory = testCase "Double Optional Fields Weird Signatory" $ do
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\nOrg num : 78765554\n \n" ++
        "pers number: 78676545464  \nFirs name: Eric\nLast name: Normand\nemail: eric@skrivapa.se\ncompany  :Hello\n\n\n  \n") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Contract Title" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Mariusz" True False []
                                              , SignatoryField LastNameFT  "Rak" True False []
                                              , SignatoryField EmailFT     "mariusz@skrivapa.se" True False []
                                              , SignatoryField CompanyNumberFT "78765554" True False []] False False,
               SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Eric" True False []
                                              , SignatoryField LastNameFT  "Normand" True False []
                                              , SignatoryField EmailFT     "eric@skrivapa.se" True False []
                                              , SignatoryField CompanyFT   "Hello" True False []
                                              , SignatoryField PersonalNumberFT "78676545464" True False []] False False]
      -> return ()
    a -> error $ "Did not return correct value: " ++ show a

testMinimumDistance :: Test
testMinimumDistance = testCase "Test minimum distance between keys" $
  case catMaybes [if maxLev a' b' 2
                  then Just (a', b')
                  else Nothing
                 |a <- allStrings
                 ,b <- allStrings
                 ,a /= b
                 ,a' <- a
                 ,b' <- b] of
    [] -> return ()
    a -> error $ "These strings are too close: " ++ intercalate ";" (map show a)

testWackySignature :: Test
testWackySignature = testCase "Test wacky signature (rtf)" $
  case parseSimpleEmail "Contract Title"
       ("First name: Mariusz\nLast name: Rak\nemail: mariusz@skrivapa.se\nOrg num : 78765554\n \n" ++
        "=20" ++
        " Marcus Thomasson\n" ++
        "=20\n" ++
        " <image001.gif>\n" ++
        "=20\n" ++
        "=20\n" ++
        " marcus.thomasson@fortnox.se\n" ++
        " Mobil: 0704 40 28 86\n" ++
        " Skype: matho80\n" ++
        "=20\n" ++
        " Fortnox AB (publ)\n" ++
        " Box 427, 351 06 V=E4xj=F6\n" ++
        " Tel vxl:  0470-78 50 00\n" ++
        " Fax: 0470-78 50 01\n" ++
        " www.fortnox.se\n" ++
        " =20\n" ++
        " =20\n") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Contract Title" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "Mariusz" True False []
                                              , SignatoryField LastNameFT  "Rak" True False []
                                              , SignatoryField EmailFT     "mariusz@skrivapa.se" True False []
                                              , SignatoryField CompanyNumberFT "78765554" True False []] False False]
      -> return ()
    a ->  do
      Log.debug $ "JSON returned from parse: " ++ show a
      error ("Did not return correct json; got: " ++ show a)

testExtraSpaces :: Test
testExtraSpaces = testCase "Test that those stupid clients that think Return means start new paragraph are parsed in a way that stops generating bug reports." $
  case parseSimpleEmail "Dude!"
       ("First name: eric " ++
        "\n" ++
        "\n" ++
        "Last name: normand " ++
        "\n" ++
        "\n" ++
        "email:ericwnormand@gmail.com" ++
        "\n" ++
        "\n") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Dude!" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "eric" True False []
                                              , SignatoryField LastNameFT "normand" True False []
                                              , SignatoryField EmailFT "ericwnormand@gmail.com" True False []] False False]
      -> return ()
    a -> do
      error ("Did not return correct json; got: " ++ show a)

{- Doesn't work! No way to parse it.
testExtraSpaces2 :: Test
testExtraSpaces2 = testCase "Test that those stupid clients that think Return means start new paragraph are parsed in a way that stops generating bug reports." $
  case parseSimpleEmail "Dude!"
       ("First name: eric " ++
        "\n" ++
        "\n" ++
        "Last name: normand " ++
        "\n" ++
        "\n" ++
        "email:\nericwnormand@gmail.com " ++
        "\n" ++
        "\n" ++
        "first name: eric " ++
        "\n" ++
        "\n" ++
        "last name: normand " ++
        "\n" ++
        "\n" ++
        "email:\n" ++
        "eric@scrive.com ") of
    Left msg -> error msg
    Right (title, sigs) |
      title == "Dude!" &&
      sigs == [SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "eric" True []
                                              , SignatoryField LastNameFT "normand" True []
                                              , SignatoryField EmailFT "ericwnormand@gmail.com" True []]
              ,SignatoryDetails (SignOrder 0) [ SignatoryField FirstNameFT "eric" True []
                                              , SignatoryField LastNameFT "normand" True []
                                              , SignatoryField EmailFT "eric@scrive.com" True []]]
      -> return ()
    a -> do
      error ("Did not return correct json; got: " ++ show a)

-}