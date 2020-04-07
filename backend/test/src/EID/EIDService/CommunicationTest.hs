module EID.EIDService.CommunicationTest (nemidTests) where

import Test.Framework

import EID.EIDService.Types
import TestingUtil
import TestKontra

nemidTests :: TestEnvSt -> Test
nemidTests env = testGroup
  "NemID"
  [
  -- Primitive properties
    testThat "Date of birth is properly resolved from SSN"
             env
             testDateOfBirthIsProperlyResolved
  ]

testDateOfBirthIsProperlyResolved :: TestEnv ()
testDateOfBirthIsProperlyResolved = do
  dateOfBirthEqual "22.05.2050" "220550-6218"
  dateOfBirthEqual "10.10.2010" "101010-4321"
  dateOfBirthEqual "10.10.1897" "101097-8726"
  dateOfBirthEqual "23.10.1945" "231045-0637"
  where
    dateOfBirthEqual :: Text -> Text -> TestEnv ()
    dateOfBirthEqual expectedDateOfBirth dkPersonalNumber =
      assertEqual "Date of birth is not equal to desired one" expectedDateOfBirth
        $ dateOfBirthFromDKPersonalNumber dkPersonalNumber

