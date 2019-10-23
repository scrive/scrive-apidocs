module Generators.DocumentGeneratorsTest
  ( documentGeneratorsTests
  ) where

import Test.Framework

import Doc.API.V2.Guards
import File.Model
import Generators.DocumentGenerators
import Generators.OccurenceControl
import TestingUtil
import TestKontra
import User.Types.User

documentGeneratorsTests :: TestEnvSt -> Test
documentGeneratorsTests env = testGroup
  "Document generators"
  [ testThat "startableDocumentOC generates startable documents"
             env
             testStartableDocumentOC
  ]

testStartableDocumentOC :: TestEnv ()
testStartableDocumentOC = do
  user <- addNewRandomUser
  fid  <- addNewRandomFile
  file <- randomQuery $ GetFileByFileID fid
  let generator = startableDocumentOC (userid user) file
  testGeneratorHelper 0.2 generator (isJust . documentCanBeStarted)
