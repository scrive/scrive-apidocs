module Doc.TestJSON where

import Test.Framework
import Test.QuickCheck
import Text.JSON.Gen

import Doc.JSON
import Doc.DocStateData
import TestingUtil
import TestKontra

documentJSONTests :: TestEnvSt -> Test
documentJSONTests env = testGroup "Document JSON tests" [
  testThat "test general api document json creation" env dcrTest

{-
 ,
-}
 ]
dcrTest :: TestEnv ()
dcrTest = doTimes 100 $ do
  (mainfile, title, fn, sn, em) <- rand 10 arbitrary
  let o = dcrFromJSON $ runJSONGen $ do
            object "mainfile" $ do value "name" mainfile
            value "title"     $ title
            objects "involved" $ [ object "data" $ do
                                     object "fstname" $ do value "value" fn
                                     object "sndname" $ do value "value" sn
                                     object "email" $ do value "value" em
                                 ]
      dcr :: Either String DocumentCreationRequest = Right $ DocumentCreationRequest {
        dcrTitle = Just title,
        dcrType  = Signable Contract,
        dcrTags  = [],
        dcrInvolved = [InvolvedRequest { irIsAuthor = False,
                                         irIsPartner = True,
                                         irData = [SignatoryField FirstNameFT fn True False [],
                                                   SignatoryField LastNameFT  sn True False [],
                                                   SignatoryField EmailFT     em True False []],
                                         irAttachments = [],
                                         irSignOrder = Nothing }],
        dcrMainFile = Just mainfile,
        dcrAttachments = []
        }
  assertEqual ("Are not the same! " ++ show o ++ " and " ++ show dcr) o dcr
