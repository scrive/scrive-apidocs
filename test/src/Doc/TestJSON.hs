module Doc.TestJSON where

import Test.Framework
import Test.QuickCheck
import Text.JSON
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Util.JSON
import Misc
import Doc.JSON
import Doc.DocStateData
import Doc.DocInfo
import TestingUtil
import TestKontra

documentJSONTests :: TestEnvSt -> Test
documentJSONTests env = testGroup "Document JSON tests" [
  testThat "test general api document json creation" env dcrTest,
  testProperty "document_id must be equal" 
  (\doc -> True ==> 
           documentid doc == (read $ fromJSONString $ fromRight $ jsget "document_id" (jsonDocumentForSignatory doc))),

  testProperty "title must be equal"
  (\doc -> True ==>
           (documenttitle doc) == (fromJSONString $ fromRight $ jsget "title" (jsonDocumentForSignatory doc))),

  testGroup "type must be correct" [
    testProperty "Signable Contract" 
    (\doc -> documenttype doc == Signable Contract ==>
             1 == (fromJSONRational $ fromRight $ jsget "type" (jsonDocumentForSignatory doc))),
    testProperty "Signable Offer" 
    (\doc -> documenttype doc == Signable Offer ==>
             3 == (fromJSONRational $ fromRight $ jsget "type" (jsonDocumentForSignatory doc))),
    testProperty "Signable Offer" 
    (\doc -> documenttype doc == Template Contract ==>
             2 == (fromJSONRational $ fromRight $ jsget "type" (jsonDocumentForSignatory doc))),
    testProperty "Signable Offer" 
    (\doc -> documenttype doc == Template Offer ==>
             4 == (fromJSONRational $ fromRight $ jsget "type" (jsonDocumentForSignatory doc)))
    ],
  
  testGroup "status" [
    testProperty "Preparation"
    (\doc -> documentstatus doc == Preparation ==>
             let s = (fromJSONRational $ fromRight $ jsget "status" (jsonDocumentForSignatory doc)) in
             0 <= s && s <= 9),
    testProperty "Pending"
    (\doc -> documentstatus doc == Pending ==>
             let s = (fromJSONRational $ fromRight $ jsget "status" (jsonDocumentForSignatory doc)) in
             10 <= s && s <= 19),
    testProperty "Rejected"
    (\doc -> documentstatus doc == Rejected || documentstatus doc == Canceled || documentstatus doc == Timedout ==>
             let s = (fromJSONRational $ fromRight $ jsget "status" (jsonDocumentForSignatory doc)) in
             20 <= s && s <= 29),
    testProperty "Closed"
    (\doc -> documentstatus doc == Closed ==>
             let s = (fromJSONRational $ fromRight $ jsget "status" (jsonDocumentForSignatory doc)) in
             30 <= s && s <= 39),
    testProperty "Error"
    (\doc -> isDocumentError doc ==>
             let s = (fromJSONRational $ fromRight $ jsget "status" (jsonDocumentForSignatory doc)) in
             40 <= s && s <= 49)
    ],
    testProperty "does not have files"
    (\doc -> True ==> (isLeft $ jsget "files" (jsonDocumentForSignatory doc))),
    testGroup "authorization" [
      testProperty "email"
      (\doc -> EmailIdentification `elem` documentallowedidtypes doc ==>
               1 == (fromJSONRational $ fromRight $ jsget "authorization" (jsonDocumentForSignatory doc))),
      testProperty "elegitimation"
      (\doc -> ELegitimationIdentification `elem` documentallowedidtypes doc ==>
               10 == (fromJSONRational $ fromRight $ jsget "authorization" (jsonDocumentForSignatory doc)))
      ]
  ]

dcrTest :: TestEnv ()
dcrTest = doNTimes 100 $ do
  (mainfile, title, fn, sn, em) <- rand 10 arbitrary
  let o = dcrFromJSON $ JSObject $ toJSObject $
          [("mainfile", JSObject $ toJSObject $ 
                        [("name", JSString $ toJSString mainfile)])
          ,("title"   , JSString $ toJSString title)
          ,("involved", JSArray [JSObject $ toJSObject $
                                 [("data", JSObject $ toJSObject $
                                           [("fstname", JSObject $ toJSObject $
                                                        [("value", JSString $ toJSString fn)]),
                                            ("sndname", JSObject $ toJSObject $
                                                        [("value", JSString $ toJSString sn)]),
                                            ("email",   JSObject $ toJSObject $
                                                        [("value", JSString $ toJSString em)])])]])]
      dcr :: Either String DocumentCreationRequest = Right $ DocumentCreationRequest {
        dcrTitle = title,
        dcrType  = Signable Contract,
        dcrTags  = [],
        dcrInvolved = [InvolvedRequest { irRole = [SignatoryPartner],
                                         irData = [SignatoryField FirstNameFT fn [],
                                                   SignatoryField LastNameFT  sn [],
                                                   SignatoryField EmailFT     em []],
                                         irAttachments = [] }],
        dcrMainFile = mainfile
        }
  assertEqual ("Are not the same! " ++ show o ++ " and " ++ show dcr) o dcr
