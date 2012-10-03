module Doc.TestJSON where

import Test.Framework
import Test.QuickCheck
import Text.JSON
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Util.JSON
import Utils.Either
import Doc.JSON
import Doc.DocStateData
import Doc.DocInfo
import TestingUtil
import TestKontra

docjson :: JSONPath path => Document -> path -> JSValue
docjson doc s = fromRight $ jsget s $ jsonDocumentForSignatory doc

documentJSONTests :: TestEnvSt -> Test
documentJSONTests env = testGroup "Document JSON tests" [
  testThat "test general api document json creation" env dcrTest,
  testProperty "document_id must be equal"
  (\doc -> True ==> documentid doc == (read $ fromJSONString $ docjson doc "document_id")),

  testProperty "title must be equal"
  (\doc -> True ==> documenttitle doc == (fromJSONString $ docjson doc "title")),

  testProperty "type must be correct" $ \doc ->
    let dt = fromJSONRational $ docjson doc "type" in
    disjoin
    [ documenttype doc == Signable Contract ==> 1 == dt
    , documenttype doc == Signable Offer ==>    3 == dt
    , documenttype doc == Template Contract ==> 2 == dt
    , documenttype doc == Template Offer ==>    4 == dt
    ],

  testProperty "Preparation" $ \doc ->
    let s = fromJSONRational $ docjson doc "status" in
    disjoin
    [ documentstatus doc == Preparation                        ==> 0 <= s && s <= 9
    , documentstatus doc == Pending                            ==> 10 <= s && s <= 19
    , documentstatus doc `elem` [Rejected, Canceled, Timedout] ==> 20 <= s && s <= 29
    , documentstatus doc == Closed                             ==> 30 <= s && s <= 39
    , isDocumentError doc                                      ==> 40 <= s && s <= 49
    ],
    testProperty "does not have files"
    (\doc -> True ==> (isLeft $ jsget "files" (jsonDocumentForSignatory doc))),
    testProperty "authentication" $ \doc ->
      let da = fromJSONRational $ docjson doc "authentication" in
      disjoin
      [ StandardAuthentication == documentauthenticationmethod doc  ==> 1 == da
      , ELegAuthentication == documentauthenticationmethod doc   ==> 2 == da
      ],
    testProperty "delivery" $ \doc ->
      let da = fromJSONRational $ docjson doc "delivery" in
      disjoin
      [ EmailDelivery == documentdeliverymethod doc ==> 1 == da
      , PadDelivery == documentdeliverymethod doc   ==> 2 == da
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
        dcrTitle = Just title,
        dcrType  = Signable Contract,
        dcrTags  = [],
        dcrInvolved = [InvolvedRequest { irRole = [SignatoryPartner],
                                         irData = [SignatoryField FirstNameFT fn [],
                                                   SignatoryField LastNameFT  sn [],
                                                   SignatoryField EmailFT     em []],
                                         irAttachments = [],
                                         irSignOrder = Nothing }],
        dcrMainFile = Just mainfile,
        dcrAttachments = []
        }
  assertEqual ("Are not the same! " ++ show o ++ " and " ++ show dcr) o dcr
