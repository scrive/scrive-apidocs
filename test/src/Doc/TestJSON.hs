module Doc.TestJSON where

import Test.Framework
import Test.QuickCheck
import Text.JSON.Gen
import Text.JSON.FromJSValue
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Maybe (isNothing, fromJust)

import Doc.JSON
import Doc.DocStateData
import Doc.DocInfo
import TestingUtil
import TestKontra

getFromDoc :: FromJSValue a => Document -> String -> a
getFromDoc doc s =
  fromJust $ fromJSValueField s $ jsonDocumentForSignatory doc

documentJSONTests :: TestEnvSt -> Test
documentJSONTests env = testGroup "Document JSON tests" [
  testThat "test general api document json creation" env dcrTest,
  testProperty "document_id must be equal"
  (\doc -> True ==> documentid doc == (read $ getFromDoc doc "document_id")),

  testProperty "title must be equal"
  (\doc -> True ==> documenttitle doc == (getFromDoc doc "title")),

  testProperty "type must be correct" $ \doc ->
    let dt = getFromDoc doc "type" :: Int in
    disjoin
    [ documenttype doc == Signable Contract ==> 1 == dt
    , documenttype doc == Signable Offer ==>    3 == dt
    , documenttype doc == Template Contract ==> 2 == dt
    , documenttype doc == Template Offer ==>    4 == dt
    ],

  testProperty "Preparation" $ \doc ->
    let s = getFromDoc doc "status" :: Int in
    disjoin
    [ documentstatus doc == Preparation ==>  0 <= s && s <= 9
    , documentstatus doc == Pending ==>      10 <= s && s <= 19
    , documentstatus doc `elem` [Rejected, Canceled, Timedout] ==>  20 <= s && s <= 29
    , documentstatus doc == Closed ==>       30 <= s && s <= 39
    , isDocumentError doc          ==> 40 <= s && s <= 49
    ],
  testProperty "does not have files"
    (\doc -> True ==> (isNothing (fromJSValueField "files" (jsonDocumentForSignatory doc) :: Maybe String))),
  testProperty "delivery" $ \doc ->
      let da = getFromDoc doc "delivery" :: Int in
      disjoin
      [ EmailDelivery == documentdeliverymethod doc ==> 1 == da
      , PadDelivery == documentdeliverymethod doc   ==> 2 == da
      ]
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
                                         irData = [SignatoryField FirstNameFT fn True [],
                                                   SignatoryField LastNameFT  sn True [],
                                                   SignatoryField EmailFT     em True []],
                                         irAttachments = [],
                                         irSignOrder = Nothing }],
        dcrMainFile = Just mainfile,
        dcrAttachments = []
        }
  assertEqual ("Are not the same! " ++ show o ++ " and " ++ show dcr) o dcr
