module Doc.TestJSON where

import qualified Data.ByteString.UTF8 as BS
import Test.Framework
import Test.QuickCheck
import Text.JSON()
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Util.JSON
import Misc
import Doc.JSON
import TestingUtil()
import Doc.DocStateData
import Doc.DocInfo

documentJSONTests :: Test
documentJSONTests = testGroup "Document JSON tests" [
  testProperty "document_id must be equal" 
  (\doc -> True ==> 
           documentid doc == (read $ fromJSONString $ fromRight $ jsget "document_id" (jsonDocumentForSignatory doc))),

  testProperty "title must be equal"
  (\doc -> True ==>
           (BS.toString $ documenttitle doc) == (fromJSONString $ fromRight $ jsget "title" (jsonDocumentForSignatory doc))),

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
    (\doc -> documentstatus doc == Pending || documentstatus doc == AwaitingAuthor ==>
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

