module ESignatureTest (eSignatureTests) where

import Data.Monoid
import Test.Framework
import Test.QuickCheck

import Control.Logic
import DB hiding (query, update)
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import EID.Signature.Model
import TestingUtil
import TestKontra as T
import Util.SignatoryLinkUtils

eSignatureTests :: TestEnvSt -> Test
eSignatureTests env = testGroup "E-signature" [
    testThat "can perform operations on BankID signatures in the database" env testBankIDSignatures
  ]

testBankIDSignatures :: TestEnv ()
testBankIDSignatures = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just SignatoryLink{signatorylinkid = slid} = getAuthorSigLink doc
  bids :: BankIDSignature <- rand 20 arbitrary
  dbUpdate $ MergeBankIDSignature slid bids
  Just (BankIDSignature_ bids') <- dbQuery $ GetESignature slid
  assertEqual "Signatures are the same" bids bids'
  let new_bids = bids {
        bidsSignatoryName = bidsSignatoryName bids <> "!"
      }
  dbUpdate $ MergeBankIDSignature slid new_bids
  Just (BankIDSignature_ new_bids') <- dbQuery $ GetESignature slid
  assertEqual "Updated signatures are the same" new_bids new_bids'
