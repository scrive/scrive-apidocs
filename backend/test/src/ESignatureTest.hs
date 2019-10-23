module ESignatureTest (eSignatureTests) where

import Test.Framework
import Test.QuickCheck

import DB hiding (query, update)
import Doc.DocStateData
import EID.Signature.Model
import TestingUtil
import TestKontra as T
import Util.SignatoryLinkUtils

eSignatureTests :: TestEnvSt -> Test
eSignatureTests env = testGroup "E-signature" [
    testThat "can perform operations on CGI Swedish BankID signatures in the database" env testCGISEBankIDSignatures
  ]

testCGISEBankIDSignatures :: TestEnv ()
testCGISEBankIDSignatures = do
  author <- addNewRandomUser
  doc <- addRandomDocument (rdaDefault author)
    { rdaStatuses = OneOf [Pending]
    , rdaTypes = OneOf [Signable]
    }
  let Just SignatoryLink{signatorylinkid = slid} = getAuthorSigLink doc
  bids :: CGISEBankIDSignature <- rand 20 arbitrary
  dbUpdate $ MergeCGISEBankIDSignature slid bids
  Just (CGISEBankIDSignature_ bids') <- dbQuery $ GetESignature slid
  assertEqual "Signatures are the same" bids bids'
  let new_bids = bids {
        cgisebidsSignatoryName = cgisebidsSignatoryName bids <> "!"
      }
  dbUpdate $ MergeCGISEBankIDSignature slid new_bids
  Just (CGISEBankIDSignature_ new_bids') <- dbQuery $ GetESignature slid
  assertEqual "Updated signatures are the same" new_bids new_bids'
