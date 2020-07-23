{-# OPTIONS_GHC -Wno-orphans #-}
module Flow.DocumentCheckerTest where

import Data.Set (Set)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base ((@=?), Assertion)
import Test.QuickCheck
import qualified Data.Set as Set

import Doc.DocumentID
import Doc.SignatoryLinkID
import Flow.DocumentChecker
import Flow.Model.Types.FlowUserId
import Flow.VariableCollector
import User.UserID

tests :: Test
tests = testGroup
  "DocumentChecker"
  [ testCase "simple scenario" testSimpleScenario
  , testProperty "matched users match"                  testMatchedUsersMatch
  , testProperty "unmatched flow user IDs do not match" testUnmatchedFlowUserIdsDoNotMatch
  , testProperty "unmatched signatories do not match"   testUnmatchedSignatoriesDoNotMatch
  ]

testSimpleScenario :: Assertion
testSimpleScenario = expected @=? output
  where
    expected             = MatchingResult { .. }
    output               = matchUsers flowRoles signatoryRoles
    matched              = Set.fromList [UserMatch (Email email) signatory1]
    unmatchedFlowUserIds = Set.fromList [flowRole]
    unmatchedSignatories = Set.fromList [sigRole]
    flowRoles = Set.fromList [DocRoleFor SigningParty (Email email) docId, flowRole]
    flowRole             = DocRoleFor Approver (UserId uid) docId
    signatoryRoles = Set.fromList [DocRoleFor SigningParty signatory1 docId, sigRole]
    uid                  = unsafeUserID 1
    sigRole              = DocRoleFor Viewer signatory2 docId
    signatory1 = Signatory (unsafeSignatoryLinkID 1) (Just email) Nothing Nothing
    signatory2 = Signatory (unsafeSignatoryLinkID 2) Nothing (Just "123") Nothing
    docId                = unsafeDocumentID 1
    email                = "foo@bar.com"

-- Check that signatories' data is unique per document.
uniqueSignatories :: Set SignatoryDocRole -> Bool
uniqueSignatories sigs =
  checkUnique email && checkUnique phoneNumber && checkUnique userId
  where
    checkUnique :: Ord a => (Signatory -> Maybe a) -> Bool
    checkUnique f = Set.size just
      == Set.size (Set.map (\sdr -> (document sdr, f $ user sdr)) just)
      where just = Set.filter (isJust . f . user) sigs

-- Check that every signatory has a unique ID.
uniqueSignatoryIds :: Set SignatoryDocRole -> Bool
uniqueSignatoryIds sigs = Set.size users == Set.size (Set.map id users)
  where users = Set.map user sigs

-- Check that users match in all the returned matches.
testMatchedUsersMatch :: Set FlowUserIdDocRole -> Set SignatoryDocRole -> Property
testMatchedUsersMatch flows sigs =
  uniqueSignatoryIds sigs ==> property . all isCorrect . Set.toList $ matched output
  where
    isCorrect UserMatch {..} = flowUserIdMatchesSignatory flowUserId signatory
    output = matchUsers flows sigs

-- Check that unmatched flow users really do not match.
testUnmatchedFlowUserIdsDoNotMatch
  :: Set FlowUserIdDocRole -> Set SignatoryDocRole -> Property
testUnmatchedFlowUserIdsDoNotMatch flows sigs =
  uniqueSignatoryIds sigs ==> property . all isCorrect . Set.toList $ unmatchedFlowUserIds
    output
  where
    isCorrect flowUserIdDocRole = not $ any (docRolesMatch flowUserIdDocRole) sigs
    output = matchUsers flows sigs

-- Check that unmatched signatories really do not match.
testUnmatchedSignatoriesDoNotMatch
  :: Set FlowUserIdDocRole -> Set SignatoryDocRole -> Property
testUnmatchedSignatoriesDoNotMatch flows sigs =
  uniqueSignatoryIds sigs
    &&  uniqueSignatories sigs
    ==> property
    .   all isCorrect
    .   Set.toList
    $   unmatchedSignatories output
  where
    isCorrect signatoryDocRole = not $ any (`docRolesMatch` signatoryDocRole) flows
    output = matchUsers flows sigs

-- Arbitrary instances in this module are deliberately based on tiny sets
-- so that we can exhaustively check matching over all combinations of their elements.

instance Arbitrary Role where
  arbitrary = elements [Approver, Viewer, SigningParty]

instance Arbitrary DocumentID where
  arbitrary = elements [unsafeDocumentID 1, unsafeDocumentID 2]

instance Arbitrary SignatoryLinkID where
  arbitrary = unsafeSignatoryLinkID <$> arbitrary

fewEmails :: Gen Text
fewEmails = elements ["splendid@scrive.com", "keep@calm.co.uk", "tests@are.fun"]

fewPhoneNumbers :: Gen Text
fewPhoneNumbers = elements ["1729", "2020", "666"]

fewUserIds :: Gen UserID
fewUserIds = elements [unsafeUserID 1, unsafeUserID 2, unsafeUserID 3]

instance Arbitrary FlowUserId where
  arbitrary =
    oneof [Email <$> fewEmails, PhoneNumber <$> fewPhoneNumbers, UserId <$> fewUserIds]

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen x = oneof [pure Nothing, Just <$> x]

instance Arbitrary Signatory where
  arbitrary =
    Signatory
      <$> arbitrary
      <*> maybeGen fewEmails
      <*> maybeGen fewPhoneNumbers
      <*> maybeGen fewUserIds

instance (Arbitrary u, Arbitrary d) => Arbitrary (DocRoleFor u d) where
  arbitrary = DocRoleFor <$> arbitrary <*> arbitrary <*> arbitrary
