module StatsTest (statsTests) where

import DB.Classes
import User.Model
import Doc.Transitory
import Doc.DocUtils
import Doc.DocStateData
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import Doc.Invariants
import MinutesTime
import Test.HUnit.Base (Assertion)
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import qualified Data.ByteString.UTF8 as BS

import Data.Functor
import Data.Maybe
import Data.Convertible(convert)
import Database.HDBC(SqlValue)
import Database.HDBC.PostgreSQL
import Control.Monad
import Control.Monad.Trans
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import File.FileID
import Stats.Control
import Stats.Model

statsTests :: Connection -> Test
statsTests conn = testGroup "Stats" 
  [
    testCase "test invite stat" $ testInviteStat conn
  ]

testInviteStat :: Connection -> Assertion
testInviteStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." (length stats') 0
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending did
  Right doc <- dbUpdate $ SetInvitationTime did time 0
  _ <- addSignStatInviteEvent doc
  stats <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length stats') 1
  assertEqual ("Should be Invite stat but was " ++ show (ssQuantity $ head stats')) (ssQuantity $ head stats') SignStatInvite
