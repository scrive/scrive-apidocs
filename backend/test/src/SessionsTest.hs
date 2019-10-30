module SessionsTest (sessionsTests) where

import Data.Int
import Data.Time
import Happstack.Server
import Test.Framework
import Test.QuickCheck

import BrandedDomain.Model
import Context
import DB hiding (query, update)
import Doc.DocStateData
import Doc.Tokens.Model
import EID.CGI.GRP.Transaction.Model
import KontraMonad
import MinutesTime
import Session.Constant
import Session.Model
import Session.Types
import TestingUtil
import TestKontra as T
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.SignatoryLinkUtils

testCookieDomain :: Text
testCookieDomain = "some.domain.com"

sessionsTests :: TestEnvSt -> Test
sessionsTests env = testGroup
  "Sessions"
  [ testThat "can create new session"       env testNewSessionInsertion
  , testThat "can update existing session"  env testSessionUpdate
  , testThat "can insert document ticket"   env testDocumentTicketInsertion
  , testThat "can reinsert document ticket" env testDocumentTicketReinsertion
  , testThat "can add eleg transaction"     env testElegTransactionInsertion
  , testThat "can update eleg transaction"  env testElegTransactionUpdate
  , testThat "test default session timeout" env testDefaultSessionTimeout
  , testThat "can set custom session timeout in group settings"
             env
             testCustomSessionTimeout
  , testThat "test default session timeout delay" env testDefaultSessionTimeoutDelay
  , testThat "test custom session timeout delay"  env testCustomSessionTimeoutDelay
  , testThat "test custom session timeout inheritance"
             env
             testCustomSessionTimeoutInheritance
  ]

testNewSessionInsertion :: TestEnv ()
testNewSessionInsertion = do
  uid <- createTestUserAndGetId
  replicateM_ 60 $ do
    void $ insertNewSession uid
  runSQL_ $ "SELECT COUNT(*) FROM sessions WHERE user_id =" <?> uid
  user_sessions :: Int64 <- fetchOne runIdentity
  assertEqual "there are only 51 sessions for one user" 51 user_sessions

testSessionUpdate :: TestEnv ()
testSessionUpdate = do
  uid         <- createTestUserAndGetId
  (sess, ctx) <- insertNewSession uid
  void $ do
    rq <- mkRequest GET []
    runTestKontra rq ctx $ updateSession sess (sesID sess) (sesUserID sess) (Just uid)

  msess' <- getSession (sesID sess) (sesToken sess) testCookieDomain
  assertBool "modified session successfully taken from the database" (isJust msess')

  let sess' = fromJust msess'
  assertEqual "session successfully modified" (sesPadUserID sess') (Just uid)

  msess'' <- getSession (sesID sess) (sesToken sess) "other.domain.com"
  assertBool "we should only be able to fetch session where domain does match"
             (isNothing msess'')

testDefaultSessionTimeout :: TestEnv ()
testDefaultSessionTimeout = do
  time1 <- currentTime
  setTestTime time1

  userId        <- createTestUserAndGetId
  (session1, _) <- insertNewSession userId

  let time2    = sesExpires session1
      diffTime = diffUTCTime time2 time1

  assertApproxEqual "difftime is around default timeout +/- 10 seconds"
                    (fromIntegral defaultSessionTimeoutSecs)
                    10
                    diffTime

  -- Test that session is still valid 3 hours before expiry.
  -- Don't test closer than 2 hours as it will extend the expiry and
  -- make it difficult to test the next step on expired sessions.
  modifyTestTime $ secondsAfter (defaultSessionTimeoutSecs - 3 * 60 * 60)
  mSession3 <- getSession (sesID session1) (sesToken session1) testCookieDomain
  assertJust mSession3

  modifyTestTime $ secondsAfter (4 * 60 * 60)
  mSession4 <- getSession (sesID session1) (sesToken session1) testCookieDomain
  assertNothing mSession4

testCustomSessionTimeout :: TestEnv ()
testCustomSessionTimeout = do
  time1 <- currentTime
  setTestTime time1

  (user, userGroup) <- createTestUser

  let userId      = userid user
      userGroupId = ugID userGroup
  groupSettings1 <- assertJustAndExtract $ ugSettings userGroup

  assertEqual "initial group session timeout should be nothing"
              (ugsSessionTimeoutSecs groupSettings1)
              Nothing

  do
    -- Test that group settings can be updated with
    -- new session policy

    -- Test a long session timeout of 7 days
    let sessionTimeout = 7 * 24 * 60 * 60

    let groupSettings2 = set #ugsSessionTimeoutSecs (Just sessionTimeout) groupSettings1
    dbUpdate $ UserGroupUpdateSettings userGroupId (Just groupSettings2)

    (session1, _) <- insertNewSession userId

    let time2    = sesExpires session1
        diffTime = diffUTCTime time2 time1

    assertApproxEqual "difftime is around custom timeout (7 days) +/- 10 seconds"
                      (fromIntegral sessionTimeout)
                      10
                      diffTime

    modifyTestTime $ daysAfter 6
    mSession3 <- getSession (sesID session1) (sesToken session1) testCookieDomain
    assertBool "session should still be valid after 6 days" $ isJust mSession3

    modifyTestTime $ minutesAfter 1 . daysAfter 1
    mSession4 <- getSession (sesID session1) (sesToken session1) testCookieDomain
    assertBool "session should become invalid after ~7 days" $ isNothing mSession4

  do
    -- Test that session policy can be removed with new sessions
    -- having default timeout

    setTestTime time1

    let groupSettings2 = set #ugsSessionTimeoutSecs Nothing groupSettings1
    dbUpdate $ UserGroupUpdateSettings userGroupId (Just groupSettings2)

    (session2, _) <- insertNewSession userId

    let time2    = sesExpires session2
        diffTime = diffUTCTime time2 time1

    assertApproxEqual "difftime is around default timeout +/- 10 seconds"
                      (fromIntegral defaultSessionTimeoutSecs)
                      10
                      diffTime


    modifyTestTime $ secondsAfter (defaultSessionTimeoutSecs - 2 * 60 * 60)
    mSession3 <- getSession (sesID session2) (sesToken session2) testCookieDomain
    assertJust mSession3

    modifyTestTime $ secondsAfter (3 * 60 * 60)
    mSession4 <- getSession (sesID session2) (sesToken session2) testCookieDomain
    assertNothing mSession4

testDefaultSessionTimeoutDelay :: TestEnv ()
testDefaultSessionTimeoutDelay = do
  setTestTime =<< currentTime  -- freeze time
  userId       <- createTestUserAndGetId
  (session, _) <- insertNewSession userId

  do
    -- Fast forward to 1 hour before session expires
    modifyTestTime (secondsAfter (defaultSessionTimeoutSecs - 60 * 60))
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust mSession

  do
    -- Fast forward to 30 mins after original session expiry
    modifyTestTime (secondsAfter (90 * 60))
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust mSession

  do
    -- Fast forward to 3 hour after original session expiry
    modifyTestTime (secondsAfter (((2 * 60) + 30) * 60))
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertNothing mSession

testCustomSessionTimeoutDelay :: TestEnv ()
testCustomSessionTimeoutDelay = do
  setTestTime =<< currentTime  -- freeze time
  (user, userGroup) <- createTestUser

  let userId                = userid user
      userGroupId           = ugID userGroup
      (Just groupSettings1) = ugSettings userGroup
      sessionTimeout        = 15 * 60  -- test 15 mins session timout

  do
    let groupSettings2 = set #ugsSessionTimeoutSecs (Just sessionTimeout) groupSettings1
    dbUpdate $ UserGroupUpdateSettings userGroupId (Just groupSettings2)

  (session, _) <- insertNewSession userId

  do
    modifyTestTime (secondsAfter $ 10 * 60)
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust mSession

  do
    modifyTestTime (secondsAfter $ 10 * 60)
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust mSession

  do
    modifyTestTime (secondsAfter $ 20 * 60)
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertNothing mSession

testCustomSessionTimeoutInheritance :: TestEnv ()
testCustomSessionTimeoutInheritance = do
  userGroup11 :: UserGroupRoot <- rand 10 arbitrary
  let groupSettings11 = ugrSettings userGroup11

      sessionTimeout  = 7 * 24 * 60 * 60
      groupSettings12 = set #ugsSessionTimeoutSecs (Just sessionTimeout) groupSettings11
      userGroup12     = set #ugSettings (Just groupSettings12) $ ugFromUGRoot userGroup11

  userGroup13              <- dbUpdate $ UserGroupCreate userGroup12

  userGroup21 :: UserGroup <- rand 10 arbitrary
  let userGroup22 = userGroup21
        { ugParentGroupID = Just $ ugID userGroup13
        , ugSettings = Nothing
        }
  userGroup23 <- dbUpdate $ UserGroupCreate userGroup22

  userGroup24 :: Maybe UserGroupWithParents <- dbQuery $ UserGroupGetWithParents $ ugID
    userGroup23

  let groupSettings2            = ugwpSettings <$> userGroup24

  let timeoutVal :: Maybe Int32 = ugsSessionTimeoutSecs =<< groupSettings2

  assertEqual "session timeout value should be inherited from parent user group"
              (Just sessionTimeout)
              timeoutVal

  do
    time1 <- currentTime
    setTestTime time1

    (Just user) <- addNewCompanyUser "John" "Smith" "smith@example.com"
      $ ugID userGroup23

    let userId = userid user
    (session1, _) <- insertNewSession userId

    let time2    = sesExpires session1
        diffTime = diffUTCTime time2 time1

    assertApproxEqual "difftime is around custom timeout (7 days) +/- 10 seconds"
                      (fromIntegral sessionTimeout)
                      10
                      diffTime

    modifyTestTime $ daysAfter 6
    mSession3 <- getSession (sesID session1) (sesToken session1) testCookieDomain
    assertBool "session should still be valid after 6 days" $ isJust mSession3

    modifyTestTime $ minutesAfter 1 . daysAfter 1
    mSession4 <- getSession (sesID session1) (sesToken session1) testCookieDomain
    assertBool "session should become invalid after ~7 days" $ isNothing mSession4

testDocumentTicketInsertion :: TestEnv ()
testDocumentTicketInsertion = replicateM_ 10 $ do
  (_, _, ctx) <- addDocumentAndInsertToken
  runSQL_
    $   "SELECT COUNT(*) FROM document_session_tokens WHERE session_id ="
    <?> ctx ^. #ctxSessionID
  tokens :: Int64 <- fetchOne runIdentity
  assertEqual "token successfully inserted into the database" 1 tokens

testDocumentTicketReinsertion :: TestEnv ()
testDocumentTicketReinsertion = replicateM_ 10 $ do
  (_, doc, ctx) <- addDocumentAndInsertToken
  void $ do
    let Just asl = getAuthorSigLink doc
    rq <- mkRequest GET []
    runTestKontra rq ctx $ do
      sid <- getNonTempSessionID
      dbUpdate $ AddDocumentSession sid (signatorylinkid asl)
  return ()

testElegTransactionInsertion :: TestEnv ()
testElegTransactionInsertion = replicateM_ 10 $ do
  (mtrans, _) <- addCgiGrpTransaction
  assertBool "cgi grp transaction successfully inserted into the database" (isJust mtrans)

testElegTransactionUpdate :: TestEnv ()
testElegTransactionUpdate = replicateM_ 10 $ do
  (Just trans, ctx) <- addCgiGrpTransaction
  let newtrans = case trans of
        (CgiGrpAuthTransaction slid tid orf sid) ->
          (CgiGrpAuthTransaction slid tid orf sid)
        (CgiGrpSignTransaction slid _ tid orf sid) ->
          (CgiGrpSignTransaction slid "new order ref" tid orf sid)
  (mtrans', _) <- do
    rq <- mkRequest GET []
    runTestKontra rq ctx $ do
      dbUpdate $ MergeCgiGrpTransaction newtrans
      dbQuery
        $ GetCgiGrpTransaction (cgiTransactionType newtrans) (cgiSignatoryLinkID trans)
  assertBool "cgi grp transaction present is the database" $ isJust mtrans'
  let Just trans' = mtrans'
  assertBool "cgi grp transaction properly modified" $ newtrans == trans'

insertNewSession :: UserID -> TestEnv (Session, Context)
insertNewSession uid = do
  rq  <- mkRequestWithHeaders GET [] [("host", [testCookieDomain])]
  ctx <- mkContext defaultLang
  runTestKontra rq ctx $ do
    session1  <- emptySession
    mSession2 <- updateSession session1 (sesID session1) (Just uid) Nothing

    session2  <- assertJustAndExtract mSession2

    let sessionId    = sesID session2
        sessionToken = sesToken session2

    mSession3 <- getSession sessionId sessionToken testCookieDomain
    assertJustAndExtract mSession3

addDocumentAndInsertToken :: TestEnv (User, Document, Context)
addDocumentAndInsertToken = do
  author <- addNewRandomUser
  doc    <- addRandomDocument (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                  , rdaStatuses = OneOf [Pending]
                                                  }
  (_, ctx) <- do
    let Just asl = getAuthorSigLink doc
    rq  <- mkRequest GET []
    ctx <- mkContext defaultLang
    runTestKontra rq ctx $ do
      sess <- emptySession
      sid  <- getNonTempSessionID
      dbUpdate $ AddDocumentSession sid (signatorylinkid asl)
      ctx' <- getContext
      updateSession sess (ctx' ^. #ctxSessionID) (sesUserID sess) (sesPadUserID sess)
  return (author, doc, ctx)

addCgiGrpTransaction :: TestEnv (Maybe CgiGrpTransaction, Context)
addCgiGrpTransaction = do
  (_, doc, ctx) <- addDocumentAndInsertToken
  let Just asl = getAuthorSigLink doc
  trans_ <- rand 20 arbitrary
  let trans = case trans_ of
        (CgiGrpAuthTransaction _ tid orf _) ->
          CgiGrpAuthTransaction (signatorylinkid asl) tid orf $ ctx ^. #ctxSessionID
        (CgiGrpSignTransaction _ tbs tid orf _) ->
          CgiGrpSignTransaction (signatorylinkid asl) tbs tid orf $ ctx ^. #ctxSessionID
  rq <- mkRequest GET []
  runTestKontra rq ctx $ do
    dbUpdate $ MergeCgiGrpTransaction trans
    dbQuery $ GetCgiGrpTransaction (cgiTransactionType trans) (signatorylinkid asl)

createTestUserAndGetId :: TestEnv UserID
createTestUserAndGetId = (userid . fst) <$> createTestUser

createTestUser :: TestEnv (User, UserGroup)
createTestUser = do
  bd        <- dbQuery $ GetMainBrandedDomain
  pwd       <- createPassword "password_8866"
  ug        <- addNewUserGroup
  Just user <- createNewUser ("Andrzej", "Rybczak")
                             "andrzej@scrive.com"
                             (Just pwd)
                             (ugID ug, True)
                             defaultLang
                             (bdid bd)
                             AccountRequest
  return $ (user, ug)
