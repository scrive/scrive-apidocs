module SessionsTest (sessionsTests, insertNewSession) where

import Data.Int
import Data.Time
import Happstack.Server
import Test.Framework
import Test.QuickCheck
import qualified Control.Exception.Lifted as E

import Context
import DB
import Doc.DocControl
import Doc.DocStateData
import Doc.Model.Update
import Doc.Tokens.Model
import Doc.Types.SignatoryAccessToken
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
  [ testThat "can create new session"             env testNewSessionInsertion
  , testThat "can update existing session"        env testSessionUpdate
  , testThat "can insert document ticket"         env testDocumentTicketInsertion
  , testThat "can reinsert document ticket"       env testDocumentTicketReinsertion
  , testThat "can add eleg transaction"           env testElegTransactionInsertion
  , testThat "can update eleg transaction"        env testElegTransactionUpdate
  , testThat "test default session timeout"       env testDefaultSessionTimeout
  , testThat "test default session timeout delay" env testDefaultSessionTimeoutDelay
  , testThat "can set custom session timeout in group settings"
             env
             testCustomSessionTimeout
  , testThat "test custom session timeout delay" env testCustomSessionTimeoutDelay
  , testThat "test custom session timeout inheritance"
             env
             testCustomSessionTimeoutInheritance
  , testThat "document session timeout is working" env testDocumentSessionTimeout
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
  assertJust'_ mSession3

  modifyTestTime $ secondsAfter (4 * 60 * 60)
  mSession4 <- getSession (sesID session1) (sesToken session1) testCookieDomain
  assertNothing' mSession4

testCustomSessionTimeout :: TestEnv ()
testCustomSessionTimeout = do
  time1 <- currentTime
  setTestTime time1

  (user, userGroup) <- createTestUser

  let userId      = user ^. #id
      userGroupId = userGroup ^. #id
  groupSettings1 <- assertJust' $ userGroup ^. #settings

  assertEqual "initial group session timeout should be nothing"
              (groupSettings1 ^. #sessionTimeoutSecs)
              Nothing

  do
    -- Test that group settings can be updated with
    -- new session policy

    -- Test a long session timeout of 7 days
    let sessionTimeout = 7 * 24 * 60 * 60

    let groupSettings2 = set #sessionTimeoutSecs (Just sessionTimeout) groupSettings1
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

    let groupSettings2 = set #sessionTimeoutSecs Nothing groupSettings1
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
    assertJust'_ mSession3

    modifyTestTime $ secondsAfter (3 * 60 * 60)
    mSession4 <- getSession (sesID session2) (sesToken session2) testCookieDomain
    assertNothing' mSession4

testDefaultSessionTimeoutDelay :: TestEnv ()
testDefaultSessionTimeoutDelay = do
  setTestTime =<< currentTime  -- freeze time
  userId       <- createTestUserAndGetId
  (session, _) <- insertNewSession userId

  do
    -- Fast forward to 1 hour before session expires
    modifyTestTime (secondsAfter (defaultSessionTimeoutSecs - 60 * 60))
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust'_ mSession

  do
    -- Fast forward to 30 mins after original session expiry
    modifyTestTime (secondsAfter (90 * 60))
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust'_ mSession

  do
    -- Fast forward to 3 hour after original session expiry
    modifyTestTime (secondsAfter (((2 * 60) + 30) * 60))
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertNothing' mSession

testCustomSessionTimeoutDelay :: TestEnv ()
testCustomSessionTimeoutDelay = do
  setTestTime =<< currentTime  -- freeze time
  (user, userGroup) <- createTestUser

  let userId                = user ^. #id
      userGroupId           = userGroup ^. #id
      (Just groupSettings1) = userGroup ^. #settings
      sessionTimeout        = 15 * 60  -- test 15 mins session timout

  do
    let groupSettings2 = set #sessionTimeoutSecs (Just sessionTimeout) groupSettings1
    dbUpdate $ UserGroupUpdateSettings userGroupId (Just groupSettings2)

  (session, _) <- insertNewSession userId

  do
    modifyTestTime (secondsAfter $ 10 * 60)
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust'_ mSession

  do
    modifyTestTime (secondsAfter $ 10 * 60)
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertJust'_ mSession

  do
    modifyTestTime (secondsAfter $ 20 * 60)
    mSession <- getSession (sesID session) (sesToken session) testCookieDomain
    assertNothing' mSession

testCustomSessionTimeoutInheritance :: TestEnv ()
testCustomSessionTimeoutInheritance = do
  userGroup11 :: UserGroupRoot <- rand 10 arbitrary
  let groupSettings11 = userGroup11 ^. #settings

      sessionTimeout  = 7 * 24 * 60 * 60
      groupSettings12 = set #sessionTimeoutSecs (Just sessionTimeout) groupSettings11
      userGroup12     = set #settings (Just groupSettings12) $ ugFromUGRoot userGroup11

  userGroup13              <- dbUpdate $ UserGroupCreate userGroup12

  userGroup21 :: UserGroup <- rand 10 arbitrary
  let userGroup22 =
        userGroup21 & (#parentGroupID ?~ userGroup13 ^. #id) & (#settings .~ Nothing)
  userGroup23 <- dbUpdate $ UserGroupCreate userGroup22

  userGroup24 :: Maybe UserGroupWithParents <-
    dbQuery . UserGroupGetWithParents $ userGroup23 ^. #id

  let groupSettings2 = ugwpSettings <$> userGroup24
      timeoutVal     = view #sessionTimeoutSecs =<< groupSettings2

  assertEqual "session timeout value should be inherited from parent user group"
              (Just sessionTimeout)
              timeoutVal

  do
    time1 <- currentTime
    setTestTime time1

    user <- instantiateUser $ randomUserTemplate { groupID = return $ userGroup23 ^. #id }

    let userId = user ^. #id
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

testDocumentSessionTimeout :: TestEnv ()
testDocumentSessionTimeout = do
  (user, userGroup) <- createTestUser
  initGroupSettings <- assertJust' $ userGroup ^. #settings

  assertEqual "initial document session timeout should be nothing"
              (initGroupSettings ^. #documentSessionTimeoutSecs)
              Nothing

  -- Test 1 (user specified a custom document session timeout)
  let testTimeout1 = 10 * 60 -- 10 minutes
      groupSettings1 =
        set #documentSessionTimeoutSecs (Just testTimeout1) initGroupSettings
  do
    setTestTime =<< currentTime
    dbUpdate $ UserGroupUpdateSettings (userGroup ^. #id) (Just groupSettings1)

    testSessionInSignView user $ \session -> do
      modifyTestTime (secondsAfter 10)

      resSession <- getSession (sesID session) (sesToken session) testCookieDomain
      assertBool "session should be valid after 10 seconds" $ isJust resSession

      modifyTestTime (secondsAfter 10 . minutesAfter 10)
      resSession2 <- getSession (sesID session) (sesToken session) testCookieDomain
      assertBool "session should be invalid after 10 minutes and 10 seconds"
        $ isNothing resSession2

  -- Test 2 (user did not specify a custom session timeout)
  do
    setTestTime =<< currentTime
    dbUpdate $ UserGroupUpdateSettings (userGroup ^. #id) (Just initGroupSettings)

    testSessionInSignView user $ \session -> do
      -- Don't test closer than 2 hours as it will extend the expiry
      modifyTestTime (secondsAfter $ defaultSessionTimeoutSecs - 3 * 60 * 60)

      resSession <- getSession (sesID session) (sesToken session) testCookieDomain
      assertBool "session should be valid before the end of the default timeout"
        $ isJust resSession

      modifyTestTime (secondsAfter $ 3 * 60 * 60 + 1)
      resSession2 <- getSession (sesID session) (sesToken session) testCookieDomain
      assertBool "session should be invalid after the default timeout"
        $ isNothing resSession2

  where
    testSessionInSignView :: User -> (Session -> TestEnv ()) -> TestEnv ()
    testSessionInSignView author testAction = do
      ctx <- mkContext defaultLang
      doc <- addRandomDocument (rdaDefault author) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Pending]
                                                   }
      let did       = documentid doc
          signatory = head $ documentsignatorylinks doc
          slid      = signatorylinkid signatory

      do
        req  <- mkRequestWithHeaders GET [] [("host", [testCookieDomain])]
        mh   <- dbUpdate $ NewSignatoryAccessToken slid SignatoryAccessTokenForAPI Nothing

        eRes <- E.try . runTestKontra req ctx $ handleSignShowSaveMagicHash did slid mh

        case eRes of
          Right (_, ctx') -> do
            mSession <- dbQuery $ GetSession (view #sessionID ctx')
            case mSession of
              Just session -> testAction session
              Nothing      -> assertFailure "Session not found!"
          Left err ->
            assertFailure $ "Error when running handleSignShowSaveMagicHash: " <> show
              (err :: E.SomeException)

testDocumentTicketInsertion :: TestEnv ()
testDocumentTicketInsertion = replicateM_ 10 $ do
  (_, _, ctx) <- addDocumentAndInsertToken
  runSQL_
    $   "SELECT COUNT(*) FROM document_session_tokens WHERE session_id ="
    <?> (ctx ^. #sessionID)
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

testElegTransactionInsertion :: TestEnv ()
testElegTransactionInsertion = replicateM_ 10 $ do
  (mtrans, _) <- addCgiGrpTransaction
  assertBool "cgi grp transaction successfully inserted into the database" (isJust mtrans)

testElegTransactionUpdate :: TestEnv ()
testElegTransactionUpdate = replicateM_ 10 $ do
  (Just trans, ctx) <- addCgiGrpTransaction
  let newtrans = case trans of
        (CgiGrpAuthTransaction slid tid orf sid) ->
          CgiGrpAuthTransaction slid tid orf sid
        (CgiGrpSignTransaction slid _ tid orf sid) ->
          CgiGrpSignTransaction slid "new order ref" tid orf sid
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

    session2  <- assertJust' mSession2

    let sessionId    = sesID session2
        sessionToken = sesToken session2

    mSession3 <- getSession sessionId sessionToken testCookieDomain
    assertJust' mSession3

addDocumentAndInsertToken :: TestEnv (User, Document, Context)
addDocumentAndInsertToken = do
  author <- instantiateRandomUser
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
      updateSession sess (ctx' ^. #sessionID) (sesUserID sess) (sesPadUserID sess)
  return (author, doc, ctx)

addCgiGrpTransaction :: TestEnv (Maybe CgiGrpTransaction, Context)
addCgiGrpTransaction = do
  (_, doc, ctx) <- addDocumentAndInsertToken
  let Just asl = getAuthorSigLink doc
  trans_ <- rand 20 arbitrary
  let trans = case trans_ of
        (CgiGrpAuthTransaction _ tid orf _) ->
          CgiGrpAuthTransaction (signatorylinkid asl) tid orf $ ctx ^. #sessionID
        (CgiGrpSignTransaction _ tbs tid orf _) ->
          CgiGrpSignTransaction (signatorylinkid asl) tbs tid orf $ ctx ^. #sessionID
  rq <- mkRequest GET []
  runTestKontra rq ctx $ do
    dbUpdate $ MergeCgiGrpTransaction trans
    dbQuery $ GetCgiGrpTransaction (cgiTransactionType trans) (signatorylinkid asl)

createTestUserAndGetId :: TestEnv UserID
createTestUserAndGetId = view (_1 % #id) <$> createTestUser

createTestUser :: TestEnv (User, UserGroup)
createTestUser = do
  ug   <- instantiateRandomFreeUserGroup
  user <- instantiateUser $ randomUserTemplate { email    = return "andrzej@scrive.com"
                                               , groupID  = return $ ug ^. #id
                                               , password = Just "password_8866"
                                               }
  return (user, ug)
