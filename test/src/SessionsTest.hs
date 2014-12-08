module SessionsTest (sessionsTests) where

import Control.Applicative
import Data.Int
import Data.Maybe
import Happstack.Server
import Test.Framework
import Test.QuickCheck

import Company.Model
import Context
import Control.Logic
import DB hiding (query, update)
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.Tokens.Model
import KontraMonad
import Session.Data
import Session.Model
import TestingUtil
import TestKontra as T
import User.Model
import Util.SignatoryLinkUtils
import Utils.Default
import Utils.HTTP

sessionsTests :: TestEnvSt -> Test
sessionsTests env = testGroup "Sessions" [
    testThat "can create new session" env testNewSessionInsertion
  , testThat "can update existing session" env testSessionUpdate
  , testThat "can insert document ticket" env testDocumentTicketInsertion
  , testThat "can reinsert document ticket" env testDocumentTicketReinsertion
  , testThat "can add eleg transaction" env testElegTransactionInsertion
  , testThat "can update eleg transaction" env testElegTransactionUpdate
  ]

testNewSessionInsertion :: TestEnv ()
testNewSessionInsertion = do
  uid <- testUser
  doTimes 12 $ do
    (msess, _) <- insertNewSession uid
    assertBool "session successfully taken from the database" (isJust msess)
  runSQL_ $ "SELECT COUNT(*) FROM sessions WHERE user_id =" <?> uid
  user_sessions :: Int64 <- fetchOne unSingle
  assertEqual "there are only 5 sessions for one user" 5 user_sessions

testSessionUpdate :: TestEnv ()
testSessionUpdate = do
  uid <- testUser
  (Just sess, ctx) <- insertNewSession uid
  _ <- do
    rq <- mkRequest GET []
    runTestKontra rq ctx $ updateSession sess (sess { sesPadUserID = Just uid })
  msess' <- getSession (sesID sess) (sesToken sess) (domainFromString $ defaultUri)
  assertBool "modified session successfully taken from the database" (isJust msess')

  let sess' = fromJust msess'
  assertEqual "session successfully modified" (sesPadUserID sess') (Just uid)

  msess'' <- getSession (sesID sess) (sesToken sess) "other.domain.com"
  assertBool "we should only be able to fetch session where domain does match" (isNothing msess'')


testDocumentTicketInsertion :: TestEnv ()
testDocumentTicketInsertion = doTimes 10 $ do
  (_, _, ctx) <- addDocumentAndInsertToken
  runSQL_ $ "SELECT COUNT(*) FROM document_session_tokens WHERE session_id =" <?> ctxsessionid ctx
  tokens :: Int64 <- fetchOne unSingle
  assertEqual "token successfully inserted into the database" 1 tokens

testDocumentTicketReinsertion :: TestEnv ()
testDocumentTicketReinsertion = doTimes 10 $ do
  (_, doc, ctx) <- addDocumentAndInsertToken
  _ <- do
    let Just asl = getAuthorSigLink doc
    rq <- mkRequest GET []
    runTestKontra rq ctx $ do
      dbUpdate $ AddDocumentSessionToken (signatorylinkid asl) (signatorymagichash asl)
  return ()

testElegTransactionInsertion :: TestEnv ()
testElegTransactionInsertion = doTimes 10 $ do
  (mtrans, _) <- addElegTransaction
  assertBool "eleg transaction successfully inserted into the database" $ isJust mtrans

testElegTransactionUpdate :: TestEnv ()
testElegTransactionUpdate = doTimes 10 $ do
  (Just trans, ctx) <- addElegTransaction
  let newtrans = trans { transactionsignatorylinkid = Nothing }
  (mtrans', _) <- do
    rq <- mkRequest GET []
    runTestKontra rq ctx $ do
      dbUpdate $ MergeELegTransaction newtrans
      dbQuery (GetELegTransaction $ transactiontransactionid newtrans)
  assertBool "eleg transaction present is the database" $ isJust mtrans'
  let Just trans' = mtrans'
  assertBool "eleg transaction properly modified" $ newtrans == trans'

insertNewSession :: UserID -> TestEnv (Maybe Session, Context)
insertNewSession uid = do
  (sess, ctx) <- do
    rq <- mkRequest GET []
    ctx <- mkContext defaultValue
    runTestKontra rq ctx $ do
      initialSession <- emptySession
      updateSession initialSession (initialSession { sesUserID = Just uid })
      return initialSession
  -- FIXME: this sucks, but there is no way to get id of newly inserted
  -- session and modifying normal code to get access to it seems like
  -- a bad idea
  runSQL_ "SELECT id FROM sessions ORDER BY id DESC LIMIT 1"
  sid <- fetchOne unSingle
  msess <- getSession sid (sesToken sess) (domainFromString $ defaultUri)
  return (msess, ctx)

addDocumentAndInsertToken :: TestEnv (User, Document, Context)
addDocumentAndInsertToken = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  (_, ctx) <- do
    let Just asl = getAuthorSigLink doc
    rq <- mkRequest GET []
    ctx <- mkContext defaultValue
    runTestKontra rq ctx $ do
      sess <- emptySession
      dbUpdate $ AddDocumentSessionToken (signatorylinkid asl) (signatorymagichash asl)
      ctx' <- getContext
      updateSession sess (sess { sesID = ctxsessionid ctx' })
  return (author, doc, ctx)

addElegTransaction :: TestEnv (Maybe ELegTransaction, Context)
addElegTransaction = do
  (_, doc, ctx) <- addDocumentAndInsertToken
  let Just asl = getAuthorSigLink doc
  trans <- (\tr -> tr {
      transactionsignatorylinkid = Just $ signatorylinkid asl
    , transactiondocumentid = documentid doc
    }) <$> rand 20 arbitrary
  rq <- mkRequest GET []
  runTestKontra rq ctx $ do
    dbUpdate $ MergeELegTransaction trans
    dbQuery (GetELegTransaction $ transactiontransactionid trans)

testUser :: TestEnv UserID
testUser = do
  pwd <- createPassword "admin"
  company <- dbUpdate $ CreateCompany
  Just user <- dbUpdate $ AddUser ("Andrzej", "Rybczak") "andrzej@scrive.com" (Just pwd) (companyid company,True) defaultValue Nothing
  return $ userid user
