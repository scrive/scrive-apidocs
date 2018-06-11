module SessionsTest (sessionsTests) where

import Data.Int
import Happstack.Server
import Test.Framework
import Test.QuickCheck

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import Context
import DB hiding (query, update)
import Doc.DocInfo
import Doc.DocStateData
import Doc.Tokens.Model
import EID.CGI.GRP.Transaction.Model
import KontraMonad
import Session.Data
import Session.Model
import TestingUtil
import TestKontra as T
import User.Model
import Util.SignatoryLinkUtils

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
  replicateM_ 60 $ do
    (msess, _) <- insertNewSession uid
    assertBool "session successfully taken from the database" (isJust msess)
  runSQL_ $ "SELECT COUNT(*) FROM sessions WHERE user_id =" <?> uid
  user_sessions :: Int64 <- fetchOne runIdentity
  assertEqual "there are only 51 sessions for one user" 51 user_sessions

testSessionUpdate :: TestEnv ()
testSessionUpdate = do
  uid <- testUser
  (Just sess, ctx) <- insertNewSession uid
  _ <- do
    rq <- mkRequest GET []
    runTestKontra rq ctx $ updateSession sess (sesID sess) (sesUserID sess) (Just uid)
  msess' <- getSession (sesID sess) (sesToken sess) "some.domain.com"
  assertBool "modified session successfully taken from the database" (isJust msess')

  let sess' = fromJust msess'
  assertEqual "session successfully modified" (sesPadUserID sess') (Just uid)

  msess'' <- getSession (sesID sess) (sesToken sess) "other.domain.com"
  assertBool "we should only be able to fetch session where domain does match" (isNothing msess'')


testDocumentTicketInsertion :: TestEnv ()
testDocumentTicketInsertion = replicateM_ 10 $ do
  (_, _, ctx) <- addDocumentAndInsertToken
  runSQL_ $ "SELECT COUNT(*) FROM document_session_tokens WHERE session_id =" <?> get ctxsessionid ctx
  tokens :: Int64 <- fetchOne runIdentity
  assertEqual "token successfully inserted into the database" 1 tokens

testDocumentTicketReinsertion :: TestEnv ()
testDocumentTicketReinsertion = replicateM_ 10 $ do
  (_, doc, ctx) <- addDocumentAndInsertToken
  _ <- do
    let Just asl = getAuthorSigLink doc
    rq <- mkRequest GET []
    runTestKontra rq ctx $ do
      dbUpdate $ AddDocumentSessionToken (signatorylinkid asl) (signatorymagichash asl)
  return ()

testElegTransactionInsertion :: TestEnv ()
testElegTransactionInsertion = replicateM_ 10 $ do
  (mtrans, _) <- addCgiGrpTransaction
  assertBool "cgi grp transaction successfully inserted into the database" $ isJust mtrans

testElegTransactionUpdate :: TestEnv ()
testElegTransactionUpdate = replicateM_ 10 $ do
  (Just trans, ctx) <- addCgiGrpTransaction
  let newtrans = case trans of
        (CgiGrpAuthTransaction slid tid orf sid) ->  (CgiGrpAuthTransaction slid tid orf sid)
        (CgiGrpSignTransaction slid _ tid orf sid) -> (CgiGrpSignTransaction slid "new order ref"  tid orf sid)
  (mtrans', _) <- do
    rq <- mkRequest GET []
    runTestKontra rq ctx $ do
      dbUpdate $ MergeCgiGrpTransaction newtrans
      dbQuery (GetCgiGrpTransaction (cgiTransactionType newtrans) (cgiSignatoryLinkID trans))
  assertBool "cgi grp transaction present is the database" $ isJust mtrans'
  let Just trans' = mtrans'
  assertBool "cgi grp transaction properly modified" $ newtrans == trans'

insertNewSession :: UserID -> TestEnv (Maybe Session, Context)
insertNewSession uid = do
  (sess, ctx) <- do
    rq <- mkRequestWithHeaders GET [] [("host",["some.domain.com"])]
    ctx <- mkContext def
    runTestKontra rq ctx $ do
      initialSession <- emptySession
      updateSession initialSession  (sesID initialSession) (Just uid) Nothing
      return initialSession
  -- FIXME: this sucks, but there is no way to get id of newly inserted
  -- session and modifying normal code to get access to it seems like
  -- a bad idea
  runSQL_ "SELECT id FROM sessions ORDER BY id DESC LIMIT 1"
  sid <- fetchOne runIdentity
  msess <- getSession sid (sesToken sess) "some.domain.com"
  return (msess, ctx)

addDocumentAndInsertToken :: TestEnv (User, Document, Context)
addDocumentAndInsertToken = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable && isPending)
  (_, ctx) <- do
    let Just asl = getAuthorSigLink doc
    rq <- mkRequest GET []
    ctx <- mkContext def
    runTestKontra rq ctx $ do
      sess <- emptySession
      dbUpdate $ AddDocumentSessionToken (signatorylinkid asl) (signatorymagichash asl)
      ctx' <- getContext
      updateSession sess (get ctxsessionid ctx') (sesUserID sess) (sesPadUserID sess)
  return (author, doc, ctx)


addCgiGrpTransaction :: TestEnv (Maybe CgiGrpTransaction, Context)
addCgiGrpTransaction = do
  (_, doc, ctx) <- addDocumentAndInsertToken
  let Just asl = getAuthorSigLink doc
  trans_ <-rand 20 arbitrary
  let trans = case trans_ of
        (CgiGrpAuthTransaction _  tid orf _) ->  (CgiGrpAuthTransaction (signatorylinkid asl) tid orf $ get ctxsessionid ctx)
        (CgiGrpSignTransaction _  tbs tid orf _) -> (CgiGrpSignTransaction (signatorylinkid asl) tbs tid orf $ get ctxsessionid ctx)
  rq <- mkRequest GET []
  runTestKontra rq ctx $ do
    dbUpdate $ MergeCgiGrpTransaction trans
    dbQuery $ GetCgiGrpTransaction (cgiTransactionType trans) $ signatorylinkid asl

testUser :: TestEnv UserID
testUser = do
  bd <- dbQuery $ GetMainBrandedDomain
  pwd <- createPassword "admin"
  company <- dbUpdate $ CreateCompany
  Just user <- dbUpdate $ AddUser ("Andrzej", "Rybczak") "andrzej@scrive.com" (Just pwd) (companyid company,True) def (get bdid bd) AccountRequest (companyusergroupid company)
  return $ userid user
