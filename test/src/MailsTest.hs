module MailsTest (mailsTests) where

import Control.Applicative
import Happstack.Server
import Test.Framework

import ActionQueue.UserAccountRequest
import DB
import Context
import TestingUtil
import TestKontra as T
import User.Model
import IPAddress
import Utils.Default
import Doc.Model
import Doc.DocViewMail
import Doc.DocStateData
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import Mails.SendMail
import Company.Model
import Test.QuickCheck
import Control.Monad
import MinutesTime
import Util.SignatoryLinkUtils
import User.UserView
import Util.HasSomeUserInfo
import Mails.Events
import Data.Char
import Text.XML.HaXml.Parse (xmlParse')
import Control.Monad.Trans
import Util.Actor
import qualified Log as Log
import Control.Concurrent
import Data.Maybe

mailsTests :: [String] -> TestEnvSt -> Test
mailsTests params env  = testGroup "Mails" [
    testThat "Document emails" env $ testDocumentMails $ toMailAddress params
  , testThat "Branded document emails" env $ testBrandedDocumentMails $ toMailAddress params
  , testThat "User emails" env $ testUserMails $ toMailAddress params
  ]

testBrandedDocumentMails :: Maybe String -> TestEnv ()
testBrandedDocumentMails mailTo = do
  company' <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company') False
  let cui = CompanyUI {
        companyemailfont = Just "Helvetica Neue, Arial, sans-serif"
      , companyemailbordercolour = Just "#dee4ed"
      , companyemailbuttoncolour = Just "215"
      , companyemailemailbackgroundcolour = Just "#0f0"
      , companyemailbackgroundcolour = Just "orange"
      , companyemailtextcolour = Just "green"
      , companyemaillogo = Nothing
      , companysignviewlogo = Nothing
      , companysignviewtextcolour = Nothing
      , companysignviewtextfont = Nothing
      , companysignviewbarscolour = Nothing
      , companysignviewbarstextcolour = Nothing
      , companysignviewbackgroundcolour = Nothing
      , companycustomlogo  = Nothing
      , companycustombarscolour = Nothing
      , companycustombarstextcolour = Nothing
      , companycustombarssecondarycolour = Nothing
      , companycustombackgroundcolour = Nothing
      }
  _ <- dbUpdate $ UpdateCompanyUI (companyid company') cui
  sendDocumentMails mailTo author

testDocumentMails :: Maybe String -> TestEnv ()
testDocumentMails mailTo = do
  author <- addNewRandomUser
  sendDocumentMails mailTo author

sendDocumentMails :: Maybe String -> User -> TestEnv ()
sendDocumentMails mailTo author = do
  forM_ allLangs $ \l ->
    forM_ [Contract,Offer,Order] $ \doctype -> do
        -- make  the context, user and document all use the same lang
        ctx <- mailingContext l
        _ <- dbUpdate $ SetUserSettings (userid author) $ (usersettings author) { lang = l }
        let aa = authorActor (ctxtime ctx) noIP (userid author) (getEmail author)
        Just d <- randomUpdate $ NewDocument author "Document title" (Signable doctype) 0 aa
        True <- dbUpdate $ SetDocumentLang (documentid d) l (systemActor $ ctxtime ctx)

        let docid = documentid d
        let asl = head $ documentsignatorylinks d
        let authordetails = signatorydetails asl
        file <- addNewRandomFile
        randomUpdate $ AttachFile docid (fileid file) (systemActor $ ctxtime ctx)

        isl <- rand 10 arbitrary
        now <- getMinutesTime
        let sigs = [authordetails, isl]
        True <- randomUpdate $ ResetSignatoryDetails docid sigs (systemActor now)
        randomUpdate $ PreparationToPending docid (systemActor now) Nothing
        Just d2 <- dbQuery $ GetDocumentByDocumentID docid
        let asl2 = head $ documentsignatorylinks d2
        True <- randomUpdate $ MarkDocumentSeen docid (signatorylinkid asl2) (signatorymagichash asl2)
             (signatoryActor now noIP (maybesignatory asl2) (getEmail asl2) (signatorylinkid asl2))
        randomUpdate $ \si -> SignDocument docid (signatorylinkid asl2) (signatorymagichash asl2) si SignatoryScreenshots.emptySignatoryScreenshots (systemActor now)
        Just doc <- dbQuery $ GetDocumentByDocumentID docid
        let [sl] = filter (not . isAuthor) (documentsignatorylinks doc)
        req <- mkRequest POST []
        --Invitation Mails
        let checkMail s mg = do
                              Log.debug $ "Checking mail " ++ s
                              m <- fst <$> (runTestKontra req ctx $ mg)
                              validMail (s ++ " "++ show doctype) m
                              sendoutForManualChecking (s ++ " " ++ show doctype ) req ctx mailTo m
        checkMail "Invitation" $ mailInvitation True ctx Sign doc (Just sl) False
        -- DELIVERY MAILS
        checkMail "Deferred invitation"    $  mailDeferredInvitation (ctxhostpart ctx) doc sl
        checkMail "Undelivered invitation" $  mailUndeliveredInvitation (ctxhostpart ctx) doc sl
        checkMail "Delivered invitation"   $  mailDeliveredInvitation  (ctxhostpart ctx) doc sl
        --remind mails
        checkMail "Reminder notsigned" $ mailDocumentRemind Nothing ctx doc sl False
        --reject mail
        checkMail "Reject"  $ mailDocumentRejected  Nothing  ctx doc sl False
        -- awaiting author email
        when (doctype == Contract) $ do
          checkMail "Awaiting author" $ mailDocumentAwaitingForAuthor  ctx doc (defaultValue :: Lang)
        -- Virtual signing
        randomUpdate $ \ip -> SignDocument docid (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.emptySignatoryScreenshots
                                   (signatoryActor (10 `minutesAfter` now) ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl))
        (Just sdoc) <- randomQuery $ GetDocumentByDocumentID docid
        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed ctx sdoc Nothing sl False
        -- Reminder after send
        checkMail "Reminder signed" $ mailDocumentRemind Nothing ctx doc (head $ documentsignatorylinks sdoc) False
  kCommit
  when (isJust mailTo) $ do
    Log.debug "Delay for mails to get send"
    liftIO $ threadDelay 200000000
    Log.debug "Mails not send will be purged"


testUserMails :: Maybe String -> TestEnv ()
testUserMails mailTo = do
  forM_ allLangs $ \l ->  do
    -- make a user and context that use the same lang
    ctx <- mailingContext l
    user <- addNewRandomUserWithLang l

    req <- mkRequest POST []
    let checkMail s mg = do
                           Log.debug $ "Checking mail " ++ s
                           m <- fst <$> (runTestKontra req ctx $ mg)
                           validMail s m
                           sendoutForManualChecking s req ctx mailTo m
    checkMail "New account" $ do
          al <- newUserAccountRequestLink (ctxlang ctx) (userid user) AccountRequest
          newUserMail ctx (getEmail user) (getEmail user) al
    checkMail "New account by admin" $ do
          al <- newUserAccountRequestLink (ctxlang ctx) (userid user) ByAdmin
          mailNewAccountCreatedByAdmin ctx (ctxlang ctx) (getSmartName user) (getEmail user) al Nothing
    checkMail "Reset password mail" $ do
          al <- newUserAccountRequestLink (ctxlang ctx) (userid user) AccountRequest
          resetPasswordMail ctx user al
  kCommit
  when (isJust mailTo) $ do
    Log.debug "Delay for mails to get send"
    liftIO $ threadDelay 200000000
    Log.debug "Mails not send will be purged"



-- MAIL TESTING UTILS
validMail :: String -> Mail -> TestEnv ()
validMail name m = do
    let c = content m
    let exml = xmlParse' name c
    case (any isAlphaNum $ title m) of
         True -> assertSuccess
         False -> assertFailure ("Empty title of mail " ++ name)
    case exml of
         Right _ -> assertSuccess
         Left err -> assertFailure ("Not valid HTML mail " ++ name ++ " : " ++ c ++ " " ++ err)

addNewRandomUserWithLang :: Lang -> TestEnv User
addNewRandomUserWithLang l = do
  user <- addNewRandomUser
  _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
           lang = l
         }
  (Just uuser) <- dbQuery $ GetUserByID (userid user)
  return uuser

mailingContext :: Lang -> TestEnv Context
mailingContext lang = do
    ctx <- mkContext lang
    return $ ctx { ctxhostpart = "http://dev.skrivapa.se" }


sendoutForManualChecking ::  String -> Request -> Context ->  Maybe String -> Mail -> TestEnv ()
sendoutForManualChecking _ _ _ Nothing _ = assertSuccess
sendoutForManualChecking _ req ctx (Just email) m = do
    _ <- runTestKontra req ctx $ do
           _ <- scheduleEmailSendout (ctxmailsconfig ctx) $ m {
                  to = [MailAddress { fullname = "Tester",
                                      email = email}]
           }
           assertSuccess
    assertSuccess


toMailAddress :: [String] -> Maybe String
toMailAddress [] = Nothing
toMailAddress (a:_) = if ('@' `elem` a)
                        then Just a
                        else Nothing
