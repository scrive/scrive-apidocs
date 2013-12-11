module MailsTest (mailsTests) where

import Control.Applicative
import Happstack.Server
import Test.Framework

import ActionQueue.UserAccountRequest
import DB
import Context
import Crypto.RNG (CryptoRNG)
import TestingUtil
import TestKontra as T
import User.Model
import Utils.Default
import Doc.Model
import Doc.DocViewMail
import Doc.DocStateData
import Doc.DocumentMonad (withDocumentM, theDocument)
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import Mails.SendMail
import Company.Model
import Company.CompanyUI
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
        companyuicompanyid = companyid company'
      , companyemailfont = Just "Helvetica Neue, Arial, sans-serif"
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
  _ <- dbUpdate $ SetCompanyUI (companyid company') cui
  sendDocumentMails mailTo author

testDocumentMails :: Maybe String -> TestEnv ()
testDocumentMails mailTo = do
  author <- addNewRandomUser
  sendDocumentMails mailTo author

sendDocumentMails :: Maybe String -> User -> TestEnv ()
sendDocumentMails mailTo author = do
  forM_ allLangs $ \l ->  do
      -- make  the context, user and document all use the same lang
      ctx <- mailingContext l
      _ <- dbUpdate $ SetUserSettings (userid author) $ (usersettings author) { lang = l }
      let aa = authorActor ctx author
      req <- mkRequest POST []
      runTestKontra req ctx $ (fromJust <$> randomUpdate (NewDocument author "Document title" Signable 0 aa)) `withDocumentM` do
        True <- dbUpdate $ SetDocumentLang l (systemActor $ ctxtime ctx)

        asl <- head . documentsignatorylinks <$> theDocument
        file <- addNewRandomFile
        randomUpdate $ AttachFile file (systemActor $ ctxtime ctx)

        islf <- rand 10 arbitrary

        now <- getMinutesTime
        let sigs = [defaultValue {signatoryfields = signatoryfields asl, signatoryisauthor = True,signatoryispartner = True} , defaultValue {signatoryfields = islf, signatoryispartner = True}]
        True <- randomUpdate $ ResetSignatoryDetails sigs (systemActor now)
        randomUpdate $ PreparationToPending (systemActor now) Nothing
        asl2 <- head . documentsignatorylinks <$> theDocument
        randomUpdate $ MarkDocumentSeen (signatorylinkid asl2) (signatorymagichash asl2)
             (signatoryActor ctx asl2)
        randomUpdate $ SignDocument (signatorylinkid asl2) (signatorymagichash asl2) Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor now)
        [sl] <- filter (not . isAuthor) . documentsignatorylinks <$> theDocument
        --Invitation Mails
        let checkMail s mg = do
                              Log.debug $ "Checking mail " ++ s
                              m <- mg
                              validMail s m
                              sendoutForManualChecking ctx mailTo m
        checkMail "Invitation" $ mailInvitation True Sign (Just sl) False =<< theDocument
        -- DELIVERY MAILS
        checkMail "Deferred invitation"    $  mailDeferredInvitation (ctxmailsconfig ctx) Nothing (ctxhostpart ctx) sl =<< theDocument
        checkMail "Undelivered invitation" $  mailUndeliveredInvitation (ctxmailsconfig ctx)  Nothing (ctxhostpart ctx) sl =<< theDocument
        checkMail "Delivered invitation"   $  mailDeliveredInvitation (ctxmailsconfig ctx)  Nothing (ctxhostpart ctx) sl =<< theDocument
        --remind mails
        checkMail "Reminder notsigned" $ mailDocumentRemind Nothing sl False =<< theDocument
        --reject mail
        checkMail "Reject"  $ mailDocumentRejected  Nothing sl False =<< theDocument
        -- awaiting author email
        checkMail "Awaiting author" $ mailDocumentAwaitingForAuthor (defaultValue :: Lang) =<< theDocument
        -- Virtual signing
        randomUpdate $ SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.emptySignatoryScreenshots
                                   (signatoryActor ctx{ ctxtime = 10 `minutesAfter` now } sl)

        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed Nothing sl False =<< theDocument
        -- Reminder after send
        checkMail "Reminder signed" $ theDocument >>= \d -> mailDocumentRemind Nothing (head $ documentsignatorylinks d) False d
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
                           sendoutForManualChecking ctx mailTo m
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
validMail :: MonadIO m => String -> Mail -> m ()
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


sendoutForManualChecking :: (CryptoRNG m, MonadDB m) => Context ->  Maybe String -> Mail -> m ()
sendoutForManualChecking _ Nothing _ = assertSuccess
sendoutForManualChecking ctx (Just email) m = do
           _ <- scheduleEmailSendout (ctxmailsconfig ctx) $ m {
                  to = [MailAddress { fullname = "Tester",
                                      email = email}]
           }
           assertSuccess

toMailAddress :: [String] -> Maybe String
toMailAddress [] = Nothing
toMailAddress (a:_) = if ('@' `elem` a)
                        then Just a
                        else Nothing
