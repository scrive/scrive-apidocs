module MailsTest (mailsTests) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Maybe
import Happstack.Server
import Test.Framework
import Test.QuickCheck
import Text.XML.HaXml.Parse (xmlParse')

import ActionQueue.UserAccountRequest
import Company.CompanyUI
import Company.Model
import Context
import DB
import DB.TimeZoneName (defaultTimeZoneName, mkTimeZoneName)
import Doc.DocStateData
import Doc.DocumentMonad (withDocumentM, theDocument)
import Doc.DocViewMail
import Doc.Model
import Mails.Events
import Mails.SendMail
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Model
import User.UserView
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Default
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import qualified Log

mailsTests :: TestEnvSt -> Test
mailsTests env  = testGroup "Mails" [
    testThat "Document emails" env $ testDocumentMails
  , testThat "Branded document emails" env $ testBrandedDocumentMails
  , testThat "User emails" env $ testUserMails
  ]

testBrandedDocumentMails :: TestEnv ()
testBrandedDocumentMails = do
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
      , companysignviewprimarycolour = Nothing
      , companysignviewprimarytextcolour = Nothing
      , companysignviewsecondarycolour = Nothing
      , companysignviewsecondarytextcolour = Nothing
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
  sendDocumentMails author

testDocumentMails ::TestEnv ()
testDocumentMails = do
  author <- addNewRandomUser
  sendDocumentMails author

sendDocumentMails :: User -> TestEnv ()
sendDocumentMails author = do
  forM_ allLangs $ \l ->  do
      -- make  the context, user and document all use the same lang
      ctx <- mailingContext l
      _ <- dbUpdate $ SetUserSettings (userid author) $ (usersettings author) { lang = l }
      let aa = authorActor ctx author
      req <- mkRequest POST []
      runTestKontra req ctx $ (fromJust <$> randomUpdate (NewDocument defaultValue author "Document title" Signable defaultTimeZoneName 0 aa)) `withDocumentM` do
        True <- dbUpdate $ SetDocumentLang l (systemActor $ ctxtime ctx)

        asl <- head . documentsignatorylinks <$> theDocument
        file <- addNewRandomFile
        randomUpdate $ AttachFile file (systemActor $ ctxtime ctx)

        islf <- rand 10 arbitrary

        now <- currentTime
        let sigs = [defaultValue {signatoryfields = signatoryfields asl, signatoryisauthor = True,signatoryispartner = True, maybesignatory = maybesignatory asl} , defaultValue {signatoryfields = islf, signatoryispartner = True}]
        True <- randomUpdate $ ResetSignatoryDetails sigs (systemActor now)
        tz <- mkTimeZoneName "Europe/Stockholm"
        randomUpdate $ PreparationToPending (systemActor now) tz
        asl2 <- head . documentsignatorylinks <$> theDocument
        randomUpdate . MarkDocumentSeen (signatorylinkid asl2) (signatorymagichash asl2)
             =<< signatoryActor ctx asl2
        randomUpdate $ SignDocument (signatorylinkid asl2) (signatorymagichash asl2) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor now)
        [sl] <- filter (not . isAuthor) . documentsignatorylinks <$> theDocument
        --Invitation Mails
        let checkMail s mg = do
                              Log.mixlog_ $ "Checking mail " ++ s
                              m <- mg
                              validMail s m
        checkMail "Invitation" $ mailInvitation True Sign (Just sl) =<< theDocument
        -- DELIVERY MAILS
        checkMail "Deferred invitation"    $  mailDeferredInvitation (ctxmailsconfig ctx) Nothing (ctxhostpart ctx) sl =<< theDocument
        checkMail "Undelivered invitation" $  mailUndeliveredInvitation (ctxmailsconfig ctx)  Nothing (ctxhostpart ctx) sl =<< theDocument
        checkMail "Delivered invitation"   $  mailDeliveredInvitation (ctxmailsconfig ctx)  Nothing (ctxhostpart ctx) sl =<< theDocument
        --remind mails
        checkMail "Reminder notsigned" $ mailDocumentRemind Nothing sl True =<< theDocument
        --reject mail
        checkMail "Reject"  $ mailDocumentRejected Nothing sl =<< theDocument
        -- awaiting author email
        checkMail "Awaiting author" $ mailDocumentAwaitingForAuthor (defaultValue :: Lang) =<< theDocument
        -- Virtual signing
        randomUpdate . SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots
                                   =<< (signatoryActor ctx{ ctxtime = 10 `minutesAfter` now } sl)

        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed False sl False False =<< theDocument
        -- Reminder after send
        checkMail "Reminder signed" $ theDocument >>= \d -> mailDocumentRemind Nothing (head $ documentsignatorylinks d) True d
  commit


testUserMails :: TestEnv ()
testUserMails = do
  forM_ allLangs $ \l ->  do
    -- make a user and context that use the same lang
    ctx <- mailingContext l
    user <- addNewRandomUserWithLang l

    req <- mkRequest POST []
    let checkMail s mg = do
                           Log.mixlog_ $ "Checking mail " ++ s
                           m <- fst <$> (runTestKontra req ctx $ mg)
                           validMail s m
    checkMail "New account" $ do
          al <- newUserAccountRequestLink (ctxlang ctx) (userid user) AccountRequest
          newUserMail ctx (getEmail user) (getEmail user) al
    checkMail "New account by admin" $ do
          al <- newUserAccountRequestLink (ctxlang ctx) (userid user) ByAdmin
          mailNewAccountCreatedByAdmin ctx (ctxlang ctx) (getSmartName user) (getEmail user) al Nothing
    checkMail "Reset password mail" $ do
          al <- newUserAccountRequestLink (ctxlang ctx) (userid user) AccountRequest
          resetPasswordMail ctx user al
  commit



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
