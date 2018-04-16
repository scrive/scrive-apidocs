module MailsTest (mailsTests) where

import Control.Monad.Trans
import Data.Char
import Happstack.Server
import Log
import Test.Framework
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML as XML

import Company.CompanyUI
import Company.Model
import Context
import DB
import DB.TimeZoneName (defaultTimeZoneName, mkTimeZoneName)
import Doc.DocStateData
import Doc.DocumentMonad (theDocument, withDocumentM)
import Doc.DocViewMail
import Doc.Model
import Mails.Events
import Mails.SendMail
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Model
import User.UserAccountRequest
import User.UserView
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

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
      , companyMailTheme = Nothing
      , companySignviewTheme = Nothing
      , companyServiceTheme = Nothing
      , companyBrowserTitle = Nothing
      , companySmsOriginator = Nothing
      , companyFavicon = Nothing
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
      ctx <- mkContext l
      _ <- dbUpdate $ SetUserSettings (userid author) $ (usersettings author) { lang = l }
      let aa = authorActor ctx author
      req <- mkRequest POST []
      runTestKontra req ctx $ (randomUpdate (NewDocument author "Document title" Signable defaultTimeZoneName 0 aa)) `withDocumentM` do
        True <- dbUpdate $ SetDocumentLang l (systemActor $ get ctxtime ctx)

        asl <- head . documentsignatorylinks <$> theDocument
        file <- addNewRandomFile
        randomUpdate $ AttachFile file (systemActor $ get ctxtime ctx)

        islf <- rand 10 arbitrary

        now <- currentTime
        let sigs = [def {signatoryfields = signatoryfields asl, signatoryisauthor = True,signatoryispartner = True, maybesignatory = maybesignatory asl} , def {signatoryfields = islf, signatoryispartner = True}]
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
                              logInfo_ $ "Checking mail" <+> T.pack s
                              m <- mg
                              validMail s m
        checkMail "Invitation" $ mailInvitation True Sign (Just sl) =<< theDocument
        -- DELIVERY MAILS
        checkMail "Deferred invitation"    $  mailDeferredInvitation (get ctxmailnoreplyaddress ctx) (get ctxbrandeddomain ctx) sl =<< theDocument
        checkMail "Undelivered invitation" $  mailUndeliveredInvitation (get ctxmailnoreplyaddress ctx) (get ctxbrandeddomain ctx) sl =<< theDocument
        checkMail "Delivered invitation"   $  mailDeliveredInvitation (get ctxmailnoreplyaddress ctx) (get ctxbrandeddomain ctx) sl =<< theDocument
        --remind mails
        checkMail "Reminder notsigned" $ mailDocumentRemind False Nothing sl True =<< theDocument
        checkMail "Reminder notsigned" $ mailDocumentRemind True Nothing sl True =<< theDocument
        --reject mail
        checkMail "Reject"  $ mailDocumentRejected True Nothing True sl =<< theDocument
        checkMail "Reject"  $ mailDocumentRejected True Nothing False sl =<< theDocument
        -- awaiting author email
        checkMail "Awaiting author" $ mailDocumentAwaitingForAuthor (def :: Lang) =<< theDocument
        -- Virtual signing
        randomUpdate . SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots
                                   =<< (signatoryActor (set ctxtime (10 `minutesAfter` now) ctx) sl)

        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed False sl False False =<< theDocument
        -- Reminder after send
        checkMail "Reminder signed" $ theDocument >>= \d -> mailDocumentRemind True Nothing (head $ documentsignatorylinks d) True d
  commit


testUserMails :: TestEnv ()
testUserMails = do
  forM_ allLangs $ \l ->  do
    -- make a user and context that use the same lang
    ctx <- mkContext l
    user <- addNewRandomUserWithLang l

    req <- mkRequest POST []
    let checkMail s mg = do
                           logInfo_ $ "Checking mail" <+> T.pack s
                           m <- fst <$> (runTestKontra req ctx $ mg)
                           validMail s m
    checkMail "New account" $ do
          al <- newUserAccountRequestLink (get ctxlang ctx) (userid user) AccountRequest
          newUserMail ctx (getEmail user) al
    checkMail "New account by admin" $ do
          al <- newUserAccountRequestLink (get ctxlang ctx) (userid user) ByAdmin
          mailNewAccountCreatedByAdmin ctx (get ctxlang ctx) (getEmail user) al
    checkMail "Reset password mail" $ do
          al <- newUserAccountRequestLink (get ctxlang ctx) (userid user) AccountRequest
          resetPasswordMail ctx user al
  commit



-- MAIL TESTING UTILS
validMail :: MonadIO m => String -> Mail -> m ()
validMail name m = do
    let c    = content m
        c'   = "<html>" <> TL.pack c <> "</html>"
             -- ^ XML parser freaks out if there's content after root element.
        exml = XML.parseText def c'
    when (not . any isAlphaNum $ title m) $
      assertFailure ("Empty title of mail " ++ name)
    case exml of
      Left exc -> assertFailure $ "Invalid HTML mail " ++ name ++ " : " ++ c ++
                                  " " ++ show exc
      Right _  -> assertSuccess

addNewRandomUserWithLang :: Lang -> TestEnv User
addNewRandomUserWithLang l = do
  user <- addNewRandomUser
  void . dbUpdate $ SetUserSettings (userid user)
                                    ((usersettings user) { lang = l })
  Just uuser <- dbQuery $ GetUserByID (userid user)
  return uuser
