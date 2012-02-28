module MailsTest (mailsTests) where

import Control.Applicative
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)

import DB.Classes
import Context
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import Misc
import Doc.Model
import Doc.DocViewMail
import Doc.DocStateData
import Mails.SendMail
import Company.Model
--import Mails.MailsConfig
import qualified Data.ByteString.UTF8 as BS
import Test.QuickCheck
import Control.Monad
import MinutesTime
import Util.SignatoryLinkUtils
import User.UserView
import Kontra
import Util.HasSomeUserInfo
import Mails.Events
import Data.Char
import Text.XML.HaXml.Parse (xmlParse')
import Control.Monad.Trans
import EvidenceLog.Model

mailsTests :: [String] -> DBEnv -> Test
mailsTests params env  = testGroup "Mails" [
    testCase "Document emails" $ testDocumentMails env (toMailAddress params),
    testCase "User emails" $ testUserMails env (toMailAddress params)
    ]

gRight :: (Show a, MonadIO m) => m (Either a b) -> m b
gRight ac = do
  r <- ac
  case r of
    Left m -> do
      assertFailure (show m)
      return undefined
    Right d -> return d


testDocumentMails  :: DBEnv -> Maybe String -> Assertion
testDocumentMails  env mailTo = withTestEnvironment env $ do
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  forM_ allLocales $ \l ->
    forM_ [Contract,Offer,Order] $ \doctype -> do
        -- make  the context, user and document all use the same locale
        ctx <- mailingContext l env
        _ <- dbUpdate $ SetUserSettings (userid author) $ (usersettings author) { locale = l }
        let aa = AuthorActor (ctxtime ctx) (IPAddress 0) (userid author) (BS.toString $ getEmail author)
        d' <- gRight $ randomUpdate $ NewDocument author mcompany (BS.fromString "Document title") (Signable doctype) 0 aa
        d <- gRight . dbUpdate $ SetDocumentLocale (documentid d') l (SystemActor $ ctxtime ctx)

        let docid = documentid d
        let asl = head $ documentsignatorylinks d
        let authordetails = signatorydetails asl
        file <- addNewRandomFile
        _ <- gRight $ randomUpdate $ AttachFile docid (fileid file) (SystemActor $ ctxtime ctx)

        isl <- rand 10 arbitrary
        now <- getMinutesTime
        let authorrole = [SignatoryAuthor, SignatoryPartner]
            sigs = [(authordetails,authorrole), (isl,[SignatoryPartner])]
        _ <- gRight $ randomUpdate $ ResetSignatoryDetails docid sigs (SystemActor now)
        d2 <- gRight $ randomUpdate $ PreparationToPending docid (SystemActor now)
        let asl2 = head $ documentsignatorylinks d2
        _ <- gRight $ randomUpdate $ MarkDocumentSeen docid (signatorylinkid asl2) (signatorymagichash asl2) 
             (SignatoryActor now (IPAddress 0) (maybesignatory asl2) (BS.toString $ getEmail asl2) (signatorylinkid asl2))
        doc <- gRight $ randomUpdate $ \si -> SignDocument docid (signatorylinkid asl2) (signatorymagichash asl2) si (SystemActor now)
        let [sl] = filter (not . isAuthor) (documentsignatorylinks doc)
        req <- mkRequest POST []
        --Invitation Mails
        let checkMail s mg = do
                              m <- fst <$> (runTestKontra req ctx $ mg)
                              validMail (s ++ " "++ show doctype) m
                              sendoutForManualChecking (s ++ " " ++ show doctype ) req ctx mailTo m
        checkMail "Invitation" $ mailInvitation True ctx Sign doc (Just sl)
        -- DELIVERY MAILS
        checkMail "Deferred invitation"    $  mailDeferredInvitation (ctxhostpart ctx) doc
        checkMail "Undelivered invitation" $  mailUndeliveredInvitation (ctxhostpart ctx) doc sl
        checkMail "Delivered invitation"   $  mailDeliveredInvitation doc sl
        --remind mails
        checkMail "Reminder notsigned" $ mailDocumentRemind Nothing ctx doc sl
        --reject mail
        checkMail "Reject"  $ mailDocumentRejected  Nothing  ctx doc sl
        -- awaiting author email
        when (doctype == Contract) $ do
          checkMail "Awaiting author" $ mailDocumentAwaitingForAuthor  ctx doc (mkLocaleFromRegion defaultValue)
        -- Virtual signing
        _ <- randomUpdate $ \ip -> SignDocument docid (signatorylinkid sl) (signatorymagichash sl) Nothing 
                                   (SignatoryActor (10 `minutesAfter` now) ip (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl))
        (Just sdoc) <- randomQuery $ GetDocumentByDocumentID docid
        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed ctx sdoc
        -- Reminder after send
        checkMail "Reminder signed" $ mailDocumentRemind Nothing ctx doc (head $ documentsignatorylinks sdoc)


testUserMails :: DBEnv -> Maybe String -> Assertion
testUserMails env mailTo = withTestEnvironment env $ do
  forM_ allLocales $ \l ->  do
    -- make a user and context that use the same locale
    ctx <- mailingContext l env
    user <- addNewRandomAdvancedUserWithLocale l

    req <- mkRequest POST []
    let checkMail s mg = do
                           m <- fst <$> (runTestKontra req ctx $ mg)
                           validMail s m
                           sendoutForManualChecking s req ctx mailTo m
    checkMail "New account" $ do
          al <- newAccountCreatedLink user
          newUserMail (ctxhostpart ctx) (getEmail user) (getEmail user) al
    checkMail "New account by admin" $ do
          al <- newAccountCreatedLink user
          mailNewAccountCreatedByAdmin ctx (ctxlocale ctx) (getSmartName user) (getEmail user) al Nothing
    checkMail "Reset password mail" $ do
          al <- newAccountCreatedLink user
          resetPasswordMail (ctxhostpart ctx) user al


-- MAIL TESTING UTILS
validMail :: String -> Mail -> DB ()
validMail name m = do
    let c = content m
    let exml = xmlParse' name c
    case (any isAlphaNum $ title m) of
         True -> assertSuccess
         False -> assertFailure ("Empty title of mail " ++ name)
    case exml of
         Right _ -> assertSuccess
         Left err -> assertFailure ("Not valid HTML mail " ++ name ++ " : " ++ c ++ " " ++ err)

addNewRandomAdvancedUserWithLocale :: Locale -> DB User
addNewRandomAdvancedUserWithLocale l = do
  user <- addNewRandomAdvancedUser
  _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
           locale = l
         }
  (Just uuser) <- dbQuery $ GetUserByID (userid user)
  return uuser

mailingContext :: Locale -> DBEnv -> DB Context
mailingContext locale env = do
    globaltemplates <- readGlobalTemplates
    ctx <- mkContext locale globaltemplates
    return $ ctx {
                ctxdbenv = env,
                ctxhostpart = "http://dev.skrivapa.se"
              }


sendoutForManualChecking ::  String -> Request -> Context ->  Maybe String -> Mail -> DB ()
sendoutForManualChecking _ _ _ _ _ = assertSuccess
{-
sendoutForManualChecking _ _ _ Nothing _ = assertSuccess
sendoutForManualChecking titleprefix req ctx (Just email) m = do
    _ <- runTestKontra req ctx $ do
            let mailToSend =  m {to = [MailAddress {fullname=BS.fromString "Tester",
                                                email=BS.fromString email}],
                                 title = BS.fromString $ "(" ++ titleprefix ++"): " ++ (BS.toString $ title m)}
            a <- rand 10 arbitrary
            success <- sendMail testMailer a mailToSend
            assertBool "Mail could not be send" success
    assertSuccess

testMailer:: Mailer
testMailer = createSendgridMailer $ MailsSendgrid {
        isBackdoorOpen = False,
        ourInfoEmail = "test@scrive.com",
        ourInfoEmailNiceName = "test",
        sendgridSMTP = "smtps://smtp.sendgrid.net",
        sendgridRestAPI = "https://sendgrid.com/api",
        sendgridUser = "duzyrak@gmail.com",
        sendgridPassword = "zimowisko"}
-}

toMailAddress :: [String] -> Maybe String
toMailAddress [] = Nothing
toMailAddress (a:_) = if ('@' `elem` a)
                        then Just a
                        else Nothing
