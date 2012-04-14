module MailsTest (mailsTests) where

import Control.Applicative
import Happstack.Server
import Test.Framework

import Crypto.RNG
import DB
import Context
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import IPAddress
import Misc
import Doc.Model
import Doc.DocViewMail
import Doc.DocStateData
import Mails.SendMail
import Company.Model
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

mailsTests :: [String] -> (Nexus, CryptoRNGState) -> Test
mailsTests params env  = testGroup "Mails" [
    testThat "Document emails" env $ testDocumentMails $ toMailAddress params
  , testThat "Branded document emails" env $ testBrandedDocumentMails $ toMailAddress params
  , testThat "User emails" env $ testUserMails $ toMailAddress params
  ]

gRight :: (Show a, MonadIO m) => m (Either a b) -> m b
gRight ac = do
  r <- ac
  case r of
    Left m -> do
      assertFailure (show m)
      return undefined
    Right d -> return d

testBrandedDocumentMails :: Maybe String -> TestEnv ()
testBrandedDocumentMails mailTo = do
  company' <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company') False
  let cui = CompanyUI {
        companybarsbackground = Just "orange"
      , companybarstextcolour = Just "green"
      , companylogo = Nothing
      }
  _ <- dbUpdate $ UpdateCompanyUI (companyid company') cui
  mcompany <- dbQuery $ GetCompany (companyid company')
  sendDocumentMails mailTo author mcompany

testDocumentMails :: Maybe String -> TestEnv ()
testDocumentMails mailTo = do
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  sendDocumentMails mailTo author mcompany

sendDocumentMails :: Maybe String -> User -> Maybe Company -> TestEnv ()
sendDocumentMails mailTo author mcompany = do
  forM_ allLocales $ \l ->
    forM_ [Contract,Offer,Order] $ \doctype -> do
        -- make  the context, user and document all use the same locale
        ctx <- mailingContext l
        _ <- dbUpdate $ SetUserSettings (userid author) $ (usersettings author) { locale = l }
        let aa = AuthorActor (ctxtime ctx) noIP (userid author) (getEmail author)
        d' <- gRight $ randomUpdate $ NewDocument author mcompany "Document title" (Signable doctype) 0 aa
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
             (SignatoryActor now noIP (maybesignatory asl2) (getEmail asl2) (signatorylinkid asl2))
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
                                   (SignatoryActor (10 `minutesAfter` now) ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl))
        (Just sdoc) <- randomQuery $ GetDocumentByDocumentID docid
        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed ctx sdoc
        -- Reminder after send
        checkMail "Reminder signed" $ mailDocumentRemind Nothing ctx doc (head $ documentsignatorylinks sdoc)


testUserMails :: Maybe String -> TestEnv ()
testUserMails mailTo = do
  forM_ allLocales $ \l ->  do
    -- make a user and context that use the same locale
    ctx <- mailingContext l
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

addNewRandomAdvancedUserWithLocale :: Locale -> TestEnv User
addNewRandomAdvancedUserWithLocale l = do
  user <- addNewRandomAdvancedUser
  _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) {
           locale = l
         }
  (Just uuser) <- dbQuery $ GetUserByID (userid user)
  return uuser

mailingContext :: Locale -> TestEnv Context
mailingContext locale = do
    globaltemplates <- readGlobalTemplates
    ctx <- mkContext locale globaltemplates
    return $ ctx { ctxhostpart = "http://dev.skrivapa.se" }

sendoutForManualChecking ::  String -> Request -> Context ->  Maybe String -> Mail -> TestEnv ()
sendoutForManualChecking _ _ _ _ _ = assertSuccess

{-
sendoutForManualChecking ::  String -> Request -> Context ->  Maybe String -> Mail -> DB ()
sendoutForManualChecking _ _ _ Nothing _ = assertSuccess
sendoutForManualChecking _ req ctx (Just email) m = do
    _ <- runTestKontra req ctx $ do
           _ <- scheduleEmailSendout (ctxmailsconfig ctx) $ m {
                  to = [MailAddress { fullname = BS.fromString "Tester",
                                      email = BS.fromString email}]
                , from = Nothing
           }
           assertSuccess
    assertSuccess
-}

toMailAddress :: [String] -> Maybe String
toMailAddress [] = Nothing
toMailAddress (a:_) = if ('@' `elem` a)
                        then Just a
                        else Nothing
