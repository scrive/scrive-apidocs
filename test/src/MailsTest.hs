{-# OPTIONS_GHC -Wwarn #-}

module MailsTest (mailsTests) where

import Control.Applicative
import Database.HDBC.PostgreSQL
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.Char8 as BS

import AppControl
import DB.Classes
import Context
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Misc
import Doc.DocState
import Doc.DocViewMail
import Mails.SendMail
import User.Model
import Company.Model
import Control.Monad.Trans
import Mails.MailsConfig
import Mails.SendMail
import TestKontra
import TestingUtil
import qualified Data.ByteString.UTF8 as BS
import System.Random
import Test.QuickCheck
import Control.Monad
import MinutesTime
import Util.SignatoryLinkUtils
import Data.Maybe
import HtmlTest
import User.UserView
import Kontra
import Util.HasSomeUserInfo
import Text.XML.HaXml.Parse (xmlParse')

mailsTests :: Connection -> [String] -> Test
mailsTests conn params  = testGroup "Mails" [
    testCase "Document emails" $ testDocumentMails conn (toMailAddress params),
    testCase "User emails" $ testUserMails conn (toMailAddress params)
    ]

testDocumentMails  :: Connection -> Maybe String -> Assertion
testDocumentMails  conn mailTo = withTestEnvironment conn $ do
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  forM_ allValues $ \l ->   
    forM_ [Contract,Offer,Order] $ \doctype -> do
        let tstr s = s ++ " " ++ show doctype 
        (Right d) <- randomUpdate $ NewDocument author mcompany (BS.fromString "Document title") (Signable doctype)
        let docid = documentid d 
        let authordetails = signatorydetails $ head $ documentsignatorylinks d
        isl <- rand 10 arbitrary
        now <- getMinutesTime
        _ <- randomUpdate $ UpdateDocumentSimple docid (authordetails,author) [isl]
        (Right doc) <- randomUpdate $ \ip -> AuthorSignDocument docid now ip Nothing         
        let [sl] = filter (not . isAuthor) (documentsignatorylinks doc)
        ctx <- mailingContext l conn
        req <- mkRequest POST []
        --Invitation Mails
        let checkMail s mg = do
                              m <- fst <$> (runTestKontra req ctx $ mg) 
                              validMail (s ++ " "++ show doctype) m
                              sendoutForManualChecking (s ++ " " ++ show doctype ) req ctx mailTo m
        checkMail "Invitation" $ mailInvitation True ctx Sign doc (Just sl)
        --remind mails
        checkMail "Reminder notsigned" $ mailDocumentRemind Nothing ctx doc sl
        --cancel by author mail
        checkMail "Cancel" $ mailCancelDocumentByAuthor True Nothing  ctx doc
        --reject mail
        checkMail "Reject"  $ mailDocumentRejected  Nothing  ctx doc sl
        -- awaiting author email 
        --when (doctype == Contract) $ do
        --  checkMail "Awaiting author" $ mailDocumentAwaitingForAuthor  ctx doc 
        -- Virtual signing 
        _ <- randomUpdate $ \ip -> SignDocument docid (signatorylinkid sl)  (10 `minutesAfter` now) ip Nothing []
        (Just sdoc) <- randomQuery $ GetDocumentByDocumentID docid
        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed ctx sdoc
        -- Reminder after send
        checkMail "Reminder signed" $ mailDocumentRemind Nothing ctx doc (head $ documentsignatorylinks sdoc)


testUserMails :: Connection -> Maybe String -> Assertion
testUserMails conn mailTo = withTestEnvironment conn $ do
  forM_ allValues $ \l ->  do  
    user <- addNewRandomAdvancedUser
    ctx <- mailingContext l conn
    req <- mkRequest POST []
    let checkMail s mg = do
                           m <- fst <$> (runTestKontra req ctx $ mg) 
                           validMail s m
                           sendoutForManualChecking s req ctx mailTo m
    checkMail "New account" $ do 
          al <- newAccountCreatedLink user
          newUserMail (ctxhostpart ctx) (getEmail user) (getEmail user) al False
    checkMail "New account by admin" $ do 
          al <- newAccountCreatedLink user
          mailNewAccountCreatedByAdmin ctx (getSmartName user) (getEmail user) al Nothing
    checkMail "New account after signing contract" $ do 
          al <- newAccountCreatedLink user
          mailAccountCreatedBySigningContractReminder (ctxhostpart ctx)  (getSmartName user) (getEmail user) al 
    checkMail "Reset password mail" $ do 
          al <- newAccountCreatedLink user
          resetPasswordMail (ctxhostpart ctx) user al 
    
    
-- MAIL TESTING UTILS
validMail :: String -> Mail -> DB ()
validMail name m = do
    let c = BS.toString $ content m
    let exml = xmlParse' name c
    liftIO $ putStrLn $  name
    liftIO $ putStrLn $  c
    case exml of 
         Right _ -> assertSuccess
         Left err -> assertFailure ("Valid HTML mail " ++ name ++ " : " ++ c) 


mailingContext :: Region -> Connection -> DB Context
mailingContext r conn = do
    ctx <- mkContext =<< localizedVersion (Scrive,r,defaultRegionLang r)  <$> readGlobalTemplates
    return $ ctx {
                ctxdbconn = conn,
                ctxhostpart = "http://dev.skrivapa.se"
              }


sendoutForManualChecking ::  String -> Request -> Context ->  Maybe String -> Mail -> DB ()
sendoutForManualChecking _ _ _ Nothing _ = assertSuccess
sendoutForManualChecking titleprefix req ctx (Just email) m = do
    runTestKontra req ctx $ do
        let mailToSend =  m {to = [MailAddress {fullname=BS.fromString "Tester",
                                                email=BS.fromString email}],
                             title = BS.fromString $ "(" ++ titleprefix ++"): " ++ (BS.toString $ title m)}
        a <- rand 10 arbitrary 
        success <- sendMail testMailer a mailToSend
        assertBool "Mail could not be send" success
    assertSuccess 

testMailer:: Mailer
testMailer = createSendgridMailer $ MailsSendgrid {
        mailbackdooropen = False, 
        ourInfoEmail = "test@skrivapa.se",
        ourInfoEmailNiceName = "test",
        sendgridSMTP = "smtps://smtp.sendgrid.net",
        sendgridRestAPI = "https://sendgrid.com/api",
        sendgridUser = "duzyrak@gmail.com",
        sendgridPassword = "zimowisko"}

toMailAddress :: [String] -> Maybe String
toMailAddress [] = Nothing
toMailAddress (a:_) = if ('@' `elem` a)
                        then Just a
                        else Nothing