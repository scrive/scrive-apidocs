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

mailsTests :: Connection -> [String] -> Test
mailsTests conn params  = testGroup "Mails" [
    testCase "All invitation mails" $ testDocumentInvitationMail conn (toMailAddress params)
    ]

testDocumentInvitationMail :: Connection -> Maybe String -> Assertion
testDocumentInvitationMail conn mailTo = withTestEnvironment conn $ do
    author <- addNewRandomAdvancedUser
    mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
    (Right d) <- randomUpdate $ \t -> NewDocument author mcompany t (Signable Contract)
    let docid = documentid d 
    let authordetails = signatorydetails $ head $ documentsignatorylinks d
    (sl1,sl2) <- rand 10 arbitrary
    time <- getMinutesTime
    _ <- randomUpdate $ UpdateDocumentSimple docid (authordetails,author) [sl1,sl2]
    (Right doc) <- randomUpdate $ \ip -> AuthorSignDocument docid time ip Nothing         
    ctx <- mailingContext conn
    req <- mkRequest POST []
    --Generating all invitation mails
    let jsignatories = Just <$> filter (not . isAuthor) (documentsignatorylinks doc)
    mailInvitations <- (map fst) <$> mapM (runTestKontra req ctx) ((mailInvitation True ctx Sign doc) <$> jsignatories)
    --Sending for manual checking
    mapM_ (sendoutForManualChecking req ctx mailTo) mailInvitations 
    return ()

-- MAIL TESTING UTILS
validMail :: Mail -> Bool
validMail _m = True


mailingContext :: Connection -> DB Context
mailingContext conn = do
    ctx <- mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates
    return $ ctx {
                ctxdbconn = conn,
                ctxhostpart = "http://dev.skrivapa.se"
              }


sendoutForManualChecking ::  Request -> Context ->  Maybe String -> Mail -> DB ()
sendoutForManualChecking req ctx Nothing _ = assertSuccess
sendoutForManualChecking req ctx (Just email) m = do
    runTestKontra req ctx $ do
        let mailToSend =  m {to = [MailAddress {fullname=BS.fromString "Tester",
                                                email=BS.fromString email}],
                             title = BS.fromString $ "(Testing) " ++ (BS.toString $ title m)}
        a <- rand 10 arbitrary 
        liftIO $ putStrLn $ show "Really sending"
        success <- sendMail testMailer a mailToSend
        liftIO $ putStrLn $ show "Done"
        assertBool "Mail has been relly send" success
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