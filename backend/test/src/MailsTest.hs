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
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

mailsTests :: TestEnvSt -> Test
mailsTests env = testGroup
  "Mails"
  [ testThat "Document emails"         env testDocumentMails
  , testThat "Branded document emails" env testBrandedDocumentMails
  , testThat "User emails"             env testUserMails
  ]

testBrandedDocumentMails :: TestEnv ()
testBrandedDocumentMails = do
  ug     <- instantiateRandomUserGroup
  author <- instantiateUser $ randomUserTemplate { groupID = return $ ug ^. #id }
  void . dbUpdate . UserGroupUpdate $ set #ui (Just defaultUserGroupUI) ug
  sendDocumentMails author

testDocumentMails :: TestEnv ()
testDocumentMails = do
  author <- instantiateRandomUser
  sendDocumentMails author

sendDocumentMails :: User -> TestEnv ()
sendDocumentMails author = do
  forM_ allLangs $ \l -> do
      -- make  the context, user and document all use the same lang
    ctx <- mkContext l
    void . dbUpdate $ SetUserSettings (author ^. #id) (author ^. #settings & #lang .~ l)
    let aa           = authorActor ctx author
        homeFolderId = fromJust $ author ^. #homeFolderID
    req <- mkRequest POST []
    runTestKontra req ctx $ do
      let genDoc = randomUpdate $ NewDocument author
                                              "Document title"
                                              Signable
                                              defaultTimeZoneName
                                              0
                                              aa
                                              homeFolderId
      withDocumentM genDoc $ do
        res <- dbUpdate $ SetDocumentLang l (systemActor $ ctx ^. #time)
        unless res $ unexpectedError "Expected True"

        asl  <- head . documentsignatorylinks <$> theDocument
        file <- addNewRandomFile
        randomUpdate $ AttachFile file (systemActor $ ctx ^. #time)

        islf <- rand 10 arbitrary

        now  <- currentTime
        let sigs =
              [ defaultSignatoryLink { signatoryfields   = signatoryfields asl
                                     , signatoryisauthor = True
                                     , signatoryrole     = SignatoryRoleSigningParty
                                     , maybesignatory    = maybesignatory asl
                                     }
              , defaultSignatoryLink { signatoryfields = islf
                                     , signatoryrole   = SignatoryRoleSigningParty
                                     }
              ]
        success <- randomUpdate $ ResetSignatoryDetails sigs (systemActor now)
        unless success $ unexpectedError "Expected True"
        tz <- mkTimeZoneName "Europe/Stockholm"
        randomUpdate $ PreparationToPending (systemActor now) tz
        asl2 <- head . documentsignatorylinks <$> theDocument
        randomUpdate . MarkDocumentSeen (signatorylinkid asl2) =<< signatoryActor ctx asl2
        randomUpdate $ SignDocument (signatorylinkid asl2)
                                    Nothing
                                    Nothing
                                    SignatoryScreenshots.emptySignatoryScreenshots
                                    (systemActor now)
        sls <- filter (not . isAuthor) . documentsignatorylinks <$> theDocument
        sl  <- case sls of
          [sl] -> return sl
          _    -> unexpectedError "Expected only a single sig link!"
        --Invitation Mails
        let checkMail s mg = do
              logInfo_ $ "Checking mail" <+> T.pack s
              m <- mg
              validMail (T.pack s) m
        checkMail "Invitation" $ mailInvitation True Sign (Just sl) =<< theDocument
        -- DELIVERY MAILS
        checkMail "Deferred invitation"
          $ mailDeferredInvitation (ctx ^. #mailNoreplyAddress) (ctx ^. #brandedDomain) sl
          =<< theDocument
        checkMail "Undelivered invitation"
          $   mailUndeliveredInvitation (ctx ^. #mailNoreplyAddress)
                                        (ctx ^. #brandedDomain)
                                        sl
          =<< theDocument
        checkMail "Delivered invitation"
          $   mailDeliveredInvitation (ctx ^. #mailNoreplyAddress)
                                      (ctx ^. #brandedDomain)
                                      sl
          =<< theDocument
        checkMail "Undelivered confirmation" $ do
          doc <- theDocument
          mailUndeliveredConfirmation (ctx ^. #mailNoreplyAddress)
                                      (ctx ^. #brandedDomain)
                                      sl
                                      doc
        --remind mails
        checkMail "Reminder notsigned" $ do
          doc <- theDocument
          mailDocumentRemind False Nothing doc sl True False
        checkMail "Reminder notsigned" $ do
          doc <- theDocument
          mailDocumentRemind True Nothing doc sl True False
        --reject mail
        checkMail "Reject" $ mailDocumentRejected True Nothing True sl =<< theDocument
        checkMail "Reject" $ mailDocumentRejected True Nothing False sl =<< theDocument
        -- awaiting author email
        checkMail "Awaiting author"
          $   mailDocumentAwaitingForAuthor (defaultLang :: Lang)
          =<< theDocument
        -- Virtual signing
        randomUpdate
          .   SignDocument (signatorylinkid sl)
                           Nothing
                           Nothing
                           SignatoryScreenshots.emptySignatoryScreenshots
          =<< signatoryActor (set #time (10 `minutesAfter` now) ctx) sl

        -- Sending closed email
        checkMail "Closed" $ mailDocumentClosed False sl False False False =<< theDocument
        -- Reminder after send
        checkMail "Reminder signed" $ do
          doc <- theDocument
          mailDocumentRemind True
                             Nothing
                             doc
                             (head $ documentsignatorylinks doc)
                             True
                             False

        checkMail "Party process finalized" $ do
          doc <- theDocument
          mailPartyProcessFinalizedNotification doc
                                                (head $ documentsignatorylinks doc)
                                                DocumentSigned
  commit


testUserMails :: TestEnv ()
testUserMails = do
  forM_ allLangs $ \l -> do
    -- make a user and context that use the same lang
    ctx  <- mkContext l
    user <- addNewRandomUserWithLang l

    req  <- mkRequest POST []
    let checkMail s mg = do
          logInfo_ $ "Checking mail" <+> T.pack s
          m <- fst <$> runTestKontra req ctx mg
          validMail (T.pack s) m
    checkMail "New account" $ do
      al <- newUserAccountRequestLink (ctx ^. #lang) (user ^. #id) AccountRequest
      newUserMail ctx (getEmail user) al
    checkMail "New account by admin" $ do
      al <- newUserAccountRequestLink (ctx ^. #lang) (user ^. #id) ByAdmin
      mailNewAccountCreatedByAdmin ctx (ctx ^. #lang) (getEmail user) al
    checkMail "Reset password mail" $ do
      al <- newUserAccountRequestLink (ctx ^. #lang) (user ^. #id) AccountRequest
      resetPasswordMail ctx user al
  commit



-- MAIL TESTING UTILS
validMail :: MonadIO m => Text -> Mail -> m ()
validMail name m = do
  let c    = content m
      c'   = "<html>" <> TL.fromStrict c <> "</html>"
           -- ^ XML parser freaks out if there's content after root element.
      exml = XML.parseText XML.def c'
  unless (T.any isAlphaNum $ title m) . assertFailure $ T.unpack
    ("Empty title of mail " <> name)
  case exml of
    Left exc -> assertFailure
      $ T.unpack ("Invalid HTML mail " <> name <> " : " <> c <> " " <> showt exc)
    Right _ -> assertSuccess

addNewRandomUserWithLang :: Lang -> TestEnv User
addNewRandomUserWithLang l = do
  user <- instantiateRandomUser
  void . dbUpdate $ SetUserSettings (user ^. #id) (user ^. #settings & #lang .~ l)
  Just uuser <- dbQuery $ GetUserByID (user ^. #id)
  return uuser
