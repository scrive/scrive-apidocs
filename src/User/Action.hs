module User.Action
    (
     handleAccountSetupFromSign,
     handleActivate,
     createInvitedUser,
     phoneMeRequest,
     checkPasswordsMatch
    )
    where

import Control.Monad.Error
import Data.Functor
import Data.Maybe

import DB.Classes
import Doc.Model
import Doc.DocStateData
import InputValidation
import Kontra
import Mails.SendMail
import Misc
import Templates.Templates
import User.Model
import User.UserView
import Util.FlashUtil
import Util.HasSomeUserInfo
import qualified Log
import Util.MonadUtils
import Stats.Control
import EvidenceLog.Model
import User.History.Model
import ScriveByMail.Model
import qualified ScriveByMail.Action as MailAPI

handleAccountSetupFromSign :: Kontrakcja m => Document -> SignatoryLink -> m (Maybe User)
handleAccountSetupFromSign document signatorylink = do
  ctx <- getContext
  let firstname = getFirstName signatorylink
      lastname = getLastName signatorylink
      email = getEmail signatorylink
  muser <- runDBQuery $ GetUserByEmail (currentServiceID ctx) (Email email)
  user <- maybe (guardJustM $ createInvitedUser (firstname, lastname) email Nothing)
                return
                muser
  mactivateduser <- handleActivate (Just $ firstname) (Just $ lastname) user BySigning
  case mactivateduser of
    Just (activateduser, _) -> do
      let actor = SignatoryActor (ctxtime ctx) (ctxipnumber ctx)  (maybesignatory signatorylink)  (getEmail signatorylink) (signatorylinkid signatorylink)
      _ <- runDBUpdate $ SaveDocumentForUser (documentid document) activateduser (signatorylinkid signatorylink) actor
      _ <- addUserSaveAfterSignStatEvent activateduser
      return $ Just activateduser
    Nothing -> return Nothing

handleActivate :: Kontrakcja m => Maybe String -> Maybe String -> User -> SignupMethod -> m (Maybe (User, [Document]))
handleActivate mfstname msndname actvuser signupmethod = do
  Log.debug $ "Attempting to activate account for user " ++ (show $ getEmail actvuser)
  when (isJust $ userhasacceptedtermsofservice actvuser) internalError
  switchLocale (getLocale actvuser)
  ctx <- getContext
  mtos <- getDefaultedField False asValidCheckBox "tos"
  callme <- isFieldSet "callme"
  phone <-  fromMaybe "" <$> getField "phone"
  mpassword <- getRequiredField asValidPassword "password"
  mpassword2 <- getRequiredField asValidPassword "password2"
  case (mtos, mfstname, msndname, mpassword, mpassword2) of
    (Just tos, Just fstname, Just sndname, Just password, Just password2) -> do
      case checkPasswordsMatch password password2 of
        Right () ->
          if tos
            then do
              passwordhash <- createPassword password
              runDB $ do
                _ <- dbUpdate $ SetUserInfo (userid actvuser) $ (userinfo actvuser){
                         userfstname = fstname
                       , usersndname = sndname
                       , userphone = phone
                       }
                _ <- dbUpdate $ LogHistoryUserInfoChanged (userid actvuser) (ctxipnumber ctx) (ctxtime ctx)
                                                          (userinfo actvuser) ((userinfo actvuser){ userfstname = fstname , usersndname = sndname })
                                                          (userid <$> ctxmaybeuser ctx)
                _ <- dbUpdate $ SetUserPassword (userid actvuser) passwordhash
                _ <- dbUpdate $ LogHistoryPasswordSetup (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
                _ <- dbUpdate $ AcceptTermsOfService (userid actvuser) (ctxtime ctx)
                _ <- dbUpdate $ LogHistoryTOSAccept (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
                _ <- dbUpdate $ SetSignupMethod (userid actvuser) signupmethod
                return ()
              mdelays <- runDBQuery $ GetMailAPIDelaysForEmail (getEmail actvuser) (ctxtime ctx)

              newdocs <- case mdelays of
                Nothing -> return []
                Just (delayid, texts) -> do
                  results <- forM texts (\t -> catchError (MailAPI.doMailAPI t) (\_ -> return Nothing))
                  runDBUpdate $ DeleteMailAPIDelays delayid (ctxtime ctx)
                  return $ catMaybes results

              tosuser <- guardJustM $ runDBQuery $ GetUserByID (userid actvuser)
              _ <- addUserSignTOSStatEvent tosuser
              _ <- addUserLoginStatEvent (ctxtime ctx) tosuser
              logUserToContext $ Just tosuser
              when (callme) $ phoneMeRequest tosuser phone
              return $ Just (tosuser, newdocs)
            else do
              addFlashM flashMessageMustAcceptTOS
              return Nothing
        Left flash -> do
          addFlashM flash
          return Nothing
    _ -> return Nothing

createInvitedUser :: Kontrakcja m => (String, String) -> String -> Maybe Locale -> m (Maybe User)
createInvitedUser names email mlocale = do
    ctx <- getContext
    let locale = fromMaybe (ctxlocale ctx) mlocale
    passwd <- createPassword =<< randomPassword
    muser <- runDBUpdate $ AddUser names email (Just passwd) False Nothing Nothing locale
    case muser of
      Just user -> do
                   _ <- runDBUpdate $ LogHistoryAccountCreated (userid user) (ctxipnumber ctx) (ctxtime ctx) (Email email) (userid <$> ctxmaybeuser ctx)
                   return muser
      _         -> return muser

phoneMeRequest :: Kontrakcja m => User -> String -> m ()
phoneMeRequest user phone = do
  ctx <- getContext
  let content = "<p>User " ++ getFirstName user ++ " "
                    ++ getLastName user ++ " "
                    ++ "&lt;" ++ getEmail user ++ "&gt; "
                    ++ "has requested a call on "
                    ++ "&lt;" ++ phone ++ "&gt;.  "
                    ++ "They have just signed the TOS, "
                    ++ "and they're setup with lang "
                    ++ "&lt;" ++ (codeFromLang $ getLang user) ++ "&gt;.</p>"
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
            to = [MailAddress { fullname = "info@skrivapa.se", email = "info@skrivapa.se" }]
          , title = "Phone Call Request"
          , content = content
      }
  _ <- addUserPhoneAfterTOS user
  return ()

checkPasswordsMatch :: TemplatesMonad m => String -> String -> Either (m FlashMessage) ()
checkPasswordsMatch p1 p2 =
    if p1 == p2
       then Right ()
       else Left flashMessagePasswordsDontMatch
