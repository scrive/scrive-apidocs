module User.Action (
    handleAccountSetupFromSign
  , handleActivate
  , createUser
  , phoneMeRequest
  , checkPasswordsMatch
  ) where

import Control.Monad
import Data.Functor
import Data.Maybe
import qualified Control.Exception.Lifted as E

import ActionQueue.AccessNewAccount (newAccessNewAccountLink)
import Company.CompanyID
import DB
import Doc.Model
import Doc.DocStateData
import InputValidation
import Kontra
import Mails.SendMail
import Happstack.Fields
import Text.StringTemplates.Templates
import User.Model
import User.UserView
import Util.FlashUtil
import Util.HasSomeUserInfo
import qualified Log
import Util.MonadUtils
import Stats.Control
import Util.Actor
import User.History.Model
import ScriveByMail.Model
import qualified ScriveByMail.Action as MailAPI
import ThirdPartyStats.Core
import Payments.Model
import Administration.AddPaymentPlan
import Crypto.RNG
import MinutesTime
import BrandedDomains

handleAccountSetupFromSign :: Kontrakcja m => Document -> SignatoryLink -> m (Maybe User)
handleAccountSetupFromSign document signatorylink = do
  ctx <- getContext
  let firstname = getFirstName signatorylink
      lastname = getLastName signatorylink
  email <- guardJustM $ getRequiredField asValidEmail "email"
  muser <- dbQuery $ GetUserByEmail (Email email)
  user <- maybe (guardJustM $ createUser (Email email) (firstname, lastname) Nothing (documentlang document))
                return
                muser
  mactivateduser <- handleActivate (Just $ firstname) (Just $ lastname) user BySigning
  case mactivateduser of
    Just (activateduser, _) -> do
      let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx)  (maybesignatory signatorylink)  (getEmail signatorylink) (signatorylinkid signatorylink)
      _ <- dbUpdate $ SaveDocumentForUser (documentid document) activateduser (signatorylinkid signatorylink) actor
      _ <- addUserSaveAfterSignStatEvent activateduser
      return $ Just activateduser
    Nothing -> return Nothing

handleActivate :: Kontrakcja m => Maybe String -> Maybe String -> User -> SignupMethod -> m (Maybe (User, [Document]))
handleActivate mfstname msndname actvuser signupmethod = do
  Log.debug $ "Attempting to activate account for user " ++ (show $ getEmail actvuser)
  when (isJust $ userhasacceptedtermsofservice actvuser) internalError
  switchLang (getLang actvuser)
  ctx <- getContext
  mtos <- getDefaultedField False asValidCheckBox "tos"
  callme <- isFieldSet "callme"
  stoplogin <- isFieldSet "stoplogin"
  promo <- isFieldSet "promo"
  haspassword <- isFieldSet "password"
  phone <-  fromMaybe "" <$> getField "phone"
  companyname <- fromMaybe "" <$> getField "company"
  position <- fromMaybe "" <$> getField "position"
  case (mtos, mfstname, msndname) of
    (Just tos, Just fstname, Just sndname) -> do
          if tos
            then do
              _ <- dbUpdate $ SetUserInfo (userid actvuser) $ (userinfo actvuser) {
                  userfstname = fstname
                , usersndname = sndname
                , userphone = phone
                , usercompanyname = companyname
                , usercompanyposition = position
              }
              _ <- dbUpdate $ LogHistoryUserInfoChanged (userid actvuser)
                (ctxipnumber ctx) (ctxtime ctx) (userinfo actvuser)
                ((userinfo actvuser) { userfstname = fstname , usersndname = sndname })
                (userid <$> ctxmaybeuser ctx)
              _ <- dbUpdate $ LogHistoryPasswordSetup (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              _ <- dbUpdate $ AcceptTermsOfService (userid actvuser) (ctxtime ctx)
              _ <- dbUpdate $ LogHistoryTOSAccept (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              _ <- dbUpdate $ SetSignupMethod (userid actvuser) signupmethod

              ds <- dbQuery $ GetSignatoriesByEmail (Email $ getEmail actvuser) (14 `daysBefore` ctxtime ctx)

              forM_ ds $ \(d, s) -> do
                let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx) (Just $ userid actvuser) (getEmail actvuser) s
                dbUpdate $ SaveDocumentForUser d actvuser s actor

              mdelays <- dbQuery $ GetMailAPIDelaysForEmail (getEmail actvuser) (ctxtime ctx)
              newdocs <- case mdelays of
                Nothing -> return []
                Just (delayid, texts) -> do
                  results <- forM texts (\t -> MailAPI.doMailAPI t `E.catch` (\(_::KontraError) -> return Nothing))
                  dbUpdate $ DeleteMailAPIDelays delayid (ctxtime ctx)
                  return $ catMaybes results

              when (haspassword) $ do
                mpassword <- getRequiredField asValidPassword "password"
                _ <- case (mpassword) of
                    Just password -> do
                        passwordhash <- createPassword password
                        _ <- dbUpdate $ SetUserPassword (userid actvuser) passwordhash
                        return []
                    Nothing -> return [] -- TODO what do I do here?
                return ()

              when (signupmethod == BySigning) $
                scheduleNewAccountMail ctx actvuser
              tosuser <- guardJustM $ dbQuery $ GetUserByID (userid actvuser)
              _ <- addUserSignTOSStatEvent tosuser
              Log.debug $ "Attempting successfull. User " ++ (show $ getEmail actvuser) ++ "is logged in."
              when (not stoplogin) $ do 
                _ <- addUserLoginStatEvent (ctxtime ctx) tosuser
                logUserToContext $ Just tosuser
              when (callme) $ phoneMeRequest (Just tosuser) phone
              when (promo) $ addManualPricePlan (userid actvuser) TrialTeamPricePlan ActiveStatus
              return $ Just (tosuser, newdocs)
            else do
              Log.debug $ "No TOS accepted. We cant activate user."
              addFlashM flashMessageMustAcceptTOS
              return Nothing
    _ -> do
        Log.debug $ "Create account attempt failed (params missing)"
        return Nothing

scheduleNewAccountMail :: (TemplatesMonad m, CryptoRNG m, MonadDB m) => Context -> User -> m ()
scheduleNewAccountMail ctx user = do
  link <- newAccessNewAccountLink $ userid user
  mail <- accessNewAccountMail ctx user link
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }

createUser :: (CryptoRNG m, KontraMonad m, MonadDB m, TemplatesMonad m) => Email -> (String, String) -> Maybe CompanyID -> Lang -> m (Maybe User)
createUser email names mcompanyid lang = do
  ctx <- getContext
  passwd <- createPassword =<< randomPassword
  muser <- dbUpdate $ AddUser names (unEmail email) (Just passwd) mcompanyid lang (bdurl <$> currentBrandedDomain ctx)
  case muser of
    Just user -> do
      _ <- dbUpdate $ LogHistoryAccountCreated (userid user) (ctxipnumber ctx) (ctxtime ctx) email (userid <$> ctxmaybeuser ctx)
      return muser
    _ -> return muser

phoneMeRequest :: Kontrakcja m => Maybe User -> String -> m ()
phoneMeRequest muser phone = do
  ctx <- getContext
  let content = case muser of
        Just user -> "<p>User " ++ getFirstName user ++ " "
                     ++ getLastName user ++ " "
                     ++ "&l   t;" ++ getEmail user ++ "&gt; "
                     ++ "has requested a call on "
                     ++ "&lt;" ++ phone ++ "&gt;.  "
                     ++ "They have just signed the TOS, "
                     ++ "and they're setup with lang "
                     ++ "&lt;" ++ (codeFromLang $ getLang user) ++ "&gt;.</p>"
        Nothing -> "<p>A person "
                    ++ "has requested a call on "
                    ++ "&lt;" ++ phone ++ "&gt;.  "
                    ++ "</p>"
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
            to = [MailAddress { fullname = "info@skrivapa.se", email = "info@skrivapa.se" }]
          , title = "Phone Call Request"
          , content = content
      }
  when (isJust muser) $ do
    let user = fromJust muser
        name = getFirstName user ++ " " ++ getLastName user
    now <- ctxtime <$> getContext
    _ <- addUserPhoneAfterTOS user
    asyncLogEvent "Phone Request" [UserIDProp (userid user),
                                   IPProp (ctxipnumber ctx),
                                   NameProp name,
                                   TimeProp now,
                                   MailProp (useremail $ userinfo user)]
    asyncLogEvent SetUserProps [UserIDProp (userid user),
                                IPProp (ctxipnumber ctx),
                                someProp "Phone Request" now]
    return ()

checkPasswordsMatch :: TemplatesMonad m => String -> String -> Either (m FlashMessage) ()
checkPasswordsMatch p1 p2 =
    if p1 == p2
       then Right ()
       else Left flashMessagePasswordsDontMatch
