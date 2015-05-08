module User.Action (
    handleAccountSetupFromSign
  , handleActivate
  , createUser
  , phoneMeRequest
  , checkPasswordsMatch
  ) where

import Control.Monad.Catch
import Data.Functor
import Log
import Text.StringTemplates.Templates

import ActionQueue.AccessNewAccount (newAccessNewAccountLink)
import Administration.AddPaymentPlan
import BrandedDomain.BrandedDomain
import Company.CompanyID
import Company.Model
import Crypto.RNG
import DB
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocumentID)
import Doc.Model
import Happstack.Fields
import InputValidation
import Kontra
import KontraPrelude
import MailContext (MailContextMonad(..), MailContext(..))
import Mails.SendMail
import MinutesTime
import Payments.Model
import ThirdPartyStats.Core
import User.Email
import User.History.Model
import User.Model
import User.UserView
import Util.FlashUtil
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils

handleAccountSetupFromSign :: (Kontrakcja m, DocumentMonad m) => SignatoryLink -> m (Maybe User)
handleAccountSetupFromSign signatorylink = do
  let firstname = getFirstName signatorylink
      lastname = getLastName signatorylink
      cname = getCompanyName signatorylink
      cnumber = getCompanyNumber signatorylink
  email <- guardJustM $ getOptionalField asValidEmail "email"
  muser <- dbQuery $ GetUserByEmail (Email email)
  user <- case muser of
            Just u -> do
              when (isJust $ userhasacceptedtermsofservice u) $ do -- Don't remove - else people will be able to hijack accounts
                internalError
              return u
            Nothing -> do
               company <- dbUpdate $ CreateCompany
               _ <- dbUpdate $ SetCompanyInfo (companyid company) $ (companyinfo company) {
                    companyname = cname
                  , companynumber = cnumber
                }
               guardJustM $ documentlang <$> theDocument >>= createUser (Email email) (firstname, lastname) (companyid company,True)
  company <- dbQuery $ GetCompanyByUserID (userid user)
  mactivateduser <- handleActivate (Just $ firstname) (Just $ lastname) (user,company) BySigning
  case mactivateduser of
    Just activateduser -> do
      _ <- dbUpdate $ SaveDocumentForUser activateduser (signatorylinkid signatorylink)

      return $ Just activateduser
    Nothing -> return Nothing

handleActivate :: Kontrakcja m => Maybe String -> Maybe String -> (User,Company) -> SignupMethod -> m (Maybe User)
handleActivate mfstname msndname (actvuser,company) signupmethod = do
  logInfo_ $ "Attempting to activate account for user " ++ (show $ getEmail actvuser)
  when (isJust $ userhasacceptedtermsofservice actvuser) $ do  -- Don't remove - else people will be able to hijack accounts
    internalError
  switchLang (getLang actvuser)
  ctx <- getContext
  mtos <- getDefaultedField False asValidCheckBox "tos"
  callme <- isFieldSet "callme"
  stoplogin <- isFieldSet "stoplogin"
  promo <- isFieldSet "promo"
  haspassword <- isFieldSet "password"
  phone <-  fromMaybe (getMobile actvuser) <$> getField "phone"
  companyname <- fromMaybe (getCompanyName company) <$> getField "company"
  position <- fromMaybe "" <$> getField "position"
  case mtos of
    (Just tos) -> do
          if tos
            then do
              _ <- dbUpdate $ SetUserInfo (userid actvuser) $ (userinfo actvuser) {
                  userfstname = fromMaybe "" mfstname
                , usersndname = fromMaybe "" msndname
                , userphone = phone
                , usercompanyposition = position
              }
              _ <- dbUpdate $ SetCompanyInfo (companyid company) $ (companyinfo company) {
                  companyname = companyname
              }
              _ <- dbUpdate $ LogHistoryUserInfoChanged (userid actvuser)
                (ctxipnumber ctx) (ctxtime ctx) (userinfo actvuser)
                ((userinfo actvuser) { userfstname = fromMaybe "" mfstname , usersndname =  fromMaybe "" msndname })
                (userid <$> ctxmaybeuser ctx)
              _ <- dbUpdate $ LogHistoryPasswordSetup (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              _ <- dbUpdate $ AcceptTermsOfService (userid actvuser) (ctxtime ctx)
              _ <- dbUpdate $ LogHistoryTOSAccept (userid actvuser) (ctxipnumber ctx) (ctxtime ctx) (userid <$> ctxmaybeuser ctx)
              _ <- dbUpdate $ SetSignupMethod (userid actvuser) signupmethod

              ds <- dbQuery $ GetSignatoriesByEmail (Email $ getEmail actvuser) (14 `daysBefore` ctxtime ctx)

              forM_ ds $ \(d, s) -> do
                withDocumentID d $ dbUpdate $ SaveDocumentForUser actvuser s

              when (haspassword) $ do
                mpassword <- getOptionalField asValidPassword "password"
                _ <- case (mpassword) of
                    Just password -> do
                        passwordhash <- createPassword password
                        void $ dbUpdate $ SetUserPassword (userid actvuser) passwordhash
                    Nothing -> return () -- TODO what do I do here?
                return ()

              when (signupmethod == BySigning) $
                scheduleNewAccountMail ctx actvuser
              tosuser <- guardJustM $ dbQuery $ GetUserByID (userid actvuser)

              logInfo_ $ "Attempting successfull. User " ++ (show $ getEmail actvuser) ++ "is logged in."
              when (not stoplogin) $ do
                logUserToContext $ Just tosuser
              when (callme) $ phoneMeRequest (Just tosuser) phone
              when (promo) $ addCompanyPlanManual (companyid company) TrialPricePlan ActiveStatus
              return $ Just tosuser
            else do
              logInfo_ $ "No TOS accepted. We cant activate user."
              addFlashM flashMessageMustAcceptTOS
              return Nothing
    _ -> do
        logInfo_ $ "Create account attempt failed (params missing)"
        return Nothing

scheduleNewAccountMail :: (TemplatesMonad m, CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m) => Context -> User -> m ()
scheduleNewAccountMail ctx user = do
  link <- newAccessNewAccountLink $ userid user
  mail <- accessNewAccountMail ctx user link
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }

createUser :: (CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Email -> (String, String) -> (CompanyID,Bool) -> Lang -> m (Maybe User)
createUser email names companyandrole lang = do
  mctx <- getMailContext
  passwd <- createPassword =<< randomPassword
  muser <- dbUpdate $ AddUser names (unEmail email) (Just passwd) companyandrole lang (bdid $ mctxcurrentBrandedDomain mctx)
  case muser of
    Just user -> do
      _ <- dbUpdate $ LogHistoryAccountCreated (userid user) (mctxipnumber mctx) (mctxtime mctx) email (userid <$> mctxmaybeuser mctx)
      return muser
    _ -> return muser

phoneMeRequest :: Kontrakcja m => Maybe User -> String -> m ()
phoneMeRequest muser phone = do
  ctx <- getContext
  let content = case muser of
        Just user -> "<p>User " ++ getFirstName user ++ " "
                     ++ getLastName user ++ " "
                     ++ "&lt;" ++ getEmail user ++ "&gt; "
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
    let user = $fromJust muser
        name = getFirstName user ++ " " ++ getLastName user
    now <- ctxtime <$> getContext

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
