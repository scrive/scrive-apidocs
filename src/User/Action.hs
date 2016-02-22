module User.Action (
    handleAccountSetupFromSign
  , handleActivate
  , createUser
  , phoneMeRequest
  ) where

import Control.Conditional (whenM)
import Control.Monad.Catch
import Data.Functor
import Log
import Text.StringTemplates.Templates
import qualified Data.Foldable as F

import ActionQueue.AccessNewAccount (newAccessNewAccountLink)
import Administration.AddPaymentPlan
import BrandedDomain.BrandedDomain
import Company.CompanyID
import Company.Model
import Crypto.RNG
import DB
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model
import Happstack.Fields
import InputValidation
import Kontra
import KontraPrelude
import Log.Identifier
import Mails.SendMail
import MinutesTime
import Payments.Model
import ThirdPartyStats.Core
import User.Email
import User.History.Model
import User.Model
import User.UserView
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils

handleAccountSetupFromSign :: (Kontrakcja m, DocumentMonad m) => SignatoryLink -> m User
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
  activateduser <- handleActivate (Just $ firstname) (Just $ lastname) (user,company) BySigning
  _ <- dbUpdate $ SaveDocumentForUser activateduser (signatorylinkid signatorylink)
  return activateduser

handleActivate :: Kontrakcja m => Maybe String -> Maybe String -> (User,Company) -> SignupMethod -> m User
handleActivate mfstname msndname (actvuser,company) signupmethod = do
  logInfo "Attempting to activate account for user" $ object [
      identifier_ $ userid actvuser
    , "email" .= getEmail actvuser
    ]
  -- Don't remove - else people will be able to hijack accounts
  when (isJust $ userhasacceptedtermsofservice actvuser) $ do
    internalError

  whenM (not <$> isFieldSet "tos") $ do
    logInfo_ "Can't activate account, 'tos' parameter is missing"
    internalError

  switchLang (getLang actvuser)
  ctx <- getContext
  phone <-  fromMaybe (getMobile actvuser) <$> getField "phone"
  companyname <- fromMaybe (getCompanyName company) <$> getField "company"
  position <- fromMaybe "" <$> getField "position"

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

  dbUpdate $ ConnectSignatoriesToUser (Email $ getEmail actvuser) (userid actvuser) (14 `daysBefore` ctxtime ctx)

  mpassword <- getOptionalField asValidPassword "password"
  F.forM_ mpassword $ \password -> do
    passwordhash <- createPassword password
    void . dbUpdate $ SetUserPassword (userid actvuser) passwordhash

  when (signupmethod == BySigning) $ do
    scheduleNewAccountMail ctx actvuser
  tosuser <- guardJustM $ dbQuery $ GetUserByID (userid actvuser)

  logInfo "Attempt successful, user logged in" $ object [
      identifier_ $ userid actvuser
    , "email" .= getEmail actvuser
    ]

  whenM (not <$> isFieldSet "stoplogin") $ do
    logUserToContext $ Just tosuser
  whenM (isFieldSet "callme") $ do
    phoneMeRequest (Just tosuser) phone
  whenM (isFieldSet "promo") $ do
    addCompanyPlanManual (companyid company) TrialPricePlan ActiveStatus

  return tosuser

scheduleNewAccountMail :: (TemplatesMonad m, CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m) => Context -> User -> m ()
scheduleNewAccountMail ctx user = do
  link <- newAccessNewAccountLink $ userid user
  mail <- accessNewAccountMail ctx user link
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }

createUser :: (CryptoRNG m, KontraMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Email -> (String, String) -> (CompanyID,Bool) -> Lang -> m (Maybe User)
createUser email names companyandrole lang = do
  ctx <- getContext
  passwd <- createPassword =<< randomPassword
  muser <- dbUpdate $ AddUser names (unEmail email) (Just passwd) companyandrole lang (bdid $ ctxbrandeddomain ctx)
  case muser of
    Just user -> do
      _ <- dbUpdate $ LogHistoryAccountCreated (userid user) (ctxipnumber ctx) (ctxtime ctx) email (userid <$> getContextUser ctx)
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
            to = [MailAddress { fullname = "info@scrive.com", email = "info@scrive.com" }]
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
