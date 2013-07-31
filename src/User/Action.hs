module User.Action (
    handleAccountSetupFromSign
  , handleActivate
  , createUser
  , createUser'
  , phoneMeRequest
  , checkPasswordsMatch
  ) where

import Control.Monad
import Data.Functor
import Data.Maybe

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
import Util.HasSomeCompanyInfo
import qualified Log
import Util.MonadUtils
import Util.Actor
import User.History.Model
import ThirdPartyStats.Core
import Payments.Model
import Administration.AddPaymentPlan
import Crypto.RNG
import MinutesTime
import BrandedDomains
import Company.Model

handleAccountSetupFromSign :: Kontrakcja m => Document -> SignatoryLink -> m (Maybe User)
handleAccountSetupFromSign document signatorylink = do
  ctx <- getContext
  let firstname = getFirstName signatorylink
      lastname = getLastName signatorylink
      cname = getCompanyName signatorylink
      cnumber = getCompanyNumber signatorylink
  email <- guardJustM $ getOptionalField asValidEmail "email"
  muser <- dbQuery $ GetUserByEmail (Email email)
  user <- case muser of
            Just u -> return u
            Nothing -> do
               company <- dbUpdate $ CreateCompany
               _ <- dbUpdate $ SetCompanyInfo (companyid company) $ (companyinfo company) {
                    companyname = cname
                  , companynumber = cnumber
                }
               guardJustM $ createUser (Email email) (firstname, lastname) (companyid company,True) (documentlang document)
  company <- dbQuery $ GetCompanyByUserID (userid user)
  mactivateduser <- handleActivate (Just $ firstname) (Just $ lastname) (user,company) BySigning
  case mactivateduser of
    Just activateduser -> do
      let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx)  (maybesignatory signatorylink)  (getEmail signatorylink) (signatorylinkid signatorylink)
      _ <- dbUpdate $ SaveDocumentForUser (documentid document) activateduser (signatorylinkid signatorylink) actor

      return $ Just activateduser
    Nothing -> return Nothing

handleActivate :: Kontrakcja m => Maybe String -> Maybe String -> (User,Company) -> SignupMethod -> m (Maybe User)
handleActivate mfstname msndname (actvuser,company) signupmethod = do
  Log.debug $ "Attempting to activate account for user " ++ (show $ getEmail actvuser)
  when (isJust $ userhasacceptedtermsofservice actvuser) internalError
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
  case (mtos, mfstname, msndname) of
    (Just tos, Just fstname, Just sndname) -> do
          if tos
            then do
              _ <- dbUpdate $ SetUserInfo (userid actvuser) $ (userinfo actvuser) {
                  userfstname = fstname
                , usersndname = sndname
                , userphone = phone
                , usercompanyposition = position
              }
              _ <- dbUpdate $ SetCompanyInfo (companyid company) $ (companyinfo company) {
                  companyname = companyname
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

              Log.debug $ "Attempting successfull. User " ++ (show $ getEmail actvuser) ++ "is logged in."
              when (not stoplogin) $ do
                logUserToContext $ Just tosuser
              when (callme) $ phoneMeRequest (Just tosuser) phone
              when (promo) $ addCompanyPlanManual (companyid company) TrialTeamPricePlan ActiveStatus
              return $ Just tosuser
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

createUser :: (CryptoRNG m, KontraMonad m, MonadDB m, TemplatesMonad m) => Email -> (String, String) -> (CompanyID,Bool) -> Lang -> m (Maybe User)
createUser email names companyandrole lang = do
  ctx <- getContext
  createUser' ctx email names companyandrole lang

createUser' :: (CryptoRNG m, HasMailContext c, MonadDB m, TemplatesMonad m) => c -> Email -> (String, String) -> (CompanyID,Bool) -> Lang -> m (Maybe User)
createUser' ctx email names companyandrole lang = do
  let mctx = mailContext ctx
  passwd <- createPassword =<< randomPassword
  muser <- dbUpdate $ AddUser names (unEmail email) (Just passwd) companyandrole lang (bdurl <$> mctxcurrentBrandedDomain mctx)
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
    let user = fromJust muser
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
