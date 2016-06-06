module User.Action (
    handleActivate
  , createUser
  ) where

import Control.Conditional (whenM)
import Control.Monad.Catch
import Data.Functor
import Log
import Text.StringTemplates.Templates
import qualified Data.Foldable as F

import Administration.AddPaymentPlan
import BrandedDomain.BrandedDomain
import Company.CompanyID
import Company.Model
import Crypto.RNG
import DB
import Doc.Model
import Happstack.Fields
import InputValidation
import Kontra
import KontraPrelude
import Log.Identifier
import MinutesTime
import Payments.Model
import User.Email
import User.History.Model
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils

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

  tosuser <- guardJustM $ dbQuery $ GetUserByID (userid actvuser)

  logInfo "Attempt successful, user logged in" $ object [
      identifier_ $ userid actvuser
    , "email" .= getEmail actvuser
    ]

  whenM (not <$> isFieldSet "stoplogin") $ do
    logUserToContext $ Just tosuser
  whenM (isFieldSet "promo") $ do
    addCompanyPlanManual (companyid company) TrialPricePlan ActiveStatus

  return tosuser

createUser :: (CryptoRNG m, KontraMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Email -> (String, String) -> (CompanyID,Bool) -> Lang -> SignupMethod -> m (Maybe User)
createUser email names companyandrole lang sm = do
  ctx <- getContext
  passwd <- createPassword =<< randomPassword
  muser <- dbUpdate $ AddUser names (unEmail email) (Just passwd) companyandrole lang (bdid $ ctxbrandeddomain ctx) sm
  case muser of
    Just user -> do
      _ <- dbUpdate $ LogHistoryAccountCreated (userid user) (ctxipnumber ctx) (ctxtime ctx) email (userid <$> getContextUser ctx)
      return muser
    _ -> return muser
