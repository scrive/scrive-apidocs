module User.UserControl(
    handleAccountGet
  , handleRequestPhoneCall
  , sendChangeToExistingEmailInternalWarningMail
  , handleGetChangeEmail
  , handlePostChangeEmail
  , getUserInfoUpdate
  , getCompanyInfoUpdate
  , handleUsageStatsJSONForUserDays
  , handleUsageStatsJSONForUserMonths
  , isUserDeletable
  , sendNewUserMail
  , createNewUserByAdmin
  , handleAcceptTOSGet
  , handleAcceptTOSPost
  , handleAccountSetupGet
  , handleAccountSetupGetWithMethod
  , handleAccountSetupPostWithMethod
  , handleAccountSetupPost
  , handleAccessNewAccountGet
  , handleAccessNewAccountPost
  , handlePasswordReminderGet
  , handlePasswordReminderPost
  , handleContactUs
  , getDaysStats   -- Exported for admin section
  , getMonthsStats -- Exported for admin section
) where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Text.JSON (JSValue(..))
import Text.JSON.Gen

import ActionQueue.Core
import ActionQueue.EmailChangeRequest
import ActionQueue.PasswordReminder
import ActionQueue.AccessNewAccount
import ActionQueue.UserAccountRequest
import AppView
import DB hiding (update, query)
import Company.Model
import Company.CompanyUI
import InputValidation
import Kontra
import KontraLink
import MagicHash (MagicHash)
import Mails.SendMail
import MinutesTime hiding (toClockTime)
import Happstack.Fields
import Redirect
import Text.StringTemplates.Templates
import User.Model
import User.UserView
import Util.FlashUtil
import Util.MonadUtils
import Util.HasSomeUserInfo
import qualified Log
import User.Action
import User.Utils
import User.History.Model
import ListUtil
import qualified Text.StringTemplates.Fields as F
import Routing
import BrandedDomains
import Analytics.Include


handleAccountGet :: Kontrakcja m => m (Either KontraLink Response)
handleAccountGet = checkUserTOSGet $ do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Just user -> renderFromBody kontrakcja =<< showAccount user
         Nothing -> sendRedirect $ LinkLogin (ctxlang ctx) NotLogged

-- please treat this function like a public query form, it's not secure
handleRequestPhoneCall :: Kontrakcja m => m KontraLink
handleRequestPhoneCall = do
  Context{ctxmaybeuser} <- getContext
  memail <- getOptionalField asValidEmail "email"
  mphone <-  getOptionalField asValidPhone "phone"
  muser <- maybe (return Nothing) (dbQuery . GetUserByEmail . Email) memail
  case (muser, mphone) of
    (Just user, Just phone) -> do
      --only set the phone number if they're actually logged in
      -- it is possible to request a phone call from the sign view without being logged in!
      -- this function could be called by anyone!
      when (isJust ctxmaybeuser && fmap userid ctxmaybeuser == Just (userid user)) $ do
        _ <- dbUpdate $ SetUserInfo (userid user) $ (userinfo user){ userphone = phone }
        return ()
      phoneMeRequest muser phone
    (_, Just phone) -> phoneMeRequest Nothing phone
    _ -> return ()
  return $ LinkDesignView

sendChangeToExistingEmailInternalWarningMail :: Kontrakcja m => User -> Email -> m ()
sendChangeToExistingEmailInternalWarningMail user newemail = do
  ctx <- getContext
  let securitymsg =
        "User " ++ getEmail user ++ " (" ++ show (userid user) ++ ")"
        ++ " has requested that their email be changed to " ++ unEmail newemail
        ++ " but this email is already used by another account."
      content =
        securitymsg
        ++ "Maybe they're trying to attempt to merge accounts and need help, "
        ++ "or maybe they're a hacker trying to figure out who is and isn't a user."
  Log.security securitymsg
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
      to = [MailAddress { fullname = "info@skrivapa.se", email = "info@skrivapa.se" }]
    , title = "Request to Change Email to Existing Account"
    , content = content
    }

handleGetChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m (Either KontraLink (Either KontraLink String))
handleGetChangeEmail uid hash = withUserGet $ do
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  case mnewemail of
    Just newemail -> Right <$> pageDoYouWantToChangeEmail newemail
    Nothing -> (addFlashM flashMessageProblemWithEmailChange) >> (return  $ Left LinkAccount)

handlePostChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m KontraLink
handlePostChangeEmail uid hash = withUserPost $ do
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  Context{ctxmaybeuser = Just user, ctxipnumber, ctxtime} <- getContext
  mpassword <- getOptionalField asDirtyPassword "password"
  case mpassword of
    Nothing -> return ()
    Just password | verifyPassword (userpassword user) password -> do
      changed <- maybe (return False)
                      (dbUpdate . SetUserEmail (userid user))
                      mnewemail
      if changed
        then do
            _ <- dbUpdate $ LogHistoryDetailsChanged (userid user) ctxipnumber ctxtime
                                                     [("email", unEmail $ useremail $ userinfo user, unEmail $ fromJust mnewemail)]
                                                     (Just $ userid user)
            addFlashM $ flashMessageYourEmailHasChanged
        else addFlashM $ flashMessageProblemWithEmailChange
      _ <- dbUpdate $ DeleteAction emailChangeRequest uid
      return ()
    Just _password -> do
      addFlashM $ flashMessageProblemWithPassword
  return $ LinkAccount

getUserInfoUpdate :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoUpdate  = do
    -- a lot doesn't have validation rules defined, but i put in what we do have
    mfstname          <- getValidField asValidName "fstname"
    msndname          <- getValidField asValidName "sndname"
    mpersonalnumber   <- getField "personalnumber"
    mphone            <- getField "phone"
    mcompanyposition  <- getValidField asValidPosition "companyposition"
    return $ \ui ->
        ui {
            userfstname = fromMaybe (userfstname ui) mfstname
          , usersndname = fromMaybe (usersndname ui) msndname
          , userpersonalnumber = fromMaybe (userpersonalnumber ui) mpersonalnumber
          , usercompanyposition = fromMaybe (usercompanyposition ui) mcompanyposition
          , userphone  = fromMaybe (userphone ui) mphone
        }
    where
        getValidField = getDefaultedField ""

getCompanyInfoUpdate :: Kontrakcja m => m (CompanyInfo -> CompanyInfo)
getCompanyInfoUpdate = do
    -- a lot doesn't have validation rules defined, but i put in what we do have
  mcompanyname <- getValidField asValidCompanyName "companyname"
  mcompanynumber <- getValidField asValidCompanyNumber "companynumber"
  mcompanyaddress <- getValidField asValidAddress "companyaddress"
  mcompanyzip <- getField "companyzip"
  mcompanycity <- getField "companycity"
  mcompanycountry <- getField "companycountry"
  mcompanysmsoriginator <- getField "companysmsoriginator"
  return $ \ci ->
      ci {
         companyname = fromMaybe (companyname ci) mcompanyname
      ,  companynumber = fromMaybe (companynumber ci) mcompanynumber
      ,  companyaddress = fromMaybe (companyaddress ci) mcompanyaddress
      ,  companyzip = fromMaybe (companyzip ci) mcompanyzip
      ,  companycity = fromMaybe (companycity ci) mcompanycity
      ,  companycountry = fromMaybe (companycountry ci) mcompanycountry
      ,  companysmsoriginator = fromMaybe (companysmsoriginator ci) mcompanysmsoriginator
      }
  where
    getValidField = getDefaultedField ""


handleUsageStatsJSONForUserDays :: Kontrakcja m => m JSValue
handleUsageStatsJSONForUserDays = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getDaysStats (Right $ usercompany user)
    else getDaysStats (Left $ userid user)


handleUsageStatsJSONForUserMonths :: Kontrakcja m => m JSValue
handleUsageStatsJSONForUserMonths = do
  user  <- guardJustM $ ctxmaybeuser <$> getContext
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getMonthsStats (Right $ usercompany user)
    else getMonthsStats (Left $ userid user)

getDaysStats :: Kontrakcja m => Either UserID CompanyID -> m JSValue
getDaysStats euc = do
  ctxtime <- ctxtime <$> getContext
  let timespans = [ (formatMinutesTime "%Y-%m-%d" t, formatMinutesTime "%Y-%m-%d" (daysAfter 1 t))
                     | daysBack <- [0 .. 30]
                     , t <- [daysBefore daysBack ctxtime]
                    ]
  case euc of
    Left uid -> do
      stats <- dbQuery $ GetUsageStats (Left uid) timespans
      return $ singlePageListToJSON $ userStatsToJSON (formatMinutesTime "%Y-%m-%d") stats
    Right cid -> do
      totalS <- renderTemplate_ "statsOrgTotal"
      stats <- dbQuery $ GetUsageStats (Right cid) timespans
      return $ singlePageListToJSON $ companyStatsToJSON (formatMinutesTime "%Y-%m-%d") totalS stats

getMonthsStats :: Kontrakcja m => Either UserID CompanyID -> m JSValue
getMonthsStats euc = do
  ctxtime <- ctxtime <$> getContext
  let timespans = [ (formatMinutesTime "%Y-%m-01" t, formatMinutesTime "%Y-%m-01" (monthsBefore (-1) t))
                     | monthsBack <- [0 .. 6]
                     , t <- [monthsBefore monthsBack ctxtime]
                    ]
  case euc of
    Left uid -> do
      stats <- dbQuery $ GetUsageStats (Left uid) timespans
      return $ singlePageListToJSON $ userStatsToJSON (formatMinutesTime "%Y-%m") stats
    Right cid -> do
      totalS <- renderTemplate_ "statsOrgTotal"
      stats <- dbQuery $ GetUsageStats (Right cid) timespans
      return $ singlePageListToJSON $ companyStatsToJSON (formatMinutesTime "%Y-%m") totalS stats

{- |
    Checks for live documents owned by the user.
-}
isUserDeletable :: Kontrakcja m => User -> m Bool
isUserDeletable user = do
  dbQuery $ IsUserDeletable (userid user)

--there must be a better way than all of these weird user create functions
-- TODO clean up

sendNewUserMail :: Kontrakcja m => User -> m ()
sendNewUserMail user = do
  ctx <- getContext
  al <- newUserAccountRequestLink (ctxlang ctx) (userid user) AccountRequest
  mail <- newUserMail ctx (getEmail user) (getSmartName user) al
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = getSmartName user, email = getEmail user }]}
  return ()

createNewUserByAdmin :: Kontrakcja m => String -> (String, String) -> Maybe String -> (CompanyID, Bool) -> Lang -> m (Maybe User)
createNewUserByAdmin email names custommessage companyandrole lang = do
    ctx <- getContext
    muser <- createUser (Email email) names companyandrole lang
    case muser of
         Just user -> do
             let fullname = composeFullName names
             chpwdlink <- newUserAccountRequestLink (ctxlang ctx) (userid user) ByAdmin
             mail <- mailNewAccountCreatedByAdmin ctx (getLang user) fullname email chpwdlink custommessage
             scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = fullname, email = email }]}
             return muser
         Nothing -> return muser

handleAcceptTOSGet :: Kontrakcja m => m (Either KontraLink String)
handleAcceptTOSGet = withUserGet $ pageAcceptTOS

handleAcceptTOSPost :: Kontrakcja m => m ()
handleAcceptTOSPost = do
  Context{ctxmaybeuser,ctxtime, ctxipnumber} <- getContext
  userid <- guardJustM $ return $ userid <$>ctxmaybeuser
  tos <- getDefaultedField False asValidCheckBox "tos"
  when (Just True == tos) $ do
      _ <- dbUpdate $ AcceptTermsOfService userid ctxtime

      _ <- dbUpdate $ LogHistoryTOSAccept userid ctxipnumber ctxtime (Just userid)
      addFlashM flashMessageUserDetailsSaved
  return ()

handleAccountSetupGet :: Kontrakcja m => UserID -> MagicHash -> m (Either KontraLink Response)
handleAccountSetupGet uid token = handleAccountSetupGetWithMethod uid token AccountRequest

handleAccountSetupGetWithMethod :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m (Either KontraLink Response)
handleAccountSetupGetWithMethod uid token sm = do
  ctx <- getContext
  muser <- getUserAccountRequestUser uid token
  case (muser, userhasacceptedtermsofservice =<< muser, ctxmaybeuser ctx) of
    (Just user, Nothing,_) -> do
      company <-  getCompanyForUser user
      companyui <- dbQuery $ GetCompanyUI (usercompany user)
      mbd <- return $ currentBrandedDomain ctx
      let background =
              companycustombackgroundcolour companyui `mplus` (bdbackgroundcolorexternal <$>  mbd) `mplus` (bdbackgroundcolour <$> mbd)
      Right <$> (simpleHtmlResponse =<< (renderTemplateAsPage ctx "accountSetupPage" False $ do
                                            F.value "fstname" $ getFirstName user
                                            F.value "sndname" $ getLastName user
                                            F.value "userid"  $ show uid
                                            F.value "company" $ companyname $ companyinfo $ company
                                            F.value "companyAdmin" $ useriscompanyadmin user
                                            F.value "signupmethod" $ show sm
                                            brandingFields mbd (Just companyui)
                                            F.value "backgroundcolour" background
                                            ))
    (Just _user, Just _, Just _)  -> return $ Left $ LinkDesignView
    (Just _user, Just _, Nothing) -> return $ Left $ LinkLogin (ctxlang ctx) NotLogged
    _ -> return $ Left $ LinkSignup $ ctxlang ctx

handleAccountSetupPostWithMethod :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m JSValue
handleAccountSetupPostWithMethod uid token sm = do
  user <- guardJustM $ getUserAccountRequestUser uid token
  company <-  getCompanyForUser user
  if isJust $ userhasacceptedtermsofservice user
    then runJSONGenT $ do
           value "ok" False
           value "error" ("already_active" :: String)
    else do
      mfstname <- getOptionalField asValidName "fstname"
      msndname <- getOptionalField asValidName "sndname"
      mactivateduser <- handleActivate mfstname msndname (user,company) sm
      case mactivateduser of
        Nothing -> runJSONGenT $ do
                    value "ok" False
                    value "error" ("reload" :: String)
        Just _ -> do
          _ <- dbUpdate $ DeleteAction userAccountRequest uid
          ctx <- getContext
          _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) { lang = ctxlang ctx }
          addFlashM flashMessageUserActivated
          link <- getHomeOrDesignViewLink
          runJSONGenT $ do
            value "ok" True
            value "location" $ show link
            value "userid" $ show uid

handleAccountSetupPost :: Kontrakcja m => UserID -> MagicHash -> m JSValue
handleAccountSetupPost uid token = handleAccountSetupPostWithMethod uid token AccountRequest

{- |
    This is where we get to when the user clicks the link in their new-account
    email.  This will show them a page where they need to set their password.
-}
handleAccessNewAccountGet :: Kontrakcja m => UserID -> MagicHash -> m (Either KontraLink (Either Response ThinPage))
handleAccessNewAccountGet uid token = do
  muser <- getAccessNewAccountUser uid token
  case muser of
    Just user -> do
      switchLang (getLang user)
      let changePassLink = show $ LinkAccessNewAccount uid token
      ctx <- getContext
      case (currentBrandedDomain ctx) of
        Just bd -> do
          ad <- getAnalyticsData
          content <- renderTemplate "accessNewAccountPageWithBranding" $ do
                        F.value "linkchangepassword" $ changePassLink
                        F.value "logolink" $ bdlogolink bd
                        F.value "background" $ bdbackgroundcolorexternal $ bd
                        F.value "buttoncolorclass" $ bdbuttonclass $ bd

                        standardPageFields ctx kontrakcja ad
          Right . Left <$> simpleHtmlResonseClrFlash content
        Nothing -> do
          content <- renderTemplate "accessNewAccountPage" $ do
                        F.value "linkchangepassword" $ changePassLink
          return $ Right $ Right $ ThinPage content
    Nothing -> do
      addFlashM flashMessageAccessNewAccountLinkNotValid
      return $ Left LinkLoginDirect

-- TODO: Too much code duplication around new account access and password reminders
handleAccessNewAccountPost :: Kontrakcja m => UserID -> MagicHash -> m JSValue
handleAccessNewAccountPost uid token = do
  muser <- getAccessNewAccountUser uid token
  case muser of
    Just user -> do
      switchLang (getLang user)
      Context{ctxtime, ctxipnumber, ctxmaybeuser} <- getContext
      password <- guardJustM $ getField "password"
      _ <- dbUpdate $ DeleteAction accessNewAccount uid
      passwordhash <- createPassword password
      _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
      _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) ctxipnumber ctxtime (userid <$> ctxmaybeuser)
      addFlashM flashMessageUserPasswordChanged

      logUserToContext $ Just user
      runJSONGenT $ do
          value "logged" True
          value "location" $ show LinkArchive
    Nothing -> runJSONGenT $ value "logged" False

{- |
    This is where we get to when the user clicks the link in their password reminder
    email.  This'll show them the usual landing page, but with option to changing their password.
-}
handlePasswordReminderGet :: Kontrakcja m => UserID -> MagicHash -> m (Either KontraLink (Either Response ThinPage))
handlePasswordReminderGet uid token = do
  muser <- getPasswordReminderUser uid token
  case muser of
    Just user -> do
      switchLang (getLang user)
      let changePassLink = show $ LinkPasswordReminder uid token
      ctx <- getContext
      case (currentBrandedDomain ctx) of
        Just bd -> do
          ad <- getAnalyticsData
          content <- renderTemplate "changePasswordPageWithBranding" $ do
                        F.value "linkchangepassword" $ changePassLink
                        F.value "logolink" $ bdlogolink bd
                        F.value "background" $ bdbackgroundcolorexternal $ bd
                        F.value "buttoncolorclass" $ bdbuttonclass $ bd

                        standardPageFields ctx kontrakcja ad
          Right . Left <$> simpleHtmlResonseClrFlash content
        Nothing -> do
          content <- renderTemplate "changePasswordPage" $ do
                        F.value "linkchangepassword" $ changePassLink
          return $ Right $ Right $ ThinPage content
    Nothing -> do
      addFlashM flashMessagePasswordChangeLinkNotValid
      return $ Left LinkLoginDirect


handlePasswordReminderPost :: Kontrakcja m => UserID -> MagicHash -> m JSValue
handlePasswordReminderPost uid token = do
  muser <- getPasswordReminderUser uid token
  case muser of
    Just user -> do
      switchLang (getLang user)
      Context{ctxtime, ctxipnumber, ctxmaybeuser} <- getContext
      password <- guardJustM $ getField "password"
      _ <- dbUpdate $ DeleteAction passwordReminder uid
      passwordhash <- createPassword password
      _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
      _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) ctxipnumber ctxtime (userid <$> ctxmaybeuser)
      addFlashM flashMessageUserPasswordChanged

      logUserToContext $ Just user
      runJSONGenT $ do
          value "logged" True
          value "location" $ show LinkDesignView
    Nothing -> runJSONGenT $ value "logged" False

-- please treat this function like a public query form, it's not secure
handleContactUs :: Kontrakcja m => m KontraLink
handleContactUs = do
  Context{..} <- getContext
  ctx <- getContext
  fname   <- getField' "firstname"
  lname   <- getField' "lastname"
  email   <- getField' "email"
  message <- getField' "message"
  plan    <- getField' "plan"

  let uid = maybe "user not logged in" ((++) "user with id " . show . userid) ctxmaybeuser
      mbd = currentBrandedDomain ctx
      domainInfo = case mbd of
                     Nothing -> ""
                     Just bd -> " (from domain " ++ bdurl bd ++ " )"
      content = "<p>Hi there!</p>" ++
                "<p>Someone requested information from the payments form" ++
                domainInfo ++
                ".</p>" ++
                "<p>Name: " ++ fname ++ " " ++ lname ++ "</p>" ++
                "<p>Email: " ++ email ++ "</p>" ++
                "<p>Message: \n" ++ message ++ "</p>" ++
                "<p>Looking at plan: " ++ plan ++ "</p>" ++
                "<p>" ++ uid ++ "</p>" ++
                "<p>Have a good one!</p>"
      contactEmail = "info@scrive.com"
      mpartnerEmail = bdcontactemail <$> mbd
      sendEmailTo emailAddress = scheduleEmailSendout ctxmailsconfig $ emptyMail {
                                   to = [MailAddress { fullname = emailAddress, email = emailAddress }]
                                 , title = "Contact request (" ++ plan ++ ")"
                                 , content = content
                                 }

  sendEmailTo contactEmail
  case mpartnerEmail of
    Nothing -> return ()
    Just partnerEmail -> sendEmailTo partnerEmail

  return $ LoopBack
