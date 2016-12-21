module User.UserControl(
    handleAccountGet
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
import Happstack.Server hiding (simpleHTTP)
import Log
import Text.JSON (JSValue(..))
import Text.StringTemplates.Templates
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import ActionQueue.AccessNewAccount
import ActionQueue.Core
import ActionQueue.EmailChangeRequest
import ActionQueue.PasswordReminder
import ActionQueue.UserAccountRequest
import Analytics.Include
import AppView
import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Company.Model
import DB hiding (query, update)
import Happstack.Fields
import InputValidation
import Kontra
import KontraLink
import KontraPrelude
import Log.Identifier
import MagicHash (MagicHash)
import Mails.SendMail
import MinutesTime
import Redirect
import User.Action
import User.Email
import User.History.Model
import User.Model
import User.UserView
import User.Utils
import Util.FlashUtil
import Util.HasSomeUserInfo
import Util.MonadUtils

handleAccountGet :: Kontrakcja m => m (Either KontraLink Response)
handleAccountGet = checkUserTOSGet $ do
    ctx <- getContext
    case (ctxmaybeuser ctx) of
         Nothing -> sendRedirect $ LinkLogin (ctxlang ctx) NotLogged
         Just user -> do
           pb <- renderTemplate "showAccount" $ do
                   F.value "companyAdmin" $ useriscompanyadmin user
                   entryPointFields ctx
           renderFromBodyWithFields pb (F.value "account" True)

sendChangeToExistingEmailInternalWarningMail :: Kontrakcja m => User -> Email -> m ()
sendChangeToExistingEmailInternalWarningMail user newemail = do
  let securitymsg =
        "User " ++ getEmail user ++ " (" ++ show (userid user) ++ ")"
        ++ " has requested that their email be changed to " ++ unEmail newemail
        ++ " but this email is already used by another account."
      content =
        securitymsg
        ++ "Maybe they're trying to attempt to merge accounts and need help, "
        ++ "or maybe they're a hacker trying to figure out who is and isn't a user."
  logInfo "User has requested that their email be changed, but new proposed email is already used by another account" $ object [
      identifier_ $ userid user
    , "email" .= getEmail user
    , "new_email" .= unEmail newemail
    ]
  scheduleEmailSendout $ emptyMail {
      to = [MailAddress { fullname = "info@scrive.com", email = "info@scrive.com" }]
    , title = "Request to Change Email to Existing Account"
    , content = content
    }

handleGetChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m (Either KontraLink (Either (FlashMessage, KontraLink) String))
handleGetChangeEmail uid hash = withUserGet $ do
  ctx <- getContext
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  case mnewemail of
    Just newemail -> Right <$> pageDoYouWantToChangeEmail ctx newemail
    Nothing -> return $ Left (flashMessageProblemWithEmailChange, LinkAccount)

handlePostChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m (Either KontraLink (Maybe FlashMessage, KontraLink))
handlePostChangeEmail uid hash = withUserPost $ do
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  Context{ctxmaybeuser = Just user, ctxipnumber, ctxtime} <- getContext
  mpassword <- getOptionalField asDirtyPassword "password"
  mflashmessage <- case mpassword of
    Nothing -> return Nothing
    Just password | verifyPassword (userpassword user) password -> do
      changed <- maybe (return False)
                      (dbUpdate . SetUserEmail (userid user))
                      mnewemail
      flashmessage <- if changed
        then do
            _ <- dbUpdate $ LogHistoryDetailsChanged (userid user) ctxipnumber ctxtime
                                                     [("email", unEmail $ useremail $ userinfo user, unEmail $ $fromJust mnewemail)]
                                                     (Just $ userid user)
            return flashMessageYourEmailHasChanged
        else
            return flashMessageProblemWithEmailChange
      _ <- dbUpdate $ DeleteAction emailChangeRequest uid
      return $ Just flashmessage
    Just _password -> do
      return $ Just flashMessageProblemWithPassword
  return (mflashmessage, LinkAccount)

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
  return $ \ci ->
      ci {
         companyname = fromMaybe (companyname ci) mcompanyname
      ,  companynumber = fromMaybe (companynumber ci) mcompanynumber
      ,  companyaddress = fromMaybe (companyaddress ci) mcompanyaddress
      ,  companyzip = fromMaybe (companyzip ci) mcompanyzip
      ,  companycity = fromMaybe (companycity ci) mcompanycity
      ,  companycountry = fromMaybe (companycountry ci) mcompanycountry
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
getDaysStats = getStats PartitionByDay (idays 30)

getMonthsStats :: Kontrakcja m => Either UserID CompanyID -> m JSValue
getMonthsStats = getStats PartitionByMonth (imonths 6)

getStats :: Kontrakcja m => StatsPartition -> Interval -> Either UserID CompanyID -> m JSValue
getStats statsPartition interval = \case
  Left uid -> do
    stats <- dbQuery $ GetUsageStats (Left uid) statsPartition interval
    return $ userStatsToJSON timeFormat stats
  Right cid -> do
    totalS <- renderTemplate_ "statsOrgTotal"
    stats <- dbQuery $ GetUsageStats (Right cid) statsPartition interval
    return $ companyStatsToJSON timeFormat totalS stats
  where timeFormat :: UTCTime -> String
        timeFormat = case statsPartition of
                       PartitionByDay   -> formatTimeYMD
                       PartitionByMonth -> formatTime' "%Y-%m"

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
  al <- newUserAccountRequestLink (lang $ usersettings user) (userid user) AccountRequest
  mail <- newUserMail ctx (getEmail user) (getSmartName user) al
  scheduleEmailSendout $ mail { to = [MailAddress { fullname = getSmartName user, email = getEmail user }]}
  return ()

createNewUserByAdmin :: Kontrakcja m => String -> (String, String) -> (CompanyID, Bool) -> Lang -> m (Maybe User)
createNewUserByAdmin email names companyandrole lg = do
    ctx <- getContext
    muser <- createUser (Email email) names companyandrole lg ByAdmin
    case muser of
         Just user -> do
             let fullname = composeFullName names
             chpwdlink <- newUserAccountRequestLink (lang $ usersettings user) (userid user) ByAdmin
             mail <- mailNewAccountCreatedByAdmin ctx (getLang user) fullname email chpwdlink
             scheduleEmailSendout $ mail { to = [MailAddress { fullname = fullname, email = email }]}
             return muser
         Nothing -> return muser

handleAcceptTOSGet :: Kontrakcja m => m (Either KontraLink String)
handleAcceptTOSGet = withUserGet $ pageAcceptTOS =<< getContext

handleAcceptTOSPost :: Kontrakcja m => m (Maybe FlashMessage)
handleAcceptTOSPost = do
  Context{ctxmaybeuser,ctxtime, ctxipnumber} <- getContext
  userid <- guardJustM $ return $ userid <$>ctxmaybeuser
  tos <- getDefaultedField False asValidCheckBox "tos"
  case tos of
    Just True -> do
      _ <- dbUpdate $ AcceptTermsOfService userid ctxtime
      _ <- dbUpdate $ LogHistoryTOSAccept userid ctxipnumber ctxtime (Just userid)
      return $ Just flashMessageUserDetailsSaved
    _ ->
      return Nothing

handleAccountSetupGet :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m (Either KontraLink Response)
handleAccountSetupGet uid token sm = do
  ctx <- getContext
  muser <- getUserAccountRequestUser uid token
  case (muser, userhasacceptedtermsofservice =<< muser, ctxmaybeuser ctx) of
    (Just user, Nothing,_) -> do
      company <-  getCompanyForUser user
      companyui <- dbQuery $ GetCompanyUI (usercompany user)
      ad <- getAnalyticsData
      content <- renderTemplate "accountSetupPage" $ do
        standardPageFields ctx (Just companyui) ad
        F.value "fstname" $ getFirstName user
        F.value "sndname" $ getLastName user
        F.value "email"   $ getEmail user
        F.value "userid"  $ show uid
        F.value "company" $ companyname $ companyinfo $ company
        F.value "companyAdmin" $ useriscompanyadmin user
        F.value "signupmethod" $ show sm
      Right <$> simpleHtmlResponse content
    (Just _user, Just _, Just _)  -> return $ Left $ LinkDesignView
    (Just _user, Just _, Nothing) -> return $ Left $ LinkLogin (ctxlang ctx) NotLogged
    _ -> return $ Left $ LinkSignup $ ctxlang ctx

handleAccountSetupPost :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m (Maybe FlashMessage, JSValue)
handleAccountSetupPost uid token sm = do
  user <- guardJustM $ getUserAccountRequestUser uid token
  company <-  getCompanyForUser user
  if isJust $ userhasacceptedtermsofservice user
    then
      jsvalue <- J.runJSONGenT $ do
        J.value "ok" False
        J.value "error" ("already_active" :: String)
      return (Nothing, jsvalue)
    else do
      mfstname <- getOptionalField asValidName "fstname"
      msndname <- getOptionalField asValidName "sndname"
      _ <- handleActivate mfstname msndname (user,company) sm
      _ <- dbUpdate $ DeleteAction userAccountRequest uid
      ctx <- getContext
      _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) { lang = ctxlang ctx }
      link <- getHomeOrDesignViewLink
      jsvalue <- J.runJSONGenT $ do
        J.value "ok" True
        J.value "location" $ show link
        J.value "userid" $ show uid
      return (Just flashMessageUserActivated, jsvalue)

{- |
    This is where we get to when the user clicks the link in their new-account
    email.  This will show them a page where they need to set their password.
-}
handleAccessNewAccountGet :: Kontrakcja m => UserID -> MagicHash -> m (Either (Maybe FlashMessage, KontraLink) Response)
handleAccessNewAccountGet uid token = do
  museraccount <- getAccessNewAccountUser uid token
  case museraccount of
    Just user -> do
      switchLang (getLang user)
      let changePassLink = show $ LinkAccessNewAccount uid token
      ctx <- getContext
      --let bd = ctxbrandeddomain ctx
      ad <- getAnalyticsData
      content <- renderTemplate "accessNewAccountPageWithBranding" $ do
                        F.value "linkchangepassword" $ changePassLink
                        standardPageFields ctx Nothing ad
      Right <$> simpleHtmlResonseClrFlash content
    Nothing -> do
      muser <- dbQuery $ GetUserByID uid
      Left <$> case muser of
        Just user@User{userpassword = Just _} -> do
          -- user has already set up an account, redirect to the archive
          ctx <- getContext
          kontralink <- case ctxmaybeuser ctx of
            Just currentUser | currentUser == user -> return LinkArchive
            Just currentUser -> do
              logInfo "New account email button link clicked by s different logged user" $ object [
                  identifier ("new_" <>) uid
                , identifier ("logged_" <>) $ userid currentUser
                ]
              return LinkArchive
            Nothing -> return $ LinkLogin (ctxlang ctx) NotLogged
          return (Nothing, kontralink)
        _ -> do
          ctx <- getContext
          return (Just flashMessageAccessNewAccountLinkNotValid, LinkLoginDirect (ctxlang ctx))

-- TODO: Too much code duplication around new account access and password reminders
handleAccessNewAccountPost :: Kontrakcja m => UserID -> MagicHash -> m (Maybe FlashMessage, JSValue)
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
      logUserToContext $ Just user
      jsvalue <- J.runJSONGenT $ do
          J.value "logged" True
          J.value "location" $ show LinkArchive
      return (Just flashMessageUserPasswordChanged, jsvalue)
    Nothing ->
      jsvalue <- J.runJSONGenT $ J.value "logged" False
      return (Nothing, jsvalue)


{- |
    This is where we get to when the user clicks the link in their password reminder
    email.  This'll show them the usual landing page, but with option to changing their password.
-}
handlePasswordReminderGet :: Kontrakcja m => UserID -> MagicHash -> m (Either (FlashMessage, KontraLink) Response)
handlePasswordReminderGet uid token = do
  muser <- getPasswordReminderUser uid token
  case muser of
    Just user -> do
      switchLang (getLang user)
      let changePassLink = show $ LinkPasswordReminder uid token
      ctx <- getContext
      ad <- getAnalyticsData
      content <- renderTemplate "changePasswordPageWithBranding" $ do
        F.value "linkchangepassword" $ changePassLink
        standardPageFields ctx Nothing ad
      Right <$> simpleHtmlResonseClrFlash content
    Nothing -> do
      ctx <- getContext
      return $ Left (flashMessagePasswordChangeLinkNotValid, LinkLoginDirect (ctxlang ctx))


handlePasswordReminderPost :: Kontrakcja m => UserID -> MagicHash -> m (Maybe FlashMessage, JSValue)
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
      logUserToContext $ Just user
      jsvalue <- J.runJSONGenT $ do
          J.value "logged" True
          J.value "location" $ show LinkDesignView
      return (flashMessageUserPasswordChanged, jsvalue)
    Nothing ->
      jsvalue <- J.runJSONGenT $ J.value "logged" False
      return (Nothing, jsvalue)

-- please treat this function like a public query form, it's not secure
handleContactUs :: Kontrakcja m => m KontraLink
handleContactUs = do
  ctx <- getContext
  fname   <- getField' "firstname"
  lname   <- getField' "lastname"
  email   <- getField' "email"
  message <- getField' "message"
  plan    <- getField' "plan"

  let uid = maybe "user not logged in" ((++) "user with id " . show . userid) (ctxmaybeuser ctx)
      domainInfo = " (from domain " ++ bdUrl (ctxbrandeddomain ctx) ++ " )"
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
      sendEmailTo emailAddress = scheduleEmailSendout $ emptyMail {
                                   to = [MailAddress { fullname = emailAddress, email = emailAddress }]
                                 , title = "Contact request (" ++ plan ++ ")"
                                 , content = content
                                 }
  sendEmailTo contactEmail
  return $ LoopBack
