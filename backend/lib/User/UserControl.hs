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
  , handlePasswordReminderGet
  , handlePasswordReminderPost
  , handleContactSales
  , getDaysStats   -- Exported for admin section
  , getMonthsStats -- Exported for admin section
) where

import Data.Time.Calendar
import Data.Time.Clock
import Log
import Text.JSON (JSValue(..))
import Text.StringTemplates.Templates
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import BrandedDomain.BrandedDomain
import Company.CompanyUI.Model
import Company.Model
import DB hiding (query, update)
import Happstack.Fields
import InputValidation
import InternalResponse
import Kontra
import KontraLink
import Log.Identifier
import MagicHash (MagicHash)
import Mails.SendMail
import MinutesTime
import User.Action
import User.Email
import User.EmailChangeRequest
import User.History.Model
import User.JSON
import User.Model
import User.PasswordReminder
import User.UserAccountRequest
import User.UserView
import User.Utils
import Util.HasSomeUserInfo
import Util.MonadUtils

handleAccountGet :: Kontrakcja m => m (InternalKontraResponse)
handleAccountGet = withUserTOS $ \(user, _) -> do
  ctx <- getContext
  pb <- renderTemplate "showAccount" $ do
    F.value "companyAdmin" $ useriscompanyadmin user
    F.value "apiLogEnabled" $ get ctxisapilogenabled ctx
    entryPointFields ctx
  internalResponse <$> renderFromBodyWithFields pb (F.value "account" True)

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
      logPair_ user
    , "new_email" .= unEmail newemail
    ]
  scheduleEmailSendout $ emptyMail {
      to = [MailAddress { fullname = "info@scrive.com", email = "info@scrive.com" }]
    , title = "Request to Change Email to Existing Account"
    , content = content
    }

handleGetChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m InternalKontraResponse
handleGetChangeEmail uid hash = withUser $ \_ -> do
  ctx <- getContext
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  case mnewemail of
    Just newemail ->
      internalResponse <$> pageDoYouWantToChangeEmail ctx newemail
    Nothing -> do
      flashmessage <- flashMessageProblemWithEmailChange
      return $ internalResponseWithFlash flashmessage LinkAccount

handlePostChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m InternalKontraResponse
handlePostChangeEmail uid hash =  withUser $ \user -> do
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  ctx <- getContext
  let (ipnumber, time) = (get ctxipnumber ctx, get ctxtime ctx)
  mpassword <- getOptionalField asDirtyPassword "password"
  case mpassword of
    Nothing -> return $ internalResponse $ LinkAccount
    -- No need to check TOTP here, withUser gives us logged in user
    Just password | maybeVerifyPassword (userpassword user) password -> do
      changed <- maybe (return False)
                      (dbUpdate . SetUserEmail (userid user))
                      mnewemail
      flashmessage <- if changed
        then do
            _ <- dbUpdate $ LogHistoryDetailsChanged (userid user) ipnumber time
                                                     [("email", unEmail $ useremail $ userinfo user, unEmail $ fromJust mnewemail)]
                                                     (Just $ userid user)
            flashMessageYourEmailHasChanged
        else
            flashMessageProblemWithEmailChange
      _ <- dbUpdate $ DeleteEmailChangeRequest uid
      return $ internalResponseWithFlash flashmessage $ LinkAccount
    Just _password -> do
      flashmessage <-  flashMessageProblemWithPassword
      return $ internalResponseWithFlash flashmessage $ LinkAccount

getUserInfoUpdate :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoUpdate  = do
    -- a lot doesn't have validation rules defined, but i put in what we do have
    mfstname          <- getOptionalField asValidName "fstname"
    msndname          <- getOptionalField asValidName "sndname"
    mpersonalnumber   <- getField "personalnumber"
    mphone            <- getField "phone"
    mcompanyposition  <- getOptionalField asValidPosition "companyposition"
    return $ \ui ->
        ui {
            userfstname         = fromMaybe (userfstname         ui) mfstname
          , usersndname         = fromMaybe (usersndname         ui) msndname
          , userpersonalnumber  = fromMaybe (userpersonalnumber  ui) mpersonalnumber
          , usercompanyposition = fromMaybe (usercompanyposition ui) mcompanyposition
          , userphone           = fromMaybe (userphone           ui) mphone
        }

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
  user <- guardJustM $ get ctxmaybeuser <$> getContext
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getDaysStats (Right $ usercompany user)
    else getDaysStats (Left $ userid user)


handleUsageStatsJSONForUserMonths :: Kontrakcja m => m JSValue
handleUsageStatsJSONForUserMonths = do
  user  <- guardJustM $ get ctxmaybeuser <$> getContext
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getMonthsStats (Right $ usercompany user)
    else getMonthsStats (Left $ userid user)

getDaysStats :: Kontrakcja m => Either UserID CompanyID -> m JSValue
getDaysStats = getStats PartitionByDay (idays 30)

getMonthsStats :: Kontrakcja m => Either UserID CompanyID -> m JSValue
getMonthsStats = getStats PartitionByMonth (imonths 6)

getStats :: Kontrakcja m => StatsPartition -> Interval -> Either UserID CompanyID -> m JSValue
getStats statsPartition interval eid = do
    -- @note: This is a hack around the fact that we don't yet have enough data
    -- in `chargeable_items` table to use queries for longer periods.  The code
    -- can be reset 6 months after 20170601 (if a 6 month interval is indeed
    -- used).
    now <- currentTime
    let timeDiffSinceFstJune =
            diffUTCTime now (UTCTime (fromGregorian 2017 6 1) 0)
        intervalDaysSinceFstJune =
            idays . floor $ timeDiffSinceFstJune / nominalDay
        queryConstructor =
            if intervalDaysSinceFstJune >= interval
            then GetUsageStatsNew
            else GetUsageStatsOld
    case eid of
      Left uid -> do
        stats <- dbQuery $ queryConstructor (Left uid) statsPartition interval
        return $ userStatsToJSON timeFormat stats
      Right cid -> do
        totalS <- renderTemplate_ "statsOrgTotal"
        stats <- dbQuery $ queryConstructor (Right cid) statsPartition interval
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
  mail <- newUserMail ctx (getEmail user) al
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
             mail <- mailNewAccountCreatedByAdmin ctx (getLang user) email chpwdlink
             scheduleEmailSendout $ mail { to = [MailAddress { fullname = fullname, email = email }]}
             return muser
         Nothing -> return muser

handleAcceptTOSGet :: Kontrakcja m => m InternalKontraResponse
handleAcceptTOSGet = withUser $ \_ -> internalResponse <$> (pageAcceptTOS =<< getContext)

handleAcceptTOSPost :: Kontrakcja m => m ()
handleAcceptTOSPost = do
  ctx <- getContext
  let (maybeuser, time, ipnumber) = ( get ctxmaybeuser ctx, get ctxtime ctx
                                    , get ctxipnumber ctx )
  userid <- guardJustM $ return $ userid <$> maybeuser
  tos <- getDefaultedField False asValidCheckBox "tos"
  case tos of
    Just True -> do
      _ <- dbUpdate $ AcceptTermsOfService userid time
      _ <- dbUpdate $ LogHistoryTOSAccept  userid ipnumber time (Just userid)
      return ()
    _ -> internalError


handleAccountSetupGet :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m InternalKontraResponse
handleAccountSetupGet uid token sm = do
  ctx <- getContext
  muser <- getUserAccountRequestUser uid token
  case (muser, userhasacceptedtermsofservice =<< muser) of
    (Just user, Nothing) -> do
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
        F.value "companyPosition" $ usercompanyposition $ userinfo user
        F.value "mobile" $ getMobile user
        F.value "signupmethod" $ show sm
      internalResponse <$> (simpleHtmlResponse content)
    (Just _user, Just _)  -> return $ internalResponse $ LinkDesignView
    _ -> do
      flashmessage <- case sm of
        CompanyInvitation -> flashMessageUserAccountRequestExpiredCompany
        _                 -> flashMessageUserAccountRequestExpired
      return . internalResponseWithFlash flashmessage . LinkLogin $ get ctxlang ctx

handleAccountSetupPost :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m JSValue
handleAccountSetupPost uid token sm = do
  user <- guardJustM $ getUserAccountRequestUser uid token
  company <-  getCompanyForUser user
  if isJust $ userhasacceptedtermsofservice user
    then do
      J.runJSONGenT $ do
        J.value "ok" False
        J.value "error" ("already_active" :: String)
    else do
      mfstname <- getOptionalField asValidName "fstname"
      msndname <- getOptionalField asValidName "sndname"
      _ <- handleActivate mfstname msndname (user,company) sm
      _ <- dbUpdate $ DeleteUserAccountRequest uid
      ctx <- getContext
      _ <- dbUpdate $ SetUserSettings (userid user) $ (usersettings user) { lang = get ctxlang ctx }
      link <- getHomeOrDesignViewLink
      J.runJSONGenT $ do
        J.value "ok" True
        J.value "location" $ show link
        J.value "userid" $ show uid

{- |
    This is where we get to when the user clicks the link in their password reminder
    email.  This'll show them the usual landing page, but with option to changing their password.
-}

handlePasswordReminderGet :: Kontrakcja m => UserID -> MagicHash -> m InternalKontraResponse
handlePasswordReminderGet uid token = do
  muser <- getPasswordReminderUser uid token
  case muser of
    Just user -> do
      switchLang (getLang user)
      ctx <- getContext
      ad <- getAnalyticsData
      content <- renderTemplate "changePasswordPageWithBranding" $ do
        F.value "linkchangepassword" $ show $ LinkPasswordReminder uid token
        standardPageFields ctx Nothing ad
      internalResponse <$> (simpleHtmlResponse content)
    Nothing -> do
      ctx <- getContext
      flashmessage <- flashMessagePasswordChangeLinkNotValid
      return $ internalResponseWithFlash flashmessage $ LinkLoginDirect (get ctxlang ctx)


handlePasswordReminderPost :: Kontrakcja m => UserID -> MagicHash -> m JSValue
handlePasswordReminderPost uid token = do
  muser <- getPasswordReminderUser uid token
  case muser of
    Just user | not (useraccountsuspended user) -> do
      switchLang (getLang user)
      ctx <- getContext
      let time      = get ctxtime ctx
          ipnumber  = get ctxipnumber ctx
          maybeuser = get ctxmaybeuser ctx
      password <- guardJustM $ getField "password"
      _ <- dbUpdate $ DeletePasswordReminder uid
      passwordhash <- createPassword password
      _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
      _ <- dbUpdate $ LogHistoryPasswordSetup (userid user) ipnumber time
           (userid <$> maybeuser)
      logUserToContext $ Just user
      J.runJSONGenT $ do
          J.value "logged" True
          J.value "location" $ show LinkDesignView
    Just _ -> do
      {- MR: useraccountsuspended must be true here. This is a hack for Hi3G. It will be removed in future -}
      J.runJSONGenT $ J.value "logged" False
    Nothing -> J.runJSONGenT $ J.value "logged" False

-- please treat this function like a public query form, it's not secure
handleContactSales :: Kontrakcja m => m ()
handleContactSales = do
  ctx <- getContext
  fname   <- getField' "fstname"
  lname   <- getField' "sndname"
  email   <- getField' "email"
  message <- getField' "message"
  plan    <- getField' "plan"

  let uid = maybe "user not logged in" ((++) "user with id " . show . userid)
            (get ctxmaybeuser ctx)
      domainInfo = " (from domain " ++ (get (bdUrl . ctxbrandeddomain) ctx) ++ " )"
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
      salesEmail = "leads@scrive.com"
      sendEmailTo emailAddress = scheduleEmailSendout $ emptyMail {
                                   to = [MailAddress { fullname = emailAddress, email = emailAddress }]
                                 , title = "Contact request (" ++ plan ++ ")"
                                 , content = content
                                 }
  sendEmailTo contactEmail
  sendEmailTo salesEmail
  return ()
