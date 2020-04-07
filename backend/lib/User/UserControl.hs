module User.UserControl (
    handleAccountGet
  , sendChangeToExistingEmailInternalWarningMail
  , handleGetChangeEmail
  , handlePostChangeEmail
  , handleUsageStatsJSONForUserDays
  , handleUsageStatsJSONForUserMonths
  , handleUsageStatsJSONForShareableLinks
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
  , UsageStatsFor(..)
  , getUsageStats -- Exported for admin section
  , getShareableLinksStats -- Exported for admin section
) where

import Data.Time.Calendar
import Data.Time.Clock
import Happstack.Server
import Log
import Text.JSON (JSValue(..))
import qualified Data.Text as T
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import DB
import Happstack.Fields
import InputValidation
import InternalResponse
import IPAddress
import Kontra
import KontraLink
import Log.Identifier
import MagicHash (MagicHash)
import Mails.SendMail
import MinutesTime
import PasswordService.Control
import Session.Model
import Templates (renderTextTemplate, renderTextTemplate_)
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
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Util.MonadUtils
import qualified API.V2 as V2

handleAccountGet :: Kontrakcja m => m InternalKontraResponse
handleAccountGet = withUser . withTosCheck $ \user -> do
  ctx <- getContext
  pb  <- renderTextTemplate "showAccount" $ do
    F.value "companyAdmin" $ user ^. #isCompanyAdmin
    F.value "apiLogEnabled" $ ctx ^. #isApiLogEnabled
    entryPointFields ctx
  internalResponse <$> renderFromBodyWithFields pb (F.value "account" True)

sendChangeToExistingEmailInternalWarningMail :: Kontrakcja m => User -> Email -> m ()
sendChangeToExistingEmailInternalWarningMail user newemail = do
  let securitymsg =
        "User "
          <> getEmail user
          <> " ("
          <> showt (user ^. #id)
          <> ")"
          <> " has requested that their email be changed to "
          <> unEmail newemail
          <> " but this email is already used by another account."
      content =
        securitymsg
          <> "Maybe they're trying to attempt to merge accounts and need help, "
          <> "or maybe they're a hacker trying to figure out who is and isn't a user."
  logInfo
      "User has requested that their email be changed, but new proposed email is already used by another account"
    $ object [logPair_ user, "new_email" .= unEmail newemail]
  scheduleEmailSendout $ emptyMail
    { to      = [MailAddress { fullname = "info@scrive.com", email = "info@scrive.com" }]
    , title   = "Request to Change Email to Existing Account"
    , content = content
    }

handleGetChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m InternalKontraResponse
handleGetChangeEmail uid hash = withUser $ \_ -> do
  ctx       <- getContext
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  case mnewemail of
    Just newemail ->
      internalResponse . T.unpack <$> pageDoYouWantToChangeEmail ctx newemail
    Nothing -> do
      flashmessage <- flashMessageProblemWithEmailChange
      return $ internalResponseWithFlash flashmessage LinkAccount

handlePostChangeEmail :: Kontrakcja m => UserID -> MagicHash -> m InternalKontraResponse
handlePostChangeEmail uid hash = withUser $ \user -> do
  mnewemail <- getEmailChangeRequestNewEmail uid hash
  ctx       <- getContext
  let (ipnumber, time) = (ctx ^. #ipAddr, ctx ^. #time)
  mpassword <- getOptionalField asDirtyPassword "password"
  case mpassword of
    Nothing -> return $ internalResponse LinkAccount
    -- No need to check TOTP here, withUser gives us logged in user
    Just password | maybeVerifyPassword (user ^. #password) password -> do
      changed <- maybe (return False) (dbUpdate . SetUserEmail (user ^. #id)) mnewemail
      flashmessage <- if changed
        then do
          void . dbUpdate $ LogHistoryDetailsChanged
            (user ^. #id)
            ipnumber
            time
            [("email", unEmail $ user ^. #info % #email, unEmail $ fromJust mnewemail)]
            (Just $ user ^. #id)
          flashMessageYourEmailHasChanged
        else flashMessageProblemWithEmailChange
      void . dbUpdate $ DeleteEmailChangeRequest uid
      return $ internalResponseWithFlash flashmessage LinkAccount
    Just _password -> do
      flashmessage <- flashMessageProblemWithPassword
      return $ internalResponseWithFlash flashmessage LinkAccount

handleUsageStatsJSONForUserDays :: Kontrakcja m => m Response
handleUsageStatsJSONForUserDays = V2.api $ do
  (user, _)          <- V2.getAPIUserWithAnyPrivileges
  withCompany        <- isFieldSet "withCompany"
  includeZeroRecords <- isFieldSet "includeZeroRecords"
  stats              <-
    getUsageStats PartitionByDay includeZeroRecords
      $ if user ^. #isCompanyAdmin && withCompany
          then UsageStatsForUserGroup $ user ^. #groupID
          else UsageStatsForUser $ user ^. #id
  return $ V2.Ok stats

handleUsageStatsJSONForUserMonths :: Kontrakcja m => m Response
handleUsageStatsJSONForUserMonths = V2.api $ do
  (user, _)          <- V2.getAPIUserWithAnyPrivileges
  withCompany        <- isFieldSet "withCompany"
  includeZeroRecords <- isFieldSet "includeZeroRecords"
  stats              <-
    getUsageStats PartitionByMonth includeZeroRecords
      $ if user ^. #isCompanyAdmin && withCompany
          then UsageStatsForUserGroup $ user ^. #groupID
          else UsageStatsForUser $ user ^. #id
  return $ V2.Ok stats

handleUsageStatsJSONForShareableLinks :: Kontrakcja m => StatsPartition -> m JSValue
handleUsageStatsJSONForShareableLinks statsPartition = do
  user               <- guardJustM $ view #maybeUser <$> getContext
  includeZeroRecords <- isFieldSet "includeZeroRecords"
  getShareableLinksStats statsPartition includeZeroRecords $ if user ^. #isCompanyAdmin
    then UsageStatsForUserGroup $ user ^. #groupID
    else UsageStatsForUser $ user ^. #id


getStatsInterval :: StatsPartition -> Interval
getStatsInterval statsPartition = case statsPartition of
  PartitionByDay   -> idays 30
  PartitionByMonth -> imonths 6

getUsageStats
  :: Kontrakcja m
  => StatsPartition
  -> Bool  -- ^ If set produce reports for days/months with no activity, otherwise omit those.
  -> UsageStatsFor
  -> m JSValue
getUsageStats statsPartition includeZeroRecords forWhom = do
  let interval = getStatsInterval statsPartition
  stats <- dbQuery $ GetUsageStats forWhom statsPartition interval
  now   <- currentTime
  -- We are generating one record for each timestamp. If includeZeroRecords
  -- isn't set, we only use the timestamps we've got data for.
  let timestamps | includeZeroRecords = allTimestampsForPartition statsPartition now
                 | otherwise          = map head . group $ map uusTimeWindowStart stats
  case forWhom of
    UsageStatsForUser _uid ->
      return $ userStatsToJSON (timeFormat statsPartition) timestamps stats
    UsageStatsForUserGroup _ugid -> do
      totalS <- renderTextTemplate_ "statsOrgTotal"
      return $ companyStatsToJSON (timeFormat statsPartition) totalS timestamps stats

getShareableLinksStats
  :: Kontrakcja m
  => StatsPartition
  -> Bool  -- ^ If set produce reports for days/months with no activity, otherwise omit those.
  -> UsageStatsFor
  -> m JSValue
getShareableLinksStats statsPartition includeZeroRecords forWhom = do
  let interval = getStatsInterval statsPartition
  stats <- dbQuery $ GetUsageStatsOnShareableLinks forWhom statsPartition interval
  now   <- currentTime
  -- We are generating one record for each timestamp. If includeZeroRecords
  -- isn't set, we only use the timestamps we've got data for.
  let timestamps | includeZeroRecords = allTimestampsForPartition statsPartition now
                 | otherwise          = map head . group $ map slusTimeWindowStart stats
  case forWhom of
    UsageStatsForUser _uid -> do
      allLinksS <- renderTextTemplate_ "statsShareableLinksTotal"
      return $ shareableLinkStatsToJSON (timeFormat statsPartition)
                                        allLinksS
                                        timestamps
                                        stats
    UsageStatsForUserGroup _ugid -> do
      totalS <- renderTextTemplate_ "statsOrgTotal"
      return
        $ shareableLinkStatsToJSON (timeFormat statsPartition) totalS timestamps stats

-- Internal helper. Given a partition scheme and the current time, calculate all
-- the timestamps that postgresql (cf. `GetUsageStats`) will potentially spit
-- out, assuming there was activity in that timeframe. In the sql query we
-- truncate timestamps to start-of-day (resp. start-of-month) and include all
-- items with timestamp >= truncate(now) - i, where i = 30 days (resp. 6
-- months).
allTimestampsForPartition :: StatsPartition -> UTCTime -> [UTCTime]
allTimestampsForPartition PartitionByDay (UTCTime today _sec) = do
  i <- [0, -1 .. -30]
  -- the last 30 days (plus the started day) by descending date
  return $ UTCTime (addDays i today) 0
allTimestampsForPartition PartitionByMonth (UTCTime today _sec) = do
  let (year, month, _) = toGregorian today
  let startOfThisMonth = fromGregorian year month 1
  i <- [0, -1 .. -6]
  -- the last 6 months (plus the started month) by descending date
  return $ UTCTime (addGregorianMonthsClip i startOfThisMonth) 0

timeFormat :: StatsPartition -> UTCTime -> Text
timeFormat statsPartition = case statsPartition of
  PartitionByDay   -> T.pack . formatTimeYMD
  PartitionByMonth -> T.pack . formatTime' "%Y-%m"
{- |
    Checks for live documents owned by the user.
-}
isUserDeletable :: Kontrakcja m => User -> m Bool
isUserDeletable = fmap isNothing . dbQuery . IsUserDeletable

--there must be a better way than all of these weird user create functions
-- TODO clean up

sendNewUserMail :: Kontrakcja m => User -> m ()
sendNewUserMail user = do
  ctx <- getContext
  al <- newUserAccountRequestLink (user ^. #settings % #lang) (user ^. #id) AccountRequest
  mail <- newUserMail ctx (getEmail user) al
  scheduleEmailSendout $ mail
    { to = [MailAddress { fullname = getSmartName user, email = getEmail user }]
    }
  return ()

createNewUserByAdmin
  :: Kontrakcja m => Text -> (Text, Text) -> (UserGroupID, Bool) -> Lang -> m (Maybe User)
createNewUserByAdmin email names usergroupandrole lg = do
  ctx   <- getContext
  muser <-
    createUser (Email email) names usergroupandrole lg ByAdmin
      =<< getCreateUserContextFromContext
  case muser of
    Just user -> do
      let fullname = composeFullName names
      chpwdlink <- newUserAccountRequestLink (user ^. #settings % #lang)
                                             (user ^. #id)
                                             ByAdmin
      mail <- mailNewAccountCreatedByAdmin ctx (getLang user) email chpwdlink
      scheduleEmailSendout
        $ mail { to = [MailAddress { fullname = fullname, email = email }] }
      return muser
    Nothing -> return muser

handleAcceptTOSGet :: Kontrakcja m => m InternalKontraResponse
handleAcceptTOSGet =
  withUser $ \_ -> internalResponse . T.unpack <$> (pageAcceptTOS =<< getContext)

handleAcceptTOSPost :: Kontrakcja m => m ()
handleAcceptTOSPost = do
  ctx <- getContext
  let (maybeuser, time, ipnumber) = (ctx ^. #maybeUser, ctx ^. #time, ctx ^. #ipAddr)
  userid <- guardJustM . return $ view #id <$> maybeuser
  tos    <- getDefaultedField False asValidCheckBox "tos"
  case tos of
    Just True -> do
      void . dbUpdate $ AcceptTermsOfService userid time
      void . dbUpdate $ LogHistoryTOSAccept userid ipnumber time (Just userid)
    _ -> internalError


handleAccountSetupGet
  :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m InternalKontraResponse
handleAccountSetupGet uid token sm = do
  ctx   <- getContext
  muser <- getUserAccountRequestUser uid token
  case (muser, view #hasAcceptedTOS =<< muser) of
    (Just user, Nothing) -> do
      ugwp    <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
      ad      <- getAnalyticsData
      content <- renderTextTemplate "accountSetupPage" $ do
        standardPageFields ctx (Just $ ugwpUIWithID ugwp) ad
        F.value "fstname" $ getFirstName user
        F.value "sndname" $ getLastName user
        F.value "email" $ getEmail user
        F.value "userid" $ show uid
        F.value "company" $ ugwpUG ugwp ^. #name
        F.value "companyAdmin" $ user ^. #isCompanyAdmin
        F.value "companyPosition" $ user ^. #info % #companyPosition
        F.value "mobile" $ getMobile user
        F.value "signupmethod" $ show sm
      internalResponse <$> simpleHtmlResponse content
    (Just _user, Just _) -> return $ internalResponse LinkDesignView
    _                    -> do
      flashmessage <- case sm of
        CompanyInvitation -> flashMessageUserAccountRequestExpiredCompany
        _                 -> flashMessageUserAccountRequestExpired
      return . internalResponseWithFlash flashmessage . LinkLogin $ ctx ^. #lang

handleAccountSetupPost :: Kontrakcja m => UserID -> MagicHash -> SignupMethod -> m JSValue
handleAccountSetupPost uid token sm = do
  user <- guardJustM $ getUserAccountRequestUser uid token
  ug   <- dbQuery . UserGroupGetByUserID $ user ^. #id
  if isJust $ user ^. #hasAcceptedTOS
    then do
      J.runJSONGenT $ do
        J.value "ok" False
        J.value "error" ("already_active" :: String)
    else do
      mfstname <- getOptionalField asValidName "fstname"
      msndname <- getOptionalField asValidName "sndname"
      void $ handleActivate mfstname msndname (user, ug) sm
      void . dbUpdate $ DeleteUserAccountRequest uid
      ctx <- getContext
      void . dbUpdate $ SetUserSettings (user ^. #id)
                                        (user ^. #settings & #lang .~ ctx ^. #lang)
      link <- getHomeOrDesignViewLink
      J.runJSONGenT $ do
        J.value "ok" True
        J.value "location" $ show link
        J.value "userid" $ show uid

{- |
    This is where we get to when the user clicks the link in their password reminder
    email.  This'll show them the usual landing page, but with option to changing their password.
-}

handlePasswordReminderGet
  :: Kontrakcja m => UserID -> MagicHash -> m InternalKontraResponse
handlePasswordReminderGet uid token = do
  muser <- getPasswordReminderUser uid token
  case muser of
    Just user -> do
      switchLang (getLang user)
      ctx     <- getContext
      ad      <- getAnalyticsData
      content <- renderTextTemplate "changePasswordPageWithBranding" $ do
        F.value "linkchangepassword" . show $ LinkPasswordReminder uid token
        standardPageFields ctx Nothing ad
      internalResponse <$> simpleHtmlResponse content
    Nothing -> do
      ctx          <- getContext
      flashmessage <- flashMessagePasswordChangeLinkNotValid
      return . internalResponseWithFlash flashmessage $ LinkLoginDirect (ctx ^. #lang)


handlePasswordReminderPost :: Kontrakcja m => UserID -> MagicHash -> m JSValue
handlePasswordReminderPost uid token = do
  muser  <- getPasswordReminderUser uid token
  ipIsOK <- case muser of
    Just u -> do
      ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ u ^. #id
      let masklist = ugwpSettings ugwp ^. #ipAddressMaskList
      ctx <- getContext
      return $ null masklist || any (ipAddressIsInNetwork $ ctx ^. #ipAddr) masklist
    Nothing -> return True
  case muser of
    Just user | not (user ^. #accountSuspended) && ipIsOK -> do
      switchLang (getLang user)
      ctx <- getContext
      let time      = ctx ^. #time
          ipnumber  = ctx ^. #ipAddr
          maybeuser = ctx ^. #maybeUser
      password     <- guardJustM $ getField "password"
      goodPassword <- checkPassword (ctx ^. #passwordServiceConf) password
      if goodPassword
        then do
          void . dbUpdate $ DeletePasswordReminder uid
          passwordhash <- createPassword password
          void . dbUpdate $ SetUserPassword (user ^. #id) passwordhash
          void . dbUpdate $ LogHistoryPasswordSetup (user ^. #id)
                                                    ipnumber
                                                    time
                                                    (view #id <$> maybeuser)
          terminateAllUserSessionsExceptCurrent (user ^. #id)
          logUserToContext $ Just user
          J.runJSONGenT $ do
            J.value "logged" True
            J.value "location" $ show LinkDesignView
        else do
          J.runJSONGenT $ J.value "logged" False
    Just _ -> do
      {- MR: useraccountsuspended must be true here. This is a hack for Hi3G. It will be removed in future -}
      {- BC: or IP is blacklisted. This may be a hack, not sure -}
      J.runJSONGenT $ J.value "logged" False
    Nothing -> J.runJSONGenT $ J.value "logged" False

-- please treat this function like a public query form, it's not secure
handleContactSales :: Kontrakcja m => m ()
handleContactSales = do
  ctx     <- getContext
  fname   <- getField' "fstname"
  lname   <- getField' "sndname"
  email   <- getField' "email"
  message <- getField' "message"
  plan    <- getField' "plan"

  let uid = maybe "user not logged in"
                  (("user with id " <>) . showt)
                  (ctx ^? #maybeUser % _Just % #id)
      domainInfo = " (from domain " <> (ctx ^. #brandedDomain % #url) <> " )"
      content =
        "<p>Hi there!</p>"
          <> "<p>Someone requested information from the payments form"
          <> domainInfo
          <> ".</p>"
          <> "<p>Name: "
          <> fname
          <> " "
          <> lname
          <> "</p>"
          <> "<p>Email: "
          <> email
          <> "</p>"
          <> "<p>Message: \n"
          <> message
          <> "</p>"
          <> "<p>Looking at plan: "
          <> plan
          <> "</p>"
          <> "<p>"
          <> uid
          <> "</p>"
          <> "<p>Have a good one!</p>"
      contactEmail = "info@scrive.com"
      salesEmail   = "leads@scrive.com"
      sendEmailTo emailAddress = scheduleEmailSendout $ emptyMail
        { to      = [MailAddress { fullname = emailAddress, email = emailAddress }]
        , title   = "Contact request (" <> plan <> ")"
        , content = content
        }
  sendEmailTo contactEmail
  sendEmailTo salesEmail
  return ()
