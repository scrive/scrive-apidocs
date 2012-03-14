module User.UserView (
    -- pages
    showUser,
    showUserSecurity,
    showUserMailAPI,
    showUsageStats,
    pageAcceptTOS,
    activatePageViewNotValidLink,

    -- mails
    newUserMail,
    viralInviteMail,
    mailNewAccountCreatedByAdmin,
    resetPasswordMail,
    mailRequestChangeEmail,

    -- modals
    modalAccountSetup,
    modalDoYouWantToChangeEmail,

    -- flash messages
    flashMessageLoginRedirectReason,
    flashMessageUserDetailsSaved,
    flashMessageCompanyCreated,
    flashMessageNoAccountType,
    flashMessageInvalidAccountType,
    flashMessageMustAcceptTOS,
    flashMessageBadOldPassword,
    flashMessagePasswordsDontMatch,
    flashMessageUserPasswordChanged,
    flashMessagePasswordChangeLinkNotValid,
    flashMessageUserWithSameEmailExists,
    flashMessageViralInviteSent,
    flashMessageOtherUserSentInvitation,
    flashMessageNoRemainedInvitationEmails,
    flashMessageActivationLinkNotValid,
    flashMessageUserActivated,
    flashMessageUserAlreadyActivated,
    flashMessageChangePasswordEmailSend,
    flashMessageNoRemainedPasswordReminderEmails,
    flashMessageNewActivationLinkSend,
    flashMessageUserSignupDone,
    flashMessageThanksForTheQuestion,
    flashMessageChangeEmailMailSent,
    flashMessageMismatchedEmails,
    flashMessageProblemWithEmailChange,
    flashMessageProblemWithPassword,
    flashMessageYourEmailHasChanged,

    --modals
    modalNewPasswordView,
    modalUserSignupDone,

    --utils
    userBasicFields,

    userStatsDayToJSON,
    userStatsMonthToJSON,
    companyStatsDayToJSON,
    companyStatsMonthToJSON
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Maybe
import ActionSchedulerState
import Company.Model
import Kontra
import KontraLink
import MagicHash (MagicHash)
import Mails.SendMail(Mail)
import Templates.Templates
import Templates.TemplatesUtils
import Text.StringTemplate.GenericStandard()
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import FlashMessage
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import User.Model
import MinutesTime
import Util.JSON
import Text.JSON
import Data.Either
import Misc
import ScriveByMail.Model
import ScriveByMail.View

showUser :: TemplatesMonad m => User -> Maybe Company -> Bool -> m String
showUser user mcompany createcompany = renderTemplateFM "showUser" $ do
    userFields user
    companyFields mcompany
    field "createcompany" $ createcompany
    field "linkaccount" $ show (LinkAccount False)

companyFields :: MonadIO m => Maybe Company -> Fields m
companyFields mcompany = do
    field "companyid" $ show $ fmap companyid mcompany
    field "address" $ fmap (companyaddress . companyinfo) mcompany
    field "city" $ fmap (companycity . companyinfo) mcompany
    field "country" $ fmap (companycountry . companyinfo) mcompany
    field "zip" $ fmap (companyzip . companyinfo) mcompany
    field "companyname" $ getCompanyName mcompany
    field "companynumber" $ getCompanyNumber mcompany
    field "companyimagelink" False

userFields :: MonadIO m => User -> Fields m
userFields user = do
    let fullname          = BS.toString $ getFullName user
        fullnameOrEmail   = BS.toString $ getSmartName user
        fullnamePlusEmail = if null fullname
                            then              "<" ++ (BS.toString $ getEmail user) ++ ">"
                            else fullname ++ " <" ++ (BS.toString $ getEmail user) ++ ">"
    field "id" $ show $ userid user
    field "fstname" $ getFirstName user
    field "sndname" $ getLastName user
    field "email" $ getEmail user
    field "personalnumber" $ getPersonalNumber user
    field "phone" $ userphone $ userinfo user
    field "mobile" $ usermobile $ userinfo user
    field "companyposition" $ usercompanyposition $ userinfo user
    field "userimagelink" False
    field "fullname" $ fullname
    field "fullnameOrEmail" $ fullnameOrEmail
    field "fullnamePlusEmail" $ fullnamePlusEmail
    field "iscompanyaccount" $ isJust $ usercompany user

    --field "invoiceaddress" $ BS.toString $ useraddress $ userinfo user
    menuFields user

showUserSecurity :: TemplatesMonad m => User -> m String
showUserSecurity user = renderTemplateFM "showUserSecurity" $ do
    field "linksecurity" $ show LinkAccountSecurity
    field "fstname" $ getFirstName user
    field "sndname" $ getLastName user
    field "userimagelink" False
    fieldF "region" $ do
        field "se" $ REGION_SE == (getRegion user)
        field "gb" $ REGION_GB == (getRegion user)
    fieldF "lang" $ do
        field "en" $ LANG_EN == (getLang user)
        field "se" $ LANG_SE == (getLang user)
    field "footer" $ customfooter $ usersettings user
    field "advancedMode" $ Just AdvancedMode == (preferreddesignmode $ usersettings user)
    menuFields user

showUserMailAPI :: TemplatesMonad m => User -> Maybe MailAPIInfo -> Maybe MailAPIInfo -> m String
showUserMailAPI user mapi mcapi =
    renderTemplateFM "showUserMailAPI" $ do
        field "linkmailapi" $ show LinkUserMailAPI
        field "mailapienabled" $ isJust mapi
        when (isJust mapi) $ mailAPIInfoFields (fromJust mapi)
        field "hascompanymailapi" $ isJust mcapi
        when (isJust mcapi) $ fieldF "company" $ mailAPIInfoFields (fromJust mcapi)
        menuFields user

_usageStatisticsFieldsByDay :: (Functor m, MonadIO m) => [(Int, [Int])] -> [Fields m]
_usageStatisticsFieldsByDay stats = map f stats
  where f (ct, s:c:i:_) = do
                field "date" $ showAsDate ct
                field "closed" c
                field "signatures" s
                field "sent" i
        f _ = error $ "statisticsFieldsByDay: bad stats"

_usageStatisticsFieldsByMonth :: (Functor m, MonadIO m) => [(Int, [Int])] -> [Fields m]
_usageStatisticsFieldsByMonth stats = map f stats
  where f (ct, s:c:i:_) = do
                field "date" $ showAsMonth ct
                field "closed" c
                field "signatures" s
                field "sent" i
        f _ = error $ "statisticsFieldsByMonth: bad stats"

userStatsDayToJSON :: [(Int, [Int])] -> JSValue
userStatsDayToJSON = JSArray . rights . map f
  where f (d, s:c:i:_) = Right jsempty >>=
                         jsset ["fields", "date"] (showAsDate d) >>=
                         jsset ["fields", "closed"] c >>=
                         jsset ["fields", "sent"] i >>=
                         jsset ["fields", "signatures"] s
        f _ = Left "Bad stat"

userStatsMonthToJSON :: [(Int, [Int])] -> JSValue
userStatsMonthToJSON = JSArray . rights . map f
  where f (d, s:c:i:_) = Right jsempty >>=
                         jsset ["fields", "date"] (showAsMonth d) >>=
                         jsset ["fields", "closed"] c >>=
                         jsset ["fields", "sent"] i >>=
                         jsset ["fields", "signatures"] s
        f _ = Left "Bad stat"

companyStatsDayToJSON :: String -> [(Int, String, [Int])] -> JSValue
companyStatsDayToJSON ts ls = JSArray $ rights $ [f e | e@(_,n,_) <- ls, n=="Total"]
  where f (d, _, s:c:i:_) = Right jsempty >>=
                            jsset ["fields", "date"] (showAsDate d) >>=
                            jsset ["fields", "closed"] c >>=
                            jsset ["fields", "sent"] i >>=
                            jsset ["fields", "name"] ts >>=
                            jsset ["fields", "signatures"] s >>=
                            jsset "subfields" [fromRight $
                                               Right jsempty >>=
                                               jsset "date" (showAsDate d') >>=
                                               jsset "closed" c' >>=
                                               jsset "sent"   i' >>=
                                               jsset "name"   n' >>=
                                               jsset "signatures" s' |
                                               (d',n',s':c':i':_) <- ls,
                                               d' == d,
                                               n' /= "Total"]


        f _ = Left "Bad stat"

companyStatsMonthToJSON :: String -> [(Int, String, [Int])] -> JSValue
companyStatsMonthToJSON ts ls = JSArray $ rights $ [f e | e@(_,n,_) <- ls, n=="Total"]
  where f (d, _, s:c:i:_) = Right jsempty >>=
                            jsset ["fields", "date"] (showAsMonth d) >>=
                            jsset ["fields", "closed"] c >>=
                            jsset ["fields", "sent"] i >>=
                            jsset ["fields", "name"] ts >>=
                            jsset ["fields", "signatures"] s >>=
                            jsset "subfields" [fromRight $
                                               Right jsempty >>=
                                               jsset "date" (showAsMonth d') >>=
                                               jsset "closed" c' >>=
                                               jsset "sent"   i' >>=
                                               jsset "name"   n' >>=
                                               jsset "signatures" s' |
                                               (d',n',s':c':i':_) <- ls,
                                               d' == d,
                                               n' /= "Total"]
        f _ = Left "Bad stat"

showUsageStats :: TemplatesMonad m => User -> m String
showUsageStats user =
    renderTemplateFM "showUsageStats" $ do
      menuFields user

pageAcceptTOS :: TemplatesMonad m => m String
pageAcceptTOS = renderTemplateM "pageAcceptTOS" ()

menuFields :: MonadIO m => User -> Fields m
menuFields user = do
    field "iscompanyadmin" $ useriscompanyadmin user

activatePageViewNotValidLink :: TemplatesMonad m => String -> m String
activatePageViewNotValidLink email =
  renderTemplateFM "activatePageViewNotValidLink" $ field "email" email

resetPasswordMail :: TemplatesMonad m => String -> User -> KontraLink -> m Mail
resetPasswordMail hostname user setpasslink = do
  kontramail "passwordChangeLinkMail" $ do
    field "personname"   $ getFullName user
    field "passwordlink" $ show setpasslink
    field "ctxhostpart"  $ hostname

newUserMail :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> KontraLink -> m Mail
newUserMail hostpart emailaddress personname activatelink = do
  kontramail "newUserMail" $ do
    field "personname"   $ BS.toString personname
    field "email"        $ BS.toString emailaddress
    field "activatelink" $ show activatelink
    field "ctxhostpart"  $ hostpart

viralInviteMail :: TemplatesMonad m => Context -> BS.ByteString -> KontraLink -> m Mail
viralInviteMail ctx invitedemail setpasslink = do
  let invitername = BS.toString $ maybe BS.empty getSmartName (ctxmaybeuser ctx)
  kontramail "mailViralInvite" $ do
    field "email"        $ BS.toString invitedemail
    field "invitername"  $ invitername
    field "ctxhostpart"  $ ctxhostpart ctx
    field "passwordlink" $ show setpasslink


mailNewAccountCreatedByAdmin :: (HasLocale a, TemplatesMonad m) => Context -> a -> BS.ByteString -> BS.ByteString -> KontraLink -> Maybe String -> m Mail
mailNewAccountCreatedByAdmin ctx locale personname email setpasslink custommessage = do
  kontramaillocal locale "mailNewAccountCreatedByAdmin" $ do
    field "personname"    $ BS.toString personname
    field "email"         $ BS.toString email
    field "passwordlink"  $ show setpasslink
    field "creatorname"   $ BS.toString $ maybe BS.empty getSmartName (ctxmaybeuser ctx)
    field "ctxhostpart"   $ ctxhostpart ctx
    field "custommessage"   custommessage

mailRequestChangeEmail :: (TemplatesMonad m, HasSomeUserInfo a) => String -> a -> Email -> KontraLink -> m Mail
mailRequestChangeEmail hostpart user newemail link = do
  kontramail "mailRequestChangeEmail" $ do
    field "fullname" $ getFullName user
    field "newemail" $ unEmail newemail
    field "hostpart" $ hostpart
    field "link" $ show link

-------------------------------------------------------------------------------

modalAccountSetup :: MonadIO m => KontraLink -> String -> String -> m FlashMessage
modalAccountSetup signuplink fstname sndname = do
  return $ toFlashTemplate Modal "modalAccountSetup" $
    [ ("signuplink", show signuplink)
    , ("fstname", fstname)
    , ("sndname", sndname) ]

modalDoYouWantToChangeEmail :: TemplatesMonad m => Email -> m FlashMessage
modalDoYouWantToChangeEmail newemail = do
  toModal <$> (renderTemplateFM "modalDoYouWantToChangeEmail" $
                 field "newemail" $ unEmail newemail)

flashMessageThanksForTheQuestion :: TemplatesMonad m => m FlashMessage
flashMessageThanksForTheQuestion =
    toFlashMsg OperationDone <$> renderTemplateM "flashMessageThanksForTheQuestion" ()

flashMessageLoginRedirectReason :: TemplatesMonad m => LoginRedirectReason -> m (Maybe FlashMessage)
flashMessageLoginRedirectReason reason =
  case reason of
       LoginTry             -> return Nothing
       NotLogged            -> render "notlogged"
       NotLoggedAsSuperUser -> render "notsu"
       InvalidLoginInfo _   -> render "invloginfo"
  where
    render msg = Just . toFlashMsg OperationFailed <$>
      (renderTemplateFM "flashMessageLoginPageRedirectReason" $ field msg True)

flashMessageUserDetailsSaved :: TemplatesMonad m => m FlashMessage
flashMessageUserDetailsSaved =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageUserDetailsSaved" ()

flashMessageCompanyCreated :: TemplatesMonad m => m FlashMessage
flashMessageCompanyCreated =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageCompanyCreated" ()


flashMessageNoAccountType :: TemplatesMonad m => m FlashMessage
flashMessageNoAccountType =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoAccountType" ()

flashMessageInvalidAccountType :: TemplatesMonad m => m FlashMessage
flashMessageInvalidAccountType =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageInvalidAccountType" ()

flashMessageMustAcceptTOS :: TemplatesMonad m => m FlashMessage
flashMessageMustAcceptTOS =
  toFlashMsg SigningRelated <$> renderTemplateM "flashMessageMustAcceptTOS" ()


flashMessageBadOldPassword :: TemplatesMonad m => m FlashMessage
flashMessageBadOldPassword =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageBadOldPassword" ()


flashMessagePasswordsDontMatch :: TemplatesMonad m => m FlashMessage
flashMessagePasswordsDontMatch =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessagePasswordsDontMatch" ()


flashMessageUserPasswordChanged :: TemplatesMonad m => m FlashMessage
flashMessageUserPasswordChanged =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageUserPasswordChanged" ()

flashMessagePasswordChangeLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessagePasswordChangeLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessagePasswordChangeLinkNotValid" ()


flashMessageUserWithSameEmailExists :: TemplatesMonad m => m FlashMessage
flashMessageUserWithSameEmailExists =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageUserWithSameEmailExists" ()


flashMessageViralInviteSent :: TemplatesMonad m => m FlashMessage
flashMessageViralInviteSent =
  toFlashMsg SigningRelated <$> renderTemplateM "flashMessageViralInviteSent" ()

flashMessageOtherUserSentInvitation :: TemplatesMonad m => m FlashMessage
flashMessageOtherUserSentInvitation =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageOtherUserSentInvitation" ()

flashMessageNoRemainedInvitationEmails :: TemplatesMonad m => m FlashMessage
flashMessageNoRemainedInvitationEmails =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoRemainedInvitationEmails" ()

flashMessageActivationLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessageActivationLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageActivationLinkNotValid" ()


flashMessageUserActivated :: TemplatesMonad m => m FlashMessage
flashMessageUserActivated =
  toFlashMsg SigningRelated <$> renderTemplateM "flashMessageUserActivated" ()


flashMessageUserAlreadyActivated :: TemplatesMonad m => m FlashMessage
flashMessageUserAlreadyActivated =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageUserAlreadyActivated" ()

flashMessageChangePasswordEmailSend :: TemplatesMonad m => m FlashMessage
flashMessageChangePasswordEmailSend =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageChangePasswordEmailSend" ()

flashMessageNoRemainedPasswordReminderEmails :: TemplatesMonad m => m FlashMessage
flashMessageNoRemainedPasswordReminderEmails =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoRemainedPasswordReminderEmails" ()

flashMessageNewActivationLinkSend :: TemplatesMonad m => m FlashMessage
flashMessageNewActivationLinkSend =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageNewActivationLinkSend" ()


flashMessageUserSignupDone :: TemplatesMonad m => m FlashMessage
flashMessageUserSignupDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageUserSignupDone" ()

modalNewPasswordView :: TemplatesMonad m => ActionID -> MagicHash -> m FlashMessage
modalNewPasswordView aid hash = do
  toModal <$> (renderTemplateFM "modalNewPasswordView" $ do
            field "linkchangepassword" $ show $ LinkPasswordReminder aid hash)

modalUserSignupDone :: TemplatesMonad m => Email -> m FlashMessage
modalUserSignupDone email =
  toModal <$> (renderTemplateFM "modalUserSignupDone" $ do
                 field "email" $ BS.toString (unEmail email))

flashMessageChangeEmailMailSent :: TemplatesMonad m => Email -> m FlashMessage
flashMessageChangeEmailMailSent newemail =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageChangeEmailMailSent" $
                                  field "newemail" $ unEmail newemail)

flashMessageMismatchedEmails :: TemplatesMonad m => m FlashMessage
flashMessageMismatchedEmails =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageMismatchedEmails" ()

flashMessageProblemWithEmailChange :: TemplatesMonad m => m FlashMessage
flashMessageProblemWithEmailChange =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageProblemWithEmailChange" ()

flashMessageProblemWithPassword :: TemplatesMonad m => m FlashMessage
flashMessageProblemWithPassword =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageProblemWithPassword" ()

flashMessageYourEmailHasChanged :: TemplatesMonad m => m FlashMessage
flashMessageYourEmailHasChanged =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageYourEmailHasChanged" ()

-------------------------------------------------------------------------------

{- | Basic fields for the user  -}
userBasicFields :: MonadIO m => User -> Maybe Company -> Fields m
userBasicFields u mc = do
    field "id" $ show $ userid u
    field "fullname" $ getFullName u
    field "email" $ getEmail u
    field "company" $ getCompanyName mc
    field "phone" $ userphone $ userinfo u
    field "iscompanyadmin" $ useriscompanyadmin u
    field "TOSdate" $ maybe "-" show (userhasacceptedtermsofservice u)
