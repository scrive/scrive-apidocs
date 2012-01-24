{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module RoutingTable
    ( staticRoutes
    ) where

import API.IntegrationAPI
import API.Service.ServiceControl
import API.UserAPI

import ActionSchedulerState
import AppView as V
import DB.Classes
import InputValidation
import Kontra
import KontraLink
import Mails.SendGridEvents
import Mails.SendMail
import MinutesTime
import Misc
--import PayEx.PayExInterface ()-- Import so at least we check if it compiles
import Redirect
import Routing
import Happstack.StaticRouting(Route, choice, dir, path, param, remainingPath)
import User.Model
import User.UserView as UserView
import qualified Stats.Control as Stats
import qualified Administration.AdministrationControl as Administration
import qualified AppLogger as Log (security, debug)
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import qualified Contacts.ContactsControl as Contacts
import qualified Doc.DocControl as DocControl
import qualified Archive.Control as ArchiveControl
import qualified ELegitimation.BankID as BankID
import qualified Payments.PaymentsControl as Payments
import qualified User.UserControl as UserControl
import qualified ScriveByMail.Control as ScriveByMail
import Util.FlashUtil
import Util.HasSomeUserInfo
import Doc.API

import Control.Monad.Error
import Data.Functor
import Data.List
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Happstack.State (query, update)


import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Util.MonadUtils


{- |
   The routing table for the app.
   Routes in this table should be of the form
   dir "segment1" $ dir "segment2" $ .. $ dir "segmentn" $ hgetx $ handler
   OR
   dir "segment1" $ dir "segment2" $ .. $ dir "segmentn" $ hpostx $ handler

   param "name" is also allowed, which will guard based on the
   existence of a post/get param

   No other logic should be in here and no similar logic should be in the handler.
   That is, all routing logic should be in this table to ensure that we can find
   the function for any given path and method.
-}
staticRoutes :: Route (Kontra Response)
staticRoutes = choice
     [ allLocaleDirs $ const $ hGetAllowHttp $ handleHomepage
     , hGetAllowHttp $ getContext >>= (redirectKontraResponse . LinkHome . ctxlocale)

     , publicDir "priser" "pricing" LinkPriceplan handlePriceplanPage
     , publicDir "sakerhet" "security" LinkSecurity handleSecurityPage
     , publicDir "juridik" "legal" LinkLegal handleLegalPage
     , publicDir "sekretesspolicy" "privacy-policy" LinkPrivacyPolicy handlePrivacyPolicyPage
     , publicDir "allmana-villkor" "terms" LinkTerms handleTermsPage
     , publicDir "om-scrive" "about" LinkAbout handleAboutPage
     , publicDir "partners" "partners" LinkPartners handlePartnersPage -- FIXME: Same dirs for two languages is broken
     , publicDir "kunder" "clients" LinkClients handleClientsPage
     , publicDir "kontakta" "contact" LinkContactUs handleContactUsPage
     , publicDir "scriveapi" "scriveapi" LinkAPIPage handleApiPage
     , publicDir "scrivebymail" "scrivebymail" LinkScriveByMailPage handleScriveByMailPage

     -- sitemap
     , dir "webbkarta"       $ hGetAllowHttp $ handleSitemapPage
     , dir "sitemap"         $ hGetAllowHttp $ handleSitemapPage

     -- this is SMTP to HTTP gateway
     , dir "mailapi" $ hPostNoXToken $ toK0 $ ScriveByMail.handleScriveByMail

     -- Only download function | unified for author and signatories
     , dir "download"                     $ hGet  $ toK3 $ DocControl.handleDownloadFile

     , dir "s" $ dir "eleg" $ hGet $ toK2 $ BankID.generateBankIDTransaction
     , dir "s" $ hGet $ toK0    $ sendRedirect $ LinkContracts
     , dir "s" $ hGet $ toK2    $ DocControl.handleSignShow
     , dir "s" $ hGet $ toK3    $ DocControl.handleSignShowOldRedirectToNew -- Redirect for old version to version above, remove not earlier then 31.12.2012.

     , dir "s" $ param "sign"           $ hPostNoXToken $ toK2 $ DocControl.signDocument
     , dir "s" $ param "reject"         $ hPostNoXToken $ toK2 $ DocControl.rejectDocument
     , dir "s" $ param "acceptaccount"  $ hPostNoXToken $ toK5 $ DocControl.handleAcceptAccountFromSign
     , dir "s" $ param "declineaccount" $ hPostNoXToken $ toK5 $ DocControl.handleDeclineAccountFromSign
     , dir "s" $ param "sigattachment"  $ hPostNoXToken $ toK2 $ DocControl.handleSigAttach
     , dir "s" $ param "deletesigattachment" $ hPostNoXToken $ toK2 $ DocControl.handleDeleteSigAttach

     , dir "sv" $ hGet $ toK3 $ DocControl.handleAttachmentViewForViewer

     --Q: This all needs to be done by author. Why we dont check it
     --here? MR

     --A: Because this table only contains routing logic. The logic of
     --what it does/access control is left to the handler. EN
     , dir "upload" $ hGet $ toK0 $ DocControl.handleShowUploadPage
     , dir "locale" $ hPost $ toK0 $ UserControl.handlePostUserLocale
     , dir "a"                     $ hGet  $ toK0 $ ArchiveControl.showAttachmentList
     , dir "a" $ param "archive"   $ hPost $ toK0 $ ArchiveControl.handleAttachmentArchive
     , dir "a" $ param "share"     $ hPost $ toK0 $ DocControl.handleAttachmentShare
     , dir "a" $ dir "rename"      $ hPost $ toK1 $ DocControl.handleAttachmentRename
     , dir "a"                     $ hPost $ toK0 $ DocControl.handleCreateNewAttachment

     , dir "t" $ hGet  $ toK0 $ ArchiveControl.showTemplatesList
     , dir "t" $ param "archive" $ hPost $ toK0 $ ArchiveControl.handleTemplateArchive
     , dir "t" $ param "share" $ hPost $ toK0 $ DocControl.handleTemplateShare
     , dir "t" $ param "template" $ hPost $ toK0 $ DocControl.handleCreateFromTemplate
     , dir "t" $ hPost $ toK0 $ DocControl.handleCreateNewTemplate

     , dir "o" $ hGet $ toK0 $ ArchiveControl.showOfferList
     , dir "o" $ param "archive" $ hPost $ toK0 $ ArchiveControl.handleOffersArchive
     , dir "o" $ param "remind" $ hPost $ toK0 $ DocControl.handleBulkOfferRemind

     , dir "or" $ hGet  $ toK0 $ ArchiveControl.showOrdersList
     , dir "or" $ param "archive" $ hPost $ toK0 $ ArchiveControl.handleOrdersArchive
     , dir "or" $ param "remind" $ hPost $ toK0 $ DocControl.handleBulkOrderRemind

     , dir "r" $ hGet $ toK0 $ ArchiveControl.showRubbishBinList
     , dir "r" $ param "restore" $ hPost $ toK0 $ DocControl.handleRubbishRestore
     , dir "r" $ param "reallydelete" $ hPost $ toK0 $ DocControl.handleRubbishReallyDelete

     , dir "d"                     $ hGet  $ toK0 $ ArchiveControl.showContractsList
     , dir "d"                     $ hGet  $ toK1 $ DocControl.handleIssueShowGet
     , dir "d" $ dir "eleg"        $ hGet  $ toK1 $ BankID.generateBankIDTransactionForAuthor
     , dir "d" $ {- param "doc" $ -} hPost $ toK0 $ DocControl.handleIssueNewDocument
     , dir "d" $ param "archive"   $ hPost $ toK0 $ ArchiveControl.handleContractArchive
     , dir "d" $ param "remind"    $ hPost $ toK0 $ DocControl.handleBulkContractRemind
     , dir "d"                     $ hPost $ toK1 $ DocControl.handleIssueShowPost
     , dir "docs"                  $ hGet  $ toK0 $ DocControl.jsonDocumentsList
     , dir "doc"                   $ hGet  $ toK1 $ DocControl.jsonDocument
     , dir "save"                  $ hPost $ toK1 $ DocControl.handleSaveDraft
     , dir "setattachments"        $ hPost $ toK1 $ DocControl.handleSetAttachments -- Since setting attachments can have file upload, we need extra handler for it.
     , dir "mailpreview"           $ hGet  $ toK2 $ DocControl.prepareEmailPreview

     , dir "df"                    $ hGet  $ toK2 $ DocControl.handleFileGet
     , dir "dv"                    $ hGet  $ toK1 $ DocControl.handleAttachmentViewForAuthor

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hPost $ toK2 $ DocControl.handleResend
     , dir "changeemail" $ hPost $ toK2 $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hPost $ DocControl.handleWithdrawn
     , dir "restart" $ hPost $ toK1 $ DocControl.handleRestart
     , dir "cancel"  $ hPost $ toK1 $ DocControl.handleCancel

     , dir "pages"  $ hGetAjax $ toK3 $ DocControl.showPage
     -- HTMP emails can have embedded preview image
     , dir "preview" $ hGet $ toK2 $ DocControl.showPreview
     , dir "preview" $ hGet $ toK4 $ DocControl.showPreviewForSignatory

     , dir "template"  $ hPost $ toK0 $ DocControl.handleCreateFromTemplate

     , dir "filepages" $ hGetAjax $  toK2 $ DocControl.handleFilePages
     , dir "pagesofdoc" $ hGetAjax $ toK1 $ DocControl.handlePageOfDocument
     , dir "pagesofdoc" $ hGetAjax $ toK3 $ DocControl.handlePageOfDocumentForSignatory

     , dir "csvlandpage" $ hGet $ toK1 $ DocControl.handleCSVLandpage

     -- UserControl
     , dir "account"                    $ hGet  $ toK0 $ UserControl.handleUserGet
     , dir "account"                    $ hPost $ toK0 $ UserControl.handleUserPost
     , dir "account" $ hGet $ toK2 $ UserControl.handleGetChangeEmail
     , dir "account" $ hPost $ toK2 $ UserControl.handlePostChangeEmail
     , dir "account" $ dir "security" $ hGet $ toK0 $ UserControl.handleGetUserSecurity
     , dir "account" $ dir "security" $ hPost $ toK0 $ UserControl.handlePostUserSecurity
     , dir "account" $ dir "mailapi" $ hGet $ toK0 $ UserControl.handleGetUserMailAPI
     , dir "account" $ dir "mailapi" $ hPost $ toK0 $ UserControl.handlePostUserMailAPI
     , dir "account" $ dir "usagestats" $ hGet $ toK0 $ UserControl.handleUsageStatsForUser
     , dir "account" $ dir "usagestats" $ dir "days"   $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserDays
     , dir "account" $ dir "usagestats" $ dir "months" $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserMonths
     , dir "contacts"  $ hGet  $ toK0 $ Contacts.showContacts
     , dir "contacts"  $ hPost $ toK0 $ Contacts.handleContactsChange
     , dir "accepttos" $ hGet  $ toK0 $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hPost $ toK0 $ UserControl.handleAcceptTOSPost

     --CompanyAccountsControl
     , dir "account" $ dir "companyaccounts" $ hGet  $ toK0 $ CompanyAccounts.handleGetCompanyAccounts
     , dir "account" $ dir "companyaccounts" $ hPost $ toK0 $ CompanyAccounts.handlePostCompanyAccounts
     , dir "companyaccounts" $ hGet  $ toK0 $ CompanyAccounts.handleCompanyAccounts
     , dir "companyaccounts" $ dir "join" $ hGet $ toK1 $ CompanyAccounts.handleGetBecomeCompanyAccount
     , dir "companyaccounts" $ dir "join" $ hPost $ toK1 $ CompanyAccounts.handlePostBecomeCompanyAccount
     -- these two are deprecated now, but we mailed out the links so we need to keep them hanging
     -- around for a litte bit
     , dir "account" $ dir "bsa" $ hGet $ toK1 $ CompanyAccounts.handleGetBecomeCompanyAccountOld
     , dir "account" $ dir "bsa" $ hPost $ toK1 $ CompanyAccounts.handlePostBecomeCompanyAccountOld

     -- super user only
     , dir "stats"      $ hGet  $ toK0 $ Administration.showStats
     , dir "createuser" $ hPost $ toK0 $ Administration.handleCreateUser
     , dir "sendgrid" $ dir "events" $ remainingPath POST $ handleSendgridEvent
     , dir "adminonly" $ hGet $ toK0 $ Administration.showAdminMainPage
     , dir "adminonly" $ dir "advuseradmin" $ hGet $ toK0 $ Administration.showAdminUserAdvanced
     , dir "adminonly" $ dir "useradminforsales" $ hGet $ toK0 $ Administration.showAdminUsersForSales
     , dir "adminonly" $ dir "userslist" $ hGet $ toK0 $ Administration.jsonUsersList
     , dir "adminonly" $ dir "useradminforpayments" $ hGet $ toK0 $ Administration.showAdminUsersForPayments
     , dir "adminonly" $ dir "useradmin" $ hGet $ toK1 $ Administration.showAdminUsers . Just
     , dir "adminonly" $ dir "useradmin" $ hGet $ toK0 $ Administration.showAdminUsers Nothing
     , dir "adminonly" $ dir "useradmin" $ dir "usagestats" $ hGet $ toK1 $ Stats.showAdminUserUsageStats
     , dir "adminonly" $ dir "useradmin" $ hPost $ toK1 $ Administration.handleUserChange
     , dir "adminonly" $ dir "companyadmin" $ hGet $ toK0 $ Administration.showAdminCompanies
     , dir "adminonly" $ dir "companyadmin" $ hGet $ toK1 $ Administration.showAdminCompany
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hGet $ toK1 $ Administration.showAdminCompanyUsers
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hPost $ toK1 $ Administration.handlePostAdminCompanyUsers, dir "adminonly" $ dir "companyaccounts" $ hGet  $ toK1 $ CompanyAccounts.handleCompanyAccountsForAdminOnly
     , dir "adminonly" $ dir "companyaccounts" $ hGet  $ toK1 $ CompanyAccounts.handleCompanyAccountsForAdminOnly
     , dir "adminonly" $ dir "companyadmin" $ dir "usagestats" $ hGet $ toK1 $ Stats.showAdminCompanyUsageStats
     , dir "adminonly" $ dir "companyadmin" $ hPost $ toK1 $ Administration.handleCompanyChange
     , dir "adminonly" $ dir "functionalitystats" $ hGet $ toK0 $ Administration.showFunctionalityStats
     , dir "adminonly" $ dir "db" $ remainingPath GET $ https $ msum
               [ Administration.indexDB >>= toResp
               , onlySuperUser $ serveDirectory DisableBrowsing [] "_local/kontrakcja_state"
               ]

     , dir "adminonly" $ dir "documents" $ hGet $ toK0 $ Administration.showDocumentsDaylyList

     , dir "adminonly" $ dir "allstatscsv" $ path GET id $ Stats.handleDocStatsCSV
     , dir "adminonly" $ dir "userstatscsv" $ path GET id $ Stats.handleUserStatsCSV

     , dir "adminonly" $ dir "runstatsonalldocs" $ hGet $ toK0 $ Stats.addAllDocsToStats
     , dir "adminonly" $ dir "stats1to2" $ hGet $ toK0 $ Stats.handleMigrate1To2

     , dir "adminonly" $ dir "runstatsonallusers" $ hGet $ toK0 $ Stats.addAllUsersToStats

     , dir "adminonly" $ dir "cleanup"           $ hPost $ toK0 $ Administration.handleDatabaseCleanup
     , dir "adminonly" $ dir "statistics"        $ hGet  $ toK0 $ Stats.showAdminSystemUsageStats
     , dir "adminonly" $ dir "skrivapausers.csv" $ hGet  $ toK0 $ Administration.getUsersDetailsToCSV
     , dir "adminonly" $ dir "payments"          $ hGet  $ toK0 $ Payments.handlePaymentsModelForViewView
     , dir "adminonly" $ dir "advpayments"       $ hGet  $ toK0 $ Payments.handlePaymentsModelForEditView
     , dir "adminonly" $ dir "advpayments"       $ hPost $ toK0 $ Payments.handleAccountModelsChange

     , dir "adminonly" $ dir "services" $ hGet $ toK0 $ Administration.showServicesPage
     , dir "adminonly" $ dir "services" $ param "create" $ hPost $ toK0 $ Administration.handleCreateService
     , dir "adminonly" $ dir "translations" $ hGet $ toK0 $ Administration.showAdminTranslations
     , dir "adminonly" $ dir "companies" $ hGet $ toK0 $ Administration.jsonCompanies

     -- a temporary service to help migration
     --, dir "adminonly" $ dir "migratesigaccounts" $ hGet $ toK0 $ Administration.migrateSigAccounts
     --, dir "adminonly" $ dir "migratecompanies" $ hGet $ toK0 $ Administration.migrateCompanies

     , dir "adminonly" $ dir "sysdump" $ hGet $ toK0 $ Administration.sysdump

     , dir "adminonly" $ dir "reseal" $ hPost $ toK1 $ Administration.resealFile
     , dir "adminonly" $ dir "replacemainfile" $ hPost $ toK1 $ Administration.replaceMainFile

     , dir "adminonly" $ dir "docproblems" $ hGet $ toK0 $ DocControl.handleInvariantViolations

     , dir "adminonly" $ dir "backdoor" $ hGet $ toK1 $ Administration.handleBackdoorQuery

     -- this stuff is for a fix
     , dir "adminonly" $ dir "510bugfix" $ hGet $ toK0 $ Administration.handleFixForBug510
     , dir "adminonly" $ dir "adminonlybugfix" $ hGet $ toK0 $ Administration.handleFixForAdminOnlyBug

     , dir "adminonly" $ dir "siglinkids_test_uniqueness" $ hGet $ toK0 $ Administration.handleCheckSigLinkIDUniqueness

       
     , dir "adminonly" $ dir "upsalesdeleted" $ hGet $ toK0 $ DocControl.handleUpsalesDeleted
       
       
     , dir "services" $ hGet $ toK0 $ handleShowServiceList
     , dir "services" $ hGet $ toK1 $ handleShowService
     , dir "services" $ dir "ui" $ hPost $ toK1 $ handleChangeServiceUI
     , dir "services" $ dir "password" $ hPost $ toK1 $ handleChangeServicePassword
     , dir "services" $ dir "settings" $ hPost $ toK1 $ handleChangeServiceSettings
     , dir "services" $ dir "logo" $ hGet $ toK1 $ handleServiceLogo
     , dir "services" $ dir "buttons_body" $ hGet $ toK1 $ handleServiceButtonsBody
     , dir "services" $ dir "buttons_rest" $ hGet $ toK1 $ handleServiceButtonsRest

     -- never ever use this
     , dir "adminonly" $ dir "neveruser" $ dir "resetservicepassword" $ hGetWrap (onlySuperUser . https) $ toK2 $ handleChangeServicePasswordAdminOnly

     , dir "adminonly" $ dir "log" $ hGetWrap (onlySuperUser . https) $ toK1 $ Administration.serveLogDirectory


     , dir "dave" $ dir "document" $ hGet $ toK1 $ Administration.daveDocument
     , dir "dave" $ dir "user"     $ hGet $ toK1 $ Administration.daveUser
     , dir "dave" $ dir "company"  $ hGet $ toK1 $ Administration.daveCompany

     -- account stuff
     , dir "logout"      $ hGet  $ toK0 $ handleLogout
     , allLocaleDirs $ const $ dir "login" $ hGet  $ toK0 $ handleLoginGet
     , allLocaleDirs $ const $ dir "login" $ hPostNoXToken $ toK0 $ handleLoginPost
     --, dir "signup"      $ hGet  $ signupPageGet
     , dir "signup"      $ hPostAllowHttp $ toK0 $ signupPagePost
     --, dir "vip"         $ hGet  $ signupVipPageGet
     --, dir "vip"         $ hPostNoXToken $ signupVipPagePost
     , dir "amnesia"     $ hPostNoXToken $ toK0 $ forgotPasswordPagePost
     , dir "amnesia"     $ hGet $ toK2 $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hPostNoXToken $ toK2 UserControl.handlePasswordReminderPost
     , dir "accountsetup"  $ hGet $ toK2 $ UserControl.handleAccountSetupGet
     , dir "accountsetup"  $ hPostNoXToken $ toK2 $ UserControl.handleAccountSetupPost
     , dir "accountremoval" $ hGet $ toK2 $ UserControl.handleAccountRemovalGet
     , dir "accountremoval" $ hPostNoXToken $ toK2 $ UserControl.handleAccountRemovalPost

     -- viral invite
     , dir "invite"      $ hPostNoXToken $ toK0 $ UserControl.handleViralInvite
     , dir "question"    $ hPostAllowHttp $ toK0 $ UserControl.handleQuestion

     -- someone wants a phone call
     , dir "phone" $ hPostAllowHttp $ toK0 $ UserControl.handlePhoneCallRequest

     -- a general purpose blank page
     --, dir "/blank" $ hGet $ toK0 $ simpleResponse ""

     , userAPI
     , integrationAPI
     , documentAPI
     -- static files
     , remainingPath GET $ msum
         [ allowHttp $ serveHTMLFiles
         , allowHttp $ serveDirectory DisableBrowsing [] "public"
         ]
     ]

{- |
    This is a helper function for routing a public dir.
-}
publicDir :: String -> String -> (Locale -> KontraLink) -> Kontra Response -> Route (Kontra Response)
publicDir swedish english link handler = choice [
    -- the correct url with region/lang/publicdir where the publicdir must be in the correct lang
    allLocaleDirs $ \locale -> dirByLang locale swedish english $ hGetAllowHttp $ handler

    -- if they use the swedish name without region/lang we should redirect to the correct swedish locale
  , dir swedish $ hGetAllowHttp $ redirectKontraResponse $ link (mkLocaleFromRegion REGION_SE)

    -- if they use the english name without region/lang we should redirect to the correct british locale
  , dir english $ hGetAllowHttp $ redirectKontraResponse $ link (mkLocaleFromRegion REGION_GB)
  ]


forAllTargetedLocales :: (Locale -> Route h) -> Route h
forAllTargetedLocales r = choice (map r targetedLocales)

allLocaleDirs :: (Locale -> Route a) -> Route a
allLocaleDirs r = forAllTargetedLocales $ \l -> regionDir l $ langDir l $ r l

regionDir :: Locale -> Route a -> Route a
regionDir = dir . codeFromRegion . getRegion

langDir :: Locale -> Route a -> Route a
langDir = dir . codeFromLang . getLang

dirByLang :: HasLocale l => l -> String -> String -> Route a -> Route a
dirByLang locale swedishdir englishdir
  | getLang locale == LANG_SE = dir swedishdir
  | otherwise = dir englishdir

handleHomepage :: Kontra (Either Response (Either KontraLink String))
handleHomepage = do
  ctx@Context{ ctxmaybeuser,ctxservice } <- getContext
  loginOn <- isFieldSet "logging"
  referer <- getField "referer"
  email   <- getField "email"
  case (ctxmaybeuser, ctxservice) of
    (Just _user, _) -> do
      response <- V.simpleResponse =<< firstPage ctx loginOn referer email
      clearFlashMsgs
      return $ Left response
    (Nothing, Nothing) -> do
      response <- V.simpleResponse =<< firstPage ctx loginOn referer email
      clearFlashMsgs
      return $ Left response
    _ -> Left <$> embeddedErrorPage

handleSitemapPage :: Kontra Response
handleSitemapPage = handleWholePage sitemapPage

handlePriceplanPage :: Kontra Response
handlePriceplanPage = handleWholePage priceplanPage

handleSecurityPage :: Kontra Response
handleSecurityPage = handleWholePage securityPage

handleLegalPage :: Kontra Response
handleLegalPage = handleWholePage legalPage

handlePrivacyPolicyPage :: Kontra Response
handlePrivacyPolicyPage = handleWholePage privacyPolicyPage

handleTermsPage :: Kontra Response
handleTermsPage = handleWholePage termsPage

handleAboutPage :: Kontra Response
handleAboutPage = handleWholePage aboutPage

handlePartnersPage :: Kontra Response
handlePartnersPage = handleWholePage partnersPage

handleClientsPage :: Kontra Response
handleClientsPage = handleWholePage clientsPage

handleContactUsPage :: Kontra Response
handleContactUsPage = handleWholePage contactUsPage

handleApiPage :: Kontra Response
handleApiPage = handleWholePage apiPage

handleScriveByMailPage :: Kontra Response
handleScriveByMailPage = handleWholePage scriveByMailPage

handleWholePage :: Kontra String -> Kontra Response
handleWholePage f = do
  content <- f
  response <- V.simpleResponse content
  clearFlashMsgs
  return response

{- |
   Handles submission of the password reset form
-}
forgotPasswordPagePost :: Kontrakcja m => m KontraLink
forgotPasswordPagePost = do
  ctx <- getContext
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- runDBQuery $ GetUserByEmail Nothing $ Email email
      case muser of
        Nothing -> do
          Log.security $ "ip " ++ (show $ ctxipnumber ctx) ++ " made a failed password reset request for non-existant account " ++ (BS.toString email)
          return LoopBack
        Just user -> do
          now <- liftIO getMinutesTime
          minv <- checkValidity now <$> (query $ GetPasswordReminder $ userid user)
          case minv of
            Just Action{ actionID, actionType = PasswordReminder { prToken, prRemainedEmails, prUserID } } ->
              case prRemainedEmails of
                0 -> addFlashM flashMessageNoRemainedPasswordReminderEmails
                n -> do
                  -- I had to make it PasswordReminder because it was complaining about not giving cases
                  -- for the constructors of ActionType
                  _ <- update $ UpdateActionType actionID (PasswordReminder { prToken          = prToken
                                                                            , prRemainedEmails = n - 1
                                                                            , prUserID         = prUserID})
                  sendResetPasswordMail ctx (LinkPasswordReminder actionID prToken) user
            _ -> do -- Nothing or other ActionTypes (which should not happen)
              link <- newPasswordReminderLink user
              sendResetPasswordMail ctx link user
          addFlashM flashMessageChangePasswordEmailSend
          return LinkUpload

sendResetPasswordMail :: Kontrakcja m => Context -> KontraLink -> User -> m ()
sendResetPasswordMail ctx link user = do
  mail <- UserView.resetPasswordMail (ctxhostpart ctx) user link
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress user] }

{- |
   Handles viewing of the signup page
-}
_signupPageGet :: Kontra Response
_signupPageGet = do
    ctx <- getContext
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja  content


_signupVipPageGet :: Kontra Response
_signupVipPageGet = do
    ctx <- getContext
    content <- liftIO (signupVipPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja content
{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}
signupPagePost :: Kontrakcja m => m KontraLink
signupPagePost = do
    Context { ctxtime } <- getContext
    signup False $ Just ((60 * 24 * 31) `minutesAfter` ctxtime)

{-
    A comment next to LoopBack says never to use it. Is this function broken?
-}
signup :: Kontrakcja m => Bool -> Maybe MinutesTime -> m KontraLink
signup vip _freetill =  do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- runDBQuery $ GetUserByEmail Nothing $ Email $ email
      case (muser, muser >>= userhasacceptedtermsofservice) of
        (Just user, Nothing) -> do
          -- there is an existing user that hasn't been activated
          -- send them another invite
          UserControl.sendNewUserMail vip user
        (Nothing, Nothing) -> do
          -- this email address is new to the system, so create the user
          -- and send an invite
          mnewuser <- UserControl.createUser (Email email) BS.empty BS.empty Nothing
          maybe (return ()) (UserControl.sendNewUserMail vip) mnewuser
        (_, _) -> return ()
      -- whatever happens we want the same outcome, we just claim we sent the activation link,
      -- because we don't want any security problems with user information leaks
      addFlashM $ modalUserSignupDone (Email email)
      return LoopBack

{- |
   Sends a new activation link mail, which is really just a new user mail.
-}
_sendNewActivationLinkMail:: Context -> User -> Kontra ()
_sendNewActivationLinkMail Context{ctxhostpart, ctxesenforcer} user = do
    let email = getEmail user
    al <- newAccountCreatedLink user
    mail <- newUserMail ctxhostpart email email al False
    scheduleEmailSendout ctxesenforcer $ mail { to = [MailAddress {fullname = email, email = email}] }

{- |
   Handles viewing of the login page
-}
handleLoginGet :: Kontrakcja m => m Response
handleLoginGet = do
  ctx <- getContext
  case ctxmaybeuser ctx of
       Just _  -> sendRedirect LinkUpload
       Nothing -> do
         referer <- getField "referer"
         email   <- getField "email"
         content <- V.pageLogin referer email
         V.renderFromBody V.TopNone V.kontrakcja content

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}
handleLoginPost :: Kontrakcja m => m KontraLink
handleLoginPost = do
    ctx <- getContext
    memail  <- getOptionalField asDirtyEmail    "email"
    mpasswd <- getOptionalField asDirtyPassword "password"
    let linkemail = maybe "" BS.toString memail
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- runDBQuery $ GetUserByEmail Nothing (Email email)
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                        Log.debug $ "User " ++ show email ++ " logged in"
                        _ <- runDBUpdate $ SetUserSettings (userid user) $ (usersettings user) {
                          locale = ctxlocale ctx
                        }
                        muuser <- runDBQuery $ GetUserByID (userid user)
                        logUserToContext muuser
                        return BackToReferer
                Just _ -> do
                        Log.debug $ "User " ++ show email ++ " login failed (invalid password)"
                        return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail
                Nothing -> do
                    Log.debug $ "User " ++ show email ++ " login failed (user not found)"
                    return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail
        _ -> return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail

{- |
   Handles the logout, and sends user back to main page.
-}
handleLogout :: Kontrakcja m => m Response
handleLogout = do
    ctx <- getContext
    logUserToContext Nothing
    sendRedirect $ LinkHome (ctxlocale ctx)

{- |
   Serves out the static html files.
-}
serveHTMLFiles :: Kontra Response
serveHTMLFiles =  do
  rq <- askRq
  let fileName = last (rqPaths rq)
  guard ((length (rqPaths rq) > 0) && (isSuffixOf ".html" fileName))
  s <- guardJustM $ (liftIO $ catch (fmap Just $ BS.readFile ("html/" ++ fileName))
                                      (const $ return Nothing))
  renderFromBody V.TopNone V.kontrakcja $ BS.toString s

