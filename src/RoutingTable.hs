{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module RoutingTable (
    staticRoutes
  ) where

import API.IntegrationAPI
import API.Service.ServiceControl

import Kontra
import KontraLink
import LocaleRouting (allLocaleDirs)
import Login
import PublicPages (publicPages)
import Redirect
import Routing
import Happstack.StaticRouting(Route, choice, dir, param, remainingPath)
import qualified Stats.Control as Stats
import qualified ActionQueue.UserAccountRequest as UAR
import qualified Administration.AdministrationControl as Administration
import qualified Company.CompanyControl as Company
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import qualified Doc.DocControl as DocControl
import qualified Archive.Control as ArchiveControl
import qualified ELegitimation.BankID as BankID
import qualified User.UserControl as UserControl
import qualified ScriveByMail.Control as MailAPI
import qualified Attachment.Control as AttachmentControl
import Doc.API
import OAuth.Control

import Happstack.Server hiding (simpleHTTP, host, https, dir, path)

import qualified PadQueue.Control as PadQueue

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

staticRoutes :: Route (KontraPlus Response)
staticRoutes = choice
     [ hGetAllowHttp $ getContext >>= (redirectKontraResponse . LinkHome . ctxlocale)

     , publicPages

     -- this is SMTP to HTTP gateway
     , dir "mailapi" $ hPostNoXToken             $ toK0 $ MailAPI.handleMailAPI
     , dir "mailapi" $ dir "confirmdelay" $ hGet $ toK3 $ MailAPI.handleConfirmDelay

     -- Only download function | unified for author and signatories
     , dir "download"                     $ hGet  $ toK2 $ DocControl.handleDownloadFile
     , dir "downloadmainfile"             $ hGet  $ toK2 $ DocControl.handleDownloadMainFile

     , dir "s" $ dir "eleg" $ hGet $ toK2 $ BankID.generateBankIDTransaction
     , dir "s" $ dir "eleg" $ dir "mbi" $ hPostNoXToken $ toK2 $ BankID.initiateMobileBankID
     , dir "s" $ dir "eleg" $ dir "mbi" $ hGet  $ toK2 $ BankID.collectMobileBankID
     , dir "s" $ hGet $ toK0    $ sendRedirect $ LinkArchive
     , dir "s" $ hGet $ toK2    $ DocControl.handleSignShow
     , dir "s" $ hGet $ toK3    $ DocControl.handleSignShowOldRedirectToNew -- Redirect for old version to version above, remove not earlier then 31.12.2012.

     , dir "s" $ param "sign"           $ hPostNoXToken $ toK2 $ DocControl.signDocument
     , dir "s" $ param "sign"           $ hPostNoXToken $ toK3 $ DocControl.signDocumentIphoneCase
     , dir "s" $ param "reject"         $ hPostNoXToken $ toK2 $ DocControl.rejectDocument
     , dir "s" $ param "reject"         $ hPostNoXToken $ toK3 $ DocControl.rejectDocumentIphoneCase
     , dir "s" $ param "acceptaccount"  $ hPostNoXToken $ toK3 $ DocControl.handleAcceptAccountFromSign
     , dir "s" $ param "sigattachment"  $ hPostNoXToken $ toK2 $ DocControl.handleSigAttach
     , dir "s" $ param "deletesigattachment" $ hPostNoXToken $ toK2 $ DocControl.handleDeleteSigAttach

     --Q: This all needs to be done by author. Why we dont check it
     --here? MR

     --A: Because this table only contains routing logic. The logic of
     --what it does/access control is left to the handler. EN
     , dir "upload" $ hGet $ toK0 $ DocControl.handleShowUploadPage
     , dir "locale" $ hPost $ toK0 $ UserControl.handlePostUserLocale
     , dir "a" $ dir "rename"      $ hPost $ toK1 $ AttachmentControl.handleRename
     , dir "a" $ dir "share"       $ hPost $ toK0 $ AttachmentControl.handleShare
     , dir "a" $ dir "delete"      $ hPost $ toK0 $ AttachmentControl.handleDelete
     , dir "a"                     $ hPost $ toK0 $ AttachmentControl.handleCreateNew
     , dir "a"                     $ hGet  $ toK0 $ AttachmentControl.jsonAttachmentsList
     , dir "a"                     $ hGet  $ toK1 $ AttachmentControl.handleShow
     , dir "att"                   $ hGet  $ toK1 $ AttachmentControl.jsonAttachment

     , dir "t" $ param "template" $ hPost $ toK0 $ DocControl.handleCreateFromTemplate
     , dir "t" $ hPost $ toK0 $ DocControl.handleCreateNewTemplate

     , dir "d"                     $ hGet  $ toK0 $ ArchiveControl.showArchive
     , dir "d"                     $ hGet  $ toK1 $ DocControl.handleIssueShowGet
     , dir "d" $ dir "eleg"        $ hGet  $ toK1 $ BankID.generateBankIDTransactionForAuthor
     , dir "d" $ dir "eleg" $ dir "mbi" $ hPostNoXToken $ toK1 $ BankID.initiateMobileBankIDForAuthor
     , dir "d" $ dir "eleg" $ dir "mbi" $ hGet  $ toK1 $ BankID.collectMobileBankIDForAuthor
     , dir "d" $ {- param "doc" $ -} hPost $ toK0 $ DocControl.handleIssueNewDocument
     , dir "d" $ dir "delete"       $ hPost $ toK0 $ ArchiveControl.handleDelete
     , dir "d" $ dir "remind"       $ hPost $ toK0 $ ArchiveControl.handleSendReminders
     , dir "d" $ dir "restore"      $ hPost $ toK0 $ ArchiveControl.handleRestore
     , dir "d" $ dir "reallydelete" $ hPost $ toK0 $ ArchiveControl.handleReallyDelete
     , dir "d" $ dir "share"        $ hPost $ toK0 $ ArchiveControl.handleShare
     , dir "d" $ dir "cancel"       $ hPost $ toK0 $ ArchiveControl.handleCancel
     , dir "d"                     $ hPost $ toK1 $ DocControl.handleIssueShowPost
     , dir "docs"                  $ hGet  $ toK0 $ ArchiveControl.jsonDocumentsList
     , dir "doc"                   $ hGet  $ toK1 $ DocControl.jsonDocument
     , dir "save"                  $ hPost $ toK1 $ DocControl.handleSaveDraft
     , dir "setattachments"        $ hPost $ toK1 $ DocControl.handleSetAttachments -- Since setting attachments can have file upload, we need extra handler for it.
     , dir "parsecsv"              $ hPost $ toK0 $ DocControl.handleParseCSV
     , dir "mailpreview"           $ hGet  $ toK2 $ DocControl.prepareEmailPreview

     , dir "df"                    $ hGet  $ toK2 $ DocControl.handleFileGet

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hPost $ toK2 $ DocControl.handleResend
     , dir "changeemail" $ hPost $ toK2 $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hPost $ DocControl.handleWithdrawn
     , dir "restart" $ hPost $ toK1 $ DocControl.handleRestart

     , dir "pages"  $ hGetAjax $ toK2 $ DocControl.showPage
     -- HTMP emails can have embedded preview image
     , dir "preview" $ hGet $ toK2 $ DocControl.showPreview
     , dir "preview" $ hGet $ toK4 $ DocControl.showPreviewForSignatory

     , dir "template"  $ hPost $ toK0 $ DocControl.handleCreateFromTemplate

     , dir "filepages" $ hGetAjax $  toK1 $ DocControl.handleFilePages

     , dir "csvlandpage" $ hGet $ toK1 $ DocControl.handleCSVLandpage

     , dir "verify" $ hGet  $ toK0 $ DocControl.handleShowVerificationPage
     , dir "verify" $ hPostNoXToken $ toK0 $ DocControl.handleVerify
     
     , dir "padqueue" $ dir "add" $ hPost $ toK2 $ PadQueue.addToQueue
     , dir "padqueue" $ dir "clear" $ hPost $ toK0 $ PadQueue.clearQueue

     , dir "padqueue" $ dir "state" $ hGet $ toK0 $ PadQueue.padQueueState
     , dir "padqueue" $ hGet $ toK0 $ PadQueue.showPadQueuePage
     , dir "padqueue" $ dir "archive" $ hGet $ toK0 $ ArchiveControl.showPadDeviceArchive
     , dir "padqueue" $ dir "login"  $ hPostNoXToken $ toK0 $ PadQueue.handlePadLogin
     , dir "padqueue" $ dir "logout" $ hPostNoXToken $ toK0 $ PadQueue.handlePadLogout

     -- UserControl
     , dir "account"                    $ hGet  $ toK0 $ UserControl.handleUserGet
     , dir "account"                    $ hPost $ toK0 $ UserControl.handleUserPost
     , dir "account" $ hGet $ toK2 $ UserControl.handleGetChangeEmail
     , dir "account" $ hPost $ toK2 $ UserControl.handlePostChangeEmail
     , dir "account" $ dir "security" $ hGet $ toK0 $ UserControl.handleGetUserSecurity
     , dir "account" $ dir "security" $ hPost $ toK0 $ UserControl.handlePostUserSecurity
     , dir "account" $ dir "company" $ Company.routes
     , dir "account" $ dir "mailapi" $ hGet $ toK0 $ UserControl.handleGetUserMailAPI
     , dir "account" $ dir "mailapi" $ hPost $ toK0 $ UserControl.handlePostUserMailAPI
     , dir "account" $ dir "usagestats" $ hGet $ toK0 $ UserControl.handleUsageStatsForUser
     , dir "account" $ dir "usagestats" $ dir "days"   $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserDays
     , dir "account" $ dir "usagestats" $ dir "months" $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserMonths
     , dir "accepttos" $ hGet  $ toK0 $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hPost $ toK0 $ UserControl.handleAcceptTOSPost
     , dir "account" $ dir "phoneme" $ hPostNoXToken $ toK0 $ UserControl.handleRequestPhoneCall

     --CompanyAccountsControl
     , dir "account" $ dir "companyaccounts" $ hGet  $ toK0 $ CompanyAccounts.handleGetCompanyAccounts
     , dir "account" $ dir "companyaccounts" $ hPost $ toK0 $ CompanyAccounts.handlePostCompanyAccounts
     , dir "companyaccounts" $ hGet  $ toK0 $ CompanyAccounts.handleCompanyAccounts
     , dir "companyaccounts" $ dir "join" $ hGet $ toK1 $ CompanyAccounts.handleGetBecomeCompanyAccount
     , dir "companyaccounts" $ dir "join" $ hPost $ toK1 $ CompanyAccounts.handlePostBecomeCompanyAccount

     -- super user only
     , dir "createuser" $ hPost $ toK0 $ Administration.handleCreateUser
     , dir "adminonly" $ hGet $ toK0 $ Administration.showAdminMainPage
     , dir "adminonly" $ dir "useradminforsales" $ hGet $ toK0 $ Administration.showAdminUsersForSales
     , dir "adminonly" $ dir "userslist" $ hGet $ toK0 $ Administration.jsonUsersList
     , dir "adminonly" $ dir "useradmin" $ hGet $ toK1 $ Administration.showAdminUsers . Just
     , dir "adminonly" $ dir "useradmin" $ hGet $ toK0 $ Administration.showAdminUsers Nothing
     , dir "adminonly" $ dir "useradmin" $ dir "usagestats" $ hGet $ toK1 $ Stats.showAdminUserUsageStats
     , dir "adminonly" $ dir "useradmin" $ hPost $ toK1 $ Administration.handleUserChange
     , dir "adminonly" $ dir "companyadmin" $ hGet $ toK0 $ Administration.showAdminCompanies
     , dir "adminonly" $ dir "companyadmin" $ hGet $ toK1 $ Administration.showAdminCompany
     , dir "adminonly" $ dir "companyadmin" $ dir "branding" $ Company.adminRoutes
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hGet $ toK1 $ Administration.showAdminCompanyUsers
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hPost $ toK1 $ Administration.handlePostAdminCompanyUsers
     , dir "adminonly" $ dir "companyaccounts" $ hGet  $ toK1 $ CompanyAccounts.handleCompanyAccountsForAdminOnly
     , dir "adminonly" $ dir "companyadmin" $ dir "usagestats" $ hGet $ toK1 $ Stats.showAdminCompanyUsageStats
     , dir "adminonly" $ dir "companyadmin" $ hPost $ toK1 $ Administration.handleCompanyChange
     , dir "adminonly" $ dir "functionalitystats" $ hGet $ toK0 $ Administration.showFunctionalityStats

     , dir "adminonly" $ dir "documents" $ hGet $ toK0 $ Administration.showDocuments
     , dir "adminonly" $ dir "documentslist" $ hGet $ toK0 $ Administration.jsonDocuments

     , dir "adminonly" $ dir "allstatscsv" $ hGet $ toK0 $ Stats.handleDocStatsCSV
     , dir "adminonly" $ dir "userstatscsv" $ hGet $ toK0 $ Stats.handleUserStatsCSV
     , dir "adminonly" $ dir "signstatscsv" $ hGet $ toK0 $ Stats.handleSignStatsCSV
     , dir "adminonly" $ dir "dochistorycsv" $ hGet $ toK0 $ Stats.handleDocHistoryCSV
     , dir "adminonly" $ dir "signhistorycsv" $ hGet $ toK0 $ Stats.handleSignHistoryCSV
     , dir "adminonly" $ dir "userslistcsv" $ hGet $ toK0 $ Administration.handleUsersListCSV

     , dir "adminonly" $ dir "statistics"        $ hGet  $ toK0 $ Stats.showAdminSystemUsageStats

     , dir "adminonly" $ dir "services" $ hGet $ toK0 $ Administration.showServicesPage
     , dir "adminonly" $ dir "services" $ param "create" $ hPost $ toK0 $ Administration.handleCreateService

     , dir "adminonly" $ dir "companies" $ hGet $ toK0 $ Administration.jsonCompanies

     , dir "adminonly" $ dir "reseal" $ hPost $ toK1 $ Administration.resealFile
     , dir "adminonly" $ dir "replacemainfile" $ hPost $ toK1 $ Administration.replaceMainFile

     , dir "adminonly" $ dir "docproblems" $ hGet $ toK0 $ DocControl.handleInvariantViolations

     , dir "adminonly" $ dir "backdoor" $ hGet $ toK1 $ Administration.handleBackdoorQuery

     , dir "services" $ hGet $ toK0 $ handleShowServiceList
     , dir "services" $ hGet $ toK1 $ handleShowService
     , dir "services" $ dir "ui" $ hPost $ toK1 $ handleChangeServiceUI
     , dir "services" $ dir "password" $ hPost $ toK1 $ handleChangeServicePassword
     , dir "services" $ dir "settings" $ hPost $ toK1 $ handleChangeServiceSettings
     , dir "services" $ dir "logo" $ hGet $ toK1 $ handleServiceLogo
     , dir "services" $ dir "buttons_body" $ hGet $ toK1 $ handleServiceButtonsBody
     , dir "services" $ dir "buttons_rest" $ hGet $ toK1 $ handleServiceButtonsRest

     -- never ever use this
     , dir "adminonly" $ dir "neveruser" $ dir "resetservicepassword" $ hGetWrap (onlyAdmin . https) $ toK2 $ handleChangeServicePasswordAdminOnly

     , dir "adminonly" $ dir "log" $ hGetWrap (onlyAdmin . https) $ toK1 $ Administration.serveLogDirectory

     , dir "adminonly" $ dir "updatefields" $ hPost $ toK2 $ Administration.updateFields

     , dir "dave" $ dir "document"      $ hGet $ toK1 $ Administration.daveDocument
     , dir "dave" $ dir "document"      $ hGet $ toK2 $ Administration.daveSignatoryLink
     , dir "dave" $ dir "user"          $ hGet $ toK1 $ Administration.daveUser
     , dir "dave" $ dir "userhistory"   $ hGet $ toK1 $ Administration.daveUserHistory
     , dir "dave" $ dir "company"       $ hGet $ toK1 $ Administration.daveCompany

     -- account stuff
     , dir "logout"      $ hGet  $ toK0 $ handleLogout
     , allLocaleDirs $ const $ dir "login" $ hGet  $ toK0 $ handleLoginGet
     , allLocaleDirs $ const $ dir "login" $ hPostNoXToken $ toK0 $ handleLoginPost
     , dir "signup"      $ hPostAllowHttp $ toK0 $ signupPagePost
     , dir "amnesia"     $ hPostNoXToken $ toK0 $ forgotPasswordPagePost
     , dir "amnesia"     $ hGet $ toK2 $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hPostNoXToken $ toK2 UserControl.handlePasswordReminderPost
     , dir "accountsetup"  $ hGet $ toK2 $ UserControl.handleAccountSetupGet
     , dir "accountsetup"  $ hPostNoXToken $ toK2 $ UserControl.handleAccountSetupPost
 
     -- question form on static pages
     , dir "question"    $ hPostAllowHttp $ toK0 $ UserControl.handleQuestion

     , integrationAPI
     , documentAPI
     , oauthAPI
     , remainingPath GET $ allowHttp $ serveDirectory DisableBrowsing [] "public"

     -- to be removed after 15.07.2012 (see ActionQueue.UserAccountRequest)
     , dir "populate_uar" $ hGet $ toK0 $ UAR.populateUARTable
   ]
