{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module RoutingTable (
    staticRoutes
  ) where

import Kontra
import KontraLink
import Login
import Redirect
import Routing
import Happstack.StaticRouting(Route, choice, dir, param, remainingPath)
import qualified Administration.AdministrationControl as Administration
import qualified Company.CompanyControl as Company
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import qualified Doc.DocControl as DocControl
import qualified Archive.Control as ArchiveControl
import qualified ELegitimation.Control as BankID
import qualified User.UserControl as UserControl
import qualified ScriveByMail.Control as MailAPI
import qualified Payments.Control as Payments
import qualified Attachment.Control as AttachmentControl
import qualified EvidenceLog.Control as EvidenceLog
import Doc.API
import OAuth.Control
import LangRouting
import Happstack.Server hiding (simpleHTTP, host, https, dir, path)
import AppView
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
     [  allLangDirs $                          hGet $ toK0 $ sendRedirect $ LinkDesignView
     ,  allLangDirs $  dir "localization"    $ hGet $ toK1 localizationScript
     ,  allLangDirs $  dir "pricing"         $ hGet $ toK0 priceplanPage
     -- this is SMTP to HTTP gateway
     , dir "mailapi" $ hPostNoXToken             $ toK0 $ MailAPI.handleMailAPI
     , dir "mailapi" $ dir "confirmdelay" $ hGet $ toK3 $ MailAPI.handleConfirmDelay

     -- Only download function | unified for author and signatories
     , dir "download"                     $ hGet  $ toK2 $ DocControl.handleDownloadFile


     , dir "fromtemplate"                 $ hGet  $ toK0 $ DocControl.showCreateFromTemplate
     
     , dir "s" $ dir "eleg" $ hGet $ toK2 $ BankID.generateBankIDTransaction
     , dir "s" $ dir "eleg" $ dir "mbi" $ hPostNoXToken $ toK2 $ BankID.initiateMobileBankID
     , dir "s" $ dir "eleg" $ dir "mbi" $ hGet  $ toK2 $ BankID.collectMobileBankID
     , dir "s" $ hGet $ toK0    $ sendRedirect $ LinkArchive
     , dir "s" $ hGet $ toK2    $ DocControl.handleSignShow
     , dir "s" $ hGet $ toK3    $ DocControl.handleSignShowSaveMagicHash

     , dir "s" $ param "sign"           $ hPostNoXToken $ toK2 $ DocControl.signDocument
     , dir "s" $ param "sign"           $ hPostNoXToken $ toK3 $ DocControl.signDocumentIphoneCase
     , dir "s" $ param "reject"         $ hPostNoXToken $ toK2 $ DocControl.rejectDocument
     , dir "s" $ param "reject"         $ hPostNoXToken $ toK3 $ DocControl.rejectDocumentIphoneCase
     , dir "s" $ param "acceptaccount"  $ hPostNoXToken $ toK2 $ DocControl.handleAcceptAccountFromSign
     , dir "s" $ param "sigattachment"  $ hPostNoXToken $ toK2 $ DocControl.handleSigAttach
     , dir "s" $ param "deletesigattachment" $ hPostNoXToken $ toK2 $ DocControl.handleDeleteSigAttach

     
     , dir "lang" $ hPostNoXToken $ toK0 $ UserControl.handlePostUserLang
     , dir "a" $ dir "rename"      $ hPost $ toK1 $ AttachmentControl.handleRename
     , dir "a" $ dir "share"       $ hPost $ toK0 $ AttachmentControl.handleShare
     , dir "a" $ dir "delete"      $ hPost $ toK0 $ AttachmentControl.handleDelete
     , dir "a"                     $ hPost $ toK0 $ AttachmentControl.handleCreateNew
     , dir "a"                     $ hGet  $ toK0 $ AttachmentControl.jsonAttachmentsList
     , dir "a"                     $ hGet  $ toK1 $ AttachmentControl.handleShow
     , dir "att"                   $ hGet  $ toK1 $ AttachmentControl.jsonAttachment

     , dir "newdocument" $ hGet $ toK0 $ DocControl.handleNewDocument
     
     , dir "d"                     $ hGet  $ toK0 $ ArchiveControl.showArchive
     , dir "d"                     $ hGet  $ toK1 $ DocControl.handleIssueShowGet
     , dir "d" $ dir "eleg"        $ hGet  $ toK1 $ BankID.generateBankIDTransactionForAuthor
     , dir "d" $ dir "eleg" $ dir "mbi" $ hPostNoXToken $ toK1 $ BankID.initiateMobileBankIDForAuthor
     , dir "d" $ dir "eleg" $ dir "mbi" $ hGet  $ toK1 $ BankID.collectMobileBankIDForAuthor
     , dir "d" $ dir "save"         $ hPost $ toK1 $ DocControl.handleMarkAsSaved
     , dir "d" $ dir "delete"       $ hPost $ toK0 $ ArchiveControl.handleDelete
     , dir "d" $ dir "remind"       $ hPost $ toK0 $ ArchiveControl.handleSendReminders
     , dir "d" $ dir "restore"      $ hPost $ toK0 $ ArchiveControl.handleRestore
     , dir "d" $ dir "reallydelete" $ hPost $ toK0 $ ArchiveControl.handleReallyDelete
     , dir "d" $ dir "share"        $ hPost $ toK0 $ ArchiveControl.handleShare
     , dir "d" $ dir "cancel"       $ hPost $ toK0 $ ArchiveControl.handleCancel
     , dir "d" $ dir "zip"          $ hGet  $ toK0 $ ArchiveControl.handleZip
     , dir "d" $ dir "signview"     $ hPost $ toK1 $ DocControl.handleIssueAuthorGoToSignview
     , dir "d"                     $ hPost $ toK1 $ DocControl.handleIssueShowPost
     , dir "d" $ dir "evidencelog" $ hGet  $ toK1 $ EvidenceLog.jsonDocumentEvidenceLog
     , dir "docs"                  $ hGet  $ toK0 $ ArchiveControl.jsonDocumentsList
     , dir "setattachments"        $ hPost $ toK1 $ DocControl.handleSetAttachments -- Since setting attachments can have file upload, we need extra handler for it.
     , dir "parsecsv"              $ hPost $ toK0 $ DocControl.handleParseCSV
     , dir "mailpreview"           $ hGet  $ toK2 $ DocControl.prepareEmailPreview

     , dir "blockinginfo"          $ hGet $ toK0 $ UserControl.handleBlockingInfo

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hPost $ toK2 $ DocControl.handleResend
     , dir "changeemail" $ hPost $ toK2 $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hPost $ DocControl.handleWithdrawn
     , dir "restart" $ hPost $ toK1 $ DocControl.handleRestart

     , dir "pages"  $ hGet $ toK2 $ DocControl.showPage
     -- HTMP emails can have embedded preview image
     , dir "preview" $ hGet $ toK2 $ DocControl.showPreview
     , dir "preview" $ hGet $ toK4 $ DocControl.showPreviewForSignatory

     , dir "filepages" $ hGet $  toK1 $ DocControl.handleFilePages

     , allLangDirs $ dir "verify" $ hGet  $ toK0 $ DocControl.handleShowVerificationPage
     , allLangDirs $ dir "verify" $ hPostNoXToken $ toK0 $ DocControl.handleVerify

     , dir "padqueue" $ dir "state" $ hGet $ toK0 $ PadQueue.padQueueState
     , dir "padqueue" $ hGet $ toK0 $ PadQueue.showPadQueuePage
     , dir "padqueue" $ dir "logout" $ hPostNoXToken $ toK0 $ PadQueue.handlePadLogout

     -- UserControl
     , dir "account"                    $ hGet  $ toK0 $ UserControl.handleAccountGet
     , dir "account" $ dir "json"       $ hGet  $ toK0 $ UserControl.getUserJSON
     , dir "account"                    $ hPost $ toK0 $ UserControl.handleUserPost
     , dir "account" $ hGet $ toK2 $ UserControl.handleGetChangeEmail
     , dir "account" $ hPost $ toK2 $ UserControl.handlePostChangeEmail
     , dir "account" $ dir "security" $ hPost $ toK0 $ UserControl.handlePostUserSecurity
     , dir "account" $ dir "company" $ Company.routes
     , dir "account" $ dir "mailapi" $ hPost $ toK0 $ UserControl.handlePostUserMailAPI
     , dir "account" $ dir "usagestats" $ dir "days"   $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserDays
     , dir "account" $ dir "usagestats" $ dir "months" $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserMonths
     , dir "accepttos" $ hGet  $ toK0 $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hPost $ toK0 $ UserControl.handleAcceptTOSPost
     , dir "account" $ dir "phoneme" $ hPostNoXToken $ toK0 $ UserControl.handleRequestPhoneCall

     --CompanyAccountsControl
     , dir "account" $ dir "companyaccounts" $ hPost $ toK0 $ CompanyAccounts.handlePostCompanyAccounts
     , dir "companyaccounts" $ hGet  $ toK0 $ CompanyAccounts.handleCompanyAccounts
     , dir "companyaccounts" $ dir "join" $ hGet $ toK1 $ CompanyAccounts.handleGetBecomeCompanyAccount
     , dir "companyaccounts" $ dir "join" $ hPost $ toK1 $ CompanyAccounts.handlePostBecomeCompanyAccount

     , dir "payments" $ dir "newsubscription" $ hPost $ toK0 $ Payments.handleSyncNewSubscriptionWithRecurly
     , dir "payments" $ dir "changeplan" $ hPost $ toK0 $ Payments.handleChangePlan
     , dir "payments" $ dir "postback" $ hPostNoXToken $ toK0 $ Payments.handleRecurlyPostBack

     -- price plan page information
     , dir "payments" $ dir "pricepageinfo" $ hGetAllowHttp $ toK0 $ Payments.handlePricePageJSON
     , dir "payments" $ dir "userexists" $ hGetAllowHttp $ toK0 $ Payments.handleUserExists
     , dir "payments" $ dir "createuser" $ hPostAllowHttp $ toK0 $ Payments.handleCreateUser
     , dir "payments" $ dir "newsubscriptionoutside" $ hPostAllowHttp $ toK0 $ Payments.handleSyncNewSubscriptionWithRecurlyOutside

     -- account stuff
     , dir "logout"      $ hGet  $ toK0 $ handleLogout
     , allLangDirs $ dir "login" $ hGet $ toK0 $ handleLoginGet
     , dir "login" $ hPostNoXToken $ toK0 $ handleLoginPost
     , allLangDirs $ dir "signup"      $ hGetAllowHttp $ toK0 $ signupPageGet
     , allLangDirs $ dir "signup"      $ hPostNoXTokenHttp $ toK0 $ signupPagePost
     , dir "amnesia"     $ hPostNoXToken $ toK0 $ forgotPasswordPagePost
     , allLangDirs $ dir "amnesia"     $ hGet $ toK2 $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hPostNoXToken $ toK2 UserControl.handlePasswordReminderPost
     , allLangDirs $ dir "accountsetup"  $ hGet $ toK2 $ UserControl.handleAccountSetupGet
     , allLangDirs $ dir "accountsetup"  $ hGet $ toK3 $ UserControl.handleAccountSetupGetWithMethod
     -- This can go away ~14 days after this code reaches production
     , allLangDirs $ dir "accountsetup"  $ hPostNoXToken $ toK2 $ UserControl.handleAccountSetupPost
     , allLangDirs $ dir "accountsetup"  $ hPostNoXToken $ toK3 $ UserControl.handleAccountSetupPostWithMethod

     , dir "payments" $ dir "contact" $ hPostAllowHttp $ toK0 $ UserControl.handleContactUs

     , dir "adminonly" $ Administration.adminonlyRoutes
     , dir "dave"      $ Administration.daveRoutes
     , documentAPI
     , oauth
     , remainingPath GET $ allowHttp $ serveDirectory DisableBrowsing [] "public"

     -- public services
     , dir "serialize_image" $ hPost $ toK0 $ Company.handleSerializeImage
   ]
