{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module RoutingTable (
    staticRoutes
  ) where

import Happstack.Server hiding (simpleHTTP, host, https, dir, path)
import Happstack.StaticRouting(Route, choice, dir, remainingPath)

import AppView
import Doc.API
import Happstack.MonadPlus (runMPlusT)
import Kontra
import KontraLink
import KontraPrelude
import LangRouting
import Login
import OAuth.Control
import PadApplication.API
import Redirect
import Routing
import Salesforce.Control as Salesforce
import User.API
import qualified Administration.AdministrationControl as Administration
import qualified Archive.Control as ArchiveControl
import qualified Attachment.Control as AttachmentControl
import qualified Branding.Control as Branding
import qualified Company.CompanyControl as Company
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import qualified Doc.DocControl as DocControl
import qualified EID.CGI.GRP.Control as EID
import qualified Payments.Control as Payments
import qualified ServerUtils.ServerUtils as ServerUtils
import qualified User.UserControl as UserControl

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

staticRoutes :: Bool -> Route (KontraPlus Response)
staticRoutes production = choice
     [  allLangDirs $                          hGet $ toK0 $ sendRedirect $ LinkDesignView
     ,  allLangDirs $  dir "localization"    $ hGet $ toK1 localizationScript
     ,  allLangDirs $  dir "pricing"         $ hGet $ toK0 priceplanPage

     -- Top level handlers - buttons on top bar, when user is logged in
     , dir "fromtemplate"                 $ hGet  $ toK0 $ DocControl.showCreateFromTemplate
     , dir "newdocument" $ hGet $ toK0 $ DocControl.handleNewDocument

     -- Sign view
     , dir "s" $ hGet $ toK0    $ sendRedirect $ LinkArchive
     , dir "s" $ hGet $ toK2    $ DocControl.handleSignShow
     , dir "s" $ hGet $ toK3    $ DocControl.handleSignShowSaveMagicHash

     -- E-ID stuff
     , dir "s" $ dir "eid" $ EID.grpRoutes

     , dir "s" $ dir "acceptaccount"  $ hPostNoXToken $ toK2 $ DocControl.handleAcceptAccountFromSign

     , dir "sp" $ hGet $ toK2 $ DocControl.handleSignPadShow
     , dir "padsign" $ hPost $ toK2 $ DocControl.handleIssueGoToSignviewPad
     , allLangDirs $ dir "to-sign" $ hGet $ toK0 $ DocControl.handlePadList
     , allLangDirs $ dir "padqueue" $ hGet $ toK0 $ return LinkPadList -- Backward compatibility, redirects back to /to-sign
     , allLangDirs $ dir "postsignview" $ hGet $ toK0 $ DocControl.handlePostSignview

     -- Simple sending
     , allLangDirs $ dir "to-start" $ hGet $ toK0 $ DocControl.handleToStart
     , dir "ts" $ hGet $ toK1 $ DocControl.handleToStartShow

     -- Attachments
     , dir "a" $ dir "rename"      $ hPost $ toK1 $ AttachmentControl.handleRename
     , dir "a" $ dir "share"       $ hPost $ toK0 $ AttachmentControl.handleShare
     , dir "a" $ dir "delete"      $ hPost $ toK0 $ AttachmentControl.handleDelete
     , dir "a"                     $ hPost $ toK0 $ AttachmentControl.handleCreateNew
     , dir "a"                     $ hGet  $ toK0 $ AttachmentControl.jsonAttachmentsList
     , dir "a"                     $ hGet  $ toK1 $ AttachmentControl.handleShow
     , dir "att"                   $ hGet  $ toK1 $ AttachmentControl.jsonAttachment
     , dir "a" $ dir "download"    $ hGet  $ toK3 $ AttachmentControl.handleDownloadAttachment

     , dir "d"                     $ hGet  $ toK0 $ ArchiveControl.showArchive
     , dir "d"                     $ hGet  $ toK1 $ DocControl.handleIssueShowGet
     , dir "d" $ dir "save"         $ hPost $ toK1 $ DocControl.handleMarkAsSaved
     , dir "d" $ dir "delete"       $ hPost $ toK0 $ ArchiveControl.handleDelete
     , dir "d" $ dir "reallydelete" $ hPost $ toK0 $ ArchiveControl.handleReallyDelete
     , dir "d" $ dir "remind"       $ hPost $ toK0 $ ArchiveControl.handleSendReminders
     , dir "d" $ dir "restore"      $ hPost $ toK0 $ ArchiveControl.handleRestore
     , dir "d" $ dir "share"        $ hPost $ toK0 $ ArchiveControl.handleShare
     , dir "d" $ dir "cancel"       $ hPost $ toK0 $ ArchiveControl.handleCancel
     , dir "d" $ dir "zip"          $ hGet  $ toK0 $ ArchiveControl.handleZip
     , dir "d" $ dir "signview"     $ hPost $ toK1 $ DocControl.handleIssueAuthorGoToSignview
     , dir "d" $ dir "evidenceattachment" $ hGet $ toK2 $ DocControl.handleEvidenceAttachment
     , dir "mailpreview"           $ hGet  $ toK2 $ DocControl.prepareEmailPreview
     , dir "download" $ hGet $ toK4 $ DocControl.handleDownloadClosedFile

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hPost $ toK2 $ DocControl.handleResend
     , dir "changeemail" $ hPost $ toK2 $ DocControl.handleChangeSignatoryEmail
     , dir "changephone" $ hPost $ toK2 $ DocControl.handleChangeSignatoryPhone

     , dir "pages"  $ hGet $ toK2 $ DocControl.showPage
     -- HTMP emails can have embedded preview image
     , dir "preview" $ hGet $ toK2 $ DocControl.showPreview
     , dir "preview" $ hGet $ toK4 $ DocControl.showPreviewForSignatory

     , dir "filepages" $ hGet $  toK1 $ DocControl.handleFilePages

     , allLangDirs $ dir "verify" $ hGet  $ toK0 $ DocControl.handleShowVerificationPage
     , allLangDirs $ dir "verify" $ hPostNoXToken $ toK0 $ DocControl.handleVerify

     -- UserControl
     , dir "account"                    $ hGet  $ toK0 $ UserControl.handleAccountGet
     , dir "account" $ hGet $ toK2 $ UserControl.handleGetChangeEmail
     , dir "account" $ hPost $ toK2 $ UserControl.handlePostChangeEmail
     , dir "account" $ dir "company" $ Company.routes
     , dir "account" $ dir "usagestats" $ dir "days"   $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserDays
     , dir "account" $ dir "usagestats" $ dir "months" $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserMonths
     , dir "accepttos" $ hGet  $ toK0 $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hPost $ toK0 $ UserControl.handleAcceptTOSPost
     , dir "account" $ dir "phoneme" $ hPostNoXToken $ toK0 $ UserControl.handleRequestPhoneCall

     --CompanyAccountsControl
     , dir "account" $ dir "companyaccounts" $ dir "add" $ hPost $ toK0 $ CompanyAccounts.handleAddCompanyAccount
     , dir "account" $ dir "companyaccounts" $ dir "resend" $ hPost $ toK0 $ CompanyAccounts.handleResendToCompanyAccount
     , dir "account" $ dir "companyaccounts" $ dir "changerole" $ hPost $ toK0 $ CompanyAccounts.handleChangeRoleOfCompanyAccount
     , dir "account" $ dir "companyaccounts" $ dir "remove" $ hPost $ toK0 $ CompanyAccounts.handleRemoveCompanyAccount

     , dir "companyaccounts" $ hGet  $ toK0 $ CompanyAccounts.handleCompanyAccounts
     , dir "companyaccounts" $ dir "join" $ hGet $ toK1 $ CompanyAccounts.handleGetBecomeCompanyAccount
     , dir "companyaccounts" $ dir "join" $ hPost $ toK1 $ CompanyAccounts.handlePostBecomeCompanyAccount

     , dir "payments" $ dir "newsubscription" $ hPost $ toK0 $ Payments.handleSyncNewSubscriptionWithRecurly
     , dir "payments" $ dir "changeplan" $ hPost $ toK0 $ Payments.handleChangePlan
     , dir "payments" $ dir "postback" $ hPostNoXToken $ toK0 $ Payments.handleRecurlyPostBack

     -- price plan page information
     , dir "payments" $ dir "pricepageinfo" $ hGetAllowHttp $ toK0 $ Payments.handlePricePageJSON
     , dir "payments" $ dir "userexists" $ hGetAllowHttp $ toK0 $ Payments.handleUserExists
     , dir "payments" $ dir "newsubscriptionoutside" $ hPostAllowHttp $ toK0 $ Payments.handleSyncNewSubscriptionWithRecurlyOutside

     -- account stuff
     , allLangDirs $ dir "enter" $ hGet $ toK0 $ handleLoginGet
     , dir "logout"      $ hGet  $ toK0 $ handleLogout
     , dir "logout_ajax" $ hGet  $ toK0 $ handleLogoutAJAX
     , allLangDirs $ dir "login" $ hGet $ toK0 $ LinkLoginDirect <$> ctxlang <$> getContext -- Drop this after EE is migrated
     , dir "login" $ hPostNoXToken $ toK0 $ handleLoginPost
     , allLangDirs $ dir "signup"      $ hGetAllowHttp $ toK0 $ LinkSignup <$> ctxlang <$> getContext -- Drop this after EE is migrated
     , allLangDirs $ dir "signup"      $ hPostNoXTokenHttp $ toK0 $ apiCallSignup -- Drop handler after this comment gets to prod, and EE routs gets fixed to use API
     , allLangDirs $ dir "amnesia"     $ hGet $ toK2 $ UserControl.handlePasswordReminderGet
     , allLangDirs $ dir "amnesia"     $ hPostNoXToken $ toK2 UserControl.handlePasswordReminderPost
     , allLangDirs $ dir "mynewaccount"  $ hGet $ toK2 $ UserControl.handleAccessNewAccountGet
     , allLangDirs $ dir "mynewaccount"  $ hPostNoXToken $ toK2 $ UserControl.handleAccessNewAccountPost
     , allLangDirs $ dir "accountsetup"  $ hGet $ toK3 $ UserControl.handleAccountSetupGet
     , allLangDirs $ dir "accountsetup"  $ hPostNoXToken $ toK3 $ UserControl.handleAccountSetupPost

     , dir "payments" $ dir "contact" $ hPostAllowHttp $ toK0 $ UserControl.handleContactUs

     , dir "salesforce" $ dir "integration" $ hGet $ toK0 $ Salesforce.handleSalesforceIntegration
     , dir "salesforce" $ dir "keys"        $ hGet $ toK0 $ Salesforce.getSalesforceKeys

     , dir "adminonly" $ Administration.adminonlyRoutes
     , dir "dave"      $ Administration.daveRoutes

     , allLangDirs $ dir "unsupported_browser" $ hGet $ toK0 $ unsupportedBrowserPage
     , allLangDirs $ dir "enable-cookies" $ dir "enable-cookies.html" $ hGetAllowHttp $ toK0 $ enableCookiesPage
     , allLangDirs $ dir "terms" $ hGet $ toK0 $ handleTermsOfService
     , documentAPI
     , userAPI
     , padApplicationAPI
     , oauth
     , remainingPath GET $ (runMPlusT $ serveDirectory DisableBrowsing [] staticDir) >>= maybe respond404 return

     -- public services
     , dir "parsecsv"        $ hPost $ toK0 $ ServerUtils.handleParseCSV
     , dir "serialize_image" $ hPost $ toK0 $ ServerUtils.handleSerializeImage
     , dir "scale_image" $ hPost $ toK0 $ ServerUtils.handleScaleImage
     , dir "text_to_image" $ hGet $ toK0 $ ServerUtils.handleTextToImage
     , dir "colored_image" $ hGet $ toK0 $ ServerUtils.brandedImage


     , dir "document_signview_branding" $ hGet $ toK4 $ Branding.handleSignviewBranding
     , dir "padlist_signview_branding" $ hGet $ toK2 $ Branding.handleSignviewBrandingWithoutDocument
     , dir "internal_signview_branding" $ hGet $ toK2 $ Branding.handleSignviewBrandingInternal
     , dir "service_branding" $ hGet $ toK2 $ Branding.handleServiceBranding
     , dir "scrive_branding" $ hGet $ toK2 $ Branding.handleScriveBranding
     , dir "login_branding" $ hGet $ toK2 $ Branding.handleLoginBranding
     , dir "domain_branding" $ hGet $ toK2 $ Branding.handleDomainBranding
     , dir "login_logo" $  hGet $ toK1 $ Branding.loginLogo
     , dir "service_logo" $  hGet $ toK1 $ Branding.serviceLogo
     , dir "signview_logo" $  hGet $ toK3 $ Branding.signviewLogo
     , dir "signview_logo_without_document" $  hGet $ toK1 $ Branding.signviewLogoWithoutDocument
     , dir "email_logo" $  hGet $ toK1 $ Branding.emailLogo
     , dir "email_logo" $  hGet $ toK3 $ Branding.emailLogoForSignatory
     , dir "favicon" $  hGet $ toK1 $ Branding.faviconIcon
   ]
  where
    staticDir = if (production)
                  then "frontend/dist"
                  else "frontend/app"
