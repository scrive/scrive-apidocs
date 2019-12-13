{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module RoutingTable (
    staticRoutes
  ) where

import Happstack.Server hiding (dir, host, https, path, simpleHTTP)
import Happstack.StaticRouting (Route, choice, dir, remainingPath)

import AccessControl.API
import API.V2 (noAPIV2CallFoundHandler)
import AppView
import Attachment.API
import Doc.API
import Folder.API
import Happstack.Server.ReqHandler
import Kontra
import KontraLink
import LangRouting
import Login
import Monitor.API
import OAuth.Control
import PadApplication.API
import Partner.API
import Redirect
import Routing
import Salesforce.Control as Salesforce
import User.API
import User.APILog.API
import UserGroup.API
import qualified Administration.AdministrationControl as Administration
import qualified Archive.Control as ArchiveControl
import qualified Branding.Control as Branding
import qualified Company.CompanyControl as Company
import qualified Doc.DocControl as DocControl
import qualified EID.CGI.GRP.Control as CGI
import qualified EID.EIDService.Control as EIDService
import qualified EID.Nets.Control as NETS
import qualified ServerUtils.ServerUtils as ServerUtils
import qualified User.UserControl as UserControl
import qualified UserGroupAccounts.UserGroupAccountsControl as UserGroupAccounts

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

staticRoutes :: Bool -> Route (Kontra Response)
staticRoutes production = choice
  [ allLangDirs $ hGet $ toK0 $ handleLoginGet
  , allLangDirs $ dir "pricing" $ hGet $ toK0 $ sendRedirect $ LinkPermanentRedirect
    "/services/online/pricing/"

     -- Top level handlers - buttons on top bar, when user is logged in
  , dir "fromtemplate" $ hGet $ toK0 $ DocControl.showCreateFromTemplate
  , dir "newdocument" $ hGet $ toK0 $ DocControl.handleNewDocument
     -- This is a special hack for RBS (CORE-1081)
  , dir "newdocumentwithbpid" $ hPost $ toK0 $ DocControl.handleNewDocumentWithBPID

     -- Sign view
  , dir "s" $ hGet $ toK0 $ sendRedirect $ LinkArchive
  , dir "s" $ hGet $ toK2 $ DocControl.handleSignShow
  , dir "s" $ hGet $ toK3 $ DocControl.handleSignShowSaveMagicHash
  , dir "t" $ hGet $ toK2 $ DocControl.handleSignFromTemplate

     -- E-ID stuff
  , dir "s" $ dir "eid" $ CGI.grpRoutes
  , dir "nets" $ NETS.netsRoutes
  , dir "eid-service" $ EIDService.eidServiceRoutes
  , dir "sp" $ hGet $ toK2 $ DocControl.handleSignShow
  , dir "padsign" $ hPost $ toK2 $ DocControl.handleIssueGoToSignviewPad
  , allLangDirs $ dir "to-sign" $ hGet $ toK0 $ DocControl.handlePadList
  , allLangDirs $ dir "padqueue" $ hGet $ toK0 $ return LinkPadList -- Backward compatibility, redirects back to /to-sign

     -- Simple sending
  , allLangDirs $ dir "to-start" $ hGet $ toK0 $ DocControl.handleToStart
  , dir "ts" $ hGet $ toK1 $ DocControl.handleToStartShow
  , dir "d" $ hGet $ toK0 $ ArchiveControl.showArchive
  , dir "d" $ hGet $ toK1 $ DocControl.handleIssueShowGet
  , dir "d" $ dir "save" $ hPost $ toK1 $ DocControl.handleMarkAsSaved
  , dir "d" $ dir "delete" $ hPost $ toK0 $ ArchiveControl.handleDelete
  , dir "d" $ dir "prolong" $ hPost $ toK0 $ ArchiveControl.handleProlong
  , dir "d" $ dir "reallydelete" $ hPost $ toK0 $ ArchiveControl.handleReallyDelete
  , dir "d" $ dir "remind" $ hPost $ toK0 $ ArchiveControl.handleSendReminders
  , dir "d" $ dir "restore" $ hPost $ toK0 $ ArchiveControl.handleRestore
  , dir "d" $ dir "cancel" $ hPost $ toK0 $ ArchiveControl.handleCancel
  , dir "d" $ dir "zip" $ hGet $ toK0 $ ArchiveControl.handleZip
  , dir "d" $ dir "csv" $ hGet $ toK0 $ ArchiveControl.handleListCSV
  , dir "d" $ dir "signview" $ hGet $ toK1 $ DocControl.handleIssueGoToSignview
  , dir "d" $ dir "evidenceattachment" $ hGet $ toK2 $ DocControl.handleEvidenceAttachment
  , dir "mailpreview" $ hGet $ toK2 $ DocControl.prepareEmailPreview
  , dir "download" $ hGet $ toK4 $ DocControl.handleDownloadClosedFile

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
  , dir "resend" $ hPost $ toK2 $ DocControl.handleResend
  , dir "pages" $ hGet $ toK2 $ DocControl.showPage
     -- HTMP emails can have embedded preview image
  , dir "preview" $ hGet $ toK2 $ DocControl.showPreview
  , dir "preview" $ hGet $ toK3 $ \did slid fid ->
    DocControl.showPreviewForSignatory did slid Nothing fid
  , dir "preview" $ hGet $ toK4 $ \did slid mh fid ->
    DocControl.showPreviewForSignatory did slid (Just mh) fid
  , dir "filepages" $ hGet $ toK1 $ DocControl.handleFilePages
  , allLangDirs $ dir "verify" $ hGet $ toK0 $ DocControl.handleShowVerificationPage
  , allLangDirs $ dir "verify" $ hPostNoXToken $ toK0 $ DocControl.handleVerify
  , allLangDirs $ dir "afterforward" $ hGet $ toK1 $ DocControl.handleShowAfterForward

     -- UserControl
  , dir "account" $ hGet $ toK0 $ UserControl.handleAccountGet
  , dir "account" $ hGet $ toK2 $ UserControl.handleGetChangeEmail
  , dir "account" $ hPost $ toK2 $ UserControl.handlePostChangeEmail
  , dir "account" $ dir "company" $ Company.routes
  , dir "accepttos" $ hGet $ toK0 $ UserControl.handleAcceptTOSGet
  , dir "accepttos" $ hPost $ toK0 $ UserControl.handleAcceptTOSPost

     --UserGroupAccountsControl
  , (dir "account" . dir "companyaccounts" . dir "add" . hPost . toK0)
    UserGroupAccounts.handleAddUserGroupAccount
  , (dir "account" . dir "companyaccounts" . dir "resend" . hPost . toK0)
    UserGroupAccounts.handleResendToUserGroupAccount
  , (dir "account" . dir "companyaccounts" . dir "changerole" . hPost . toK0)
    UserGroupAccounts.handleChangeRoleOfUserGroupAccount
  , (dir "account" . dir "companyaccounts" . dir "remove" . hPost . toK0)
    UserGroupAccounts.handleRemoveUserGroupAccount
  , dir "companyaccounts" $ hGet $ toK0 $ UserGroupAccounts.handleUserGroupAccounts
  , (dir "companyaccounts" . dir "join" . hGet . toK1)
    UserGroupAccounts.handleGetBecomeUserGroupAccount
  , (dir "companyaccounts" . dir "join" . hPost . toK1)
    UserGroupAccounts.handlePostBecomeUserGroupAccount

     -- account stuff
  , allLangDirs $ dir "enter" $ hGet $ toK0 $ handleLoginGet
  , dir "logout" $ hGet $ toK0 $ handleLogout
  , dir "logout_ajax" $ hGet $ toK0 $ handleLogoutAJAX
  , (allLangDirs . dir "login" . hGet . toK0)
    (LinkLoginDirect <$> view #lang <$> getContext) -- Drop this after EE is migrated
  , dir "login" $ hPostNoXToken $ toK0 $ handleLoginPost
  , dir "loginwithredirect" $ hGet $ toK0 $ handleLoginWithRedirectGet
  , (allLangDirs . dir "signup" . hGetAllowHttp . toK0)
    (LinkSignup <$> view #lang <$> getContext) -- Drop this after EE is migrated
  , allLangDirs $ dir "amnesia" $ hGet $ toK2 $ UserControl.handlePasswordReminderGet
  , allLangDirs $ dir "amnesia" $ hPostNoXToken $ toK2
    UserControl.handlePasswordReminderPost
  , allLangDirs $ dir "mynewaccount" $ hGet $ toK2 $ \(_ :: String) (_ :: String) ->
    return LinkArchive
  , allLangDirs $ dir "accountsetup" $ hGet $ toK3 $ UserControl.handleAccountSetupGet
  , dir "contactsales" $ hPostAllowHttp $ toK0 $ UserControl.handleContactSales
  , dir "accountsetup" $ hPostNoXToken $ toK3 $ UserControl.handleAccountSetupPost
  , (dir "salesforce" . dir "integration" . hGet . toK0)
    Salesforce.handleSalesforceIntegration
  , dir "salesforce" $ dir "keys" $ hGet $ toK0 $ Salesforce.getSalesforceKeys
  , dir "adminonly" $ Administration.adminonlyRoutes
  , dir "dave" $ Administration.daveRoutes
  , allLangDirs $ dir "unsupported_browser" $ hGet $ toK0 $ unsupportedBrowserPage
  , (allLangDirs . dir "enable-cookies" . dir "enable-cookies.html" . hGetAllowHttp . toK0
    )
    enableCookiesPage
  , accessControlAPI
  , documentAPI
  , monitorAPI
  , partnerAPI
  , userAPI
  , userGroupAPI
  , folderAPIRoutes
  , padApplicationAPI
  , oauth
  , apiLogAPI
  , attachmentAPI

     -- api explorer
  , dir "api-explorer"
  $   remainingPath GET
  $   runWebSandboxT
        ( runPlusSandboxT
        $ serveDirectory EnableBrowsing ["index.html"] (staticDir ++ "/api-explorer")
        )
  >>= either return (maybe respond404 return)

     -- static files
  , remainingPath GET
  $   runWebSandboxT (runPlusSandboxT $ serveDirectory DisableBrowsing [] staticDir)
  >>= either return (maybe respond404 return)

     -- public services
  , dir "parsecsv" $ hPost $ toK0 $ ServerUtils.handleParseCSV
  , dir "serialize_image" $ hPost $ toK0 $ ServerUtils.handleSerializeImage
  , dir "colored_image" $ hGet $ toK0 $ ServerUtils.brandedImage
  , dir "document_signview_branding" $ hGet $ toK3 $ Branding.handleSignviewBranding
  , (dir "padlist_signview_branding" . hGet . toK3)
    Branding.handleSignviewBrandingWithoutDocument
  , dir "service_branding" $ hGet $ toK3 $ Branding.handleServiceBranding
  , dir "scrive_branding" $ hGet $ toK1 $ Branding.handleScriveBranding
  , dir "login_branding" $ hGet $ toK2 $ Branding.handleLoginBranding
  , dir "domain_branding" $ hGet $ toK2 $ Branding.handleDomainBranding
  , dir "login_logo" $ hGet $ toK2 $ Branding.loginLogo
  , dir "service_logo" $ hGet $ toK3 $ Branding.serviceLogo
  , dir "signview_logo" $ hGet $ toK3 $ Branding.signviewLogo
  , (dir "signview_logo_without_document" . hGet . toK3)
    Branding.signviewLogoWithoutDocument
  , dir "email_logo" $ hGet $ toK3 $ Branding.emailLogo
  , dir "favicon" $ hGet $ toK3 $ Branding.faviconIcon
  , dir "api" $ dir "v2" $ remainingPath GET $ toK0 noAPIV2CallFoundHandler
  , dir "api" $ dir "v2" $ remainingPath POST $ toK0 noAPIV2CallFoundHandler
  ]
  where staticDir = if (production) then "frontend/dist" else "frontend/app"
