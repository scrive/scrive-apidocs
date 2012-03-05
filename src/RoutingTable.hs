{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module RoutingTable (
    staticRoutes
  ) where

import API.IntegrationAPI
import API.Service.ServiceControl

import AppView as V
import Kontra
import KontraLink
import Login
import Misc
import Redirect
import Routing
import Happstack.StaticRouting(Route, choice, dir, path, param, remainingPath)
import User.Model
--import User.History.Model
import qualified Stats.Control as Stats
import qualified Administration.AdministrationControl as Administration
import qualified Company.CompanyControl as Company
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import qualified Doc.DocControl as DocControl
import qualified Archive.Control as ArchiveControl
import qualified ELegitimation.BankID as BankID
import qualified User.UserControl as UserControl
import qualified API.MailAPI as MailAPI
import Doc.API

import Control.Monad.Error
import Data.Functor
import Data.List
import Happstack.Server hiding (simpleHTTP, host, dir, path)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Util.MonadUtils
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
staticRoutes :: Route (Kontra Response)
staticRoutes = choice
     [ allLocaleDirs $ const $ hGetAllowHttp $ toK0 handleHomepage
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
     , dir "mailapi" $ hPostNoXToken $ toK0 $ MailAPI.handleMailAPI

     -- Only download function | unified for author and signatories
     , dir "download"                     $ hGet  $ toK3 $ DocControl.handleDownloadFile

     , dir "s" $ dir "eleg" $ hGet $ toK2 $ BankID.generateBankIDTransaction
     , dir "s" $ hGet $ toK0    $ sendRedirect $ LinkContracts
     , dir "s" $ hGet $ toK2    $ DocControl.handleSignShow
     , dir "s" $ hGet $ toK3    $ DocControl.handleSignShowOldRedirectToNew -- Redirect for old version to version above, remove not earlier then 31.12.2012.

     , dir "s" $ param "sign"           $ hPostNoXToken $ toK2 $ DocControl.signDocument
     , dir "s" $ param "reject"         $ hPostNoXToken $ toK2 $ DocControl.rejectDocument
     , dir "s" $ param "acceptaccount"  $ hPostNoXToken $ toK3 $ DocControl.handleAcceptAccountFromSign
     , dir "s" $ param "sigattachment"  $ hPostNoXToken $ toK2 $ DocControl.handleSigAttach
     , dir "s" $ param "deletesigattachment" $ hPostNoXToken $ toK2 $ DocControl.handleDeleteSigAttach

     , dir "pad" $ hGet $ toK2 $ DocControl.handlePadView
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

     , dir "paddevice" $ dir "archive" $ hGet $ toK0 $ ArchiveControl.showPadDeviceArchive


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
     , dir "parsecsv"              $ hPost $ toK0 $ DocControl.handleParseCSV
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

     , dir "padqueue" $ dir "add" $ hPost $ toK2 $ PadQueue.addToQueue
     , dir "padqueue" $ hGet $ toK0 $ PadQueue.showPadQueueCurrent
     -- UserControl
     , dir "account"                    $ hGet  $ toK0 $ UserControl.handleUserGet
     , dir "account"                    $ hPost $ toK0 $ UserControl.handleUserPost
     , dir "account" $ hGet $ toK2 $ UserControl.handleGetChangeEmail
     , dir "account" $ hPost $ toK2 $ UserControl.handlePostChangeEmail
     , dir "account" $ dir "security" $ hGet $ toK0 $ UserControl.handleGetUserSecurity
     , dir "account" $ dir "security" $ hPost $ toK0 $ UserControl.handlePostUserSecurity
     , dir "account" $ dir "company" $ hGet $ toK0 $ Company.handleGetCompany
     , dir "account" $ dir "company" $ hPost $ toK0 $ Company.handlePostCompany
     , dir "account" $ dir "company" $ dir "json" $ hGet $ toK0 $ Company.handleGetCompanyJSON
     , dir "account" $ dir "company" $ hGet $ toK1 $ Company.handleCompanyLogo
     , dir "account" $ dir "mailapi" $ hGet $ toK0 $ UserControl.handleGetUserMailAPI
     , dir "account" $ dir "mailapi" $ hPost $ toK0 $ UserControl.handlePostUserMailAPI
     , dir "account" $ dir "usagestats" $ hGet $ toK0 $ UserControl.handleUsageStatsForUser
     , dir "account" $ dir "usagestats" $ dir "days"   $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserDays
     , dir "account" $ dir "usagestats" $ dir "months" $ dir "json" $ hGet $ toK0 $ UserControl.handleUsageStatsJSONForUserMonths
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
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hGet $ toK1 $ Administration.showAdminCompanyUsers
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hPost $ toK1 $ Administration.handlePostAdminCompanyUsers
     , dir "adminonly" $ dir "companyaccounts" $ hGet  $ toK1 $ CompanyAccounts.handleCompanyAccountsForAdminOnly
     , dir "adminonly" $ dir "companyadmin" $ dir "usagestats" $ hGet $ toK1 $ Stats.showAdminCompanyUsageStats
     , dir "adminonly" $ dir "companyadmin" $ hPost $ toK1 $ Administration.handleCompanyChange
     , dir "adminonly" $ dir "functionalitystats" $ hGet $ toK0 $ Administration.showFunctionalityStats

     , dir "adminonly" $ dir "documents" $ hGet $ toK0 $ Administration.showDocuments
     , dir "adminonly" $ dir "documentslist" $ hGet $ toK0 $ Administration.jsonDocuments

     , dir "adminonly" $ dir "allstatscsv" $ path GET id $ Stats.handleDocStatsCSV
     , dir "adminonly" $ dir "userstatscsv" $ path GET id $ Stats.handleUserStatsCSV
     , dir "adminonly" $ dir "signstatscsv" $ path GET id $ Stats.handleSignStatsCSV
     , dir "adminonly" $ dir "dochistorycsv" $ path GET id $ Stats.handleDocHistoryCSV
     , dir "adminonly" $ dir "signhistorycsv" $ path GET id $ Stats.handleSignHistoryCSV
     , dir "adminonly" $ dir "userslistcsv" $ path GET id $ Administration.handleUsersListCSV

     , dir "adminonly" $ dir "runstatsonalldocs" $ hGet $ toK0 $ Stats.addAllDocsToStats
     , dir "adminonly" $ dir "stats1to2" $ hGet $ toK0 $ Stats.handleMigrate1To2

     , dir "adminonly" $ dir "runstatsonallusers" $ hGet $ toK0 $ Stats.addAllUsersToStats
     , dir "adminonly" $ dir "runstatssigs" $ hGet $ toK0 $ Stats.addAllSigsToStats

     , dir "adminonly" $ dir "statistics"        $ hGet  $ toK0 $ Stats.showAdminSystemUsageStats

     , dir "adminonly" $ dir "services" $ hGet $ toK0 $ Administration.showServicesPage
     , dir "adminonly" $ dir "services" $ param "create" $ hPost $ toK0 $ Administration.handleCreateService

     , dir "adminonly" $ dir "companies" $ hGet $ toK0 $ Administration.jsonCompanies

     , dir "adminonly" $ dir "reseal" $ hPost $ toK1 $ Administration.resealFile
     , dir "adminonly" $ dir "replacemainfile" $ hPost $ toK1 $ Administration.replaceMainFile

     , dir "adminonly" $ dir "docproblems" $ hGet $ toK0 $ DocControl.handleInvariantViolations

     , dir "adminonly" $ dir "backdoor" $ hGet $ toK1 $ Administration.handleBackdoorQuery

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
     , dir "adminonly" $ dir "neveruser" $ dir "resetservicepassword" $ hGetWrap (onlyAdmin . https) $ toK2 $ handleChangeServicePasswordAdminOnly

     , dir "adminonly" $ dir "log" $ hGetWrap (onlyAdmin . https) $ toK1 $ Administration.serveLogDirectory


     , dir "dave" $ dir "document"    $ hGet $ toK1 $ Administration.daveDocument
     , dir "dave" $ dir "user"        $ hGet $ toK1 $ Administration.daveUser
     , dir "dave" $ dir "userhistory" $ hGet $ toK1 $ Administration.daveUserHistory
     , dir "dave" $ dir "company"     $ hGet $ toK1 $ Administration.daveCompany
     , dir "dave" $ dir "company"     $ hGet $ toK3 $ Administration.companyClosedFilesZip

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

     -- viral invite
     , dir "invite"      $ hPostNoXToken $ toK0 $ UserControl.handleViralInvite
     , dir "question"    $ hPostAllowHttp $ toK0 $ UserControl.handleQuestion

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

