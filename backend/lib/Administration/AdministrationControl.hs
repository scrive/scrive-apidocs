-----------------------------------------------------------------------------
-- |
-- Module      :  Administration.AdministrationControl
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Handlers for all administrations tasks
--
-----------------------------------------------------------------------------
module Administration.AdministrationControl(
            adminonlyRoutes
          , daveRoutes
          , jsonCompanies -- for tests
          ) where

import Control.Monad.State
import Data.Char
import Data.Functor.Invariant
import Data.Unjson
import Happstack.Server hiding (dir, https, path, simpleHTTP)
import Happstack.StaticRouting (Route, choice, dir)
import Log
import Text.JSON
import Text.JSON.Gen hiding (object)
import Text.StringTemplates.Templates
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Unjson as Unjson
import qualified Text.StringTemplates.Fields as F

import Administration.AdministrationView
import AppView (renderFromBody, simpleHtmlResponse)
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Chargeable.Model
import Company.Model
import CompanyAccounts.Model
import DB
import Doc.Action (postDocumentClosedActions)
import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Document
import Doc.API.V2.JSON.List
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad (withDocumentID)
import Doc.Model
import Doc.Screenshot (Screenshot(..))
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots (SignatoryScreenshots(..))
import EvidenceLog.Model
import File.File
import File.Model
import File.Storage
import Happstack.Fields
import InputValidation
import InspectXML
import InspectXMLInstances ()
import InspectXMLInstances ()
import InternalResponse
import IPAddress ()
import Kontra
import KontraLink
import KontraPrelude
import Mails.Model
import MinutesTime
import PadApplication.Data (padAppModeFromText)
import Routing
import Theme.Control
import User.Email
import User.History.Model
import User.UserControl
import User.UserView
import User.Utils
import Util.Actor
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Utils.Monoid
import qualified Company.CompanyControl as Company
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import qualified Data.ByteString.RFC2397 as RFC2397

adminonlyRoutes :: Route (Kontra Response)
adminonlyRoutes =
  fmap onlySalesOrAdmin $ choice $ [
          hGet $ toK0 $ showAdminMainPage
        , dir "createuser" $ hPost $ toK0 $ handleCreateUser
        , dir "userslist" $ hGet $ toK0 $ jsonUsersList

        , dir "useradmin" $ hGet $ toK1 $ showAdminUsers
        , dir "useradmin" $ dir "details" $ hGet $ toK1 $ handleUserGetProfile
        , dir "useradmin" $ hPost $ toK1 $ handleUserChange
        , dir "useradmin" $ dir "deleteinvite" $ hPost $ toK2 $ handleDeleteInvite
        , dir "useradmin" $ dir "delete" $ hPost $ toK1 $ handleDeleteUser
        , dir "useradmin" $ dir "move" $ hPost $ toK1 $ handleMoveUserToDifferentCompany

        , dir "useradmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1 handleAdminUserUsageStatsDays
        , dir "useradmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1 handleAdminUserUsageStatsMonths

        , dir "useradmin" $ dir "sendinviteagain" $ hPost $ toK0 $ sendInviteAgain

        , dir "companyadmin" $ hGet $ toK1 $ showAdminCompany
        , dir "companyadmin" $ dir "details" $ hGet $ toK1 $ handleCompanyGetProfile

        , dir "companyadmin" $ hPost $ toK1 $ handleCompanyChange
        , dir "companyadmin" $ dir "merge" $ hPost $ toK1 $ handleMergeToOtherCompany

        , dir "companyadmin" $ dir "branding" $ Company.adminRoutes
        , dir "companyadmin" $ dir "users" $ hPost $ toK1 $ handlePostAdminCompanyUsers

        , dir "companyaccounts" $ hGet  $ toK1 $ CompanyAccounts.handleCompanyAccountsForAdminOnly
        , dir "companyadmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1 handleAdminCompanyUsageStatsDays
        , dir "companyadmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1 handleAdminCompanyUsageStatsMonths

        , dir "companyadmin" $ dir "getsubscription" $ hGet $ toK1 $ handleCompanyGetSubscription
        , dir "companyadmin" $ dir "updatesubscription" $ hPost $ toK1 $ handleCompanyUpdateSubscription

        , dir "documentslist" $ hGet $ toK0 $ jsonDocuments

        , dir "companies" $ hGet $ toK0 $ jsonCompanies

        , dir "brandeddomainslist" $ hGet $ toK0 $ jsonBrandedDomainsList
        , dir "brandeddomain" $ dir "create" $ hPost $ toK0 $ createBrandedDomain
        , dir "brandeddomain" $ dir "details" $ hGet $ toK1 $ jsonBrandedDomain
        , dir "brandeddomain" $ dir "details" $ dir "change" $ hPost $ toK1 $ updateBrandedDomain
        , dir "brandeddomain" $ dir "themes" $ hGet $ toK1 $ handleGetThemesForDomain
        , dir "brandeddomain" $ dir "newtheme" $ hPost $ toK2 $ handleNewThemeForDomain
        , dir "brandeddomain" $ dir "updatetheme" $ hPost $ toK2 $ handleUpdateThemeForDomain
        , dir "brandeddomain" $ dir "deletetheme" $ hPost $ toK2$ handleDeleteThemeForDomain
  ]

daveRoutes :: Route (Kontra Response)
daveRoutes =
  fmap onlyAdmin $ choice $ [
       dir "document"      $ hGet $ toK1 $ daveDocument
     , dir "document"      $ hGet $ toK2 $ daveSignatoryLink
     , dir "user"          $ hGet $ toK1 $ daveUser
     , dir "userhistory"   $ hGet $ toK1 $ daveUserHistory
     , dir "company"       $ hGet $ toK1 $ daveCompany
     , dir "reseal" $ hPost $ toK1 $ resealFile
     , dir "file"   $ hGet  $ toK2 $ daveFile
     , dir "backdoor" $ hGet $ handleBackdoorQuery
     , dir "randomscreenshot" $ hGet $ toK0 $ randomScreenshotForTest
    ]
{- | Main page. Redirects users to other admin panels -}
showAdminMainPage :: Kontrakcja m => m String
showAdminMainPage = onlySalesOrAdmin $ do
    ctx <- getContext
    adminMainPage ctx

{- | Process view for finding a user in basic administration -}
showAdminUsers :: Kontrakcja m => UserID -> m String
showAdminUsers uid = onlySalesOrAdmin $ do
  ctx <- getContext
  adminUserPage ctx uid

handleUserGetProfile:: Kontrakcja m => UserID -> m JSValue
handleUserGetProfile uid = onlySalesOrAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  company <- getCompanyForUser user
  userJSON user company


handleCompanyGetProfile:: Kontrakcja m => CompanyID -> m JSValue
handleCompanyGetProfile cid = onlySalesOrAdmin $ do
  company <- guardJustM $ dbQuery $ GetCompany cid
  companyJSON  company

showAdminCompany :: Kontrakcja m => CompanyID -> m String
showAdminCompany companyid = onlySalesOrAdmin $ do
  ctx <- getContext
  adminCompanyPage ctx companyid

jsonCompanies :: Kontrakcja m => m JSValue
jsonCompanies = onlySalesOrAdmin $ do
    limit    <- guardJustM $ readField "limit"
    offset   <- guardJustM $ readField "offset"
    textFilter <- getField "text" >>= \case
                     Nothing -> return []
                     Just s -> return [CompanyFilterByString s]
    usersFilter <- isFieldSet "allCompanies" >>= \case
                     True ->  return []
                     False -> return [CompanyManyUsers]
    pplanFilter <- isFieldSet "nonFree" >>= \case
                     True ->  return [CompanyWithNonFreePricePlan]
                     False -> return []
    allCompanies <- dbQuery $ GetCompanies (textFilter ++ usersFilter ++ pplanFilter) offset limit
    runJSONGenT $ do
            valueM "companies" $ forM allCompanies $ \company -> runJSONGenT $ do
              value "id"            $ show . companyid $ company
              value "companyname"   $ getCompanyName $ company
              value "companynumber" $ getCompanyNumber $ company
              value "companyaddress" $ companyaddress . companyinfo $ company
              value "companyzip"     $ companyzip . companyinfo $ company
              value "companycity"    $ companycity . companyinfo $ company
              value "companycountry" $ companycountry . companyinfo $ company

jsonUsersList ::Kontrakcja m => m JSValue
jsonUsersList = onlySalesOrAdmin $ do
    limit    <- guardJustM $ readField "limit"
    offset   <- guardJustM $ readField "offset"
    textFilter <- getField "text" >>= \case
                     Nothing -> return []
                     Just s -> return [UserFilterByString s]
    sorting <- getField "tosSorting" >>= \case
                     Just "ascending"   -> return [Asc UserOrderByAccountCreationDate]
                     Just "descending" -> return [Desc UserOrderByAccountCreationDate]
                     _ -> return [Asc UserOrderByName]
    allUsers <- dbQuery $ GetUsersWithCompanies textFilter sorting (offset,limit)

    runJSONGenT $ do
      valueM "users" $ forM (allUsers) $ \(user,mcompany) -> runJSONGenT $ do
        value "id" $ show $ userid user
        value "username" $ getFullName user
        value "email"    $ getEmail user
        value "companyposition" $ usercompanyposition $ userinfo user
        value "company"  $ getCompanyName mcompany
        value "phone"    $ userphone $ userinfo user
        value "tos"      $ formatTimeISO <$> (userhasacceptedtermsofservice user)


{- | Handling user details change. It reads user info change -}
handleUserChange :: Kontrakcja m => UserID -> m JSValue
handleUserChange uid = onlySalesOrAdmin $ do
  ctx <- getContext
  museraccounttype <- getField "useraccounttype"
  olduser <- guardJustM $ dbQuery $ GetUserByID uid
  user <- case (museraccounttype,useriscompanyadmin olduser) of
    (Just "companyadminaccount",  False) -> do
      --then we just want to make this account an admin
      newuser <- guardJustM $ do
        _ <- dbUpdate $ SetUserCompanyAdmin uid True
        _ <- dbUpdate $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx)
             [("is_company_admin", "false", "true")]
             (userid <$> ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      return newuser
    (Just "companystandardaccount", True) -> do
      --then we just want to downgrade this account to a standard
      newuser <- guardJustM $ do
        _ <- dbUpdate $ SetUserCompanyAdmin uid False
        _ <- dbUpdate
                 $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx)
                                            [("is_company_admin", "true", "false")]
                                            (userid <$> ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      return newuser
    _ -> return olduser
  infoChange <- getUserInfoChange
  let applyChanges = do
        _ <- dbUpdate $ SetUserInfo uid $ infoChange $ userinfo user
        _ <- dbUpdate
              $ LogHistoryUserInfoChanged uid (ctxipnumber ctx) (ctxtime ctx)
                    (userinfo user) (infoChange $ userinfo user)
                    (userid <$> ctxmaybeuser ctx)
        settingsChange <- getUserSettingsChange
        _ <- dbUpdate $ SetUserSettings uid $ settingsChange $ usersettings user
        return ()
  if (useremail (infoChange $ userinfo user) /= useremail (userinfo user))
    then do
      -- email address changed, check if new one is not used
      mexistinguser <- dbQuery $ GetUserByEmail $ useremail $ infoChange $ userinfo user
      case mexistinguser of
        Just _ -> runJSONGenT $ value "changed" False
        Nothing -> do
          applyChanges
          runJSONGenT $ value "changed" True
    else do
      applyChanges
      runJSONGenT $ value "changed" True


handleDeleteInvite :: Kontrakcja m => CompanyID -> UserID -> m ()
handleDeleteInvite cid uid = onlySalesOrAdmin $ do
  _ <- dbUpdate $ RemoveCompanyInvite cid uid
  return ()

handleDeleteUser :: Kontrakcja m => UserID -> m ()
handleDeleteUser uid = onlySalesOrAdmin $ do
  _ <- dbUpdate $ RemoveUserCompanyInvites uid
  _ <- dbUpdate $ DeleteUser uid
  return ()


handleMoveUserToDifferentCompany :: Kontrakcja m => UserID -> m ()
handleMoveUserToDifferentCompany uid = onlySalesOrAdmin $ do
  cid <- guardJustM $ readField "companyid"
  _ <- dbUpdate $ SetUserCompany uid cid
  _ <- dbUpdate $ SetUserCompanyAdmin uid False
  return ()


handleMergeToOtherCompany :: Kontrakcja m => CompanyID -> m ()
handleMergeToOtherCompany scid = onlySalesOrAdmin $ do
  tcid <- guardJustM $ readField "companyid"
  users <- dbQuery $ GetCompanyAccounts scid
  forM_ users $ \u -> do
      _ <- dbUpdate $ SetUserCompany (userid u) tcid
      return ()
  invites <- dbQuery $ GetCompanyInvites scid
  forM_ invites $ \i-> do
      _ <- dbUpdate $ RemoveCompanyInvite scid (inviteduserid i)
      return ()

{- | Handling company details change. It reads user info change -}
handleCompanyChange :: Kontrakcja m => CompanyID -> m ()
handleCompanyChange companyid = onlySalesOrAdmin $ do
  company <- guardJustM $ dbQuery $ GetCompany companyid
  companyInfoChange <- getCompanyInfoChange
  _ <- dbUpdate $ SetCompanyInfo companyid (companyInfoChange $ companyinfo company)
  return $ ()

handleCreateUser :: Kontrakcja m => m JSValue
handleCreateUser = onlySalesOrAdmin $ do
    email <- map toLower <$> (guardJustM $ getField "email")
    fstname <- guardJustM $ getField "fstname"
    sndname <- guardJustM $ getField "sndname"
    lang <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
    company <- dbUpdate $ CreateCompany
    muser <- createNewUserByAdmin email (fstname, sndname) (companyid company, True) lang
    runJSONGenT $ case muser of
      Nothing -> do
        value "success" False
        valueM "error_message" $ renderTemplate_ "flashMessageUserWithSameEmailExists"
      Just _ -> do
        value "success" True
        value "error_message" (Nothing :: Maybe String)

handlePostAdminCompanyUsers :: Kontrakcja m => CompanyID -> m JSValue
handlePostAdminCompanyUsers companyid = onlySalesOrAdmin $ do
  email <- getCriticalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  lang <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
  admin <- isFieldSet "iscompanyadmin"
  muser <- createNewUserByAdmin email (fstname, sndname) (companyid, admin) lang
  runJSONGenT $ case muser of
    Nothing -> do
      value "success" False
      valueM "error_message" $ renderTemplate_ "flashMessageUserWithSameEmailExists"
    Just _ -> do
      value "success" True
      value "error_message" (Nothing :: Maybe String)

{- | Reads params and returns function for conversion of company info.  With no param leaves fields unchanged -}
getCompanyInfoChange :: Kontrakcja m => m (CompanyInfo -> CompanyInfo)
getCompanyInfoChange = do
  mcompanyname    <- getField "companyname"
  mcompanynumber  <- getField "companynumber"
  mcompanyaddress <- getField "companyaddress"
  mcompanyzip     <- getField "companyzip"
  mcompanycity    <- getField "companycity"
  mcompanycountry <- getField "companycountry"
  mcompanyipaddressmasklist <- getOptionalField asValidIPAddressWithMaskList "companyipaddressmasklist"
  mcompanycgidisplayname <- fmap emptyToNothing <$> getField "companycgidisplayname"
  mcompanycgiserviceid <- fmap emptyToNothing <$> getField "companycgiserviceid"
  mcompanyallowsavesafetycopy <- getField "companyallowsavesafetycopy"
  mcompanyidledoctimeout <- (>>= \s -> if null s
                                       then Just Nothing
                                       else Just <$> (do t <- maybeRead s
                                                         guard $ t >= minCompanyIdleDocTimeout
                                                         guard $ t <= maxCompanyIdleDocTimeout
                                                         return t)) <$> getField "companyidledoctimeout"
  mcompanysmsprovider <- fmap maybeRead <$> getField' $ "companysmsprovider"
  mcompanypadappmode <- fmap (padAppModeFromText . T.pack) <$> getField' $ "companypadappmode"
  mcompanypadearchiveenabled <- getField "companypadearchiveenabled"
  return $ \CompanyInfo{..} -> CompanyInfo {
        companyname        = fromMaybe companyname mcompanyname
      , companynumber      = fromMaybe companynumber mcompanynumber
      , companyaddress     = fromMaybe companyaddress mcompanyaddress
      , companyzip         = fromMaybe companyzip mcompanyzip
      , companycity        = fromMaybe companycity mcompanycity
      , companycountry     = fromMaybe companycountry mcompanycountry
      , companyipaddressmasklist = fromMaybe companyipaddressmasklist mcompanyipaddressmasklist
      , companyallowsavesafetycopy = maybe companyallowsavesafetycopy (=="true") mcompanyallowsavesafetycopy
      , companyidledoctimeout = fromMaybe companyidledoctimeout mcompanyidledoctimeout
      , companycgidisplayname = fromMaybe companycgidisplayname mcompanycgidisplayname
      , companycgiserviceid = fromMaybe companycgiserviceid mcompanycgiserviceid
      , companysmsprovider = fromMaybe companysmsprovider mcompanysmsprovider
      , companypaymentplan = companypaymentplan
      , companypartnerid = companypartnerid
      , companypadappmode = fromMaybe companypadappmode mcompanypadappmode
      , companypadearchiveenabled = maybe companypadearchiveenabled (=="true") mcompanypadearchiveenabled
    }

{- | Reads params and returns function for conversion of user settings.  No param leaves fields unchanged -}
getUserSettingsChange :: Kontrakcja m => m (UserSettings -> UserSettings)
getUserSettingsChange = do
  mlang <- join <$> fmap langFromCode <$> getField "userlang"
  return $ \settings -> settings {
     lang = fromMaybe (lang settings) mlang
  }

{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}
getUserInfoChange :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoChange = do
  muserfstname         <- getField "userfstname"
  musersndname         <- getField "usersndname"
  muserpersonalnumber  <- getField "userpersonalnumber"
  musercompanyposition <- getField "usercompanyposition"
  muserphone           <- getField "userphone"
  museremail           <- fmap Email <$> getField "useremail"
  return $ \UserInfo{..} -> UserInfo {
        userfstname         = fromMaybe userfstname muserfstname
      , usersndname         = fromMaybe usersndname musersndname
      , userpersonalnumber  = fromMaybe userpersonalnumber muserpersonalnumber
      , usercompanyposition = fromMaybe usercompanyposition musercompanyposition
      , userphone           = fromMaybe userphone muserphone
      , useremail           = fromMaybe useremail museremail
    }

jsonDocuments :: Kontrakcja m => m Response
jsonDocuments = onlySalesOrAdmin $ do
  adminUser <- guardJustM $ ctxmaybeuser <$> getContext
  muid <- readField "userid"
  mcid <- readField "companyid"
  offset   <- guardJustM $ readField "offset"
  maxcount <- guardJustM $ readField  "max"

  requestedFilters <- getFieldBS "filter" >>= \case
      Just paramValue -> case Aeson.eitherDecode paramValue of
         Right js -> case (Unjson.parse Unjson.unjsonDef js) of
            (Result res []) -> return $ join $ toDocumentFilter (userid adminUser) <$> res
            _ -> internalError
         Left _ -> internalError
      Nothing -> return []

  requestedSorting <- getFieldBS "sorting" >>= \case
      Just paramValue -> case Aeson.eitherDecode paramValue of
         Right js -> case (Unjson.parse Unjson.unjsonDef js) of
            (Result res []) -> return $ toDocumentSorting <$> res
            _ -> internalError
         Left _ -> internalError
      Nothing -> return []

  let (domain,filtering, sorting)     = case (mcid, muid) of
        -- When fetching all documents, we don't allow any filtering, and only default sort is allowed
        (Nothing, Nothing)  -> (DocumentsOfWholeUniverse,[],[Desc DocumentOrderByMTime])
        (Just cid, Nothing) -> (DocumentsOfCompany cid,requestedFilters,requestedSorting)
        (Nothing, Just uid) -> (DocumentsVisibleToUser uid, requestedFilters,requestedSorting)
        _                   -> $unexpectedError "Can't pass both user id and company id"
  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit domain filtering sorting (offset, 1000, maxcount)
  let json = listToJSONBS (allDocsCount,(\d -> (documentAccessForAdminonly d,d)) <$> allDocs)
  return $ Response 200 Map.empty nullRsFlags json Nothing


handleBackdoorQuery :: Kontrakcja m => m Response
handleBackdoorQuery = onlySalesOrAdmin $ onlyBackdoorOpen $ do
  emailAddress <- guardJustM $ getField "email_address"
  emailTitle <- guardJustM $ getField "email_title"
  Just startDate <- MinutesTime.parseTimeISO <$> (guardJustM $ getField "start_date")
  memail <- dbQuery $ GetEmailForRecipient emailAddress emailTitle startDate
  case memail of
    Nothing -> respond404
    Just email -> renderFromBody $ mailContent email

sendInviteAgain :: Kontrakcja m => m InternalKontraResponse
sendInviteAgain = onlySalesOrAdmin $ do
  uid <- guardJustM $ readField "userid"
  user <- guardJustM $ dbQuery $ GetUserByID uid
  sendNewUserMail user
  flashmessage <- flashMessageNewActivationLinkSend
  return $ internalResponseWithFlash flashmessage LoopBack

-- This method can be used to reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlyAdmin $ withDocumentID docid $ do
  logInfo_ "Trying to reseal document (only superadmin can do that)"
  ctx <- getContext
  actor <- guardJust $ mkAdminActor ctx
  _ <- dbUpdate $ InsertEvidenceEvent
          ResealedPDF
          (return ())
          actor
  postDocumentClosedActions False True
  return LoopBack


{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: Kontrakcja m => DocumentID -> m (Either KontraLink String)
daveDocument documentid = onlyAdmin $ do
    -- for dave, we want a slash at the end, so redirect if there is no slash
    -- we have a relative link for signatorylinkids, so we need a slash at the end
    -- of the dave/document links; I evaluated a few other ways (using javascript, etc)
    -- but I could not come up with a better one than this
    --  -Eric
    location <- rqUri <$> askRq
    logInfo "Logging location" $ object [
        "location" .= location
      ]
    if "/" `isSuffixOf` location
     then do
      document <- dbQuery $ GetDocumentForDave documentid
      r <- renderTemplate "daveDocument" $ do
        F.value "daveBody" $  inspectXML document
        F.value "id" $ show documentid
        F.value "closed" $ documentstatus document == Closed
        F.value "couldBeclosed" $ isDocumentError document && all (isSignatory --> hasSigned) (documentsignatorylinks document)
      return $ Right r
     else return $ Left $ LinkDaveDocument documentid

{- |
   Used by super users to inspect a particular signatory link.
-}
daveSignatoryLink :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m  String
daveSignatoryLink documentid siglinkid = onlyAdmin $ do
    document <- dbQuery $ GetDocumentByDocumentID documentid
    siglink <- guardJust $ getSigLinkFor siglinkid document
    renderTemplate  "daveSignatoryLink" $ do
        F.value "daveBody" $ inspectXML siglink

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: Kontrakcja m => UserID ->  m String
daveUser userid = onlyAdmin $ do
    user <- guardJustM $ dbQuery $ GetUserByID userid
    return $ inspectXML user

{- |
   Used by super users to inspect a particular user's history.
-}
daveUserHistory :: Kontrakcja m => UserID -> m String
daveUserHistory userid = onlyAdmin $ do
    history <- dbQuery $ GetUserHistoryByUserID userid
    return $ inspectXML history

{- |
    Used by super users to inspect a company in xml.
-}
daveCompany :: Kontrakcja m => CompanyID -> m String
daveCompany companyid = onlyAdmin $ do
  company <- guardJustM $ dbQuery $ GetCompany companyid
  return $ inspectXML company

daveFile :: Kontrakcja m => FileID -> String -> m Response
daveFile fileid' _title = onlyAdmin $ do
   file <- dbQuery $ GetFileByFileID fileid'
   contents <- getFileContents file
   if BS.null contents
      then internalError
      else do
        let fname = filter (/=',') $ filename file -- Chrome does not like commas in this header
        return $ setHeader "Content-Disposition" ("attachment;filename=" ++ fname)
                 $ Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing

randomScreenshotForTest :: Kontrakcja m => m Response
randomScreenshotForTest = do
  now <- currentTime
  let lastWeek = 7 `daysBefore` now
  slid <- guardJustM $ dbQuery $ GetRandomSignatoryLinkIDThatSignedRecently lastWeek
  screenshots <- map snd <$> dbQuery (GetSignatoryScreenshots [slid])
  doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
  elogEvents <- dbQuery $ GetEvidenceLog $ documentid doc
  let sigElogEvents = filter ((== Just slid) . evSigLink) elogEvents
  content <- renderTemplate "screenshotReview" $ do
    F.value "userAgent" $ evClientName <$> find (isJust . evClientName) sigElogEvents
    F.value "signatoryid" $ show slid
    case screenshots of
      ((SignatoryScreenshots mfirst msigning _):_) -> do
        let screenShowImageString (Screenshot _ img) = BS.toString $ RFC2397.encode "image/jpeg" img
        F.value "firstimage" $ screenShowImageString <$> mfirst
        F.value "signingimage" $ screenShowImageString <$> msigning
      _ -> return ()
  simpleHtmlResponse content

handleAdminUserUsageStatsDays :: Kontrakcja m => UserID -> m JSValue
handleAdminUserUsageStatsDays uid = onlySalesOrAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getDaysStats (Right $ usercompany user)
    else getDaysStats (Left $ userid user)


handleAdminUserUsageStatsMonths :: Kontrakcja m => UserID -> m JSValue
handleAdminUserUsageStatsMonths uid = onlySalesOrAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getMonthsStats (Right $ usercompany user)
    else getMonthsStats (Left $ userid user)

handleAdminCompanyUsageStatsDays :: Kontrakcja m => CompanyID -> m JSValue
handleAdminCompanyUsageStatsDays cid = onlySalesOrAdmin $ do
  getDaysStats (Right $ cid)

handleAdminCompanyUsageStatsMonths :: Kontrakcja m => CompanyID -> m JSValue
handleAdminCompanyUsageStatsMonths cid = onlySalesOrAdmin $ do
  getMonthsStats (Right $ cid)

handleCompanyGetSubscription :: Kontrakcja m => CompanyID -> m JSValue
handleCompanyGetSubscription cid = onlySalesOrAdmin $ do
  company <- guardJustM $ dbQuery $ GetCompany cid
  users <- dbQuery $ GetCompanyAccounts $ cid
  docsStartedThisMonth <- fromIntegral <$> (dbQuery $ GetNumberOfDocumentsStartedThisMonth $ cid)
  subscriptionJSON company users docsStartedThisMonth

handleCompanyUpdateSubscription :: Kontrakcja m => CompanyID -> m ()
handleCompanyUpdateSubscription cid = onlySalesOrAdmin $ do
  paymentPlan <- guardJustM $ join <$> fmap paymentPlanFromText <$> getField "payment_plan"
  _ <- dbUpdate $ SetCompanyPaymentPlan cid paymentPlan
  return ()


jsonBrandedDomainsList ::Kontrakcja m => m Aeson.Value
jsonBrandedDomainsList = onlySalesOrAdmin $ do
    allBrandedDomains <- dbQuery $ GetBrandedDomains
    return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonBrandedDomainsList allBrandedDomains

jsonBrandedDomain :: Kontrakcja m => BrandedDomainID -> m Aeson.Value
jsonBrandedDomain bdid = onlySalesOrAdmin $ do
  bd <- dbQuery $ GetBrandedDomainByID bdid
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonBrandedDomain bd

updateBrandedDomain :: Kontrakcja m => BrandedDomainID -> m ()
updateBrandedDomain xbdid = onlySalesOrAdmin $ do
    obd <- dbQuery $ GetBrandedDomainByID xbdid
    when (bdMainDomain obd) $ do
      logInfo_ "Main domain can't be changed"
      internalError
    -- keep this 1to1 consistent with fields in the database
    domainJSON <- guardJustM $ getFieldBS "domain"
    case Aeson.eitherDecode $ domainJSON of
     Left err -> do
      logInfo "Error while parsing branding for adminonly" $ object [
          "error" .= err
        ]
      internalError
     Right js -> case (Unjson.parse unjsonBrandedDomain js) of
        (Result newDomain []) -> do
          _ <- dbUpdate $ UpdateBrandedDomain newDomain {bdid = bdid obd, bdMainDomain = bdMainDomain obd}
          return ()
        _ -> internalError

unjsonBrandedDomain :: UnjsonDef BrandedDomain
unjsonBrandedDomain = objectOf $ pure BrandedDomain
  <*> field "id"
      bdid
      "Id of a branded domain (unique)"
  <*> field "mainDomain"
      bdMainDomain
      "Is this a main domain"
  <*> field "url"
      bdUrl
      "URL that will match this domain"
  <*> field "smsOriginator"
      bdSmsOriginator
      "Originator for text messages"
  <*> field "emailOriginator"
      bdEmailOriginator
      "Originator for email messages"
  <*> field "noreplyEmail"
      bdNoreplyEmail
      "Email address that none should reply to"
  <*> field "mailTheme"
      bdMailTheme
      "Email theme"
  <*> field "signviewTheme"
      bdSignviewTheme
      "Signview theme"
  <*> field "serviceTheme"
      bdServiceTheme
      "Service theme"
  <*> field "loginTheme"
      bdLoginTheme
      "Login theme"
  <*> field "browserTitle"
      bdBrowserTitle
      "Browser title"
  <*> fieldBy "favicon"
      bdFavicon
      "Favicon"
       (invmap
          (\l -> B64.decodeLenient $ BSC8.pack $  drop 1 $ dropWhile ((/=) ',') l)
          (\l -> BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode l)
          unjsonDef
       )
   <*> field "participantColor1"
      bdParticipantColor1
      "Participant 1 color"
   <*> field "participantColor2"
      bdParticipantColor2
      "Participant 2 color"
   <*> field "participantColor3"
      bdParticipantColor3
      "Participant 3 color"
   <*> field "participantColor4"
      bdParticipantColor4
      "Participant 4 color"
   <*> field "participantColor5"
      bdParticipantColor5
      "Participant 5 color"
   <*> field "participantColor6"
      bdParticipantColor6
      "Participant 6 color"
   <*> field "draftColor"
      bdDraftColor
      "Draft color"
   <*> field "cancelledColor"
      bdCancelledColor
      "Cancelled color"
   <*> field "initatedColor"
      bdInitatedColor
      "Initated color"
   <*> field "sentColor"
      bdSentColor
      "Sent color"
   <*> field "deliveredColor"
      bdDeliveredColor
      "Delivered color"
   <*> field "openedColor"
      bdOpenedColor
      "Opened color"
   <*> field "reviewedColor"
      bdReviewedColor
      "Reviewed color"
   <*> field "signedColor"
      bdSignedColor
      "Signed color"

unjsonBrandedDomainsList :: UnjsonDef [BrandedDomain]
unjsonBrandedDomainsList = objectOf $
  fieldBy "domains"
  id
  "List of branded domains"
  (arrayOf unjsonBrandedDomain)


createBrandedDomain :: Kontrakcja m => m JSValue
createBrandedDomain = do
    bdid <- dbUpdate $ NewBrandedDomain
    runJSONGenT $ do
      value "id" (show bdid)
