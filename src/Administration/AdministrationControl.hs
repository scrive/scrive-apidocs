{-# LANGUAGE CPP #-}
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
          ) where
import Control.Monad.State
import Data.Functor
import Happstack.Server hiding (simpleHTTP,dir,path,https)
import Happstack.Fields
import Utils.Monad
import Utils.Monoid
import Utils.Prelude
import IPAddress ()
import Kontra
import Administration.AdministrationView
import Administration.AddPaymentPlan
import Doc.Model
import Doc.DocStateData
import Doc.SignatoryLinkID
import Doc.DocumentID
import Company.Model
import KontraLink
import MinutesTime
import DB
import User.UserControl
import User.UserView
import User.Model
import Data.Maybe
import Data.Char
import Text.StringTemplates.Templates
import Util.FlashUtil
import Data.List
import Util.MonadUtils
import qualified Log
import Doc.DocSeal (sealDocument)
import Util.HasSomeUserInfo
import InputValidation
import User.Utils
import Util.Actor
import Payments.Action
import Payments.Model
import Payments.Config
import qualified Payments.Stats

import InspectXMLInstances ()
import InspectXML
import ListUtil
import Text.JSON
import Mails.Model
import Util.HasSomeCompanyInfo
import CompanyAccounts.CompanyAccountsControl
import CompanyAccounts.Model
import Util.SignatoryLinkUtils
import User.History.Model
import qualified Text.StringTemplates.Fields as F
import Control.Logic
import Doc.DocInfo
import EvidenceLog.Model
import Routing
import Company.CompanyUI
import qualified Company.CompanyControl as Company
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import Happstack.StaticRouting(Route, choice, dir)
import Text.JSON.Gen
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import File.Model
import File.Storage

adminonlyRoutes :: Route (KontraPlus Response)
adminonlyRoutes =
  fmap onlySalesOrAdmin $ choice $ [
          hGet $ toK0 $ showAdminMainPage
        , dir "createuser" $ hPost $ toK0 $ handleCreateUser
        , dir "userslist" $ hGet $ toK0 $ jsonUsersList

        , dir "useradmin" $ hGet $ toK1 $ showAdminUsers
        , dir "useradmin" $ dir "details" $ hGet $ toK1 $ handleUserGetProfile
        , dir "useradmin" $ hPost $ toK1 $ handleUserChange


        , dir "useradmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1 handleAdminUserUsageStatsDays
        , dir "useradmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1 handleAdminUserUsageStatsMonths

        , dir "useradmin" $ dir "sendinviteagain" $ hPost $ toK0 $ sendInviteAgain

        , dir "companyadmin" $ hGet $ toK1 $ showAdminCompany
        , dir "companyadmin" $ dir "details" $ hGet $ toK1 $ handleCompanyGetProfile
        , dir "companyadmin" $ hPost $ toK1 $ handleCompanyChange

        , dir "companyadmin" $ dir "branding" $ Company.adminRoutes
        , dir "companyadmin" $ dir "users" $ hPost $ toK1 $ handlePostAdminCompanyUsers

        , dir "companyadmin" $ dir "payments" $ hGet $ toK1 $ companyPaymentsJSON
        , dir "companyadmin" $ dir "payments" $ dir "change" $ hPost $ toK1 $ handleCompanyPaymentsChange
        , dir "companyadmin" $ dir "payments" $ dir "migrate" $ hPost $ toK1 $ handleCompanyPaymentsMigrate
        , dir "companyadmin" $ dir "payments" $ dir "delete" $ hPost $ toK1 $ handleCompanyPaymentsDelete

        , dir "companyaccounts" $ hGet  $ toK1 $ CompanyAccounts.handleCompanyAccountsForAdminOnly
        , dir "companyadmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1 handleAdminCompanyUsageStatsDays
        , dir "companyadmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1 handleAdminCompanyUsageStatsMonths

        , dir "documentslist" $ hGet $ toK0 $ jsonDocuments

        , dir "paymentsstats.csv" $ hGet $ toK0 $ Payments.Stats.handlePaymentsStatsCSV

        , dir "companies" $ hGet $ toK0 $ jsonCompanies
  ]

daveRoutes :: Route (KontraPlus Response)
daveRoutes =
  fmap onlyAdmin $ choice $ [
       dir "document"      $ hGet $ toK1 $ daveDocument
     , dir "document"      $ hGet $ toK2 $ daveSignatoryLink
     , dir "user"          $ hGet $ toK1 $ daveUser
     , dir "userhistory"   $ hGet $ toK1 $ daveUserHistory
     , dir "company"       $ hGet $ toK1 $ daveCompany
     , dir "reseal" $ hPost $ toK1 $ resealFile
     , dir "file"   $ hGet  $ toK2 $ daveFile
     , dir "backdoor" $ hGet $ toK1 $ handleBackdoorQuery
    ]
{- | Main page. Redirects users to other admin panels -}
showAdminMainPage :: Kontrakcja m => m String
showAdminMainPage = onlySalesOrAdmin $ do
    ctx <- getContext
    adminMainPage ctx

{- | Process view for finding a user in basic administration -}
showAdminUsers :: Kontrakcja m => UserID -> m String
showAdminUsers uid = onlySalesOrAdmin $ adminUserPage uid

handleUserGetProfile:: Kontrakcja m => UserID -> m JSValue
handleUserGetProfile uid = onlySalesOrAdmin $ do
  ctx <- getContext
  user <- guardJustM $ dbQuery $ GetUserByID uid
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (usercompany user)
  userJSON ctx user (company,companyui)


handleCompanyGetProfile:: Kontrakcja m => CompanyID -> m JSValue
handleCompanyGetProfile cid = onlySalesOrAdmin $ do
  ctx <- getContext
  company <- guardJustM $ dbQuery $ GetCompany cid
  companyui <- dbQuery $ GetCompanyUI cid
  companyJSON ctx company companyui

showAdminCompany :: Kontrakcja m => CompanyID -> m String
showAdminCompany companyid = onlySalesOrAdmin $ adminCompanyPage companyid

companyPaymentsJSON :: Kontrakcja m => CompanyID -> m JSValue
companyPaymentsJSON cid = onlySalesOrAdmin $ do
  RecurlyConfig {..} <- ctxrecurlyconfig <$> getContext
  mpaymentplan <- dbQuery $ GetPaymentPlan cid
  quantity <- dbQuery $ GetCompanyQuantity cid
  paymentPlanJSON mpaymentplan (Just (cid,quantity)) recurlySubdomain

paymentPlanJSON :: (Monad m) => Maybe PaymentPlan -> Maybe (CompanyID,Int) -> String -> m JSValue
paymentPlanJSON mpaymentplan mci recurlysubdomain =  runJSONGenT $ do
    value "recurlysubdomain" recurlysubdomain
    value "companyid" $ show .fst <$> mci
    value "quantity"  $ snd <$> mci

    case mpaymentplan of
      Nothing -> do
        value "haspaymentplan" False
        value "priceplan" ("free" :: String)
        value "status" ("active" :: String)
      Just paymentplan -> do
        value "haspaymentplan" True
        value "recurlyplan" $ ppPaymentPlanProvider paymentplan == RecurlyProvider
        value "accountcode" $ show $ ppAccountCode paymentplan
        value "priceplan" $ case ppPricePlan paymentplan of
          FreePricePlan       -> ("free"       :: String)
          TeamPricePlan       -> ("team"       :: String)
          FormPricePlan       -> ("form"       :: String)
          EnterprisePricePlan -> ("enterprise" :: String)
          TrialTeamPricePlan  -> ("trial"      :: String)
        value "status" $ case ppStatus paymentplan of
          ActiveStatus        -> ("active"     :: String)
          OverdueStatus       -> ("overdue"    :: String)
          CanceledStatus      -> ("canceled"   :: String)
          DeactivatedStatus   -> ("deactivated":: String)

jsonCompanies :: Kontrakcja m => m JSValue
jsonCompanies = onlySalesOrAdmin $ do
    params <- getListParams
    let
      filters = companySearchingFromParams params
      sorting = companySortingFromParams params
      (offset, limit) = companyPaginationFromParams companiesPageSize params
      companiesPageSize = 100

    allCompanies <- dbQuery $ GetCompanies filters sorting offset limit
    let companies = PagedList { list       = allCompanies
                              , params     = params
                              , pageSize   = companiesPageSize
                              , listLength = length allCompanies
                              }
    runJSONGenT $ do
            valueM "list" $ forM (take companiesPageSize $ allCompanies) $ \company -> runJSONGenT $ do
                object "fields" $ do
                    value "id"            $ show . companyid $ company
                    value "companyname"   $ getCompanyName $ company
                    value "companynumber" $ getCompanyNumber $ company
                    value "companyaddress" $ companyaddress . companyinfo $ company
                    value "companyzip"     $ companyzip . companyinfo $ company
                    value "companycity"    $ companycity . companyinfo $ company
                    value "companycountry" $ companycountry . companyinfo $ company
                    value "companysmsoriginator" $ companysmsoriginator . companyinfo $ company
                value "link" $ show $ LinkCompanyAdmin $ companyid $ company
            value "paging" $ pagingParamsJSON companies

companySearchingFromParams :: ListParams -> [CompanyFilter]
companySearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> [CompanyFilterByString x]

companySortingFromParams :: ListParams -> [AscDesc CompanyOrderBy]
companySortingFromParams params =
   concatMap x (listParamsSorting params)
  where
    x "companyname"       = [Asc CompanyOrderByName]
    x "companynameREV"    = [Desc CompanyOrderByName]
    x "companynumber"     = [Asc CompanyOrderByNumber]
    x "companynumberREV"  = [Desc CompanyOrderByNumber]
    x "companyaddress"    = [Asc CompanyOrderByAddress]
    x "companyaddressREV" = [Desc CompanyOrderByAddress]
    x "companyzip"        = [Asc CompanyOrderByZip]
    x "companyzipREV"     = [Desc CompanyOrderByZip]
    x "companycity"       = [Asc CompanyOrderByCity]
    x "companycityREV"    = [Desc CompanyOrderByCity]
    x "companycountry"    = [Asc CompanyOrderByCountry]
    x "companycountryREV" = [Desc CompanyOrderByCountry]
    x _                   = []

companyPaginationFromParams :: Int -> ListParams -> (Integer,Integer)
companyPaginationFromParams pageSize params = (fromIntegral (listParamsOffset params), fromIntegral pageSize)

userSearchingFromParams :: ListParams -> [UserFilter]
userSearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> [UserFilterByString x]

userSortingFromParams :: ListParams -> [AscDesc UserOrderBy]
userSortingFromParams params =
   (concatMap x (listParamsSorting params)) ++ [Asc UserOrderByName]
  where
    x "username"    = [Asc UserOrderByName]
    x "usernameREV" = [Desc UserOrderByName]
    x "email"       = [Asc UserOrderByEmail]
    x "emailREV"    = [Desc UserOrderByEmail]
    x "tos"         = [Asc UserOrderByAccountCreationDate]
    x "tosREV"      = [Desc UserOrderByAccountCreationDate]
    x _             = [Asc UserOrderByName]


jsonUsersList ::Kontrakcja m => m JSValue
jsonUsersList = onlySalesOrAdmin $ do
    params <- getListParams
    let filters = userSearchingFromParams params
        sorting = userSortingFromParams params
        pagination = ((listParamsOffset params),(usersPageSize * 4))
        usersPageSize = 100
    allUsers <- getUsersAndStatsInv filters sorting pagination
    let users = PagedList { list       = allUsers
                          , params     = params
                          , pageSize   = usersPageSize
                          , listLength = length allUsers
                          }

    runJSONGenT $ do
            valueM "list" $ forM (take usersPageSize $ list users) $ \(user,mcompany,itype) -> runJSONGenT $ do
                object "fields" $ do
                    value "id" $ show $ userid user
                    value "username" $ getFullName user
                    value "email"    $ getEmail user
                    value "companyposition" $ usercompanyposition $ userinfo user
                    value "company"  $ getCompanyName mcompany
                    value "phone"    $ userphone $ userinfo user
                    value "tos"      $ formatMinutesTimeRealISO <$> (userhasacceptedtermsofservice user)
                    value "viral_invites" $ itype == Viral
                    value "admin_invites" $ itype == Admin
                value "link" $ show $ LinkUserAdmin $ userid user
            value "paging" $ pagingParamsJSON users

{- | Handling user details change. It reads user info change -}
handleUserChange :: Kontrakcja m => UserID -> m KontraLink
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
  _ <- dbUpdate $ SetUserInfo uid $ infoChange $ userinfo user
  _ <- dbUpdate
           $ LogHistoryUserInfoChanged uid (ctxipnumber ctx) (ctxtime ctx)
                                       (userinfo user) (infoChange $ userinfo user)
                                       (userid <$> ctxmaybeuser ctx)
  settingsChange <- getUserSettingsChange
  _ <- dbUpdate $ SetUserSettings uid $ settingsChange $ usersettings user
  return $ LinkUserAdmin $ uid

{- | Handling company details change. It reads user info change -}
handleCompanyChange :: Kontrakcja m => CompanyID -> m ()
handleCompanyChange companyid = onlySalesOrAdmin $ do
  company <- guardJustM $ dbQuery $ GetCompany companyid
  companyInfoChange <- getCompanyInfoChange
  _ <- dbUpdate $ SetCompanyInfo companyid (companyInfoChange $ companyinfo company)
  return $ ()


handleCompanyPaymentsChange :: Kontrakcja m => CompanyID -> m ()
handleCompanyPaymentsChange companyid = onlySalesOrAdmin $ do
  plan <- guardJustM $ readField "priceplan"
  status <- guardJustM $ readField "status"
  addCompanyPlanManual companyid plan status

handleCompanyPaymentsMigrate :: Kontrakcja m => CompanyID -> m ()
handleCompanyPaymentsMigrate companyid = onlySalesOrAdmin $ migratePlan companyid

handleCompanyPaymentsDelete :: Kontrakcja m => CompanyID -> m ()
handleCompanyPaymentsDelete companyid = onlySalesOrAdmin $ dbUpdate $ DeletePaymentPlan companyid




migratePlan :: Kontrakcja m => CompanyID -> m ()
migratePlan cid = do
  mcompanyid <- readField "companyid"
  case mcompanyid of
        Just dest -> do
          migrated <- changePaymentPlanOwner cid dest
          when (not migrated) internalError
        _ -> internalError

handleCreateUser :: Kontrakcja m => m KontraLink
handleCreateUser = onlySalesOrAdmin $ do
    email <- map toLower <$> (guardJustM $ getField "email")
    fstname <- guardJustM $ getField "fstname"
    sndname <- guardJustM $ getField "sndname"
    custommessage <- getField "custommessage"
    lang <- guardJustM $ readField "lang"
    muser <- createNewUserByAdmin email (fstname, sndname) custommessage Nothing lang
    when (isNothing muser) $
      addFlashM flashMessageUserWithSameEmailExists
    -- FIXME: where to redirect?
    return LoopBack

handlePostAdminCompanyUsers :: Kontrakcja m => CompanyID -> m KontraLink
handlePostAdminCompanyUsers companyid = onlySalesOrAdmin $ do
  privateinvite <- isFieldSet "privateinvite"
  if privateinvite
    then handlePrivateUserCompanyInvite companyid
    else handleCreateCompanyUser companyid
  return $ LoopBack


handlePrivateUserCompanyInvite :: Kontrakcja m => CompanyID -> m ()
handlePrivateUserCompanyInvite companyid = onlySalesOrAdmin $ do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  email <- Email <$> getCriticalField asValidEmail "email"
  existinguser <- guardJustM $ dbQuery $ GetUserByEmail email
  _ <- dbUpdate $ AddCompanyInvite CompanyInvite{
          invitedemail = email
        , invitedfstname = getFirstName existinguser
        , invitedsndname = getLastName existinguser
        , invitingcompany = companyid
        }
  company <- guardJustM $ dbQuery $ GetCompany companyid
  sendTakeoverPrivateUserMail user company existinguser

handleCreateCompanyUser :: Kontrakcja m => CompanyID -> m ()
handleCreateCompanyUser companyid = onlySalesOrAdmin $ do
  email <- getCriticalField asValidEmail "email"
  fstname <- getCriticalField asValidName "fstname"
  sndname <- getCriticalField asValidName "sndname"
  custommessage <- joinEmpty <$> getField "custommessage"
  Log.debug $ "Custom message when creating an account " ++ show custommessage
  lang <- guardJustM $ readField "lang"
  admin <- isFieldSet "iscompanyadmin"
  muser <- createNewUserByAdmin email (fstname, sndname) custommessage (Just (companyid, admin)) lang
  when (isNothing muser) $
      addFlashM flashMessageUserWithSameEmailExists
  return ()

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
  mcompanysmsoriginator <- getField "companysmsoriginator"
  return $ \CompanyInfo{..} ->  CompanyInfo {
        companyname        = fromMaybe companyname mcompanyname
      , companynumber      = fromMaybe companynumber mcompanynumber
      , companyaddress     = fromMaybe companyaddress mcompanyaddress
      , companyzip         = fromMaybe companyzip mcompanyzip
      , companycity        = fromMaybe companycity mcompanycity
      , companycountry     = fromMaybe companycountry mcompanycountry
      , companyipaddressmasklist = fromMaybe companyipaddressmasklist mcompanyipaddressmasklist
      , companysmsoriginator = fromMaybe companysmsoriginator mcompanysmsoriginator
    }

{- | Reads params and returns function for conversion of user settings.  No param leaves fields unchanged -}
getUserSettingsChange :: Kontrakcja m => m (UserSettings -> UserSettings)
getUserSettingsChange = do
  mlang <- readField "userlang"
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

jsonDocuments :: Kontrakcja m => m JSValue
jsonDocuments = onlySalesOrAdmin $ do

  params <- getListParams
  muserid <- readField "userid"
  mcompanyid <- readField "companyid"
  let sorting    = docSortingFromParams params
      searching  = docSearchingFromParams params
      pagination = ((listParamsOffset params),(listParamsLimit params))
      filters    =  case mcompanyid of
                     Nothing -> []
                     Just companyid ->   [DocumentFilterByAuthorCompany companyid]
      domain     = case muserid of
                     Nothing -> [DocumentsOfWholeUniverse]
                     Just userid ->   [DocumentsVisibleToUser userid]
      docsPageSize = 100

  allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting pagination

  let documents = PagedList { list       = allDocs
                            , params     = params
                            , pageSize   = docsPageSize
                            , listLength = length allDocs
                            }
  runJSONGenT $ do
            valueM "list" $ forM (take docsPageSize $ list documents) $ \doc-> runJSONGenT $ do
                 object "fields" $ do
                   value "id" $ show $ documentid doc
                   value "ctime" $ showMinutesTimeForAPI $ documentctime doc
                   value "mtime" $ showMinutesTimeForAPI $ documentmtime doc
                   object "author" $ do
                          value "name" $ getAuthorName doc
                          value "email" $ maybe "" getEmail $ getAuthorSigLink doc
                          value "company" $ maybe "" getCompanyName $ getAuthorSigLink doc
                   value "title"  $ documenttitle doc
                   value "status" $ take 20 $ show $ documentstatus doc
                   value "type"   $ show $ documenttype doc
                   value "signs"  $ map (getSmartName) $ documentsignatorylinks doc
            value "paging" $  pagingParamsJSON documents

docSortingFromParams :: ListParams -> [AscDesc DocumentOrderBy]
docSortingFromParams params =
   (concatMap x (listParamsSorting params)) ++ [Desc DocumentOrderByMTime] -- default order by mtime
  where
    x "status"            = [Asc DocumentOrderByStatusClass]
    x "statusREV"         = [Desc DocumentOrderByStatusClass]
    x "title"             = [Asc DocumentOrderByTitle]
    x "titleREV"          = [Desc DocumentOrderByTitle]
    x "time"              = [Asc DocumentOrderByMTime]
    x "timeREV"           = [Desc DocumentOrderByMTime]
    x "ctime"             = [Asc DocumentOrderByCTime]
    x "ctimeREV"          = [Desc DocumentOrderByCTime]
    x "signs"             = [Asc DocumentOrderByPartners]
    x "signsREV"          = [Desc DocumentOrderByPartners]
    x "type"              = [Asc DocumentOrderByType]
    x "typeREV"           = [Desc DocumentOrderByType]
    x "author"            = [Asc DocumentOrderByAuthor]
    x "authorRev"         = [Desc DocumentOrderByAuthor]
    x _                   = []


docSearchingFromParams :: ListParams -> [DocumentFilter]
docSearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> [DocumentFilterByString x]


handleBackdoorQuery :: Kontrakcja m => String -> m String
handleBackdoorQuery email = onlySalesOrAdmin $ onlyBackdoorOpen $ do
  minfo <- listToMaybe . reverse <$> dbQuery (GetEmailsByRecipient email)
  return $ maybe "No email found" mailContent minfo

sendInviteAgain :: Kontrakcja m => m KontraLink
sendInviteAgain = onlySalesOrAdmin $ do
  uid <- guardJustM $ readField "userid"
  user <- guardJustM $ dbQuery $ GetUserByID uid
  sendNewUserMail user
  addFlashM flashMessageNewActivationLinkSend
  return LoopBack

-- This method can be used do reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlyAdmin $ do
  Log.debug $ "Trying to reseal document "++ show docid ++" | Only superadmin can do that"
  ctx <- getContext
  actor <- guardJust $ mkAdminActor ctx
  _ <- dbUpdate $ InsertEvidenceEvent
          ResealedPDF
          (F.value "actor" (actorWho actor))
          (Just docid)
          actor
  doc <- do
    doc' <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
    when_ (isDocumentError doc') $ do
       dbUpdate $ FixClosedErroredDocument docid actor
    guardJustM  $ dbQuery $ GetDocumentByDocumentID docid
  res <- sealDocument doc
  case res of
      Left  _ -> Log.debug "We failed to reseal the document"
      Right _ -> Log.debug "Ok, so the document has been resealed"
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
    Log.debug $ "location: " ++ location
    if "/" `isSuffixOf` location
     then do
      document <- guardJustM . dbQuery $ GetDocumentByDocumentID documentid
      r <- renderTemplate "daveDocument" $ do
        F.value "daveBody" $  inspectXML document
        F.value "id" $ show documentid
        F.value "closed" $ documentstatus document == Closed
        F.value "couldBeclosed" $ isDocumentError document && all (isSignatory =>>^ hasSigned) (documentsignatorylinks document)
      return $ Right r
     else return $ Left $ LinkDaveDocument documentid

{- |
   Used by super users to inspect a particular signatory link.
-}
daveSignatoryLink :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m  String
daveSignatoryLink documentid siglinkid = onlyAdmin $ do
    document <- guardJustM . dbQuery $ GetDocumentByDocumentID documentid
    siglink <- guardJust $ getSigLinkFor document siglinkid
    renderTemplate  "daveSignatoryLink" $ do
        F.value "daveBody" $ inspectXML siglink
        F.value "docid" $ show documentid
        F.value "slid" $ show siglinkid
        F.objects "fields" $ for (signatoryfields $ signatorydetails siglink) $ \sf -> do
            F.value "fieldname" $ fieldTypeToString (sfType sf)
            F.value "fieldvalue" $ sfValue sf
            F.value "obligatory" $ sfObligatory sf
            F.value "label" $ show $ sfType sf
      where fieldTypeToString sf = case sf of
              FirstNameFT      -> "sigfstname"
              LastNameFT       -> "sigsndname"
              EmailFT          -> "sigemail"
              MobileFT         -> "sigmobile"
              CompanyFT        -> "sigco"
              PersonalNumberFT -> "sigpersnr"
              CompanyNumberFT  -> "sigcompnr"
              SignatureFT label-> "signature: " ++ label
              CustomFT label _ -> "Custom: " ++ label
              CheckboxFT label -> "Checkbox: " ++ label

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
   contents <- getFileIDContents fileid'
   if BS.null contents
      then internalError
      else do
          let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
          let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
          return res2

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

