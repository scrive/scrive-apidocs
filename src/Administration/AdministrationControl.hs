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

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Functor.Invariant
import Data.List
import Data.Maybe
import Data.Unjson
import Happstack.Server hiding (simpleHTTP,dir,path,https)
import Happstack.StaticRouting(Route, choice, dir)
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplates.Templates
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Unjson as Unjson
import qualified Text.StringTemplates.Fields as F

import Administration.AddPaymentPlan
import Administration.AdministrationView
import AppView (respondWithPDF)
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import CompanyAccounts.Model
import Control.Logic
import DB
import Doc.Action (postDocumentClosedActions)
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad (withDocumentID)
import Doc.Model
import Doc.SignatoryLinkID
import EvidenceLog.Model
import File.Model
import File.Storage
import Happstack.Fields
import InputValidation
import InspectXML
import InspectXMLInstances ()
import InspectXMLInstances ()
import IPAddress ()
import Kontra
import KontraLink
import ListUtil
import Mails.Model
import MinutesTime
import Payments.Action
import Payments.Config
import Payments.Model
import Routing
import Theme.Control
import User.Email
import User.History.Model
import User.UserControl
import User.UserView
import User.Utils
import Util.Actor
import Util.FlashUtil
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Utils.Monoid
import Utils.Read (maybeRead)
import qualified Company.CompanyControl as Company
import qualified CompanyAccounts.CompanyAccountsControl as CompanyAccounts
import qualified Log
import qualified Payments.Stats

adminonlyRoutes :: Route (KontraPlus Response)
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

        , dir "brandeddomainslist" $ hGet $ toK0 $ jsonBrandedDomainsList
        , dir "brandeddomain" $ dir "create" $ hPost $ toK0 $ createBrandedDomain
        , dir "brandeddomain" $ dir "details" $ hGet $ toK1 $ jsonBrandedDomain
        , dir "brandeddomain" $ dir "details" $ dir "change" $ hPost $ toK1 $ updateBrandedDomain
        , dir "brandeddomain" $ dir "themes" $ hGet $ toK1 $ handleGetThemesForDomain
        , dir "brandeddomain" $ dir "newtheme" $ hPost $ toK2 $ handleNewThemeForDomain
        , dir "brandeddomain" $ dir "updatetheme" $ hPost $ toK2 $ handleUpdateThemeForDomain
        , dir "brandeddomain" $ dir "deletetheme" $ hPost $ toK2$ handleDeleteThemeForDomain
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
  user <- guardJustM $ dbQuery $ GetUserByID uid
  company <- getCompanyForUser user
  userJSON user company


handleCompanyGetProfile:: Kontrakcja m => CompanyID -> m JSValue
handleCompanyGetProfile cid = onlySalesOrAdmin $ do
  company <- guardJustM $ dbQuery $ GetCompany cid
  companyJSON  company

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
          OnePricePlan        -> ("one"        :: String)
          FormPricePlan       -> ("form"       :: String)
          TeamPricePlan       -> ("team"       :: String)
          EnterprisePricePlan -> ("enterprise" :: String)
          CompanyPricePlan    -> ("company"    :: String)
          TrialPricePlan      -> ("trial"      :: String)
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
                value "link" $ show $ LinkCompanyAdmin $ companyid $ company
            value "paging" $ pagingParamsJSON companies

companySearchingFromParams :: ListParams -> [CompanyFilter]
companySearchingFromParams params =
  (case listParamsSearching params of
    "" -> []
    x -> [CompanyFilterByString x])
  ++
  (case (listParamsFilters params) of
    (("users","all"):_) -> []
    _ -> [CompanyManyUsers])

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
    allUsers <- dbQuery $ GetUsersWithCompanies filters sorting pagination
    let users = PagedList { list       = allUsers
                          , params     = params
                          , pageSize   = usersPageSize
                          , listLength = length allUsers
                          }

    runJSONGenT $ do
            valueM "list" $ forM (take usersPageSize $ list users) $ \(user,mcompany) -> runJSONGenT $ do
                object "fields" $ do
                    value "id" $ show $ userid user
                    value "username" $ getFullName user
                    value "email"    $ getEmail user
                    value "companyposition" $ usercompanyposition $ userinfo user
                    value "company"  $ getCompanyName mcompany
                    value "phone"    $ userphone $ userinfo user
                    value "tos"      $ formatTimeISO <$> (userhasacceptedtermsofservice user)
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
  mcompanycgidisplayname <- fmap nothingIfEmpty <$> getField "companycgidisplayname"
  mcompanyallowsavesafetycopy <- getField "companyallowsavesafetycopy"
  mcompanyidledoctimeout <- (>>= \s -> if null s
                                       then Just Nothing
                                       else Just <$> (do t <- maybeRead s
                                                         guard $ t >= minCompanyIdleDocTimeout
                                                         guard $ t <= maxCompanyIdleDocTimeout
                                                         return t)) <$> getField "companyidledoctimeout"
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

jsonDocuments :: Kontrakcja m => m JSValue
jsonDocuments = onlySalesOrAdmin $ do

  params <- getListParams
  muserid <- readField "userid"
  mcompanyid <- readField "companyid"
  let sorting    = docSortingFromParams params
      searching  = docSearchingFromParams params
      pagination = (listParamsOffset params, listParamsLimit params, docsPageSize)
      filters    =  case mcompanyid of
                     Nothing -> []
                     Just companyid ->   [DocumentFilterByAuthorCompany companyid]
      domain     = case muserid of
                     Nothing -> [DocumentsOfWholeUniverse]
                     Just userid ->   [DocumentsVisibleToUser userid]
      docsPageSize = 100

  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit domain (searching ++ filters) sorting pagination

  let documents = PagedList { list       = allDocs
                            , params     = params
                            , pageSize   = docsPageSize
                            , listLength = allDocsCount
                            }
  runJSONGenT $ do
            valueM "list" $ forM (take docsPageSize $ list documents) $ \doc-> runJSONGenT $ do
                 object "fields" $ do
                   value "id" $ show $ documentid doc
                   value "ctime" $ formatTimeAPI $ documentctime doc
                   value "mtime" $ formatTimeAPI $ documentmtime doc
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

-- This method can be used to reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlyAdmin $ withDocumentID docid $ do
  Log.mixlog_ $ "Trying to reseal document "++ show docid ++" | Only superadmin can do that"
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
    Log.mixlog_ $ "location: " ++ location
    if "/" `isSuffixOf` location
     then do
      document <- dbQuery $ GetDocumentForDave documentid
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
   contents <- getFileIDContents fileid'
   if BS.null contents
      then internalError
      else return $ respondWithPDF False contents

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
      Log.mixlog_ $ "Main domain can't be changed"
      internalError
    -- keep this 1to1 consistent with fields in the database
    domainJSON <- guardJustM $ getFieldBS "domain"
    case Aeson.eitherDecode $ domainJSON of
     Left err -> do
       Log.mixlog_ $ "Error while parsing branding for adminonly " ++ err
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
  <*> field "contactEmail"
      bdContactEmail
      "Email address to contact owner of domain"
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
          (\l -> Binary $ B64.decodeLenient $ BSC8.pack $  drop 1 $ dropWhile ((/=) ',') l)
          (\l -> BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode $ unBinary l)
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
  fieldBy "list"
  id
  "Propper list"
  (arrayOf unjsonBrandedDomain)


createBrandedDomain :: Kontrakcja m => m JSValue
createBrandedDomain = do
    bdid <- dbUpdate $ NewBrandedDomain
    runJSONGenT $ do
      value "id" (show bdid)
