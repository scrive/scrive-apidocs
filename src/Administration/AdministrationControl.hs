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
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP,dir,path,https)
import Happstack.StaticRouting(Route, choice, dir)
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplates.Templates
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BS
import qualified Text.StringTemplates.Fields as F

import Administration.AddPaymentPlan
import Administration.AdministrationView
import AppView (respondWithPDF)
import BrandedDomain.BrandedDomain
import BrandedDomain.BrandedDomainID
import BrandedDomain.Model
import Company.CompanyUI
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
import Utils.Prelude
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
        , dir "brandeddomain" $ hGet $ toK1 $ showAdminBrandedDomain
        , dir "brandeddomain" $ dir "create" $ hPost $ toK0 $ createBrandedDomain
        , dir "brandeddomain" $ dir "details" $ hGet $ toK1 $ jsonBrandedDomain
        , dir "brandeddomain" $ dir "details" $ dir "change" $ hPost $ toK1 $ updateBrandedDomain
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

showAdminBrandedDomain :: Kontrakcja m => BrandedDomainID -> m String
showAdminBrandedDomain bdid = onlySalesOrAdmin $ adminDomainBrandingPage bdid

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
                    value "companysmsoriginator" $ companysmsoriginator . companyinfo $ company
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

handleCreateUser :: Kontrakcja m => m KontraLink
handleCreateUser = onlySalesOrAdmin $ do
    email <- map toLower <$> (guardJustM $ getField "email")
    fstname <- guardJustM $ getField "fstname"
    sndname <- guardJustM $ getField "sndname"
    custommessage <- getField "custommessage"
    lang <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
    company <- dbUpdate $ CreateCompany
    muser <- createNewUserByAdmin email (fstname, sndname) custommessage (companyid company, True) lang
    when (isNothing muser) $
      addFlashM flashMessageUserWithSameEmailExists
    -- FIXME: where to redirect?
    return LoopBack

handlePostAdminCompanyUsers :: Kontrakcja m => CompanyID -> m JSValue
handlePostAdminCompanyUsers companyid = onlySalesOrAdmin $ do
  email <- getCriticalField asValidEmail "email"
  fstname <- getCriticalField asValidName "fstname"
  sndname <- getCriticalField asValidName "sndname"
  custommessage <- joinEmpty <$> getField "custommessage"
  Log.mixlog_ $ "Custom message when creating an account " ++ show custommessage
  lang <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
  admin <- isFieldSet "iscompanyadmin"
  muser <- createNewUserByAdmin email (fstname, sndname) custommessage (companyid, admin) lang
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
  mcompanysmsoriginator <- getField "companysmsoriginator"
  mcompanyallowsavesafetycopy <- getField "companyallowsavesafetycopy"
  mcompanyidledoctimeout <- (>>= \s -> if null s
                                       then Just Nothing
                                       else Just <$> [ t | t <- maybeRead s
                                                         , t >= minCompanyIdleDocTimeout
                                                         , t <= maxCompanyIdleDocTimeout ]) <$> getField "companyidledoctimeout"
  return $ \CompanyInfo{..} ->  CompanyInfo {
        companyname        = fromMaybe companyname mcompanyname
      , companynumber      = fromMaybe companynumber mcompanynumber
      , companyaddress     = fromMaybe companyaddress mcompanyaddress
      , companyzip         = fromMaybe companyzip mcompanyzip
      , companycity        = fromMaybe companycity mcompanycity
      , companycountry     = fromMaybe companycountry mcompanycountry
      , companyipaddressmasklist = fromMaybe companyipaddressmasklist mcompanyipaddressmasklist
      , companysmsoriginator = fromMaybe companysmsoriginator mcompanysmsoriginator
      , companyallowsavesafetycopy = maybe companyallowsavesafetycopy (=="true") mcompanyallowsavesafetycopy
      , companyidledoctimeout = fromMaybe companyidledoctimeout mcompanyidledoctimeout
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
      document <- dbQuery $ GetDocumentByDocumentID documentid
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
        F.value "docid" $ show documentid
        F.value "slid" $ show siglinkid
        F.objects "fields" $ for (signatoryfields $ siglink) $ \sf -> do
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

jsonBrandedDomainsList ::Kontrakcja m => m JSValue
jsonBrandedDomainsList = onlySalesOrAdmin $ do
    params <- getListParams
    let _filters = userSearchingFromParams params
        _sorting = userSortingFromParams params
        _pagination = ((listParamsOffset params),(pageSize * 4))
        pageSize = 100

    allBrandedDomains <- dbQuery $ GetBrandedDomains
    let allBrandedDomainsPagedList =
                PagedList { list       = allBrandedDomains
                          , params     = params
                          , pageSize   = pageSize
                          , listLength = length allBrandedDomains
                          }

    runJSONGenT $ do
      valueM "list" $ forM (take pageSize $ allBrandedDomains) $ \bd -> runJSONGenT $ do
        object "fields" $ do
          jsonBrandedDomainHelper bd
      value "paging" $ pagingParamsJSON allBrandedDomainsPagedList

jsonBrandedDomain :: Kontrakcja m => BrandedDomainID -> m JSValue
jsonBrandedDomain bdid = onlySalesOrAdmin $ do

  bd <- guardJustM $ dbQuery $ GetBrandedDomainByID bdid

  runJSONGenT $ do
    jsonBrandedDomainHelper bd

jsonBrandedDomainHelper :: forall (m :: * -> *).
                                 Monad m =>
                                 BrandedDomain -> JSONGenT m ()
jsonBrandedDomainHelper bd = do
  -- keep this 1to1 consistent with fields in the database
  value "id"                            $ show (bdid bd)
  value "url"                           $ bdurl bd
  value "logo"                          $ fromMaybe ""  $ BS.toString . BS.append (BS.fromString "data:image/png;base64,") .  B64.encode . unBinary <$> (bdlogo $ bd)
  value "bars_color"                    $ bdbarscolour bd
  value "bars_text_color"               $ bdbarstextcolour bd
  value "bars_secondary_color"          $ bdbarssecondarycolour bd
  value "background_color"              $ bdbackgroundcolour bd
  value "background_color_external"     $ bdbackgroundcolorexternal bd
  value "mails_background_color"        $ bdmailsbackgroundcolor bd
  value "mails_button_color"            $ bdmailsbuttoncolor bd
  value "mails_text_color"              $ bdmailstextcolor bd
  value "mails_border_color"            $ bdmailsbordercolor bd
  value "signview_primary_color"        $ bdsignviewprimarycolour bd
  value "signview_primary_text_color"   $ bdsignviewprimarytextcolour bd
  value "signview_secondary_color"      $ bdsignviewsecondarycolour bd
  value "signview_secondary_text_color" $ bdsignviewsecondarytextcolour bd
  value "button_class"                  $ bdbuttonclass bd
  value "service_link_color"            $ bdservicelinkcolour bd
  value "external_text_color"           $ bdexternaltextcolour bd
  value "header_color"                  $ bdheadercolour bd
  value "text_color"                    $ bdtextcolour bd
  value "price_color"                   $ bdpricecolour bd
  value "sms_originator"                $ bdsmsoriginator bd
  value "email_originator"              $ bdemailoriginator bd
  value "contact_email"                 $ bdcontactemail bd
  value "noreply_email"                 $ bdnoreplyemail bd

updateBrandedDomain :: Kontrakcja m => BrandedDomainID -> m ()
updateBrandedDomain xbdid = onlySalesOrAdmin $ do

    -- keep this 1to1 consistent with fields in the database
    post_url <- look "bdurl"
    post_logo <- getField "logo"
    post_bars_color <- look "bars_color"
    post_bars_text_color <- look "bars_text_color"
    post_bars_secondary_color <- look "bars_secondary_color"
    post_background_color <- look "background_color"
    post_background_color_external <- look "background_color_external"
    post_mails_background_color <- look "mails_background_color"
    post_mails_button_color <- look "mails_button_color"
    post_mails_text_color <- look "mails_text_color"
    post_mails_border_color <- look "mails_border_color"
    post_signview_primary_color <- look "signview_primary_color"
    post_signview_primary_text_color <- look "signview_primary_text_color"
    post_signview_secondary_color <- look "signview_secondary_color"
    post_signview_secondary_text_color <- look "signview_secondary_text_color"
    post_button_class <- look "button_class"
    post_service_link_color <- look "service_link_color"
    post_external_text_color <- look "external_text_color"
    post_header_color <- look "header_color"
    post_text_color <- look "text_color"
    post_price_color <- look "price_color"
    post_sms_originator <- look "sms_originator"
    post_email_originator <- look "email_originator"
    post_contact_email <- look "contact_email"
    post_noreply_email <- look "noreply_email"

    let bd = BrandedDomain {
        bdid = xbdid,
        bdurl = post_url,
        bdlogo = (Binary . B64.decodeLenient) <$> BS.fromString <$>  drop 1 <$> dropWhile ((/=) ',') <$> post_logo,
        bdbarscolour = post_bars_color,
        bdbarstextcolour = post_bars_text_color,
        bdbarssecondarycolour = post_bars_secondary_color,
        bdbackgroundcolour = post_background_color,
        bdbackgroundcolorexternal = post_background_color_external,
        bdmailsbackgroundcolor = post_mails_background_color,
        bdmailsbuttoncolor = post_mails_button_color,
        bdmailstextcolor = post_mails_text_color,
        bdmailsbordercolor = post_mails_border_color,
        bdsignviewprimarycolour = post_signview_primary_color,
        bdsignviewprimarytextcolour = post_signview_primary_text_color,
        bdsignviewsecondarycolour = post_signview_secondary_color,
        bdsignviewsecondarytextcolour = post_signview_secondary_text_color,
        bdbuttonclass = post_button_class,
        bdservicelinkcolour = post_service_link_color,
        bdexternaltextcolour = post_external_text_color,
        bdheadercolour = post_header_color,
        bdtextcolour = post_text_color,
        bdpricecolour = post_price_color,
        bdsmsoriginator = post_sms_originator,
        bdemailoriginator = post_email_originator,
        bdcontactemail = post_contact_email,
        bdnoreplyemail = post_noreply_email
             }

    _ <- dbUpdate $ UpdateBrandedDomain bd
    return ()

createBrandedDomain :: Kontrakcja m => m JSValue
createBrandedDomain = do
    bdid <- dbUpdate $ NewBrandedDomain
    runJSONGenT $ do
      value "id" (show bdid)
