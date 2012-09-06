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
            showAdminMainPage
          , showAdminUsers
          , showAdminCompanies
          , showAdminCompany
          , showAdminCompanyUsers
          , showAdminUsersForSales
          , showAllUsersTable
          , showServicesPage
          , showDocuments
          , handleUserChange
          , handleCompanyChange
          , handleCreateUser
          , handlePostAdminCompanyUsers
          , handleCreateService
          , handleUsersListCSV
          , showFunctionalityStats
          , handleBackdoorQuery
          , resealFile
          , replaceMainFile
          , daveDocument
          , daveUser
          , daveCompany
          , daveUserHistory
          , daveSignatoryLink
          , updateFields
          , serveLogDirectory
          , jsonUsersList
          , jsonCompanies
          , jsonDocuments
          ) where
import Control.Monad.State
import Data.Functor
import Happstack.Server hiding (simpleHTTP)
import Happstack.Fields
import Utils.IO
import Utils.Monoid
import Utils.Prelude
import Utils.String
import Kontra
import Administration.AdministrationView
import Crypto.RNG (CryptoRNG)
import Doc.Model
import Doc.DocStateData
import Company.Model
import KontraLink
import MinutesTime
import System.Directory
import DB
import User.UserControl
import User.UserView
import User.Model
import Data.Maybe
import Data.Char
import API.Service.Model
import Templates.Templates
import Util.FlashUtil
import Data.List
import Util.MonadUtils
import qualified Log
import Doc.DocSeal (sealDocument)
import Util.HasSomeUserInfo
import InputValidation
import User.Utils
import ScriveByMail.Model
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import Crypto.RNG(random)
import Util.Actor

import InspectXMLInstances ()
import InspectXML
import File.Model
import Util.CSVUtil
import ListUtil
import Text.JSON
import Mails.Model
import Util.HasSomeCompanyInfo
import CompanyAccounts.CompanyAccountsControl
import CompanyAccounts.Model
import Util.SignatoryLinkUtils
import Stats.Control (getUsersAndStatsInv)
import User.History.Model
import qualified Templates.Fields as F
import Control.Logic
import Doc.DocInfo
import EvidenceLog.Model

{- | Main page. Redirects users to other admin panels -}
showAdminMainPage :: Kontrakcja m => m String
showAdminMainPage = onlySalesOrAdmin $ do
    ctx <- getContext
    adminMainPage ctx


{- | Process view for finding a user in basic administration. If provided with userId string as param
it allows to edit user details -}
showAdminUsers :: Kontrakcja m => Maybe UserID -> m String
showAdminUsers Nothing = onlySalesOrAdmin adminUsersPage

showAdminUsers (Just userId) = onlySalesOrAdmin $ do
  muser <- dbQuery $ GetUserByID userId
  case muser of
    Nothing -> internalError
    Just user -> adminUserPage user =<< getCompanyForUser user
    
showAdminCompanies :: Kontrakcja m => m String
showAdminCompanies = onlySalesOrAdmin $  adminCompaniesPage

showAdminCompany :: Kontrakcja m => CompanyID -> m String
showAdminCompany companyid = onlySalesOrAdmin $ do
  company  <- guardJustM . dbQuery $ GetCompany companyid
  mmailapi <- dbQuery $ GetCompanyMailAPI companyid
  adminCompanyPage company mmailapi

jsonCompanies :: Kontrakcja m => m JSValue
jsonCompanies = onlySalesOrAdmin $ do
    params <- getListParamsNew
    let
      domains = [CompaniesOfService Nothing]
      filters = companySearchingFromParams params
      sorting = companySortingFromParams params
      (offset, limit) = companyPaginationFromParams companiesPageSize params
      companiesPageSize = 100

    allCompanies <- dbQuery $ GetCompanies domains filters sorting offset limit
    let companies = PagedList { list       = allCompanies
                              , params     = params
                              , pageSize   = companiesPageSize
                              }

    return . JSObject . toJSObject $
        [("list", JSArray $ map (\company ->
            JSObject . toJSObject $
                [("fields", JSObject . toJSObject $
                    [("id",             jsFromString . show . companyid $ company)
                    ,("companyname",    jsFromString . getCompanyName $ company)
                    ,("companynumber",  jsFromString . getCompanyNumber $ company)
                    ,("companyaddress", jsFromString . companyaddress . companyinfo $ company)
                    ,("companyzip",     jsFromString . companyzip . companyinfo $ company)
                    ,("companycity",    jsFromString . companycity . companyinfo $ company)
                    ,("companycountry", jsFromString . companycountry . companyinfo $ company)
                    ]
                 )
                ,("link", jsFromString . show . LinkCompanyAdmin . Just . companyid $ company)
                ]) (take companiesPageSize $ allCompanies) )
        ,("paging", pagingParamsJSON companies)
        ]

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


showAdminCompanyUsers :: Kontrakcja m => CompanyID -> m String
showAdminCompanyUsers cid = onlySalesOrAdmin $ adminCompanyUsersPage cid


showAdminUsersForSales :: Kontrakcja m => m String
showAdminUsersForSales = onlySalesOrAdmin $ adminUsersPageForSales

handleUsersListCSV :: Kontrakcja m => m CSV
handleUsersListCSV = onlySalesOrAdmin $ do
  users <- getUsersAndStatsInv [] [] (UserPagination 0 maxBound)
  return $ CSV { csvHeader = ["id", "fstname", "sndname", "email", "company", "position","tos"] 
               , csvFilename = "userslist.csv"
               , csvContent = map csvline $ filter active users
               }
  where
        active (u,_,_,_) =   (not (useraccountsuspended u)) && (isJust $ userhasacceptedtermsofservice u) && (isNothing $ userservice u)
        csvline (u,mc,_,_) =
                          [ show $ userid u
                          , getFirstName u
                          , getLastName u
                          , getEmail u
                          , getCompanyName mc
                          , usercompanyposition $ userinfo u
                          , show $ fromJust $ userhasacceptedtermsofservice u
                          ]


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
    x "company"     = [Asc UserOrderByCompanyName]
    x "companyREV"  = [Desc UserOrderByCompanyName]
    x "tos"         = [Asc UserOrderByAccountCreationDate]
    x "tosREV"      = [Desc UserOrderByAccountCreationDate]
    x _             = [Asc UserOrderByName]

userPaginationFromParams :: Int -> ListParams -> UserPagination
-- REVIEW: What is the magic constant 4 below?
userPaginationFromParams pageSize params = UserPagination (listParamsOffset params) (pageSize * 4)


jsonUsersList ::Kontrakcja m => m JSValue
jsonUsersList = do
    params <- getListParamsNew
    let filters = userSearchingFromParams params
        sorting = userSortingFromParams params
        pagination = userPaginationFromParams usersPageSize params
        usersPageSize = 100
    allUsers <- getUsersAndStatsInv filters sorting pagination
    let users = PagedList { list       = allUsers
                          , params     = params
                          , pageSize   = usersPageSize
                          }

    return $ JSObject
           $ toJSObject
            [("list", JSArray $ map (\(user,mcompany,docstats,itype) ->
                JSObject $ toJSObject
                    [("fields", JSObject $ toJSObject
                        [("id",       jsFromString . show $ userid user)
                        ,("username", jsFromString $ getFullName user)
                        ,("email",    jsFromString $ getEmail user)
                        ,("company",  jsFromString $ getCompanyName mcompany)
                        ,("phone",    jsFromString $ userphone $ userinfo user)
                        ,("tos",      jsFromString $ maybe "-" show (userhasacceptedtermsofservice user))
                        ,("signed_1m",     jsFromString . show $ signaturecount1m docstats)
                        ,("signed_2m",     jsFromString . show $ signaturecount2m docstats)
                        ,("signed_3m",     jsFromString . show $ signaturecount3m docstats)
                        ,("signed_6m",     jsFromString . show $ signaturecount6m docstats)
                        ,("signed_12m",    jsFromString . show $ signaturecount12m docstats)
                        ,("signed_docs",   jsFromString . show $ signaturecount docstats)
                        ,("uploaded_docs", jsFromString . show $ doccount docstats)
                        ,("viral_invites", JSBool $ not $ isAdminInvite itype)
                        ,("admin_invites", JSBool $ isAdminInvite itype)
                        ])
                    ,("link", jsFromString . show $ LinkUserAdmin $ Just $ userid user)
                    ]) (take usersPageSize $ list users)),
             ("paging", pagingParamsJSON users)]

jsFromString :: String -> JSValue
jsFromString = JSString . toJSString

isAdminInvite :: InviteType -> Bool
isAdminInvite Viral = False
isAdminInvite Admin = True




{- Shows table of all users-}
showAllUsersTable :: Kontrakcja m => m String
showAllUsersTable = onlySalesOrAdmin $ do
    users <- getUsersAndStatsInv [] [] (UserPagination 0 maxBound)
    allUsersTable users


{- | Handling user details change. It reads user info change -}
handleUserChange :: Kontrakcja m => UserID -> m KontraLink
handleUserChange uid = onlySalesOrAdmin $ do
  ctx <- getContext
  _ <- getAsString "change"
  museraccounttype <- getField "useraccounttype"
  olduser <- guardJustM $ dbQuery $ GetUserByID uid
  user <- case (museraccounttype, usercompany olduser, useriscompanyadmin olduser) of
    (Just "companyadminaccount", Just _companyid, False) -> do
      --then we just want to make this account an admin
      newuser <- guardJustM $ do
        _ <- dbUpdate $ SetUserCompanyAdmin uid True
        _ <- dbUpdate $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx) 
             [("is_company_admin", "false", "true")] 
             (userid <$> ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      return newuser
    (Just "companyadminaccount", Nothing, False) -> do
      --then we need to create a company and make this account an admin
      --we also need to tie all the existing docs to the company
      newuser <- guardJustM $ do
        company <- dbUpdate $ CreateCompany Nothing Nothing
        _ <- dbUpdate $ SetUserCompany uid (Just $ companyid company)
        _ <- dbUpdate 
                  $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx) 
                                             [("company_id", "null", show $ companyid company)] 
                                             (userid <$> ctxmaybeuser ctx)
        _ <- dbUpdate $ SetUserCompanyAdmin uid True
        _ <- dbUpdate 
                  $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx) 
                                             [("is_company_admin", "false", "true")] 
                                             (userid <$> ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      _ <- resaveDocsForUser uid
      return newuser
    (Just "companystandardaccount", Just _companyid, True) -> do
      --then we just want to downgrade this account to a standard
      newuser <- guardJustM $ do
        _ <- dbUpdate $ SetUserCompanyAdmin uid False
        _ <- dbUpdate 
                 $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx) 
                                            [("is_company_admin", "true", "false")] 
                                            (userid <$> ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      return newuser
    (Just "companystandardaccount", Nothing, False) -> do
      --then we need to create a company and make this account a standard user
      --we also need to tie all the existing docs to the company
      newuser <- guardJustM $ do
        company <- dbUpdate $ CreateCompany Nothing Nothing
        _ <- dbUpdate $ SetUserCompany uid (Just $ companyid company)
        _ <- dbUpdate 
                 $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx) 
                                            [("company_id", "null", show $ companyid company)] 
                                            (userid <$> ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      _ <- resaveDocsForUser uid
      return newuser
    (Just "privateaccount", Just _companyid, _) -> do
      --then we need to downgrade this user and possibly delete their company
      --we also need to untie all their existing docs from the company
      --we may also need to delete the company if it's empty, but i haven't implemented this bit
      newuser <- guardJustM $ do
        _ <- dbUpdate $ SetUserCompany uid Nothing
        _ <- dbUpdate 
                 $ LogHistoryDetailsChanged uid (ctxipnumber ctx) (ctxtime ctx) 
                                            [("company_id", show _companyid, "null")] 
                                            (userid <$> ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      _ <-resaveDocsForUser uid
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
  return $ LinkUserAdmin $ Just uid

resaveDocsForUser :: Kontrakcja m => UserID -> m ()
resaveDocsForUser uid = onlySalesOrAdmin $ do
  Context{ctxmaybeuser = Just admin, ctxtime, ctxipnumber} <- getContext
  let actor = adminActor ctxtime ctxipnumber (userid admin) (getEmail admin)
  user <- guardJustM $ dbQuery $ GetUserByID uid
  userdocs <- dbQuery $ GetDocumentsByAuthor uid
  mapM_ (\doc -> dbUpdate $ AdminOnlySaveForUser (documentid doc) user actor) userdocs
  return ()

{- | Handling company details change. It reads user info change -}
handleCompanyChange :: Kontrakcja m => CompanyID -> m KontraLink
handleCompanyChange companyid = onlySalesOrAdmin $ do
  _ <- getAsString "change"
  company <- guardJustM $ dbQuery $ GetCompany companyid
  companyInfoChange <- getCompanyInfoChange
  _ <- dbUpdate $ SetCompanyInfo companyid (companyInfoChange $ companyinfo company)
  mmailapi <- dbQuery $ GetCompanyMailAPI companyid
  when_ (isNothing mmailapi) $ do
    key <- random
    dbUpdate $ SetCompanyMailAPIKey companyid key 1000
  return $ LinkCompanyAdmin $ Just companyid

handleCreateUser :: Kontrakcja m => m KontraLink
handleCreateUser = onlySalesOrAdmin $ do
    email <- map toLower <$> getAsString "email"
    fstname <- getAsString "fstname"
    sndname <- getAsString "sndname"
    custommessage <- getField "custommessage"
    region <- guardJustM $ readField "region"
    muser <- createNewUserByAdmin email (fstname, sndname) custommessage Nothing (mkLocaleFromRegion region)
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
  return $ LinkCompanyUserAdmin companyid

handlePrivateUserCompanyInvite :: Kontrakcja m => CompanyID -> m ()
handlePrivateUserCompanyInvite companyid = onlySalesOrAdmin $ do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  email <- Email <$> getCriticalField asValidEmail "email"
  existinguser <- guardJustM $ dbQuery $ GetUserByEmail Nothing email
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
  region <- guardJustM $ readField "region"
  admin <- isFieldSet "iscompanyadmin"
  muser <- createNewUserByAdmin email (fstname, sndname) custommessage (Just (companyid, admin)) (mkLocaleFromRegion region)
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
  mcompanyemaildomain <- getField "companyemaildomain"
  return $ \CompanyInfo{..} ->  CompanyInfo {
        companyname        = fromMaybe companyname mcompanyname
      , companynumber      = fromMaybe companynumber mcompanynumber
      , companyaddress     = fromMaybe companyaddress mcompanyaddress
      , companyzip         = fromMaybe companyzip mcompanyzip
      , companycity        = fromMaybe companycity mcompanycity
      , companycountry     = fromMaybe companycountry mcompanycountry
      , companyemaildomain = case mcompanyemaildomain of
                               Just a | not $ null a -> Just a
                               _                     -> Nothing
    }

{- | Reads params and returns function for conversion of user settings.  No param leaves fields unchanged -}
getUserSettingsChange :: Kontrakcja m => m (UserSettings -> UserSettings)
getUserSettingsChange = do
  mregion <- readField "userregion"
  return $ \settings -> settings {
     locale = maybe (locale settings) mkLocaleFromRegion mregion
  }

{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}
getUserInfoChange :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoChange = do
  muserfstname         <- getField "userfstname"
  musersndname         <- getField "usersndname"
  muserpersonalnumber  <- getField "userpersonalnumber"
  musercompanyposition <- getField "usercompanyposition"
  muserphone           <- getField "userphone"
  musermobile          <- getField "usermobile"
  museremail           <- fmap Email <$> getField "useremail"
  musercompanyname     <- getField "usercompanyname"
  musercompanynumber   <- getField "usercompanynumber"
  return $ \UserInfo{..} -> UserInfo {
        userfstname         = fromMaybe userfstname muserfstname
      , usersndname         = fromMaybe usersndname musersndname
      , userpersonalnumber  = fromMaybe userpersonalnumber muserpersonalnumber
      , usercompanyposition = fromMaybe usercompanyposition musercompanyposition
      , userphone           = fromMaybe userphone muserphone
      , usermobile          = fromMaybe usermobile musermobile
      , useremail           = fromMaybe useremail museremail
      , usercompanyname     = fromMaybe usercompanyname musercompanyname
      , usercompanynumber   = fromMaybe usercompanynumber musercompanynumber
    }


{- Create service-}
handleCreateService :: (CryptoRNG m, Kontrakcja m) => m KontraLink
handleCreateService = onlySalesOrAdmin $ do
    name <- guardJustM $ getField "name"
    Log.debug $ "name: " ++ name
    admin <- guardJustM $ liftMM  (dbQuery . GetUserByEmail Nothing . Email) (getField "admin")
    Log.debug $ "admin: " ++ show admin
    pwdBS <- getField' "password"
    Log.debug $ "password: " ++ show pwdBS
    pwd <- createPassword pwdBS
    service <- guardJustM $ dbUpdate $ CreateService (ServiceID $ BS.fromString name) (Just pwd) (userid admin)
    Log.debug $ "service: " ++ show service
    location <- getField "location"
    Log.debug $ "location: " ++ show location
    _ <- dbUpdate $ UpdateServiceSettings (serviceid service) (servicesettings service)
                                               {servicelocation = ServiceLocation <$> location}
    Log.debug $ "LoopBack"
    return LoopBack

{- Services page-}
showServicesPage :: Kontrakcja m => m String
showServicesPage = onlySalesOrAdmin $ do
  services <- dbQuery GetServices
  servicesAdminPage services


{-
Sales leads stats:

User name
User email
Total finalized docs (total signatures)
Sales rep (editable, free text)
Status (1-5) (editable, free text)
Subaccounts
User company
User title
User phone
Date TOS accepted
Subacc (y/n)/Superaccount
-}

{-
Billing stats:

Superuser Company name
Superuser email
Payment plan
Next billing date
Last billing date
Last billing total fee
Current plan price
Current per signature price
Current TW storage price
-}

{-

Nr of Users
Total nr of signatures	*
Total nr of signatures of finished docs (these are the ones we charge for)	*
Nr of docs with cross status	*
Nr of docs with blue status	*
Nr of docs with green status	*
Nr of docs yellow status	*
Nr of docs with orange status	*
Nr of docs with red status	*
Nr of docs with red exclamation mark status	*
Nr of friend invites	*
Nr of SkrivaPå staff invites *
Nr of Signups after finalized offer	TODO
Nr of Signups after finalized contract  TODO
-}

{-
Total nr of signatures
Total nr of signatures of finished docs (these are the ones we charge for)
Nr of docs with cross status
Nr of docs with blue status
Nr of docs with green status
Nr of docs with yellow status
Nr of docs with orange status
Nr of docs with red status
Nr of docs with red exclamation mark status
Nr of friend invites
-}


{-
User list:

Email
Name
Title
Company
Phone
Sales rep
Used signatures total
Used signatures last 1 month
Used signatures last 2 months
Used signatures last 3 months
Used signatures last 6 months
Used signatures last 12 months
-}

{- |
    Shows statistics about functionality use.

    If you would like to add some stats then please add the definitions to
    the getStatDefinitions function of whatever HasFunctionalityStats instance
    is relevant.
    The getStatDefinitions defines each statistic as a label and definition function
    pair.  The label will describe it in the table.  And the definition function
    takes in the doc, user or siglink, and has to return a bool to indicate whether
    that object uses the particular functionality.
    So you should add a pair to the list to add a statistic.
-}
showFunctionalityStats :: Kontrakcja m => m String
showFunctionalityStats = onlySalesOrAdmin $ do
  ctx@Context{ ctxtime } <- getContext
  users <- dbQuery GetUsers
  documents <- dbQuery $ GetDocumentsByService $ currentServiceID ctx
  adminFunctionalityStatsPage (mkStats ctxtime users)
                              (mkStats ctxtime documents)
  where
    mkStats :: HasFunctionalityStats a => MinutesTime -> [a] -> [(String, Int)]
    mkStats time xs =
      map (\(label, deffunc) -> (label, length $ filter (\x -> isRecent time x && deffunc x) xs)) getStatDefinitions

class HasFunctionalityStats a where
  isRecent :: MinutesTime -> a -> Bool
  getStatDefinitions :: [(String, a -> Bool)]

aRecentDate :: MinutesTime -> MinutesTime
aRecentDate = minutesBefore (60 * 24 * 30 * 3)

instance HasFunctionalityStats Document where
  isRecent time doc = aRecentDate time < documentctime doc
  getStatDefinitions =
    [ ("drag n drop", anyField hasPlacement)
    , ("custom fields", anyField isCustom)
    , ("custom sign order", any ((/=) (SignOrder 1) . signatorysignorder . signatorydetails) . documentsignatorylinks)
    , ("csv", isJust . msum . fmap signatorylinkcsvupload . documentsignatorylinks)
    ]
    where
      anyField p doc =
        any p . concatMap (signatoryfields . signatorydetails) $ documentsignatorylinks doc
      hasPlacement SignatoryField{sfPlacements} = not $ null sfPlacements
      isCustom SignatoryField{sfType} =
        case sfType of
          (CustomFT _ _) -> True
          _ -> False

instance HasFunctionalityStats User where
  isRecent time user =
    case userhasacceptedtermsofservice user of
      Just tostime -> aRecentDate time < tostime
      Nothing -> False
  getStatDefinitions = []


showDocuments ::  Kontrakcja m => m  String
showDocuments = onlySalesOrAdmin $ adminDocuments =<< getContext

jsonDocuments :: Kontrakcja m => m JSValue
jsonDocuments = onlySalesOrAdmin $ do

  params <- getListParamsNew
  let sorting    = docSortingFromParams params
      searching  = docSearchingFromParams params
      pagination = docPaginationFromParams params
      filters    = []
      domain     = [DocumentsOfWholeUniverse]
      docsPageSize = 100

  allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting pagination

  let documents = PagedList { list       = allDocs
                            , params     = params
                            , pageSize   = docsPageSize
                            }

  return $ JSObject
           $ toJSObject
            [("list", JSArray $ map (\doc -> 
                JSObject $ toJSObject
                    [("fields", JSObject $ toJSObject
                        [ ("id", jsFromString $ show $ documentid doc)
                        , ("ctime", jsFromString . showMinutesTimeForAPI $ documentctime doc) 
                        , ("mtime", jsFromString . showMinutesTimeForAPI $ documentmtime doc) 
                        , ("author", JSObject $ toJSObject [
                              ("name", jsFromString $ getAuthorName doc)
                            , ("email", jsFromString $ maybe "" getEmail $ getAuthorSigLink doc)
                            , ("company", jsFromString $ maybe "" getCompanyName $ getAuthorSigLink doc)
                            ])
                        , ("title", jsFromString $ documenttitle doc)
                        , ("service", jsFromString $ maybe "" show $ documentservice doc)
                        , ("status", jsFromString $ take 20 $ show $ documentstatus doc)
                        , ("type", jsFromString . show $ documenttype doc)
                        , ("signs", JSArray $ map (jsFromString . getSmartName) $ documentsignatorylinks doc)
                        ])
                    ]) (take docsPageSize $ list documents))
            , ("paging", pagingParamsJSON documents)
            ]

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
    -- x "partner"        = comparePartners
    -- x "partnerREV"     = revComparePartners
    -- x "partnercomp"    = viewComparing partnerComps
    -- x "partnercompREV" = viewComparingRev partnerComps
    x "process"           = [Asc DocumentOrderByProcess]
    x "processREV"        = [Desc DocumentOrderByProcess]
    x "service"           = [Asc DocumentOrderByService]
    x "serviceREV"        = [Desc DocumentOrderByService]
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


docPaginationFromParams :: ListParams -> DocumentPagination
docPaginationFromParams params = DocumentPagination (listParamsOffset params) (listParamsLimit params)


handleBackdoorQuery :: Kontrakcja m => String -> m String
handleBackdoorQuery email = onlySalesOrAdmin $ onlyBackdoorOpen $ do
  minfo <- listToMaybe . filter ((email `elem`) . map addrEmail . mailTo)
    <$> dbQuery GetEmails
  return $ maybe "No email found" mailContent minfo

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
       guardTrueM $ dbUpdate $ FixClosedErroredDocument docid actor
    guardJustM  $ dbQuery $ GetDocumentByDocumentID docid   
  res <- sealDocument doc
  case res of
      Left  _ -> Log.debug "We failed to reseal the document"
      Right _ -> Log.debug "Ok, so the document has been resealed"
  return LoopBack


replaceMainFile :: Kontrakcja m => DocumentID -> m KontraLink
replaceMainFile did = onlyAdmin $ do
  Log.debug $ "Replaing main file | SUPER CRITICAL | If you see this check who did this and ask why"
  doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID did
  input <- getDataFnM (lookInput "file")
  case (input, documentfiles doc) of
       (Input contentspec _ _contentType, cf:_)  -> do
            content <- case contentspec of
                Left filepath -> liftIO $ BSL.readFile filepath
                Right c -> return c
            fn <- fromMaybe "file" <$> fmap filename <$> (dbQuery $ GetFileByFileID cf)
            Context{ctxipnumber,ctxtime, ctxmaybeuser = Just user} <- getContext
            let actor = adminActor ctxtime ctxipnumber (userid user) (getEmail user)
            file <- dbUpdate $ NewFile fn (Binary $ concatChunks content)
            _ <- dbUpdate $ ChangeMainfile did (fileid file) actor
            return LoopBack
       _ -> internalError

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
            F.value "label" $ show $ sfType sf
      where fieldTypeToString sf = case sf of
              FirstNameFT      -> "sigfstname"
              LastNameFT       -> "sigsndname"
              EmailFT          -> "sigemail"
              CompanyFT        -> "sigco"
              PersonalNumberFT -> "sigpersnr"
              CompanyNumberFT  -> "sigcompnr"
              SignatureFT      -> "signature"
              CustomFT label _ -> "Custom: " ++ label
              CheckboxOptionalFT label -> "Checkbox*: " ++ label
              CheckboxObligatoryFT label -> "Checkbox: " ++ label


updateFields :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
updateFields did slid = onlyAdmin $ do
  Log.debug $ "Changing signatory fields | SUPER CRITICAL | If you see this check who did this ask who did this and why"
  fieldname <- getDataFnM (look "fieldname")
  fieldvalue <- getDataFnM (look "fieldvalue")
  ctx <- getContext
  actor <- guardJust $ mkAdminActor ctx
  _ <- dbUpdate $ UpdateFieldsNoStatusCheck did slid (fieldname, fieldvalue) actor
  return LoopBack

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


serveLogDirectory ::  Kontrakcja m => String -> m Response
serveLogDirectory filename = onlyAdmin $ do
    contents <- liftIO $ getDirectoryContents "log"
    when (filename `notElem` contents) $ do
        Log.debug $ "Log '" ++ filename ++ "' not found"
        respond404
    (_,bsstdout,_) <- liftIO $ readProcessWithExitCode' "tail" ["log/" ++ filename, "-n", "40"] BSL.empty
    ok $ addHeader "Refresh" "5" $ toResponseBS (BS.fromString "text/plain; charset=utf-8") $ bsstdout
