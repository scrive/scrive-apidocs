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
          , showAdminUserAdvanced
          , showAdminUsers
          , showAdminCompanies
          , showAdminCompany
          , showAdminCompanyUsers
          , showAdminUsersForSales
          , showAdminUsersForPayments
          , showAdminUserUsageStats
          , showAdminCompanyUsageStats
          , showAllUsersTable
          , showStats
          , showServicesPage
          , showAdminTranslations
          , showDocuments
          , indexDB
          , getUsersDetailsToCSV
          , handleUserChange
          , handleCompanyChange
          , handleDatabaseCleanup
          , handleCreateUser
          , handlePostAdminCompanyUsers
          , handleCreateService
          , handleStatistics
          , showFunctionalityStats
          , handleBackdoorQuery
          , handleFixForBug510
          , handleFixForAdminOnlyBug
          , resealFile
          , replaceMainFile
          , handleCheckSigLinkIDUniqueness
          , daveDocument
          , daveUser
          , daveCompany
          , sysdump
          , serveLogDirectory
          , jsonUsersList
          , jsonCompanies
          , jsonDocuments
          ) where
import Control.Monad.State
import Data.Functor
import AppView
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (query)
import Misc
import Kontra
import Administration.AdministrationView
#ifndef DOCUMENTS_IN_POSTGRES
import Doc.DocControl (postDocumentChangeAction) -- required for 510 bug fix migration
import Happstack.State (update) -- required for 510 bug fix migration
import Doc.DocProcess
import Util.SignatoryLinkUtils
#endif
import Doc.Transitory
import Doc.DocStateData
import Data.ByteString.UTF8 (fromString,toString)
import Data.ByteString (ByteString, hGetContents)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as L
import Company.Model
import KontraLink
import MinutesTime
import System.Directory
import DB.Classes
import User.UserControl
import User.UserView
import User.Model
import Data.Maybe
import System.Process
import System.IO (hClose)
import Data.Char
import Happstack.Util.Common
import API.Service.Model
import Data.Monoid
import qualified Data.IntMap as IntMap
import Templates.Templates
import Text.Printf
import Util.FlashUtil
import Data.List
import Templates.TextTemplates
import Util.MonadUtils
import qualified AppLogger as Log
import Doc.DocSeal (sealDocument)
import Util.HasSomeUserInfo
import Redirect
import ActionSchedulerState
import Doc.DocInfo
import InputValidation
import User.Utils
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import InspectXMLInstances ()
import InspectXML
import ForkAction
import File.Model
import ListUtil
import Text.JSON
import Util.HasSomeCompanyInfo
import CompanyAccounts.CompanyAccountsControl
import CompanyAccounts.Model
import Util.SignatoryLinkUtils

{- | Main page. Redirects users to other admin panels -}
showAdminMainPage :: Kontrakcja m => m Response
showAdminMainPage = onlySuperUser $ do
  content <- adminMainPage
  renderFromBody TopEmpty kontrakcja content

{- | Process view for advanced user administration -}
showAdminUserAdvanced :: Kontrakcja m => m Response
showAdminUserAdvanced = onlySuperUser $ do
  users <- runDBQuery GetUsers
  mcompanies <- mapM getCompanyForUser users
  params <- getAdminListPageParams
  content <- adminUsersAdvancedPage (zip users mcompanies) params
  renderFromBody TopEmpty kontrakcja content

{- | Process view for finding a user in basic administration. If provided with userId string as param
it allows to edit user details -}
showAdminUsers :: Kontrakcja m => Maybe UserID -> m Response
showAdminUsers Nothing = onlySuperUser $ do
      content <- adminUsersPage
      renderFromBody TopEmpty kontrakcja content

showAdminUsers (Just userId) = onlySuperUser $ do
  muser <- runDBQuery $ GetUserByID userId
  case muser of
    Nothing -> mzero
    Just user -> do
      mcompany <- getCompanyForUser user
      content <- adminUserPage user mcompany
      renderFromBody TopEmpty kontrakcja content

showAdminCompanies :: Kontrakcja m => m String
showAdminCompanies = onlySuperUser $  adminCompaniesPage

showAdminCompany :: Kontrakcja m => CompanyID -> m String
showAdminCompany companyid = onlySuperUser $ adminCompanyPage =<< (guardJustM . runDBQuery $ GetCompany companyid)

jsonCompanies :: Kontrakcja m => m JSValue
jsonCompanies = onlySuperUser $ do
    params <- getListParamsNew
    allCompanies <- runDBQuery $ GetCompanies Nothing
    let companies = companiesSortSearchPage params allCompanies
    return . JSObject . toJSObject $
        [("list", JSArray $ map (\company ->
            JSObject . toJSObject $
                [("fields", JSObject . toJSObject $
                    [("id",             jsFromString . show . companyid $ company)
                    ,("companyname",    jsFromBString . getCompanyName $ company)
                    ,("companynumber",  jsFromBString . getCompanyNumber $ company)
                    ,("companyaddress", jsFromBString . companyaddress . companyinfo $ company)
                    ,("companyzip",     jsFromBString . companyzip . companyinfo $ company)
                    ,("companycity",    jsFromBString . companycity . companyinfo $ company)
                    ,("companycountry", jsFromBString . companycountry . companyinfo $ company)
                    ]
                 )
                ,("link", jsFromString . show . LinkCompanyAdmin . Just . companyid $ company)
                ]) (list companies) )
        ,("paging", pagingParamsJSON companies)
        ]

companiesSortSearchPage :: ListParams -> [Company] -> PagedList Company
companiesSortSearchPage =
    listSortSearchPage companiesSortFunc companiesSearchFunc companiesPageSize

companiesSortFunc :: SortingFunction Company
companiesSortFunc "companyname"       = viewComparing    (companyname    . companyinfo)
companiesSortFunc "companynameREV"    = viewComparingRev (companyname    . companyinfo)
companiesSortFunc "companynumber"     = viewComparing    (companynumber  . companyinfo)
companiesSortFunc "companynumberREV"  = viewComparingRev (companynumber  . companyinfo)
companiesSortFunc "companyaddress"    = viewComparing    (companyaddress . companyinfo)
companiesSortFunc "companyaddressREV" = viewComparingRev (companyaddress . companyinfo)
companiesSortFunc "companyzip"        = viewComparing    (companyzip     . companyinfo)
companiesSortFunc "companyzipREV"     = viewComparingRev (companyzip     . companyinfo)
companiesSortFunc "companycity"       = viewComparing    (companycity    . companyinfo)
companiesSortFunc "companycityREV"    = viewComparingRev (companycity    . companyinfo)
companiesSortFunc "companycountry"    = viewComparing    (companycountry . companyinfo)
companiesSortFunc "companycountryREV" = viewComparingRev (companycountry . companyinfo)
companiesSortFunc _                   = const $ const EQ

companiesSearchFunc :: SearchingFunction Company
companiesSearchFunc search_str company =
     any (isInfixOf (map toUpper search_str) . (map toUpper))
    $ [
        show $ companyid company
      , BS.toString $ companyname    $ companyinfo $ company
      , BS.toString $ companynumber  $ companyinfo $ company
      , BS.toString $ companyaddress $ companyinfo $ company
      , BS.toString $ companyzip     $ companyinfo $ company
      , BS.toString $ companycity    $ companyinfo $ company
      , BS.toString $ companycountry $ companyinfo $ company
      ]


companiesPageSize :: Int
companiesPageSize = 100

showAdminCompanyUsers :: Kontrakcja m => CompanyID -> m Response
showAdminCompanyUsers cid = onlySuperUser $ do
  content <- adminCompanyUsersPage cid
  renderFromBody TopEmpty kontrakcja content

showAdminUsersForSales :: Kontrakcja m => m String
showAdminUsersForSales = onlySuperUser $ adminUsersPageForSales

jsonUsersList ::Kontrakcja m => m JSValue
jsonUsersList = do
    params <- getListParamsNew
    allUsers <- getUsersAndStatsInv
    let users = usersSortSearchPage params allUsers
    return $ JSObject
           $ toJSObject
            [("list", JSArray $ map (\(user,mcompany,docstats,itype) ->
                JSObject $ toJSObject
                    [("fields", JSObject $ toJSObject
                        [("id",       jsFromString . show $ userid user)
                        ,("username", jsFromBString $ getFullName user)
                        ,("email",    jsFromBString $ getEmail user)
                        ,("company",  jsFromBString $ getCompanyName mcompany)
                        ,("phone",    jsFromBString $ userphone $ userinfo user)
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
                        ,("subaccounts", jsFromString "")
                        ])
                    ,("link", jsFromString . show $ LinkUserAdmin $ Just $ userid user)
                    ]) (list users)),
             ("paging", pagingParamsJSON users)]

jsFromString :: String -> JSValue
jsFromString = JSString . toJSString

jsFromBString :: BS.ByteString -> JSValue
jsFromBString = JSString . toJSString . BS.toString

isAdminInvite :: InviteType -> Bool
isAdminInvite Viral = False
isAdminInvite Admin = True

usersSortSearchPage :: ListParams -> [(User, Maybe Company, DocStats, InviteType)]
                       -> PagedList (User, Maybe Company, DocStats, InviteType)
usersSortSearchPage =
    listSortSearchPage usersSortFunc usersSearchFunc usersPageSize

usersSortFunc :: SortingFunction (User, Maybe Company, DocStats, InviteType)
usersSortFunc "username"       = viewComparing (getFullName . (\(u,_,_,_) -> u))
usersSortFunc "usernameREV"    = viewComparingRev (getFullName . (\(u,_,_,_) -> u))
usersSortFunc "email"          = viewComparing (getEmail . (\(u,_,_,_) -> u))
usersSortFunc "emailREV"       = viewComparingRev (getEmail . (\(u,_,_,_) -> u))
usersSortFunc "company"        = viewComparing (getCompanyName . (\(_,c,_,_) -> c))
usersSortFunc "companyREV"     = viewComparingRev (getCompanyName . (\(_,c,_,_) -> c))
usersSortFunc "phone"          = viewComparing (userphone . userinfo . (\(u,_,_,_) -> u))
usersSortFunc "phoneREV"       = viewComparingRev (userphone . userinfo . (\(u,_,_,_) -> u))
usersSortFunc "tos"            = viewComparing ((maybe "-" show) . (userhasacceptedtermsofservice . (\(u,_,_,_) -> u)))
usersSortFunc "tosREV"         = viewComparingRev ((maybe "-" show) . (userhasacceptedtermsofservice . (\(u,_,_,_) -> u)))
usersSortFunc "signed_docs"    = viewComparing (signaturecount . (\(_,_,d,_) -> d))
usersSortFunc "signed_docsREV" = viewComparingRev (signaturecount . (\(_,_,d,_) -> d))
usersSortFunc "signed_1m"      = viewComparing (signaturecount1m . (\(_,_,d,_) -> d))
usersSortFunc "signed_1mREV"   = viewComparingRev (signaturecount1m . (\(_,_,d,_) -> d))
usersSortFunc "signed_2m"      = viewComparing (signaturecount2m . (\(_,_,d,_) -> d))
usersSortFunc "signed_2mREV"   = viewComparingRev (signaturecount2m . (\(_,_,d,_) -> d))
usersSortFunc "signed_3m"      = viewComparing (signaturecount3m . (\(_,_,d,_) -> d))
usersSortFunc "signed_3mREV"   = viewComparingRev (signaturecount3m . (\(_,_,d,_) -> d))
usersSortFunc "signed_6m"      = viewComparing (signaturecount6m . (\(_,_,d,_) -> d))
usersSortFunc "signed_6mREV"   = viewComparingRev (signaturecount6m . (\(_,_,d,_) -> d))
usersSortFunc "signed_12m"     = viewComparing (signaturecount12m . (\(_,_,d,_) -> d))
usersSortFunc "signed_12mREV"  = viewComparingRev (signaturecount12m . (\(_,_,d,_) -> d))
usersSortFunc "uploaded_docs"    = viewComparing (doccount . (\(_,_,d,_) -> d))
usersSortFunc "uploaded_docsREV" = viewComparingRev (doccount . (\(_,_,d,_) -> d))
usersSortFunc "viral_invites"    = viewComparing (isAdminInvite . (\(_,_,_,i) -> i))
usersSortFunc "viral_invitesREV" = viewComparingRev (not . isAdminInvite . (\(_,_,_,i) -> i))
usersSortFunc _                = const $ const EQ

usersSearchFunc :: SearchingFunction (User, Maybe Company, DocStats, InviteType)
usersSearchFunc s userdata = userMatch userdata s
  where
      match s' m = isInfixOf (map toUpper s') (map toUpper (BS.toString m))
      userMatch (u,mc,_,_) s' = match s' (getCompanyName mc)
                             || match s' (getFirstName u)
                             || match s' (getLastName  u)
                             || match s' (getEmail u)

usersPageSize :: Int
usersPageSize = 100

showAdminUsersForPayments :: Kontrakcja m => m Response
showAdminUsersForPayments = onlySuperUser $ do
  users <- getUsersAndStats
  params <- getAdminListPageParams
  content <- adminUsersPageForPayments users params
  renderFromBody TopEmpty kontrakcja content

getUsersAndStats :: Kontrakcja m => m [(User, Maybe Company, DocStats)]
getUsersAndStats = do
    Context{ctxtime} <- getContext
    users <- runDBQuery GetUsers
    let queryStats user = do
          mcompany <- getCompanyForUser user
          docstats <- doc_query $ GetDocumentStatsByUser user ctxtime
          return (user, mcompany, docstats)
    users2 <- mapM queryStats users
    return users2

getUsersAndStatsInv :: Kontrakcja m => m [(User, Maybe Company, DocStats, InviteType)]
getUsersAndStatsInv = do
    users <- getUsersAndStats
    mapM addInviteType users
  where
    addInviteType = \(user,mcompany,docstats) -> do
        invite <- runDBQuery $ GetInviteInfo (userid user)
        let invitestype = case invite of
                          Nothing                     -> Admin
                          Just (InviteInfo _ _ mtype) -> fromMaybe Admin mtype
        return (user,mcompany,docstats,invitestype)

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySuperUser $ do
  documents <- doc_query $ GetDocumentsByAuthor userid
  Just user <- runDBQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user
  content <- adminUserUsageStatsPage user mcompany $ do
    statisticsFieldsForASingleUser documents
  renderFromBody TopEmpty kontrakcja content

showAdminCompanyUsageStats :: Kontrakcja m => CompanyID -> m Response
showAdminCompanyUsageStats companyid = onlySuperUser $ do
  users <- runDBQuery $ GetCompanyAccounts companyid
  userdocs <- mapM (doc_query . GetDocumentsByAuthor . userid) users
  let documents = concat userdocs
  Log.debug $ "There are " ++ (show $ length documents) ++ " docs related to company " ++ (show companyid)
  content <- adminCompanyUsageStatsPage companyid $ do
    fieldsFromStats [] documents
  renderFromBody TopEmpty kontrakcja content

{- Shows table of all users-}
showAllUsersTable :: Kontrakcja m => m Response
showAllUsersTable = onlySuperUser $ do
    users <- getUsersAndStats
    content <- allUsersTable users
    renderFromBody TopEmpty kontrakcja content


#ifndef WINDOWS
read_df :: IO ByteString
read_df = do
  (_,Just handle_out,_,handle_process) <-
      createProcess (proc "df" []) { std_out = CreatePipe, env = Just [("LANG","C")] }
  s <- hGetContents handle_out
  hClose handle_out
  _ <- waitForProcess handle_process
  return s
#endif


showStats :: Kontrakcja m => m Response
showStats = onlySuperUser $ do
    docstats <- doc_query GetDocumentStats
#ifndef WINDOWS
    df <- liftIO read_df
#else
    let df = empty
#endif
    let stats = StatsView { svDoccount = doccount docstats,
                            svSignaturecount = signaturecount docstats }
    content <- statsPage stats $ toString df
    renderFromBody TopEmpty kontrakcja content

indexDB :: Kontrakcja m => m Response
indexDB = onlySuperUser $ do
    files <- liftIO $ getDirectoryContents "_local/kontrakcja_state"
    content <- databaseContent $ sort files
    renderFromBody TopEmpty kontrakcja content

getUsersDetailsToCSV :: Kontrakcja m => m Response
getUsersDetailsToCSV = onlySuperUser $ do
      x <- runDBQuery ExportUsersDetailsToCSV
      let response = toResponseBS (fromString "text/csv") (L.fromChunks [x])
      return response

{- | Handling user details change. It reads user info change -}
handleUserChange :: Kontrakcja m => UserID -> m KontraLink
handleUserChange uid = onlySuperUser $ do
  _ <- getAsStrictBS "change"
  museraccounttype <- getFieldUTF "useraccounttype"
  olduser <- runDBOrFail $ dbQuery $ GetUserByID uid
  user <- case (fmap toString museraccounttype, usercompany olduser, useriscompanyadmin olduser) of
    (Just "companyadminaccount", Just _companyid, False) -> do
      --then we just want to make this account an admin
      newuser <- runDBOrFail $ do
        _ <- dbUpdate $ SetUserCompanyAdmin uid True
        dbQuery $ GetUserByID uid
      return newuser
    (Just "companyadminaccount", Nothing, False) -> do
      --then we need to create a company and make this account an admin
      --we also need to tie all the existing docs to the company
      newuser <- runDBOrFail $ do
        company <- dbUpdate $ CreateCompany Nothing Nothing
        _ <- dbUpdate $ SetUserCompany uid (Just $ companyid company)
        _ <- dbUpdate $ SetUserCompanyAdmin uid True
        dbQuery $ GetUserByID uid
      _ <- resaveDocsForUser uid
      return newuser
    (Just "companystandardaccount", Just _companyid, True) -> do
      --then we just want to downgrade this account to a standard
      newuser <- runDBOrFail $ do
        _ <- dbUpdate $ SetUserCompanyAdmin uid False
        dbQuery $ GetUserByID uid
      return newuser
    (Just "companystandardaccount", Nothing, False) -> do
      --then we need to create a company and make this account a standard user
      --we also need to tie all the existing docs to the company
      newuser <- runDBOrFail $ do
        company <- dbUpdate $ CreateCompany Nothing Nothing
        _ <- dbUpdate $ SetUserCompany uid (Just $ companyid company)
        dbQuery $ GetUserByID uid
      _ <- resaveDocsForUser uid
      return newuser
    (Just "privateaccount", Just _companyid, _) -> do
      --then we need to downgrade this user and possibly delete their company
      --we also need to untie all their existing docs from the company
      --we may also need to delete the company if it's empty, but i haven't implemented this bit
      newuser <- runDBOrFail $ do
        _ <- dbUpdate $ SetUserCompany uid Nothing
        dbQuery $ GetUserByID uid
      _ <-resaveDocsForUser uid
      return newuser
    _ -> return olduser
  infoChange <- getUserInfoChange
  _ <- runDBUpdate $ SetUserInfo uid $ infoChange $ userinfo user
  settingsChange <- getUserSettingsChange
  _ <- runDBUpdate $ SetUserSettings uid $ settingsChange $ usersettings user
  return $ LinkUserAdmin $ Just uid

resaveDocsForUser :: Kontrakcja m => UserID -> m ()
resaveDocsForUser uid = onlySuperUser $ do
  user <- runDBOrFail $ dbQuery $ GetUserByID uid
  userdocs <- doc_query $ GetDocumentsByUser user
  mapM_ (\doc -> doc_update $ AdminOnlySaveForUser (documentid doc) user) userdocs
  return ()

{- | Handling company details change. It reads user info change -}
handleCompanyChange :: Kontrakcja m => CompanyID -> m KontraLink
handleCompanyChange companyid = onlySuperUser $ do
  _ <- getAsStrictBS "change"
  company <- runDBOrFail $ dbQuery $ GetCompany companyid
  companyInfoChange <- getCompanyInfoChange
  _ <- runDBUpdate $ SetCompanyInfo companyid (companyInfoChange $ companyinfo company)
  return $ LinkCompanyAdmin $ Just companyid

{-| Cleaning the database -}
handleDatabaseCleanup :: Kontrakcja m => m KontraLink
handleDatabaseCleanup = onlySuperUser $  do
    -- dangerous, cleanup all old files, where old means chechpoints but the last one
    -- and all events that have numbers less than last checkpoint
    _ <- liftIO databaseCleanupWorker
    return LinkAdminOnlyIndexDB

databaseCleanupWorker :: IO [FilePath]
databaseCleanupWorker = do
  contents <- getDirectoryContents "_local/kontrakcja_state"
  let checkpoints = filter ("checkpoints-" `isPrefixOf`) contents
  let events = filter ("events-" `isPrefixOf`) contents
  let lastcheckpoint = last (sort checkpoints)
  let cutoffevent = "events-" ++ drop 12 lastcheckpoint
  let eventsToRemove = filter (< cutoffevent) events
  let checkpointsToRemove = filter (< lastcheckpoint) checkpoints
  mapM_ (\x -> removeFile ("_local/kontrakcja_state/" ++ x)) (eventsToRemove ++ checkpointsToRemove)
  getDirectoryContents "_local/kontrakcja_state" --This can be dropped


handleCreateUser :: Kontrakcja m => m KontraLink
handleCreateUser = onlySuperUser $ do
    ctx <- getContext
    email' <- getAsStrictBS "email"
    let email = BSC.map toLower email'
    fstname <- getAsStrictBS "fstname"
    sndname <- getAsStrictBS "sndname"
    custommessage <- getField "custommessage"
    freetill <- fmap (join . (fmap parseMinutesTimeDMY)) $ getField "freetill"
    region <- guardJustM $ readField "region"
    muser <- createNewUserByAdmin ctx (fstname, sndname) email freetill custommessage (mkLocaleFromRegion region)
    when (isNothing muser) $
      addFlashM flashMessageUserWithSameEmailExists
    -- FIXME: where to redirect?
    return LinkStats

handlePostAdminCompanyUsers :: Kontrakcja m => CompanyID -> m KontraLink
handlePostAdminCompanyUsers companyid = onlySuperUser $ do
  privateinvite <- isFieldSet "privateinvite"
  if privateinvite
    then handlePrivateUserCompanyInvite companyid
    else handleCreateCompanyUser companyid
  return $ LinkCompanyUserAdmin companyid

handlePrivateUserCompanyInvite :: Kontrakcja m => CompanyID -> m ()
handlePrivateUserCompanyInvite companyid = onlySuperUser $ do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  email <- Email <$> getCriticalField asValidEmail "email"
  existinguser <- guardJustM $ runDBQuery $ GetUserByEmail Nothing email
  _ <- runDBUpdate $ AddCompanyInvite CompanyInvite{
          invitedemail = email
        , invitedfstname = getFirstName existinguser
        , invitedsndname = getLastName existinguser
        , invitingcompany = companyid
        }
  company <- guardJustM $ runDBQuery $ GetCompany companyid
  sendTakeoverPrivateUserMail user company existinguser

handleCreateCompanyUser :: Kontrakcja m => CompanyID -> m ()
handleCreateCompanyUser companyid = onlySuperUser $ do
  ctx <- getContext
  email <- getCriticalField asValidEmail "email"
  fstname <- getCriticalField asValidName "fstname"
  sndname <- getCriticalField asValidName "sndname"
  custommessage <- getField "custommessage"
  region <- guardJustM $ readField "region"
  madmin <- getOptionalField asValidCheckBox "iscompanyadmin"
  muser <- createNewUserByAdmin ctx (fstname, sndname) email Nothing custommessage (mkLocaleFromRegion region)
  case muser of
    Just (User{userid}) -> do
      _ <- runDBUpdate $ SetUserCompany userid (Just companyid)
      when (fromMaybe False madmin) $ do
        _ <- runDBUpdate $ SetUserCompanyAdmin userid True
        return ()
    Nothing -> addFlashM flashMessageUserWithSameEmailExists
  return ()

{- | Reads params and returns function for conversion of company info.  With no param leaves fields unchanged -}
getCompanyInfoChange :: Kontrakcja m => m (CompanyInfo -> CompanyInfo)
getCompanyInfoChange = do
  mcompanyname    <- getFieldUTF "companyname"
  mcompanynumber  <- getFieldUTF "companynumber"
  mcompanyaddress <- getFieldUTF "companyaddress"
  mcompanyzip     <- getFieldUTF "companyzip"
  mcompanycity    <- getFieldUTF "companycity"
  mcompanycountry <- getFieldUTF "companycountry"
  return $ \CompanyInfo {
      companyname
    , companynumber
    , companyaddress
    , companyzip
    , companycity
    , companycountry
    } ->  CompanyInfo {
        companyname =  fromMaybe companyname mcompanyname
      , companynumber  =  fromMaybe companynumber mcompanynumber
      , companyaddress =  fromMaybe companyaddress mcompanyaddress
      , companyzip = fromMaybe companyzip mcompanyzip
      , companycity  = fromMaybe companycity mcompanycity
      , companycountry = fromMaybe companycountry mcompanycountry
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
  muserfstname         <- getFieldUTF "userfstname"
  musersndname         <- getFieldUTF "usersndname"
  muserpersonalnumber  <- getFieldUTF "userpersonalnumber"
  musercompanyposition <- getFieldUTF "usercompanyposition"
  muserphone           <- getFieldUTF "userphone"
  musermobile          <- getFieldUTF "usermobile"
  museremail           <- fmap Email <$> getFieldUTF "useremail"
  return $ \UserInfo {
      userfstname
    , usersndname
    , userpersonalnumber
    , usercompanyposition
    , userphone
    , usermobile
    , useremail
    } ->  UserInfo {
      userfstname = fromMaybe userfstname muserfstname
      , usersndname = fromMaybe usersndname musersndname
      , userpersonalnumber = fromMaybe userpersonalnumber muserpersonalnumber
      , usercompanyposition = fromMaybe usercompanyposition musercompanyposition
      , userphone = fromMaybe userphone muserphone
      , usermobile = fromMaybe usermobile musermobile
      , useremail =  fromMaybe useremail museremail
    }

{- | Reads params and returns structured params for user managment pages. -}
getAdminListPageParams :: Kontrakcja m => m AdminListPageParams
getAdminListPageParams = do
  search <- getDataFn' (look "search")
  startletter <-  getDataFn' (look "startletter")
  mpage <-  getDataFn' (look "page")
  let (mpage'::Maybe Int) = join $ fmap readM mpage
  return $ AdminListPageParams {search = search, startletter=startletter, page = maybe 0 id mpage'}


{- Create service-}
handleCreateService :: Kontrakcja m => m KontraLink
handleCreateService = onlySuperUser $ do
    name <- guardJustM $ getFieldUTF "name"
    Log.debug $ "name: " ++ show name
    admin <- guardJustM $ liftMM  (runDBQuery . GetUserByEmail Nothing . Email) (getFieldUTF "admin")
    Log.debug $ "admin: " ++ show admin
    pwdBS <- getFieldUTFWithDefault mempty "password"
    Log.debug $ "password: " ++ show pwdBS
    pwd <- liftIO $ createPassword pwdBS
    service <- guardJustM $ runDBUpdate $ CreateService (ServiceID name) (Just pwd) (userid admin)
    Log.debug $ "service: " ++ show service
    location <- getFieldUTF "location"
    Log.debug $ "location: " ++ show location
    _ <- runDBUpdate $ UpdateServiceSettings (serviceid service) (servicesettings service)
                                               {servicelocation = ServiceLocation <$> location}
    Log.debug $ "LoopBack"
    return LoopBack

{- Services page-}
showServicesPage :: Kontrakcja m => m Response
showServicesPage = onlySuperUser $ do
  conn <- getConnection
  services <- runDBQuery GetServices
  content <- servicesAdminPage conn services
  renderFromBody TopEmpty kontrakcja content


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

data DocStatsL = DocStatsL
                { dsAllDocuments :: !Int
                , dsPreparationDocuments :: !Int
                , dsPendingDocuments :: !Int
                , dsCanceledDocuments :: !Int
                , dsTimedOutDocuments :: !Int
                , dsClosedDocuments :: !Int
                , dsRejectedDocuments :: !Int
                , dsAwaitingAuthorDocuments :: !Int
                , dsErrorDocuments :: !Int
                , dsAllSignatures :: !Int
                , dsSignaturesInClosed :: !Int

                , dsAllUsers :: !Int
                , dsViralInvites :: !Int
                , dsAdminInvites :: !Int
                }

docStatsZero :: DocStatsL
docStatsZero = DocStatsL 0 0 0 0 0 0 0 0 0 0 0 0 0 0

addStats :: DocStatsL -> DocStatsL -> DocStatsL
addStats (DocStatsL a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14) (DocStatsL b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14) =
      DocStatsL (a1+b1) (a2+b2) (a3+b3) (a4+b4) (a5+b5) (a6+b6) (a7+b7) (a8+b8) (a9+b9) (a10+b10) (a11+b11) (a12+b12) (a13+b13) (a14+b14)

addStats1 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addStats1 (_, t1, s1, i1) (t, t2, s2, i2) = (t, t1+t2, s1+s2, i1+i2)

sumStats :: [(Int, Int, Int, Int)] -> (Int, Int, Int, Int)
sumStats = foldl1 addStats1

-- Stats are very simple:
-- Date
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
newCalculateStatsFromDocuments :: [Document] -> [(Int, Int, Int, Int)]
newCalculateStatsFromDocuments docs =
  let cls = [(asInt $ getLastSignedTime d, 1, countSignatures d, 0) | d <- docs, isClosed d]
      pds = [(asInt $ fromJust $ getInviteTime d, 0, 0, 1)                     | d <- docs, isPending d, isJust $ getInviteTime d]
      byDay = groupWith (\(a,_,_,_)->a) $ reverse $ sortWith (\(a,_,_,_)->a) (cls ++ pds)
  in map sumStats byDay

calculateStatsFromDocuments :: [Document] -> IntMap.IntMap DocStatsL
calculateStatsFromDocuments documents =
  foldl' ins IntMap.empty documents
  where
    ins mapfunc doc = foldl' (\m (k,v) -> IntMap.insertWith addStats k v m) mapfunc (stuff doc)
    stuff doc = [ (asInt $ documentctime doc, docStatsZero { dsAllDocuments = 1
                                                           , dsAllSignatures = countSignatures doc
                                                           , dsSignaturesInClosed = if documentstatus doc == Closed
                                                                                    then countSignatures doc
                                                                                    else 0
                                                           })
                , (asInt $ documentmtime doc, case documentstatus doc of
                      Preparation -> docStatsZero { dsPreparationDocuments = 1}
                      Pending     -> docStatsZero { dsPendingDocuments = 1}
                      Rejected    -> docStatsZero { dsRejectedDocuments = 1}
                      Canceled    -> docStatsZero { dsCanceledDocuments = 1}
                      DocumentError {}    -> docStatsZero { dsErrorDocuments = 1}
                      Closed      -> docStatsZero { dsClosedDocuments = 1}
                      Timedout    -> docStatsZero { dsTimedOutDocuments = 1}
                      AwaitingAuthor -> docStatsZero {dsAwaitingAuthorDocuments = 1}
                      --_ -> docStatsZero  -- catch all to make it run in case somebody adds new status
                      )
                ]

showAsDate1 :: Int -> String
showAsDate1 int = printf "%04d-%02d-%02d" (int `div` 10000) (int `div` 100 `mod` 100) (int `mod` 100)

statisticsFieldsForASingleUser :: (Functor m, MonadIO m) => [Document] -> Fields m
statisticsFieldsForASingleUser ds =
  let stats = newCalculateStatsFromDocuments ds in
  fieldFL "statistics" $ for stats (\(ct, c, s, i) -> do
                                       field "date" $ showAsDate1 ct
                                       field "closed" c
                                       field "signatures" s
                                       field "sent" i
                                       field "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double)))



fieldsFromStats :: (Functor m, MonadIO m) => [User] -> [Document] -> Fields m
fieldsFromStats _users documents = do
    let userStats = IntMap.empty -- calculateStatsFromUsers users
        documentStats = calculateStatsFromDocuments documents
        stats' = IntMap.toList (IntMap.unionWith addStats userStats documentStats)
        lastMonthStats = take 30 (reverse stats')
        allMonthsStats = reverse $ IntMap.toList $ IntMap.fromListWith addStats (map ( \(k,v) -> (k `div` 100 * 100, v)) stats')
    let fieldify showDate (date,stat) = do
          field "date" $ showDate date
          fieldF "documents" $ do
            field "all" $ dsAllDocuments stat
            field "preparation" $ dsPreparationDocuments stat
            field "pending" $ dsPendingDocuments stat
            field "error" $ dsErrorDocuments stat
            field "timeout" $ dsTimedOutDocuments stat
            field "awaitingauthor" $ dsAwaitingAuthorDocuments stat
            field "closed" $ dsClosedDocuments stat
            field "rejected" $ dsRejectedDocuments stat
            field "canceled" $ dsCanceledDocuments stat
            field "signatures" $ dsAllSignatures stat
            field "signaturesInClosed" $ dsSignaturesInClosed stat
          fieldF "users" $ do
            field "all" $ dsAllUsers stat
            field "viralInvites" $ dsViralInvites stat
            field "adminInvites" $ dsAdminInvites stat

    fieldFL "lastMonthStats" $ map (fieldify showAsDate) lastMonthStats
    fieldFL "allMonthsStats" $ map (fieldify showAsMonth) allMonthsStats

handleStatistics :: Kontrakcja m => m Response
handleStatistics =
  onlySuperUser $ do
    ctx <- getContext
    documents <- doc_query $ GetDocuments $ currentServiceID ctx
    users <- runDBQuery GetUsers
    content <- renderTemplateFM "statisticsPage" $ do
      fieldsFromStats users documents
    renderFromBody TopEmpty kontrakcja content

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
showFunctionalityStats :: Kontrakcja m => m Response
showFunctionalityStats = onlySuperUser $ do
  ctx@Context{ ctxtime } <- getContext
  users <- runDBQuery GetUsers
  documents <- doc_query $ GetDocuments $ currentServiceID ctx
  content <- adminFunctionalityStatsPage (mkStats ctxtime users)
                                         (mkStats ctxtime documents)
  renderFromBody TopEmpty kontrakcja content
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
    , ("csv", isJust . documentcsvupload)
    ]
    where
      anyField p doc =
        any p . concat . map (signatoryfields . signatorydetails) $ documentsignatorylinks doc
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

showAdminTranslations :: Kontrakcja m => m String
showAdminTranslations = do
    liftIO $ updateCSV
    adminTranslationsPage

handleBackdoorQuery :: Kontrakcja m => String -> m Response
handleBackdoorQuery email = onlySuperUser $ onlyBackdoorOpen $ do
  minfo <- query $ GetBackdoorInfoByEmail (Email $ fromString email)
  let mailcontent = maybe "No email found" (toString . bdContent) minfo
  renderFromBody TopEmpty kontrakcja mailcontent

{- |
    There was a bug where the adminonly upgrade from private to company user
    wasn't connecting the new company to the user's existing docs.
    This should fix any of those docs affected by that bug.
-}
handleFixForAdminOnlyBug :: Kontrakcja m => m Response
handleFixForAdminOnlyBug = onlySuperUser $ do
  users <- runDBQuery GetUsers
  mapM_ maybeFixForUser users
  Log.debug $ "finished adminonly bug fix"
  sendRedirect LinkUpload
  where
    maybeFixForUser user = do
      userdocs <- doc_query $ GetDocumentsByUser user
      mapM_ (maybeFixForUserAndDoc user) userdocs
      return ()
    maybeFixForUserAndDoc user doc =
      if isFixNeeded user doc
        then do
          Log.debug $ "fixing for doc " ++ (show $ documentid doc) ++ " and user " ++ (show $ userid user) ++ " " ++ (show $ getEmail user)
          _ <- doc_update $ AdminOnlySaveForUser (documentid doc) user
          return ()
        else return ()
    isFixNeeded user doc =
      any (isBadSigLink user) (documentsignatorylinks doc)
    isBadSigLink user siglink =
      Just (userid user) == maybesignatory siglink && usercompany user /= maybecompany siglink

{- |
    This handles fixing of documents broken by bug 510, which means
    that the authors were mistakenly made signatories of offers or orders.
-}
handleFixForBug510 :: Kontrakcja m => m Response
handleFixForBug510 = onlySuperUser $ do
#ifndef DOCUMENTS_IN_POSTGRES
  services <- runDBQuery $ GetServices
  mapM_ fixForService $ Nothing : map (Just . serviceid) services
  sendRedirect LinkUpload
  where
    fixForService :: Kontrakcja m => Maybe ServiceID -> m ()
    fixForService service = do
      docs <- doc_query $ GetDocuments service
      mapM_ maybeFixForDocument docs
      return ()
    maybeFixForDocument :: Kontrakcja m => Document -> m ()
    maybeFixForDocument doc =
      let isauthorsendonly = Just True == getValueForProcess doc processauthorsend
          isauthorsigning = maybe False (elem SignatoryPartner . signatoryroles) (getAuthorSigLink doc)
          nonauthorpartisgood = nonAuthorPartLooksGood doc
          hasauthorsigned = maybe False (isJust . maybesigninfo) (getAuthorSigLink doc) in
      case (isauthorsendonly && isauthorsigning,
            nonauthorpartisgood,
            hasauthorsigned) of
        (True, False, _) -> Log.debug $ mkMsg doc
          "broken, but because of counterparts looks like it was broken by something else, leaving for now"
        (True, _, True) -> Log.debug $ mkMsg doc
          "broken, but the author has already signed, so unsure how to fix, leaving for now"
        (True, True, False) -> do
          Log.debug $ mkMsg doc "fixing"
          udoc <- guardRightM . update $ FixBug510ForDocument (documentid doc)
          postDocumentChangeAction udoc doc Nothing
        _ -> return ()
    nonAuthorPartLooksGood :: Document -> Bool
    nonAuthorPartLooksGood doc =
      let nonauthorparts = filter (not . isAuthor) $ documentsignatorylinks doc
      in case nonauthorparts of
        (nonauthorpart:[]) | SignatoryPartner `elem` (signatoryroles nonauthorpart) -> True
        _ -> False
    mkMsg :: Document -> String -> String
    mkMsg doc msg = "Handling 510 bug fix for " ++
                    " doc " ++ (show $ documentid doc) ++
                    " with type " ++ (show $ documenttype doc) ++
                    " and author " ++ (show . getEmail . fromJust $ getAuthorSigLink doc) ++ " : " ++ msg
#else
   mzero
#endif

-- This method can be used do reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlySuperUser $ do
  Log.debug $ "Trying to reseal document "++ show docid ++" | Only superadmin can do that"
  mdoc <- doc_query $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing -> mzero
    Just doc -> do
        ctx <- getContext
        Log.debug "Document is valid for resealing sealing"
        res <- runDB $ sealDocument ctx doc
        case res of
            Left  _ -> Log.debug "We failed to reseal the document"
            Right _ -> Log.debug "Ok, so the document has been resealed"
        return LoopBack

replaceMainFile :: Kontrakcja m => DocumentID -> m KontraLink
replaceMainFile did = onlySuperUser $ do
  Log.debug $ "Replaing main file | SUPER CRITICAL | If you see this check who did this ask who did this and why"
  doc <- guardJustM $ doc_query $ GetDocumentByDocumentID did
  input <- getDataFnM (lookInput "file")
  case (input, documentfiles doc) of
       (Input contentspec _ _contentType, cf:_)  -> do
            content <- case contentspec of
                Left filepath -> liftIO $ BSL.readFile filepath
                Right c -> return c
            fn <- fromMaybe (BS.fromString "file") <$> fmap filename <$> (runDB $ dbQuery $ GetFileByFileID cf)
            file <- runDB $ dbUpdate $ NewFile fn (concatChunks content)
            _ <- doc_update $ ChangeMainfile did (fileid file)
            return LoopBack
       _ -> mzero



handleCheckSigLinkIDUniqueness :: Kontrakcja m => m String
handleCheckSigLinkIDUniqueness = do
  siglinkids <- doc_query GetSignatoryLinkIDs
  if length siglinkids == length (nub siglinkids)
     then return "Signatory link ids are unique globally."
     else return "Signatory link ids are NOT unique globally."

showDocuments ::  Kontrakcja m => m (Either KontraLink String)
showDocuments = onlySuperUserGet $ adminDocuments

jsonDocuments :: Kontrakcja m => m JSValue
jsonDocuments = onlySuperUser $ do
    srvs <- runDBQuery $ GetServices
    docs <- join <$> (sequence $ map (doc_query . GetDocuments) (Nothing:(map (Just . serviceid) srvs)))
    params <- getListParamsNew
    let documents = documentsSortSearchPage params docs
    return $ JSObject
           $ toJSObject
            [("list", JSArray $ map (\doc -> 
                JSObject $ toJSObject
                    [("fields", JSObject $ toJSObject
                        [ ("id", jsFromString $ show $ documentid doc)
                        , ("ctime", jsFromString . showMinutesTimeForAPI $ documentctime doc) 
                        , ("mtime", jsFromString . showMinutesTimeForAPI $ documentmtime doc) 
                        , ("author", JSObject $ toJSObject [
                              ("name", jsFromBString $ maybe (BS.fromString "") getSmartName $ getAuthorSigLink doc)
                            , ("email", jsFromBString $ maybe (BS.fromString "") getEmail $ getAuthorSigLink doc)
                            , ("company", jsFromBString $ maybe (BS.fromString "") getCompanyName $ getAuthorSigLink doc)
                            ])
                        , ("title", jsFromBString $ documenttitle doc)
                        , ("service", jsFromString $ maybe "" show $ documentservice doc)
                        , ("status", jsFromString $ take 20 $ show $ documentstatus doc)
                        , ("type", jsFromString . show $ documenttype doc)
                        , ("signs", JSArray $ map (jsFromBString . getSmartName) $ documentsignatorylinks doc)
                        ])
                    ]) (list documents))
            , ("paging", pagingParamsJSON documents)
            ]

documentsSortSearchPage :: ListParams -> [Document] -> PagedList Document
documentsSortSearchPage = 
    listSortSearchPage documentsSortFunc documentsSearchFunc documentsPageSize

documentsSortFunc :: SortingFunction Document
documentsSortFunc "ctime"      = viewComparing documentctime
documentsSortFunc "ctimeREV"   = viewComparingRev documentctime
documentsSortFunc "author"     = viewComparing ((maybe (BS.fromString "") getSmartName) . getAuthorSigLink)
documentsSortFunc "authorREV"  = viewComparingRev ((maybe (BS.fromString "") getSmartName) . getAuthorSigLink)
documentsSortFunc "title"      = viewComparing documenttitle
documentsSortFunc "titleREV"   = viewComparingRev documenttitle
documentsSortFunc "service"    = viewComparing ((maybe "" show) . documentservice)
documentsSortFunc "serviceREV" = viewComparingRev ((maybe "" show) . documentservice)
documentsSortFunc "status"     = viewComparing ((take 20) . show . documentstatus)
documentsSortFunc "statusREV"  = viewComparingRev ((take 20) . show . documentstatus)
documentsSortFunc "type"       = viewComparing documenttype
documentsSortFunc "typeREV"    = viewComparingRev documenttype
documentsSortFunc "signs"      = viewComparing ((map getSmartName) . documentsignatorylinks)
documentsSortFunc "signsREV"   = viewComparingRev ((map getSmartName) . documentsignatorylinks)
documentsSortFunc _            = const $ const EQ

documentsSearchFunc :: SearchingFunction Document
documentsSearchFunc s doc =  nameMatch doc || signMatch doc
    where
    match m = isInfixOf (map toUpper s) (map toUpper m)
    nameMatch = match . BS.toString . documenttitle
    signMatch d = any (match . BS.toString . getSmartName) (documentsignatorylinks d)



documentsPageSize :: Int
documentsPageSize = 100

{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: Kontrakcja m => DocumentID -> m (Either KontraLink String)
daveDocument documentid = onlySuperUserGet $ do
    document <- queryOrFail $ GetDocumentByDocumentID documentid
    renderTemplateFM  "daveDocument" $ do
        field "daveBody" $  inspectXML document
        field "id" $ show documentid
        field "closed" $ documentstatus document == Closed



{- |
   Used by super users to inspect a particular user.
-}
daveUser :: Kontrakcja m => UserID ->  m (Either KontraLink String)
daveUser userid = onlySuperUserGet $ do
    user <- runDBOrFail $ dbQuery $ GetUserByID userid
    return $ inspectXML user

{- |
    Used by super users to inspect a company in xml.
-}
daveCompany :: Kontrakcja m => CompanyID -> m (Either KontraLink String)
daveCompany companyid = onlySuperUserGet $ do
  company <- runDBOrFail $ dbQuery $ GetCompany companyid
  return $ inspectXML company



{- |
   Ensures logged in as a super user
-}
onlySuperUserGet :: Kontrakcja m => m a -> m (Either KontraLink a)
onlySuperUserGet action = do
    Context{ ctxadminaccounts, ctxmaybeuser, ctxlocale } <- getContext
    if isSuperUser ctxadminaccounts ctxmaybeuser
        then Right <$> action
        else return $ Left $ LinkLogin ctxlocale NotLoggedAsSuperUser

sysdump :: Kontrakcja m => m (Either KontraLink Response)
sysdump = onlySuperUserGet $ do
    dump <- liftIO getAllActionAsString
    ok $ addHeader "refresh" "5" $ toResponse dump


serveLogDirectory :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
                   String
                  -> m Response
serveLogDirectory filename = do
    contents <- liftIO $ getDirectoryContents "log"
    when (filename `notElem` contents) $ do
        Log.debug $ "Log '" ++ filename ++ "' not found"
        mzero
    (_,bsstdout,_) <- liftIO $ readProcessWithExitCode' "tail" ["log/" ++ filename, "-n", "40"] BSL.empty
    ok $ addHeader "Refresh" "5" $ toResponseBS (BS.fromString "text/plain; charset=utf-8") $ bsstdout
