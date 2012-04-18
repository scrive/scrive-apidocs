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
          , companyClosedFilesZip
          ) where
import Control.Monad.State
import Data.Functor
import Happstack.Server hiding (simpleHTTP)
import Misc
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
import qualified Data.ByteString as BSS
import qualified Data.ByteString.UTF8 as BS
import Crypto.RNG(random)

import InspectXMLInstances ()
import InspectXML
import File.Model
import ListUtil
import Text.JSON
import Mails.Model
import Util.HasSomeCompanyInfo
import CompanyAccounts.CompanyAccountsControl
import CompanyAccounts.Model
import Util.SignatoryLinkUtils
import Stats.Control (getUsersAndStats)
import EvidenceLog.Model
import User.History.Model
import Codec.Archive.Zip
import qualified Data.Map as Map
import Doc.DocStorage
import qualified Templates.Fields as F

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
    allCompanies <- dbQuery $ GetCompanies Nothing
    let companies = companiesSortSearchPage params allCompanies
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
      , companyname    $ companyinfo $ company
      , companynumber  $ companyinfo $ company
      , companyaddress $ companyinfo $ company
      , companyzip     $ companyinfo $ company
      , companycity    $ companyinfo $ company
      , companycountry $ companyinfo $ company
      ]


companiesPageSize :: Int
companiesPageSize = 100

showAdminCompanyUsers :: Kontrakcja m => CompanyID -> m String
showAdminCompanyUsers cid = onlySalesOrAdmin $ adminCompanyUsersPage cid


showAdminUsersForSales :: Kontrakcja m => m String
showAdminUsersForSales = onlySalesOrAdmin $ adminUsersPageForSales

handleUsersListCSV :: Kontrakcja m => m Response
handleUsersListCSV = onlySalesOrAdmin $ do
  users <- getUsersAndStatsInv
  ok $ setHeader "Content-Disposition" "attachment;filename=userslist.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse (usersListCSV users)

usersListCSV :: [(User, Maybe Company, DocStats, InviteType)] -> String
usersListCSV users = 
  "\"" ++ intercalate "\";\""  
  ["id", "fstname", "sndname", "email", "company", "position","tos"]
  ++ "\"\n" ++
  (concat $ map csvline $ filter active users)
    where
        active (u,_,_,_) =   (not (useraccountsuspended u)) && (isJust $ userhasacceptedtermsofservice u) && (isNothing $ userservice u)
        csvline (u,mc,_,_) = "\"" ++ intercalate "\";\""
                          [ show $ userid u
                          , getFirstName u
                          , getLastName u
                          , getEmail u
                          , getCompanyName mc
                          , usercompanyposition $ userinfo u
                          , show $ fromJust $ userhasacceptedtermsofservice u
                          ]
                          ++ "\"\n"

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
                        ,("subaccounts", jsFromString "")
                        ])
                    ,("link", jsFromString . show $ LinkUserAdmin $ Just $ userid user)
                    ]) (list users)),
             ("paging", pagingParamsJSON users)]

jsFromString :: String -> JSValue
jsFromString = JSString . toJSString

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
      match s' m = isInfixOf (map toUpper s') (map toUpper m)
      userMatch (u,mc,_,_) s' = match s' (getCompanyName mc)
                             || match s' (getFirstName u)
                             || match s' (getLastName  u)
                             || match s' (getEmail u)

usersPageSize :: Int
usersPageSize = 100

getUsersAndStatsInv :: Kontrakcja m => m [(User, Maybe Company, DocStats, InviteType)]
getUsersAndStatsInv = do
    users <- getUsersAndStats
    mapM addInviteType users
  where
    addInviteType = \(user,mcompany,docstats) -> do
        invite <- dbQuery $ GetInviteInfo (userid user)
        let invitestype = case invite of
                          Nothing                     -> Admin
                          Just (InviteInfo _ _ mtype) -> fromMaybe Admin mtype
        return (user,mcompany,docstats,invitestype)


{- Shows table of all users-}
showAllUsersTable :: Kontrakcja m => m String
showAllUsersTable = onlySalesOrAdmin $ do
    users <- getUsersAndStats
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
    ctx <- getContext
    email <- map toLower <$> getAsString "email"
    fstname <- getAsString "fstname"
    sndname <- getAsString "sndname"
    custommessage <- getField "custommessage"
    region <- guardJustM $ readField "region"
    muser <- createNewUserByAdmin ctx (fstname, sndname) email Nothing custommessage (mkLocaleFromRegion region)
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
  ctx <- getContext
  email <- getCriticalField asValidEmail "email"
  fstname <- getCriticalField asValidName "fstname"
  sndname <- getCriticalField asValidName "sndname"
  custommessage <- joinEmpty <$> getField "custommessage"
  Log.debug $ "Custom message when creating an account " ++ show custommessage
  region <- guardJustM $ readField "region"
  admin <- isFieldSet "iscompanyadmin"
  muser <- createNewUserByAdmin ctx (fstname, sndname) email Nothing custommessage (mkLocaleFromRegion region)
  case muser of
    Just (User{userid}) -> do
      _ <- dbUpdate $ SetUserCompany userid (Just companyid)
      when_ admin $ dbUpdate $ SetUserCompanyAdmin userid True
    Nothing -> addFlashM flashMessageUserWithSameEmailExists
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
    srvs <- dbQuery $ GetServices
    Log.debug "Document list for admin per service"
    docs <- join <$> (sequence $ map (dbQuery . GetDocumentsByService) (Nothing:(map (Just . serviceid) srvs)))
    Log.debug $ "Total document found:" ++ show (length docs)
    params <- getListParamsNew
    let documents = documentsSortSearchPage params docs
    Log.debug $ "Document on current list:" ++ show (length $ list documents)
    Log.debug $ "Force execution due to stack overflow:" ++ show (length $ show $ list documents)
    return $ JSObject
           $ toJSObject
            [("list", JSArray $ map (\doc -> 
                JSObject $ toJSObject
                    [("fields", JSObject $ toJSObject
                        [ ("id", jsFromString $ show $ documentid doc)
                        , ("ctime", jsFromString . showMinutesTimeForAPI $ documentctime doc) 
                        , ("mtime", jsFromString . showMinutesTimeForAPI $ documentmtime doc) 
                        , ("author", JSObject $ toJSObject [
                              ("name", jsFromString $ maybe "" getSmartName $ getAuthorSigLink doc)
                            , ("email", jsFromString $ maybe "" getEmail $ getAuthorSigLink doc)
                            , ("company", jsFromString $ maybe "" getCompanyName $ getAuthorSigLink doc)
                            ])
                        , ("title", jsFromString $ documenttitle doc)
                        , ("service", jsFromString $ maybe "" show $ documentservice doc)
                        , ("status", jsFromString $ take 20 $ show $ documentstatus doc)
                        , ("type", jsFromString . show $ documenttype doc)
                        , ("signs", JSArray $ map (jsFromString . getSmartName) $ documentsignatorylinks doc)
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
documentsSortFunc "author"     = viewComparing (maybe "" getSmartName . getAuthorSigLink)
documentsSortFunc "authorREV"  = viewComparingRev (maybe "" getSmartName . getAuthorSigLink)
documentsSortFunc "title"      = viewComparing documenttitle
documentsSortFunc "titleREV"   = viewComparingRev documenttitle
documentsSortFunc "service"    = viewComparing (maybe "" show . documentservice)
documentsSortFunc "serviceREV" = viewComparingRev (maybe "" show . documentservice)
documentsSortFunc "status"     = viewComparing (take 20 . show . documentstatus)
documentsSortFunc "statusREV"  = viewComparingRev (take 20 . show . documentstatus)
documentsSortFunc "type"       = viewComparing documenttype
documentsSortFunc "typeREV"    = viewComparingRev documenttype
documentsSortFunc "signs"      = viewComparing (map getSmartName . documentsignatorylinks)
documentsSortFunc "signsREV"   = viewComparingRev (map getSmartName . documentsignatorylinks)
documentsSortFunc _            = const $ const EQ

documentsSearchFunc :: SearchingFunction Document
documentsSearchFunc s doc =  nameMatch doc || signMatch doc
    where
    match m = isInfixOf (map toUpper s) (map toUpper m)
    nameMatch = match . documenttitle
    signMatch d = any (match . getSmartName) (documentsignatorylinks d)

documentsPageSize :: Int
documentsPageSize = 100

handleBackdoorQuery :: Kontrakcja m => String -> m String
handleBackdoorQuery email = onlySalesOrAdmin $ onlyBackdoorOpen $ do
  minfo <- listToMaybe . filter ((email `elem`) . map addrEmail . mailTo)
    <$> dbQuery GetEmails
  return $ maybe "No email found" mailContent minfo

-- This method can be used do reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlyAdmin $ do
  Log.debug $ "Trying to reseal document "++ show docid ++" | Only superadmin can do that"
  doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
  Log.debug "Document is valid for resealing sealing"
  res <- sealDocument doc
  case res of
      Left  _ -> Log.debug "We failed to reseal the document"
      Right _ -> Log.debug "Ok, so the document has been resealed"
  return LoopBack


replaceMainFile :: Kontrakcja m => DocumentID -> m KontraLink
replaceMainFile did = onlyAdmin $ do
  Log.debug $ "Replaing main file | SUPER CRITICAL | If you see this check who did this ask who did this and why"
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
            file <- dbUpdate $ NewFile fn (concatChunks content)
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
              CustomFT label _ -> label


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


companyClosedFilesZip :: Kontrakcja m => CompanyID -> Int  -> String -> m Response
companyClosedFilesZip cid start _filenamefordownload = onlyAdmin $ do
  archive <- companyFilesArchive cid start
  let res = Response 200 Map.empty nullRsFlags (fromArchive archive) Nothing
  return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "archive/zip") res


companyFilesArchive :: Kontrakcja m => CompanyID -> Int -> m Archive
companyFilesArchive cid start = do
    Log.debug $ "Getting all files archive for company " ++ show cid
    docs <- dbQuery $ GetDocumentsByCompanyWithFiltering cid [
        DocumentFilterByService Nothing
      , DocumentFilterStatuses [Closed]
      ]
    let cdocs = sortBy (\d1 d2 -> compare (documentid d1) (documentid d2)) $ filter (\doc -> documentstatus doc == Closed)  $ docs
    let sdocs = take zipCount $ drop (zipCount*start) $ cdocs
    Log.debug $ "Found  " ++ show (length $ filter (\doc -> documentstatus doc == Closed)  $ docs) ++ "document"
    mentries <- mapM docToEntry $  sdocs
    return $ foldr addEntryToArchive emptyArchive $ map fromJust $ filter isJust $ mentries
  where
    zipCount = 80

docToEntry ::  Kontrakcja m => Document -> m (Maybe Entry)
docToEntry doc = do
      let snpart = concat $ for (take 5 $ documentsignatorylinks doc) $ \sl -> (take 8 $ getFirstName sl) ++ "_"++(take 8 $ getFirstName sl) ++ "_"
      let name = filter ((/= ' ')) $ filter (isAscii) $ (documenttitle doc) ++ "_" ++ (show $ documentmtime doc) ++ "_" ++ snpart ++".pdf"
      case (documentsealedfiles doc) of
        [fid] -> do
            Log.debug $ "Getting content for the file " ++ show fid
            content <- getFileIDContents fid
            return $ Just $ toEntry name 0 $ BSL.pack $ BSS.unpack content
        _ -> do
            Log.debug $ "Bad sealed file number " ++ show (documentid doc)
            return Nothing
