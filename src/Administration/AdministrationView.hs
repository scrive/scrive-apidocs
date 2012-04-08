-----------------------------------------------------------------------------
-- |
-- Module      :  Administration.AdministrationView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  develpment
-- Portability :  portable
--
-- Almoust all the stuff that is visible under /adminsonly path
--
-----------------------------------------------------------------------------
module Administration.AdministrationView(
              adminMainPage
            , adminUserPage
            , adminUsersPage
            , adminCompanyPage
            , adminCompaniesPage
            , adminCompanyUsersPage
            , adminUsersPageForSales
            , allUsersTable
            , servicesAdminPage
            , adminFunctionalityStatsPage
            , adminDocuments
            , adminUserUsageStatsPage
            , adminCompanyUsageStatsPage
            , adminUserStatisticsPage
          ) where

import KontraLink
import Templates.Templates
import Control.Applicative
import Data.Maybe
import DB.Classes
import Misc
import User.UserView
import User.Model
import Doc.DocStateData
import Company.Model
import API.Service.Model
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import Kontra
import ScriveByMail.Model
import ScriveByMail.View
import qualified Templates.Fields as F

import Control.Monad

{-| Main admin page - can go from here to other pages -}
adminMainPage :: TemplatesMonad m => Context -> m String
adminMainPage ctx = renderTemplate "adminsmain" $ do
    F.value "admin" $ isAdmin ctx

{-| Manage users page - can find user here -}
adminUsersPage :: TemplatesMonad m => m String
adminUsersPage =
    renderTemplate "adminusers" $ do
        F.value "adminlink" $ show $ LinkAdminOnly

{- | Manage companies page - can find a company here -}
adminCompaniesPage :: TemplatesMonad m => m String
adminCompaniesPage =
    renderTemplate "admincompanies" $ do
        F.value "adminlink" $ show $ LinkAdminOnly

{- | Manage company users page - can find a company user here -}
adminCompanyUsersPage :: TemplatesMonad m => CompanyID -> m String
adminCompanyUsersPage cid =
    renderTemplate "admincompanyusers" $ do
        F.value "adminlink" $ show $ LinkAdminOnly
        F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        F.value "adminuserslink" $ show $ LinkUserAdmin Nothing
        F.value "companyid" $ show cid

{-| Manage users page - can find user here -}
adminUsersPageForSales :: TemplatesMonad m => m String
adminUsersPageForSales =
    renderTemplate "adminUsersForSales" $ do
            F.value "adminlink" $ show $ LinkAdminOnly

{-| Manage user page - can change user info and settings here -}
adminUserPage :: TemplatesMonad m => User -> Maybe Company -> m String
adminUserPage user mcompany =
    renderTemplate "adminuser" $ do
        F.value "adminuserslink" $ show $ LinkUserAdmin Nothing
        F.object "user" $ userFields user
        F.object "company" $ companyFields mcompany
        F.value "adminlink" $ show $ LinkAdminOnly

{- | Manager company page - can change company info and settings here -}
adminCompanyPage :: TemplatesMonad m => Company -> Maybe MailAPIInfo -> m String
adminCompanyPage company mmailapiinfo =
  renderTemplate "admincompany" $ do
    F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
    companyFields (Just company)
    when (isJust mmailapiinfo) $ mailAPIInfoFields (fromJust mmailapiinfo)
    F.value "hasmailapi" $ isJust mmailapiinfo
    F.value "adminlink" $ show $ LinkAdminOnly

adminUserStatisticsPage :: TemplatesMonad m => Fields m () -> m String
adminUserStatisticsPage morefields =
  renderTemplate "statisticsPage" $ do
    morefields
    F.value "adminlink" $ show $ LinkAdminOnly

adminFunctionalityStatsPage :: TemplatesMonad m => [(String, Int)]
                                              -> [(String, Int)]
                                              -> m String
adminFunctionalityStatsPage userstats docstats =
  renderTemplate "adminFunctionalityStatsPage" $ do
    F.objects "userfunctionalitystats" $ map functionalityStatFields userstats
    F.objects "docfunctionalitystats" $ map functionalityStatFields docstats
    F.value "adminlink" $ show $ LinkAdminOnly
  where
    functionalityStatFields (label, count) = do
      F.value "label" label
      F.value "count" count

{-| Manage user page - can change user info and settings here -}
-- adminUserUsageStatsPage :: KontrakcjaTemplates -> User -> DocStatsL -> IO String
adminUserUsageStatsPage :: TemplatesMonad m => User -> Maybe Company -> Fields m () -> m String
adminUserUsageStatsPage user mcompany morefields =
    renderTemplate "userusagestats" $ do
        F.value "adminuserslink" $ show $ LinkUserAdmin Nothing
        F.object "user" $ userFields user
        F.object "company" $ companyFields mcompany
        F.value "adminlink" $ show $ LinkAdminOnly
        morefields

{-| The company stats page -}
adminCompanyUsageStatsPage :: TemplatesMonad m => CompanyID -> Fields m () -> m String
adminCompanyUsageStatsPage companyid morefields =
    renderTemplate "companyusagestats" $ do
        F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        F.value "adminlink" $ show $ LinkAdminOnly
        F.value "companyid" $ show companyid
        morefields

adminDocuments :: TemplatesMonad m => Context -> m String
adminDocuments ctx = do
    renderTemplate "admindocumentslist" $ do
       F.value "adminlink" $ show $ LinkAdminOnly
       F.value "admin" $ isAdmin ctx

allUsersTable :: TemplatesMonad m => [(User,Maybe Company,DocStats)] -> m String
allUsersTable users =
    renderTemplate "allUsersTable" $ do
        F.objects "users" $ map mkUserInfoView $ users
        F.value "adminlink" $ show $ LinkAdminOnly

servicesAdminPage :: (MonadDB m, TemplatesMonad m) => [Service] -> m String
servicesAdminPage services = do
    renderTemplate "servicesAdmin" $ do
        F.value "adminlink" $ show $ LinkAdminOnly
        F.objects "services" $ for services $ \ service -> do
            F.value "name"  $ show $ serviceid service
            F.valueM "admin" $ fmap getSmartName <$> (runDBQuery $ GetUserByID $ serviceadmin $ servicesettings service)
            F.value "location" $ show $ servicelocation $ servicesettings service

mkUserInfoView :: Monad m => (User, Maybe Company, DocStats) -> Fields m ()
mkUserInfoView (user, mcompany, docstats) = do
  F.object "userdetails" $ userBasicFields user mcompany
  F.value "docstats" $ docstats
  F.object "adminview" $ do
    userFields user
    companyFields mcompany

companyFields :: Monad m => Maybe Company -> Fields m ()
companyFields mc = do
        F.value "companyid" $ maybe "" (show . companyid) mc
        F.value "companyname" $  getCompanyName mc
        F.value "companynumber" $ getCompanyNumber mc
        F.value "companyaddress" $ maybe "" (companyaddress . companyinfo) mc
        F.value "companyzip" $  maybe "" (companyzip . companyinfo)  mc
        F.value "companycity" $  maybe "" (companycity . companyinfo) mc
        F.value "companycountry" $ maybe "" (companycountry . companyinfo) mc
        F.value "companyemaildomain" $ maybe "" (fromMaybe "" . (companyemaildomain . companyinfo)) mc

{-| Full fields set about user -}
userFields :: Monad m => User -> Fields m ()
userFields u =  do
        F.value "fstname"          $ getFirstName u
        F.value "sndname"          $ getLastName u
        F.value "personalnumber"   $ getPersonalNumber u
        F.value "companyposition"  $ usercompanyposition $ userinfo u
        F.value "phone"            $ userphone $ userinfo u
        F.value "mobile"           $ usermobile $ userinfo u
        F.value "email"            $ getEmail u
        F.value "regionse"         $ REGION_SE == getRegion u
        F.value "regiongb"         $ REGION_GB == getRegion u
        F.value "langsv"           $ LANG_SE == getLang u
        F.value "langen"           $ LANG_EN == getLang u
        F.value "iscompanyaccount" $ isJust $ usercompany u
        F.value "iscompanyadmin"   $ useriscompanyadmin u
        F.value "id"               $ show (userid u)
        F.value "companynumber"    $ getCompanyNumber u
        F.value "companyname"      $ getCompanyName   u
