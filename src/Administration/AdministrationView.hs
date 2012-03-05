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
import Text.StringTemplate.GenericStandard()
import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.UTF8 (toString)
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

{-| Main admin page - can go from here to other pages -}
adminMainPage :: TemplatesMonad m => Context -> m String
adminMainPage ctx = renderTemplateFM "adminsmain" $ do
    field "admin" $ (isAdmin) ctx

{-| Manage users page - can find user here -}
adminUsersPage :: TemplatesMonad m => m String
adminUsersPage =
    renderTemplateFM "adminusers" $ do
        field "adminlink" $ show $ LinkAdminOnly

{- | Manage companies page - can find a company here -}
adminCompaniesPage :: TemplatesMonad m => m String
adminCompaniesPage =
    renderTemplateFM "admincompanies" $ do
        field "adminlink" $ show $ LinkAdminOnly

{- | Manage company users page - can find a company user here -}
adminCompanyUsersPage :: TemplatesMonad m => CompanyID -> m String
adminCompanyUsersPage cid =
    renderTemplateFM "admincompanyusers" $ do
        field "adminlink" $ show $ LinkAdminOnly
        field "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        field "adminuserslink" $ show $ LinkUserAdmin Nothing
        field "companyid" $ show cid

{-| Manage users page - can find user here -}
adminUsersPageForSales :: TemplatesMonad m => m String
adminUsersPageForSales =
    renderTemplateFM "adminUsersForSales" $ do
            field "adminlink" $ show $ LinkAdminOnly

{-| Manage user page - can change user info and settings here -}
adminUserPage :: TemplatesMonad m => User -> Maybe Company -> m String
adminUserPage user mcompany =
    renderTemplateFM "adminuser" $ do
        field "adminuserslink" $ show $ LinkUserAdmin Nothing
        fieldF "user" $ userFields user
        fieldF "company" $ companyFields mcompany
        --field "paymentmodel" $ getModelView paymentModel
        field "adminlink" $ show $ LinkAdminOnly

{- | Manager company page - can change company info and settings here -}
adminCompanyPage :: TemplatesMonad m => Company -> m String
adminCompanyPage company =
  renderTemplateFM "admincompany" $ do
    field "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
    companyFields (Just company)
    field "adminlink" $ show $ LinkAdminOnly

adminUserStatisticsPage :: TemplatesMonad m => Fields m -> m String
adminUserStatisticsPage morefields =
  renderTemplateFM "statisticsPage" $ do
    morefields
    field "adminlink" $ show $ LinkAdminOnly

adminFunctionalityStatsPage :: TemplatesMonad m => [(String, Int)]
                                              -> [(String, Int)]
                                              -> m String
adminFunctionalityStatsPage userstats docstats =
  renderTemplateFM "adminFunctionalityStatsPage" $ do
    fieldFL "userfunctionalitystats" $ map functionalityStatFields userstats
    fieldFL "docfunctionalitystats" $ map functionalityStatFields docstats
    field "adminlink" $ show $ LinkAdminOnly
  where
    functionalityStatFields (label, count) = do
      field "label" label
      field "count" count

{-| Manage user page - can change user info and settings here -}
-- adminUserUsageStatsPage :: KontrakcjaTemplates -> User -> DocStatsL -> IO String
adminUserUsageStatsPage :: TemplatesMonad m => User -> Maybe Company -> Fields m -> m String
adminUserUsageStatsPage user mcompany morefields =
    renderTemplateFM "userusagestats" $ do
        field "adminuserslink" $ show $ LinkUserAdmin Nothing
        fieldF "user" $ userFields user
        fieldF "company" $ companyFields mcompany
        field "adminlink" $ show $ LinkAdminOnly
        morefields

{-| The company stats page -}
adminCompanyUsageStatsPage :: TemplatesMonad m => CompanyID -> Fields m -> m String
adminCompanyUsageStatsPage companyid morefields =
    renderTemplateFM "companyusagestats" $ do
        field "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        field "adminlink" $ show $ LinkAdminOnly
        field "companyid" $ show companyid
        morefields

adminDocuments:: TemplatesMonad m => Context -> m String
adminDocuments ctx = do
    renderTemplateFM "admindocumentslist" $ do
       field "adminlink" $ show $ LinkAdminOnly
       field "admin" $ (isAdmin) ctx



allUsersTable :: TemplatesMonad m => [(User,Maybe Company,DocStats)] -> m String
allUsersTable users =
    renderTemplateFM "allUsersTable" $ do
        fieldFL "users" $ map mkUserInfoView $ users
        field "adminlink" $ show $ LinkAdminOnly

servicesAdminPage :: TemplatesMonad m => DBEnv -> [Service] -> m String
servicesAdminPage env services = do
    renderTemplateFM "servicesAdmin" $ do
        field "adminlink" $ show $ LinkAdminOnly
        fieldFL "services" $ for services $ \ service -> do
            field "name"  $ show $ serviceid service
            fieldM "admin" $ fmap getSmartName <$> (ioRunDB env $ dbQuery $ GetUserByID $ serviceadmin $ servicesettings service)
            field "location" $ show $ servicelocation $ servicesettings service

mkUserInfoView :: (Functor m, MonadIO m) => (User, Maybe Company, DocStats) -> Fields m
mkUserInfoView (user, mcompany, docstats) = do
  fieldF "userdetails" $ userBasicFields user mcompany
  field "docstats" $ docstats
  fieldF "adminview" $ do userFields user
                          companyFields mcompany

companyFields :: MonadIO m => Maybe Company -> Fields m
companyFields mc = do
        field "companyid" $ maybe "" (show . companyid) mc
        field "companyname" $  getCompanyName mc
        field "companynumber" $ getCompanyNumber mc
        field "companyaddress" $ maybe "" (toString . companyaddress . companyinfo) mc
        field "companyzip" $  maybe "" (toString . companyzip . companyinfo)  mc
        field "companycity" $  maybe "" (toString . companycity . companyinfo) mc
        field "companycountry" $ maybe "" (toString . companycountry . companyinfo) mc

{-| Full fields set about user -}
userFields :: MonadIO m => User -> Fields m
userFields u =  do
        field "fstname"          $ getFirstName u
        field "sndname"          $ getLastName u
        field "personalnumber"   $ getPersonalNumber u
        field "companyposition"  $ usercompanyposition $ userinfo u
        field "phone"            $ toString $ userphone $ userinfo u
        field "mobile"           $ toString $ usermobile $ userinfo u
        field "email"            $ getEmail u
        field "regionse"         $ REGION_SE == getRegion u
        field "regiongb"         $ REGION_GB == getRegion u
        field "langsv"           $ LANG_SE == getLang u
        field "langen"           $ LANG_EN == getLang u
        field "iscompanyaccount" $ isJust $ usercompany u
        field "iscompanyadmin"   $ useriscompanyadmin u
        field "id"               $ show (userid u)
        field "companynumber"    $ getCompanyNumber u
        field "companyname"      $ getCompanyName   u

