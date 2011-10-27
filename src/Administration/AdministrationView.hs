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
          , adminUsersAdvancedPage
          , adminUsersPage
          , adminCompaniesPage
          , adminCompanyUsersPage
          , adminUsersPageForSales
          , adminUsersPageForPayments
          , adminUserPage
          , adminCompanyPage
          , allUsersTable
          , databaseContent
          , statsPage
          , servicesAdminPage
          , adminDocumentsDaylyList
          , adminTranslationsPage
          , adminUserUsageStatsPage
          , adminCompanyUsageStatsPage
          , adminUserStatisticsPage
          , adminFunctionalityStatsPage
          , AdminListPageParams(..)
          , StatsView(..)) where

import KontraLink
import Templates.Templates
import Templates.TemplatesUtils
import Text.StringTemplate.GenericStandard()
import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.UTF8 (toString)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char
import Data.Typeable
import Data.Data
import Data.Maybe
import Database.HDBC.PostgreSQL
import DB.Classes
import Misc
import User.UserView
import User.Model
import Doc.DocState
import Company.Model
import API.Service.Model
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import Util.SignatoryLinkUtils
import MinutesTime

{-| Main admin page - can go from here to other pages -}
adminMainPage :: TemplatesMonad m => m String
adminMainPage = renderTemplateM "adminsmain" ()

{-| Manage users page  - advanced, will be changed-}
adminUsersAdvancedPage :: TemplatesMonad m => [(User,Maybe Company)] -> AdminListPageParams -> m String
adminUsersAdvancedPage users params =
    renderTemplateFM "adminsmanageall" $ do
        fieldFL "users" $ map (uncurry userBasicFields) $ visibleItems params users
        field "adminlistlink" $ show $ LinkUserAdmin Nothing
        adminListFields LinkUserAdmin users params

{-| Manage users page - can find user here -}
adminUsersPage :: TemplatesMonad m => [(User,Maybe Company,DocStats)] -> AdminListPageParams -> m String
adminUsersPage users params =
    renderTemplateFM "adminusers" $ do
        fieldFL "users" $ map mkUserInfoView $ visibleItems params users
        adminListFields LinkUserAdmin users params

{- | Manage companies page - can find a company here -}
adminCompaniesPage :: TemplatesMonad m => [Company] -> AdminListPageParams -> m String
adminCompaniesPage companies params =
    renderTemplateFM "admincompanies" $ do
        fieldFL "companies" $ map (companyFields . Just) $ visibleItems params companies
        adminListFields LinkCompanyAdmin companies params

{- | Manage company users page - can find a company user here -}
adminCompanyUsersPage :: TemplatesMonad m => Company -> [User] -> AdminListPageParams -> m String
adminCompanyUsersPage company users params =
    renderTemplateFM "admincompanyusers" $ do
        field "companyid" $ show $ companyid company
        field "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        field "adminuserslink" $ show $ LinkUserAdmin Nothing
        fieldFL "users" $ map (uncurry userBasicFields) . visibleItems params $ zip users (repeat $ Just company)
        adminListFields (const $ LinkCompanyUserAdmin (companyid company)) users params 

{-| Manage users page - can find user here -}
adminUsersPageForSales :: TemplatesMonad m => [(User,Maybe Company,DocStats)] -> AdminListPageParams -> m String
adminUsersPageForSales users params =
    renderTemplateFM "adminUsersForSales" $ do
        fieldFL "users" $ map mkUserInfoView $ visibleItems params users
        adminListFields LinkUserAdmin users params

{-| Manage users page - can find user here -}
adminUsersPageForPayments :: TemplatesMonad m => [(User,Maybe Company,DocStats)] -> AdminListPageParams -> m String
adminUsersPageForPayments users params =
    renderTemplateFM "adminUsersForPayments" $ do
        fieldFL "users" $ map mkUserInfoView $ visibleItems params users
        adminListFields LinkUserAdmin users params

adminListFields :: (TemplatesMonad m, AdminListable a) => (Maybe b -> KontraLink) -> [a] -> AdminListPageParams -> Fields m        
adminListFields listlink items params = do
  field "adminlistlink" $ show (listlink Nothing)
  field "adminlink" $ show $ LinkAdminOnly
  field "letters" $ letters
  field "intervals" $ intervals $ availableItems params items
  field "search" $ search params
  field "startletter" $ startletter params

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
                                              -> [(String,Int)] 
                                              -> m String
adminFunctionalityStatsPage userstats docstats siglinkstats =
  renderTemplateFM "adminFunctionalityStatsPage" $ do
    fieldFL "userfunctionalitystats" $ map functionalityStatFields userstats
    fieldFL "docfunctionalitystats" $ map functionalityStatFields docstats
    fieldFL "siglinkfunctionalitystats" $ map functionalityStatFields siglinkstats
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

adminDocumentsDaylyList :: TemplatesMonad m => MinutesTime -> [Document] -> m String
adminDocumentsDaylyList t docs = do
    renderTemplateFM "daylydocumentinfo" $ do
       fieldFL "documents" $ for docs $ \doc -> do
           field "id" $ show $ documentid doc
           field "title" $ documenttitle doc
           field "service" $ fmap show $ documentservice doc
           field "type" $ show $ documenttype doc
           field "status" $ take 20 $ show $ documentstatus doc
           field "signatories" $ map getSmartName $ documentsignatorylinks doc
           fieldF "author" $ do
               field "name"    $ fmap getSmartName    $ getAuthorSigLink doc
               field "email"   $ fmap getEmail        $ getAuthorSigLink doc
               field "company" $ fmap getCompanyName  $ getAuthorSigLink doc
           field "ctime" $ showMinutesTimeForAPI $ documentctime doc
           field "mtime" $ showMinutesTimeForAPI $ documentmtime doc
       field "adminlink" $ show $ LinkAdminOnly
       field "day" $ showDateOnly t


allUsersTable :: TemplatesMonad m => [(User,Maybe Company,DocStats)] -> m String
allUsersTable users =
    renderTemplateFM "allUsersTable" $ do
        fieldFL "users" $ map mkUserInfoView $ users
        field "adminlink" $ show $ LinkAdminOnly

databaseContent :: TemplatesMonad m => [String] -> m String
databaseContent filenames =
    renderTemplateFM "databaseContents" $ do
        field "files" $ filenames
        field "adminlink" $ show $ LinkAdminOnly

statsPage :: TemplatesMonad m => StatsView -> String -> m String
statsPage stats sysinfo =
    renderTemplateFM "pageStats" $ do
        field "stats" $ stats
        field "sysinfo" $ sysinfo
        field "adminlink" $ show $ LinkAdminOnly

servicesAdminPage :: TemplatesMonad m => Connection -> [Service] -> m String
servicesAdminPage conn services = do
    renderTemplateFM "servicesAdmin" $ do
        field "adminlink" $ show $ LinkAdminOnly
        fieldFL "services" $ for services $ \ service -> do
            field "name"  $ show $ serviceid service
            fieldM "admin" $ fmap getSmartName <$> (ioRunDB conn $ dbQuery $ GetUserByID $ serviceadmin $ servicesettings service)
            field "location" $ show $ servicelocation $ servicesettings service

adminTranslationsPage::TemplatesMonad m => m String
adminTranslationsPage = renderTemplateFM  "adminTranslations" (return ())

mkUserInfoView :: (Functor m, MonadIO m) => (User, Maybe Company, DocStats) -> Fields m
mkUserInfoView (user, mcompany, docstats) = do
  fieldF "userdetails" $ userBasicFields user mcompany
  field "docstats" $ docstats
  fieldF "adminview" $ do userFields user
                          companyFields mcompany


data StatsView = StatsView
                 { svDoccount          :: Int
                 , svSignaturecount    :: Int
                 } deriving (Data, Typeable)

{-| Paging list as options [1..21] -> [1-5,6-10,11-15,16-20,21-21]  -}
intervals::[a] ->  [Option]
intervals items =  intervals' $ (filter (\x-> 0 == x `rem` pageSize) [0..((length items) - 1)]) ++ [length items]
  where
    intervals' (a:(a':as))  = (Option {oValue= show a , oText = (show $ a+1) ++"-"++(show a'), oSelected=False}):intervals' (a':as)
    intervals' _ = []

pageSize :: Int
pageSize = 500

{-| Users on current page-}
visibleItems :: (AdminListable a) => AdminListPageParams -> [a] -> [a]
visibleItems params@(AdminListPageParams {page}) = (take pageSize) . (drop page) . (availableItems params)

{-| filtering users by search string or first letter -}
availableItems :: (AdminListable a) => AdminListPageParams -> [a] -> [a]
availableItems (AdminListPageParams {search=Just searchString,startletter=Just startLetter}) =  (searchItems searchString) . (startLetterItems startLetter)
availableItems (AdminListPageParams {search=Just searchString}) =  searchItems searchString
availableItems (AdminListPageParams {startletter=Just startLetter}) =  startLetterItems startLetter
availableItems _  = id

startLetterItems :: (AdminListable a) => String -> [a] -> [a]
startLetterItems startletter = filter (startsWith startletter)

searchItems :: (AdminListable a) => String -> [a] -> [a]
searchItems searchString =  filter (matches searchString)

class AdminListable a where
  startsWith :: String -> a -> Bool
  matches :: String -> a -> Bool

instance AdminListable User where
  startsWith s u = (isPrefixOf (map toUpper s)  . map toUpper . toString $ getFullName u)
                   || (isPrefixOf (map toUpper s)  . map toUpper . toString $ getEmail u)
  matches s u = (isInfixOf (map toUpper s) $ map toUpper $ toString $ getFullName u)
                || (isInfixOf (map toUpper s) $ map toUpper $ toString $ getEmail u)

instance AdminListable (User, Maybe Company) where
  startsWith s (u, _) = startsWith s u
  matches s (u, _) = matches s u

instance AdminListable (User, Maybe Company, DocStats) where
  startsWith s (u, _, _) = startsWith s u
  matches s (u, _, _) = matches s u

instance AdminListable (Company ) where
  startsWith s c = isPrefixOf (map toUpper s)  . map toUpper . toString $ getCompanyName c
  matches s c = isInfixOf (map toUpper s)  . map toUpper . toString $ getCompanyName c

{-| Structure for params when searching users list (also for templates)-}
data AdminListPageParams = AdminListPageParams {
                             search::Maybe String,
                             startletter::Maybe String,
                             page::Int
                            }
                            
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
        field "fstname" $ getFirstName u
        field "sndname" $ getLastName u
        field "personalnumber" $ getPersonalNumber u
        field "companyposition" $ usercompanyposition $ userinfo u
        field "phone" $ toString $ userphone $ userinfo u
        field "mobile" $ toString $ usermobile $ userinfo u
        field "email" $ getEmail u
        field "serviceskrivapa" $ SkrivaPa == (systemserver $ usersettings u)
        field "servicescrive" $ Scrive == (systemserver $ usersettings u)
        field "regionse" $ REGION_SE == getRegion u
        field "regiongb" $ REGION_GB == getRegion u
        field "langsv" $ LANG_SE == getLang u
        field "langen" $ LANG_EN == getLang u
        field "iscompanyaccount" $ isJust $ usercompany u
        field "iscompanyadmin" $ useriscompanyadmin u
{-
        field "accountplan" $ for (allValues::[UserAccountPlan]) (\x -> if (x == (accountplan $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)
        field "signeddocstorage" $ show (signeddocstorage $ usersettings u)
        field "paymentmethod" $ for (allValues::[PaymentMethod ]) (\x -> if (x == (userpaymentmethod $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)
        field "paymentaccounttype" $ for (allValues::[PaymentAccountType])
                                                                         (\x -> if (x == userpaymentaccounttype u)
                                                                                 then soption show show x
                                                                                 else option show show x)
        field "freetrialexpirationdate" $ showDateOnly <$> userfreetrialexpirationdate u
-}
        --field "paymentaccountfreesignatures" $ show $ paymentaccountfreesignatures $ userpaymentaccount u
        --field "tmppaymentchangeenddate" $ fmap (showDateOnly .  fst) $ temppaymentchange $ userpaymentpolicy  u
        --field "temppaymentchange" $ fmap (getChangeView .  snd) $ temppaymentchange $ userpaymentpolicy  u
        --field "custompaymentchange" $ getChangeView $ custompaymentchange $ userpaymentpolicy  u
        field "id" $ show (userid u)


letters :: [String]
letters = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","X","Y","Z","Å","Ä","Ö"]
