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
          , adminUsersPageForSales
          , adminUsersPageForPayments
          , adminUserPage
          , allUsersTable
          , databaseContent
          , statsPage
          , servicesAdminPage
          , adminTranslationsPage
          , adminUserUsageStatsPage
          , AdminUsersPageParams(..)
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
import Payments.PaymentsView
import Payments.PaymentsState
import Misc
import MinutesTime
import User.UserView
import User.UserState
import Doc.DocState
import API.Service.ServiceState
import Happstack.State (query)
import Util.HasSomeUserInfo

{-| Main admin page - can go from here to other pages -}
adminMainPage :: TemplatesMonad m => m String
adminMainPage = renderTemplateM "adminsmain" ()

{-| Manage users page  - advanced, will be changed-}
adminUsersAdvancedPage :: TemplatesMonad m => [User] -> AdminUsersPageParams -> m String
adminUsersAdvancedPage users params =
    renderTemplateFM "adminsmanageall" $ do
        fieldFL "users" $ map userBasicFields $ visibleUsers params users
        field "letters" $ letters
        field "adminuserlink" $ show $ LinkUserAdmin Nothing
        field "intervals" $ intervals $ avaibleUsers params users
        field "search" $ search params
        field "startletter" $ startletter params
        field "adminlink" $ show $ LinkAdminOnly

{-| Manage users page - can find user here -}
adminUsersPage :: TemplatesMonad m => [(User,DocStats,UserStats)] -> AdminUsersPageParams -> m String
adminUsersPage users params =
    renderTemplateFM "adminusers" $ do
        field "adminlink" $ show $ LinkAdminOnly
        fieldFL "users" $ map mkUserInfoView $ visibleUsers params users
        field "letters" $ letters
        field "adminuserlink" $ show $ LinkUserAdmin Nothing
        field "intervals" $ intervals $ avaibleUsers params users
        field "search" $ search params
        field "startletter" $ startletter params

{-| Manage users page - can find user here -}
adminUsersPageForSales :: TemplatesMonad m => [(User,DocStats,UserStats)] -> AdminUsersPageParams -> m String
adminUsersPageForSales users params =
    renderTemplateFM "adminUsersForSales" $ do
        field "adminlink" $ show $ LinkAdminOnly
        fieldFL "users" $ map mkUserInfoView $ visibleUsers params users
        field "letters" $ letters
        field "adminuserlink" $ show $ LinkUserAdmin Nothing
        field "intervals" $ intervals $ avaibleUsers params users
        field "search" $ search params
        field "startletter" $ startletter params

{-| Manage users page - can find user here -}
adminUsersPageForPayments :: TemplatesMonad m => [(User,DocStats,UserStats)] -> AdminUsersPageParams -> m String
adminUsersPageForPayments users params =
    renderTemplateFM "adminUsersForPayments" $ do
        field "adminlink" $ show $ LinkAdminOnly
        fieldFL "users" $ map mkUserInfoView $ visibleUsers params users
        field "letters" $ letters
        field "adminuserlink" $ show $ LinkUserAdmin Nothing
        field "intervals" $ intervals $ avaibleUsers params users
        field "search" $ search params
        field "startletter" $ startletter params

{-| Manage user page - can change user info and settings here -}
adminUserPage :: TemplatesMonad m => User -> PaymentAccountModel -> m String
adminUserPage user paymentModel =
    renderTemplateFM "adminuser" $ do
        field "adminuserslink" $ show $ LinkUserAdmin Nothing
        fieldF "user" $ userFields user
        field "paymentmodel" $ getModelView paymentModel
        field "adminlink" $ show $ LinkAdminOnly

{-| Manage user page - can change user info and settings here -}
-- adminUserUsageStatsPage :: KontrakcjaTemplates -> User -> DocStatsL -> IO String
adminUserUsageStatsPage :: TemplatesMonad m => User -> Fields m -> m String
adminUserUsageStatsPage user morefields =
    renderTemplateFM "userusagestats" $ do
        field "adminuserslink" $ show $ LinkUserAdmin Nothing
        fieldF "user" $ userFields user
        field "adminlink" $ show $ LinkAdminOnly
        morefields

allUsersTable :: TemplatesMonad m => [(User,DocStats,UserStats)] -> m String
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

servicesAdminPage :: TemplatesMonad m => [Service] -> m String
servicesAdminPage services = do
    renderTemplateFM "servicesAdmin" $ do
        field "adminlink" $ show $ LinkAdminOnly
        fieldFL "services" $ for services $ \ service -> do
            field "name"  $ show $ serviceid service
            fieldM "admin" $ fmap getSmartName <$> (query $ GetUserByUserID $ UserID $ unServiceAdmin $ serviceadmin $ servicesettings service)
            field "location" $ show $ servicelocation $ servicesettings service

adminTranslationsPage::TemplatesMonad m => m String
adminTranslationsPage = renderTemplateFM  "adminTranslations" (return ())

mkUserInfoView :: (Functor m, MonadIO m) => (User, DocStats, UserStats) -> Fields m
mkUserInfoView (userdetails', docstats', userstats') = do
  fieldF "userdetails" $ userBasicFields userdetails'
  field "docstats" $ docstats'
  field "userstats" $ userstats'
  fieldF "adminview" $ userFields userdetails'


data StatsView = StatsView
                 { svDoccount          :: Int
                 , svSignaturecount    :: Int
                 , svUsercount         :: Int
                 , svViralinvitecount  :: Int
                 , svAdmininvitecount  :: Int
                 } deriving (Data, Typeable)

{-| Paging list as options [1..21] -> [1-5,6-10,11-15,16-20,21-21]  -}
intervals::[a] ->  [Option]
intervals users =  intervals' $ (filter (\x-> 0 == x `rem` pageSize) [0..((length users) - 1)]) ++ [length users]
  where
    intervals' (a:(a':as))  = (Option {oValue= show a , oText = (show $ a+1) ++"-"++(show a'), oSelected=False}):intervals' (a':as)
    intervals' _ = []

pageSize :: Int
pageSize = 500

class UserBased a where
  getUser :: a -> User

instance UserBased User where
  getUser = id

instance UserBased (User,DocStats,UserStats) where
  getUser (user,_,_) = user

{-| Users on current page-}
visibleUsers:: (UserBased a) => AdminUsersPageParams->[a]->[a]
visibleUsers params@(AdminUsersPageParams {page}) = (take pageSize) . (drop page) . (avaibleUsers params)

{-| filtering users by search string or first letter -}
avaibleUsers :: (UserBased a) => AdminUsersPageParams->[a]->[a]
avaibleUsers (AdminUsersPageParams {search=Just searchString,startletter=Just startLetter}) =  (searchUsers searchString) . (startLetterUsers startLetter)
avaibleUsers (AdminUsersPageParams {search=Just searchString}) =  searchUsers searchString
avaibleUsers (AdminUsersPageParams {startletter=Just startLetter}) =  startLetterUsers startLetter
avaibleUsers _  = id

startLetterUsers :: (UserBased a) => String->[a]->[a]
startLetterUsers startletter = filter (\u -> (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ getFullName $ getUser u) ||
                                            (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ getEmail $ getUser u))
searchUsers :: (UserBased a) => String->[a]->[a]
searchUsers searchString =  filter (\u -> (isInfixOf (map toUpper searchString) $ map toUpper $ toString $ getFullName $ getUser u) ||
                                            (isInfixOf (map toUpper searchString)  $ map toUpper $ toString $ getEmail $ getUser u))



{-| Structure for params when searching users list (also for templates)-}
data AdminUsersPageParams = AdminUsersPageParams {
                             search::Maybe String,
                             startletter::Maybe String,
                             page::Int
                            }

{-| Full fields set about user -}
userFields :: MonadIO m => User -> Fields m
userFields u =  do
        field "fstname" $ getFirstName u
        field "sndname" $ getLastName u
        field "personalnumber" $ getPersonalNumber u
        field "companyname" $  getCompanyName u
        field "companyposition" $ usercompanyposition $ userinfo u
        field "companynumber" $ getCompanyNumber u
        field "address" $ toString $ useraddress $ userinfo u
        field "zip" $  toString $ userzip  $ userinfo u
        field "city" $  toString $ usercity $ userinfo u
        field "country" $ toString $ usercountry $ userinfo u
        field "phone" $ toString $ userphone $ userinfo u
        field "mobile" $ toString $ usermobile $ userinfo u
        field "email" $ getEmail u
        field "accounttype" $  for (allValues::[UserAccountType]) (\x -> if (x == (accounttype $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)
        field "accountplan" $ for (allValues::[UserAccountPlan]) (\x -> if (x == (accountplan $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)
        field "signeddocstorage" $ show (signeddocstorage $ usersettings u)
        field "paymentmethod" $ for (allValues::[PaymentMethod ]) (\x -> if (x == (userpaymentmethod $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)
        field "paymentaccounttype" $ for (allValues::[PaymentAccountType])
                                                                         (\x -> if (x == (paymentaccounttype $ userpaymentpolicy  u))
                                                                                 then soption show show x
                                                                                 else option show show x)
        field "freetrialexpirationdate" $ showDateOnly <$> userfreetrialexpirationdate u
        field "paymentaccountfreesignatures" $ show $ paymentaccountfreesignatures $ userpaymentaccount u
        field "tmppaymentchangeenddate" $ fmap (showDateOnly .  fst) $ temppaymentchange $ userpaymentpolicy  u
        field "temppaymentchange" $ fmap (getChangeView .  snd) $ temppaymentchange $ userpaymentpolicy  u
        field "custompaymentchange" $ getChangeView $ custompaymentchange $ userpaymentpolicy  u
        field "id" $ show (userid u)


letters :: [String]
letters = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","X","Y","Z","Å","Ä","Ö"]
