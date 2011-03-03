{-# OPTIONS_GHC -Wall #-}
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
          , adminUserPage
          , allUsersTable
          , databaseContent
          , statsPage
          , AdminUsersPageParams(..)
          , StatsView(..)) where

import KontraLink
import Templates.Templates 
import Templates.TemplatesUtils 
import Text.StringTemplate.GenericStandard()
import Data.ByteString.UTF8 (toString)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char
import Data.Typeable
import Data.Data
import Payments.PaymentsView
import Payments.PaymentsState
import Payments.PaymentsUtils
import Misc
import MinutesTime
import User.UserView
import User.UserState
import Doc.DocState
{-| Main admin page - can go from here to other pages -}
adminMainPage::KontrakcjaTemplates ->  IO String
adminMainPage templates =  renderTemplate templates "adminsmain" ()

{-| Manage users page  - advanced, will be changed-}
adminUsersAdvancedPage::KontrakcjaTemplates -> [User] -> AdminUsersPageParams -> IO String
adminUsersAdvancedPage templates users params =  renderTemplate templates "adminsmanageall" $
                                                        (setAttribute "users" $ map userSmallView $ visibleUsers params users) .
                                                        (setAttribute "letters" $ letters) .
                                                        (setAttribute "adminuserlink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "intervals" $ intervals $ avaibleUsers params users) .
                                                        (setAttribute "search" $ search params) . 
                                                        (setAttribute "startletter" $ startletter params) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly) 
{-| Manage users page - can find user here -}
adminUsersPage::KontrakcjaTemplates ->[(User,DocStats,UserStats)] -> AdminUsersPageParams -> IO String
adminUsersPage templates users params = renderTemplate templates "adminusers" $
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly) . 
                                                        (setAttribute "users" $ map mkUserInfoView $ visibleUsers params users) .
                                                        (setAttribute "letters" $ letters) .
                                                        (setAttribute "adminuserlink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "intervals" $ intervals $ avaibleUsers params users) .
                                                        (setAttribute "search" $ search params) . 
                                                        (setAttribute "startletter" $ startletter params) 
{-| Manage user page - can change user info and settings here -}
adminUserPage::KontrakcjaTemplates ->User -> PaymentAccountModel -> IO String
adminUserPage templates user paymentModel  = renderTemplate templates "adminuser" $   
                                                        (setAttribute "adminuserslink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "user" $ userAdminView user) .
                                                        (setAttribute "paymentmodel" $ getModelView paymentModel) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly)
allUsersTable::KontrakcjaTemplates -> [(User,DocStats,UserStats)] -> IO String
allUsersTable templates users =  renderTemplate templates "allUsersTable" $
                                                        (setAttribute "users" $ map mkUserInfoView $ users) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly) 
databaseContent ::KontrakcjaTemplates -> [String] -> IO String
databaseContent templates filenames = renderTemplate templates "databaseContents" $
                                                        (setAttribute "files" $ filenames) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly)
statsPage::KontrakcjaTemplates -> StatsView -> String -> IO String
statsPage templates stats sysinfo = renderTemplate templates "pageStats" $
                                                         (setAttribute "stats" $ stats) .
                                                         (setAttribute "sysinfo" $ sysinfo) .
                                                         (setAttribute "adminlink" $ show $ LinkAdminOnly)

mkUserInfoView :: (User, DocStats, UserStats) -> UserInfoView
mkUserInfoView (userdetails', docstats', userstats') = UserInfoView {
                                              userdetails = userSmallView userdetails'
                                            , docstats = docstats'
                                            , userstats = userstats'
                                           }
 
data UserInfoView = UserInfoView {
                          userdetails :: UserSmallView,
                          docstats :: DocStats,
                          userstats :: UserStats
                        } deriving (Data, Typeable)

data StatsView = StatsView {
                    svDoccount :: Int
                  , svSignaturecount :: Int
                  , svUsercount :: Int
                  , svViralinvitecount :: Int
                  , svAdmininvitecount :: Int
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
startLetterUsers startletter = filter (\u -> (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ userfullname $ getUser u) ||
                                            (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ unEmail $ useremail $ userinfo $ getUser u))
searchUsers :: (UserBased a) => String->[a]->[a]   
searchUsers searchString =  filter (\u -> (isInfixOf (map toUpper searchString) $ map toUpper $ toString $ userfullname $ getUser u) ||
                                            (isInfixOf (map toUpper searchString)  $ map toUpper $ toString $ unEmail $ useremail $ userinfo $ getUser u))                                            



{-| Structure for params when searching users list (also for templates)-}
data AdminUsersPageParams = AdminUsersPageParams {
                             search::Maybe String,
                             startletter::Maybe String,
                             page::Int
                            } 
                     
{-| Users full view (for templates) -}
data UserAdminView = UserAdminView {
                     uavuserfstname::String
                   , uavusersndname::String
                   , uavuserpersonalnumber::String
                   , uavusercompanyname::String
                   , uavusercompanyposition::String
                   , uavusercompanynumber::String
                   , uavuseraddress::String
                   , uavuserzip::String
                   , uavusercity::String
                   , uavusercountry::String
                   , uavuserphone::String
                   , uavusermobile::String
                   , uavuseremail::String 
                   , uavaccounttype::[Option]
                   , uavaccountplan::[Option]
                   , uavsigneddocstorage::String
                   , uavuserpaymentmethod::[Option]
                   , uavpaymentaccounttype ::[Option]
                   , uavpaymentaccountfreesignatures ::String
                   , uavtmppaymentchangeenddate ::Maybe String
                   , uavcustompaymentchange::PaymentChangeView
                   , uavtemppaymentchange:: Maybe PaymentChangeView
                    } deriving (Data, Typeable)
                    
{-| Conversion from 'User' to 'UserAdminView'  -}   
userAdminView ::User -> UserAdminView
userAdminView u =  UserAdminView {
                     uavuserfstname =  toString $ userfstname $ userinfo u
                   , uavusersndname =  toString $ usersndname $ userinfo u
                   , uavuserpersonalnumber =  toString $ userpersonalnumber $ userinfo u
                   , uavusercompanyname =  toString $ usercompanyname $ userinfo u
                   , uavusercompanyposition =  toString $ usercompanyposition $ userinfo u
                   , uavusercompanynumber =  toString $ usercompanynumber $ userinfo u
                   , uavuseraddress =  toString $ useraddress$ userinfo u
                   , uavuserzip =  toString $ userzip  $ userinfo u
                   , uavusercity =  toString $ usercity $ userinfo u
                   , uavusercountry =  toString $ usercountry $ userinfo u
                   , uavuserphone =  toString $ userphone $ userinfo u
                   , uavusermobile =  toString $ usermobile $ userinfo u
                   , uavuseremail =  toString $ unEmail $ useremail $ userinfo u
                   , uavaccounttype = for (allValues::[UserAccountType]) (\x -> if (x == (accounttype $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)
                   , uavaccountplan = for (allValues::[UserAccountPlan]) (\x -> if (x == (accountplan $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)
                   , uavsigneddocstorage = show (signeddocstorage $ usersettings u)
                   , uavuserpaymentmethod = for (allValues::[PaymentMethod ]) (\x -> if (x == (userpaymentmethod $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)        
                                                                                 
                   , uavpaymentaccounttype = for (allValues::[PaymentAccountType]) 
                                                                         (\x -> if (x == (paymentaccounttype $ userpaymentpolicy  u))
                                                                                 then soption show show x
                                                                                 else option show show x)      
                   , uavpaymentaccountfreesignatures = show $ paymentaccountfreesignatures $ userpaymentaccount u
                   , uavtmppaymentchangeenddate = fmap (showDateOnly .  fst) $ temppaymentchange $ userpaymentpolicy  u
                   , uavtemppaymentchange = fmap (getChangeView .  snd) $ temppaymentchange $ userpaymentpolicy  u
                   , uavcustompaymentchange = getChangeView $ custompaymentchange $ userpaymentpolicy  u
                                                                             
                   }           
                                     
letters::[String]
letters = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","X","Y","Z","Å","Ä","Ö"]
