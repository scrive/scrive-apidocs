{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections, NamedFieldPuns #-}
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
          , AdminUsersPageParams(..)) where

import KontraLink
import Templates.Templates 
import Templates.TemplatesUtils 
import Text.StringTemplate.GenericStandard()
import UserState
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

{-| Main admin page - can go from here to other pages -}
adminMainPage::KontrakcjaTemplates ->  IO String
adminMainPage templates =  renderTemplateComplex templates "adminsmain" id

{-| Manage users page  - advanced, will be changed-}
adminUsersAdvancedPage::KontrakcjaTemplates -> [User] -> AdminUsersPageParams -> IO String
adminUsersAdvancedPage templates users params =  renderTemplateComplex templates "adminsmanageall" $
                                                        (setAttribute "users" $ map userSmallView $ visibleUsers params users) .
                                                        (setAttribute "letters" $ letters) .
                                                        (setAttribute "adminuserlink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "intervals" $ intervals $ avaibleUsers params users) .
                                                        (setAttribute "search" $ search params) . 
                                                        (setAttribute "startletter" $ startletter params) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly) 
{-| Manage users page - can find user here -}
adminUsersPage::KontrakcjaTemplates ->[User] -> AdminUsersPageParams -> IO String
adminUsersPage templates users params = renderTemplateComplex templates "adminusers" $
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly) . 
                                                        (setAttribute "users" $ map userSmallView $ visibleUsers params users) .
                                                        (setAttribute "letters" $ letters) .
                                                        (setAttribute "adminuserlink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "intervals" $ intervals $ avaibleUsers params users) .
                                                        (setAttribute "search" $ search params) . 
                                                        (setAttribute "startletter" $ startletter params) 
{-| Manage user page - can change user info and settings here -}
adminUserPage::KontrakcjaTemplates ->User -> PaymentAccountModel -> IO String
adminUserPage templates user paymentModel  = renderTemplateComplex templates "adminuser" $   
                                                        (setAttribute "adminuserslink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "user" $ userAdminView user) .
                                                        (setAttribute "paymentmodel" $ getModelView paymentModel) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly)
    

allUsersTable::KontrakcjaTemplates -> [(User,Int)] -> IO String
allUsersTable templates users =  renderTemplateComplex templates "allUsersTable" $
                                                        (setAttribute "users" $ map userSmallViewWithDocsCount $ users) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly) 
databaseContent ::KontrakcjaTemplates -> [String] -> IO String
databaseContent templates filenames = renderTemplateComplex templates "databaseContents" $
                                                        (setAttribute "files" $ filenames) .
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly)
statsPage::KontrakcjaTemplates -> Int->Int -> String -> IO String
statsPage templates userscount docscount sysinfo = renderTemplateComplex templates "pageStats" $
                                                         (setAttribute "userscount" $ userscount) .
                                                         (setAttribute "docscount" $ docscount) .
                                                         (setAttribute "sysinfo" $ sysinfo) .
                                                         (setAttribute "adminlink" $ show $ LinkAdminOnly) 
{-| Paging list as options [1..21] -> [1-5,6-10,11-15,16-20,21-21]  -}                                                      
intervals::[a] ->  [Option]                                                      
intervals users =  intervals' $ (filter (\x-> 0 == x `rem` pageSize) [0..((length users) - 1)]) ++ [length users]
  where 
    intervals' (a:(a':as))  = (Option {oValue= show a , oText = (show $ a+1) ++"-"++(show a'), oSelected=False}):intervals' (a':as)
    intervals' _ = []

pageSize :: Int
pageSize = 20

{-| Users on current page-}
visibleUsers::AdminUsersPageParams->[User]->[User]
visibleUsers params@(AdminUsersPageParams {page}) = (take pageSize) . (drop page) . (avaibleUsers params)

{-| filtering users by search string or first letter -}
avaibleUsers ::AdminUsersPageParams->[User]->[User]
avaibleUsers (AdminUsersPageParams {search=Just searchString,startletter=Just startLetter}) =  (searchUsers searchString) . (startLetterUsers startLetter)
avaibleUsers (AdminUsersPageParams {search=Just searchString}) =  searchUsers searchString
avaibleUsers (AdminUsersPageParams {startletter=Just startLetter}) =  startLetterUsers startLetter
avaibleUsers _  = id

startLetterUsers ::String->[User]->[User]                
startLetterUsers startletter = filter (\u -> (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ userfullname  u) ||
                                            (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ unEmail $ useremail $ userinfo u))
searchUsers ::String->[User]->[User]   
searchUsers searchString =  filter (\u -> (isInfixOf (map toUpper searchString) $ map toUpper $ toString $ userfullname  u) ||
                                            (isInfixOf (map toUpper searchString)  $ map toUpper $ toString $ unEmail $ useremail $ userinfo u))                                            



{-| Structure for params when searching users list (also for templates)-}
data AdminUsersPageParams = AdminUsersPageParams {
                             search::Maybe String,
                             startletter::Maybe String,
                             page::Int
                            } 

{-| Users simple view (for templates) -}
data UserSmallView = UserSmallView {
                         usvId::String,  
                         usvFullname::String,
                         usvEmail::String,
                         usvDocsCount::String
                     } deriving (Data, Typeable)
                     
{-| Users full view (for templates) -}
data UserAdminView = UserAdminView {
                     uavuserfstname::String
                   , uavusersndname::String
                   , uavuserpersonalnumber::String
                   , uavusercompanyname::String
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
                   , uavpaymentaccountmoney :: String
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
                   , uavpaymentaccountmoney  = showMoney $ paymentaccountmoney $ userpaymentaccount u
                   , uavpaymentaccountfreesignatures = show $ paymentaccountfreesignatures $ userpaymentaccount u
                   , uavtmppaymentchangeenddate = fmap (showDateOnly .  fst) $ temppaymentchange $ userpaymentpolicy  u
                   , uavtemppaymentchange = fmap (getChangeView .  snd) $ temppaymentchange $ userpaymentpolicy  u
                   , uavcustompaymentchange = getChangeView $ custompaymentchange $ userpaymentpolicy  u
                                                                             
                   }           
                                     
{-| Conversion from 'User' to 'Option', for select box UserSmallView  -}      
userSmallView::User -> UserSmallView 
userSmallView u = UserSmallView {     usvId = (show $ userid u)
                                    , usvFullname = (toString $ userfullname  u)
                                    , usvEmail = (toString $ unEmail $ useremail $ userinfo u)
                                    , usvDocsCount = "" }

userSmallViewWithDocsCount::(User,Int) -> UserSmallView 
userSmallViewWithDocsCount (u,c) = UserSmallView { usvId = (show $ userid u)
                                                 , usvFullname = (toString $ userfullname  u)
                                                 , usvEmail = (toString $ unEmail $ useremail $ userinfo u)
                                                 , usvDocsCount = show c }

letters::[String]
letters = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","X","Y","Z","Å","Ä","Ö"]
