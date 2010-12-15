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
module Administration.AdministrationView(adminMainPage,adminManageAllPage,adminUsersPage,AdminUsersPageParams(..),adminUserPage) where

import KontraLink
import Templates
import Data.Typeable
import Data.Data
import Text.StringTemplate.GenericStandard()
import UserState
import Data.ByteString.UTF8 (toString)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char
import Payments.PaymentsView
import Payments.PaymentsState
import Misc
import MinutesTime

adminMainPage::KontrakcjaTemplates ->  IO String
adminMainPage templates =  renderTemplateComplex templates "adminsmain" id

adminManageAllPage::KontrakcjaTemplates -> [User] -> IO String
adminManageAllPage templates users =  renderTemplateComplex templates "adminsmanageall" 
                                                        (setAttribute "users" $ map userOption users) 

adminUsersPage::KontrakcjaTemplates ->[User] -> AdminUsersPageParams -> IO String
adminUsersPage templates users params = renderTemplateComplex templates "adminusers" $
                                                        (setAttribute "users" $ map userSmallView $ visibleUsers params users) .
                                                        (setAttribute "letters" $ letters) .
                                                        (setAttribute "adminuserlink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "intervals" $ intervals $ avaibleUsers params users) .
                                                        (setAttribute "search" $ search params) . 
                                                        (setAttribute "startletter" $ startletter params) 

adminUserPage templates user paymentModel = renderTemplateComplex templates "adminuser" $   
                                                        (setAttribute "adminlink" $ show $ LinkAdminOnly) . 
                                                        (setAttribute "adminuserslink" $ show $ LinkUserAdmin Nothing) .
                                                        (setAttribute "user" $ userAdminView user) .
                                                        (setAttribute "paymentmodel" $ getModelView paymentModel) 
    
                                                        
                                                        
                                                        
intervals users =  intervals' $ (filter (\x-> 0 == x `rem` pageSize) [0..((length users) - 1)]) ++ [length users]
  where 
    intervals' (a:(a':as))  = (Option {oValue= show a , oText = (show $ a+1) ++"-"++(show a')}):intervals' (a':as)
    intervals' _ = []

pageSize = 4

visibleUsers::AdminUsersPageParams->[User]->[User]
visibleUsers params@(AdminUsersPageParams {page}) = (take pageSize) . (drop page) . (avaibleUsers params)

avaibleUsers ::AdminUsersPageParams->[User]->[User]
avaibleUsers params@(AdminUsersPageParams {search=Just searchString,startletter=Just startLetter}) =  (searchUsers searchString) . (startLetterUsers startLetter)
avaibleUsers params@(AdminUsersPageParams {search=Just searchString}) =  searchUsers searchString
avaibleUsers params@(AdminUsersPageParams {startletter=Just startLetter}) =  startLetterUsers startLetter
avaibleUsers _  = id
                  
startLetterUsers startletter = filter (\u -> (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ userfullname  u) ||
                                            (isPrefixOf (map toUpper startletter)  $ map toUpper $ toString $ unEmail $ useremail $ userinfo u))
searchUsers searchString =  filter (\u -> (isInfixOf (map toUpper searchString) $ map toUpper $ toString $ userfullname  u) ||
                                            (isInfixOf (map toUpper searchString)  $ map toUpper $ toString $ unEmail $ useremail $ userinfo u))                                            




data AdminUsersPageParams = AdminUsersPageParams {
                             search::Maybe String,
                             startletter::Maybe String,
                             page::Int
                            } 

data UserSmallView = UserSmallView {
                         usvId::String,  
                         usvFullname::String,
                         usvEmail::String
                     } deriving (Data, Typeable)

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
                   , uavsigneddocstorage::[Option]
                   , uavuserpaymentmethod::[Option]
                   , uavpaymentaccounttype ::[Option]
                   , uavpaymentaccountmoney :: String
                   , uavpaymentaccountfreesignatures ::String
                   , uavtmppaymentchangeenddate ::Maybe String
                   , uavcustompaymentchange::PaymentChangeView
                   , uavtemppaymentchange:: Maybe PaymentChangeView
                    } deriving (Data, Typeable)
                    
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
                   , uavsigneddocstorage = for (allValues::[StorageType]) (\x -> if (x == (signeddocstorage $ usersettings u))
                                                                                 then soption show show x
                                                                                 else option show show x)                                                              
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
                                     
                      
userSmallView u = UserSmallView { usvId = (show $ userid u), usvFullname = (toString $ userfullname  u), usvEmail = (toString $ unEmail $ useremail $ userinfo u) }
  
userOption = option (show . userid ) (toString . unEmail . useremail . userinfo)


data Option = Option {
              oValue:: String,
              oText:: String,
              oSelected :: Bool
            } deriving (Data, Typeable)

option::(a->String)->(a->String)->a->Option
option f g x = Option {oValue=f x, oText=g x, oSelected = False}

soption::(a->String)->(a->String)->a->Option
soption f g x= Option {oValue=f x, oText=g x, oSelected = True}

letters = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","X","Y","Z","Å","Ä","Ö"]
