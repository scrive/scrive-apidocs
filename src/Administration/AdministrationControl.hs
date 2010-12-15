{-# LANGUAGE CPP, OverloadedStrings, TupleSections#-}
{-# OPTIONS_GHC -Wall #-}
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
module Administration.AdministrationControl(showAdminMainPage,showAdminManageAllPage,showAdminUsers, handleUserChange) where
import "mtl" Control.Monad.State
import AppView
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update,query)
import Misc
import User
import HSP (cdata)
import Administration.AdministrationView
import Payments.PaymentsState
import KontraLink
import Data.ByteString.UTF8 (fromString)
import Payments.PaymentsControl(readMoneyField,getPaymentChangeChange)
import MinutesTime

showAdminMainPage ::Kontra Response
showAdminMainPage = onlySuperUser $
                     do
                      ctx@Context {ctxtemplates} <- lift get
                      content <- liftIO $ adminMainPage ctxtemplates 
                      renderFromBody ctx TopEmpty kontrakcja $ cdata content 
                   
showAdminManageAllPage :: Kontra Response
showAdminManageAllPage = onlySuperUser $
                          do
                           ctx@Context {ctxtemplates} <- lift get
                           users <- query $ GetAllUsers
                           content <- liftIO $ adminManageAllPage ctxtemplates users 
                           renderFromBody ctx TopEmpty kontrakcja $ cdata content 
    
showAdminUsers:: Maybe String -> Kontra Response 
showAdminUsers Nothing= onlySuperUser $
                          do
                           ctx@Context {ctxtemplates} <- lift get
                           users <- query $ GetAllUsers
                           params <- getAdminUsersPageParams
                           content <- liftIO $ adminUsersPage ctxtemplates users params
                           renderFromBody ctx TopEmpty kontrakcja $ cdata content 

showAdminUsers (Just a)= onlySuperUser $
                         do 
                         ctx@Context {ctxtemplates} <- lift get
                         let muserId = maybeRead a
                         case muserId of 
                           Nothing -> mzero   
                           Just userId ->    
                            do 
                             muser <- query $ GetUserByUserID userId
                             case muser of 
                              Nothing -> mzero     
                              Just user -> do   
                                     paymentmodel <- update $ GetPaymentModel $ paymentaccounttype $ userpaymentpolicy user
                                     content <- liftIO $ adminUserPage ctxtemplates user paymentmodel
                                     renderFromBody ctx TopEmpty kontrakcja $ cdata content 


handleUserChange a = onlySuperUser $
                     do
                     ctx@Context {ctxtemplates} <- lift get
                     let muserId = maybeRead a
                     case muserId of 
                       Nothing -> mzero   
                       Just userId ->    
                        do 
                          muser <- query $ GetUserByUserID userId
                          case muser of 
                             Nothing -> mzero     
                             Just user -> do   
                                           infoChange <- getUserInfoChange
                                           settingsChange <- getUserSettingsChange
                                           paymentAccountChange <- getUserPaymentAccountChange
                                           paymentPaymentPolicy <- getUserPaymentPolicyChange
                                           update $ SetUserInfo userId $ infoChange $ userinfo user
                                           update $ SetUserSettings userId $ settingsChange $ usersettings user
                                           update $ SetUserPaymentAccount userId $ paymentAccountChange $ userpaymentaccount user
                                           update $ SetUserPaymentPolicyChange userId $ paymentPaymentPolicy $ userpaymentpolicy user
                                           return $ LinkUserAdmin $ Just userId


getUserInfoChange::Kontra (UserInfo -> UserInfo)
getUserInfoChange = do      
                     muserfstname        <- getField' fromString "userfstname" 
                     musersndname        <- getField' fromString "usersndname" 
                     muserpersonalnumber <- getField' fromString "userpersonalnumber" 
                     musercompanyname    <- getField' fromString "usercompanyname" 
                     musercompanynumber  <- getField' fromString "usercompanynumber" 
                     museraddress        <- getField' fromString "useraddress" 
                     muserzip            <- getField' fromString "userzip" 
                     musercity           <- getField' fromString "usercity" 
                     musercountry        <- getField' fromString "usercountry" 
                     muserphone          <- getField' fromString "userphone"
                     musermobile         <- getField' fromString "usermobile" 
                     museremail          <- getField' (Email . fromString)  "useremail" 
                     return (\UserInfo {
                                    userfstname      
                                  , usersndname 
                                  , userpersonalnumber
                                  , usercompanyname
                                  , usercompanynumber 
                                  , useraddress
                                  , userzip
                                  , usercity
                                  , usercountry
                                  , userphone
                                  , usermobile
                                  , useremail
                                  } ->  UserInfo {
                                            userfstname = maybe' userfstname muserfstname
                                          , usersndname = maybe' usersndname musersndname
                                          , userpersonalnumber = maybe' userpersonalnumber muserpersonalnumber
                                          , usercompanyname =  maybe' usercompanyname musercompanyname
                                          , usercompanynumber  =  maybe' usercompanynumber musercompanynumber
                                          , useraddress =  maybe' useraddress museraddress
                                          , userzip = maybe' userzip muserzip
                                          , usercity  = maybe' usercity musercity
                                          , usercountry = maybe' usercountry musercountry
                                          , userphone = maybe' userphone muserphone
                                          , usermobile = maybe' usermobile musermobile
                                          , useremail =  maybe' useremail museremail
                                        })

getUserSettingsChange::Kontra (UserSettings -> UserSettings)
getUserSettingsChange =  do 
                          maccounttype          <- readField "accounttype" 
                          maccountplan          <- readField "accountplan" 
                          msigneddocstorage     <- readField "signeddocstorage" 
                          muserpaymentmethod    <- readField "userpaymentmethod" 
                          return (\UserSettings {
                                   accounttype 
                                 , accountplan 
                                 , signeddocstorage 
                                 , userpaymentmethod }
                                       -> UserSettings {
                                            accounttype  = maybe' accounttype  maccounttype 
                                          , accountplan = maybe' accountplan maccountplan
                                          , signeddocstorage  = maybe' signeddocstorage  msigneddocstorage 
                                          , userpaymentmethod =  maybe' userpaymentmethod muserpaymentmethod
                                          })

getUserPaymentAccountChange::Kontra (UserPaymentAccount -> UserPaymentAccount)
getUserPaymentAccountChange =  do 
                          mpaymentaccountmoney                 <- readMoneyField "paymentaccountmoney" 
                          mpaymentaccountfreesignatures        <- readField "paymentaccountfreesignatures" 
                          return (\UserPaymentAccount {
                                   paymentaccountmoney
                                 , paymentaccountfreesignatures
                                  }
                                    -> UserPaymentAccount  {
                                            paymentaccountmoney  = maybe' paymentaccountmoney  mpaymentaccountmoney
                                          , paymentaccountfreesignatures = maybe' paymentaccountfreesignatures mpaymentaccountfreesignatures
                                        })                                        

getUserPaymentPolicyChange::Kontra (UserPaymentPolicy -> UserPaymentPolicy)
getUserPaymentPolicyChange =  do 
                          mtmppaymentchangeenddate   <- fmap (join . (fmap parseMinutesTimeMDY)) $ getField "tmppaymentchangeenddate" 
                          mpaymentaccounttype        <- readField "paymentaccounttype" 
                          customPaymentChange        <- getPaymentChangeChange "custom"
                          tempPaymentChange          <- getPaymentChangeChange "temp"
                          return (\UserPaymentPolicy {
                                    paymentaccounttype 
                                  , custompaymentchange
                                  , temppaymentchange
                                  }
                                    -> UserPaymentPolicy  {
                                            paymentaccounttype   = maybe' paymentaccounttype   mpaymentaccounttype 
                                          , custompaymentchange = customPaymentChange custompaymentchange
                                          , temppaymentchange = case  mtmppaymentchangeenddate of
                                                                 Nothing ->  Nothing 
                                                                 Just enddate -> case temppaymentchange of
                                                                                   Nothing -> Just (enddate,tempPaymentChange emptyChange) 
                                                                                   Just (_,change) -> Just (enddate, tempPaymentChange change) 
                                                 
                                        })      
                                        
getAdminUsersPageParams::Kontra AdminUsersPageParams
getAdminUsersPageParams = do
                          search <- getDataFn' (look "search")         
                          startletter <-  getDataFn' (look "startletter")         
                          mpage <-  getDataFn' (look "page")         
                          let mpage' = join $ fmap maybeRead mpage
                          return $ AdminUsersPageParams {search = search, startletter=startletter, page = maybe 0 id mpage'}
                                                                          