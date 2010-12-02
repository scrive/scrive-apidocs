{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeSynonymInstances, StandaloneDeriving #-}
module Payments.PaymentsState  ( PaymentAccount,accountTypes,AccountType) where

import "mtl" Control.Monad.Reader (ask)
import "mtl" Control.Monad.State hiding (State)
import Data.Generics
import Data.Maybe (isNothing,isJust, fromJust)
import Happstack.Data (Default, deriveAll, gFind')
import Happstack.Data.IxSet
import Happstack.Server.HTTP.Types ()
import Happstack.State (Version, Query, Update, deriveSerialize, getRandomR, 
                        mkMethods, query, update, mode, extension, Proxy(Proxy), Migrate, migrate)
import UserState (UserID,FlashMessage,GetUserByUserID(GetUserByUserID), User)
import MinutesTime
import Happstack.Server
import System.Random
import Happstack.Util.Common ( readM)
import System.Log.Logger (errorM)


$( deriveAll [''Ord, ''Eq, ''Default, ''Show]
   [d|
      --Mail models
      newtype PaymentAccountId = PaymentAccountId Integer
      data PaymentAccountModel = PaymentAccountModel {  
                                   modelAccountType::AccountType,
                                   modelPaymentForAccounts::PaymentForAccounts Money
      }                           
                                  
      data PaymentAccount = PaymentAccount {  
                                  accountUserID::UserID,
                                  accountModel::PaymentAccountModel,
                                  accountType::AccountType,   
                                  accountInfo::PaymentAccountInfo,
                                  accountPaymentForAccounts::PaymentForAccounts (Maybe Money)
      }

      
      --Usefull enums
      data AccountType = Private | Minimal | Medium | Maximal | Corp
      data SignatureType = EmailSignature | ElegSignature | MobileSignature | CreditCardSignature | IPadSignature 
      data StorageType = Amazon | TrustWeaver
      data DocumentType = Finalized | NotFinalized | Template
      data PaymentType = CreditCard | Invoice | Non
      data HowToPayForSignature = ForSigning | ForEachSignature
      data Currency = SEK
      data PaymentTimeframe = Once | OneM | TreeM | OneYear | Indefine
      
      --Payment values              
      data Money = Money { money:: Integer }
      data PaymentForAccounts value = PaymentForAccounts {
                                 forAccount::value,
                                 forSubaccount::value
                               }
      --General user info 
      data PaymentAccountInfo = PaymentAccountInfo {
                                       userName::String,
                                       companyName::String,
                                       orgNumber::String,
                                       address::String,
                                       zip::String,
                                       city::String,
                                       country::String,
                                       email::String,
                                       phone::String,
                                       fax::String,
                                       vat::String
                                 }
     --Payment values 
     
     {-                          
     data PaymentForTransactions = PaymentForAccounts {
                                 forAccount::PaymentValue,
                                 forSubaccount::PaymentValue
                               }   
     data PaymentForStorage = PaymentForStorage {
                                 forAccount::PaymentValue,
                                 forSubaccount::PaymentValue
                               }      
     data PaymentForHolding = PaymentForHolding {
                                 forAccount::PaymentValue,
                                 forSubaccount::PaymentValue
                               }      
    data PaymentExtra value = PaymentExtra {
                                  freeTransactions::(value,Integer),
                                  rebating::value,
                                  extras::(value,Integer)
                                  } 
   
    -}
    |])
    

                               
$(deriveSerialize ''PaymentAccount) 
instance Version (PaymentAccount) 

$(deriveSerialize ''PaymentAccountModel) 
instance Version (PaymentAccountModel) 

$(deriveSerialize ''AccountType) 
instance Version (AccountType ) 

$(deriveSerialize ''SignatureType) 
instance Version (SignatureType) 

$(deriveSerialize ''StorageType) 
instance Version (StorageType) 

$(deriveSerialize ''DocumentType) 
instance Version (DocumentType) 

$(deriveSerialize ''Currency ) 
instance Version (Currency ) 

$(deriveSerialize ''Money ) 
instance Version (Money ) 


$(deriveSerialize ''PaymentAccountInfo) 
instance Version (PaymentAccountInfo) 

$(deriveSerialize ''PaymentForAccounts) 
instance Version (PaymentForAccounts a) 
      
      
$(inferIxSet "PaymentAccounts" ''PaymentAccount 'noCalcs [''UserID])
$(inferIxSet "PaymentAccountModels" ''PaymentAccountModel  'noCalcs [''AccountType ])  
  
getAccountModel::PaymentAccount -> Update PaymentAccountModels PaymentAccountModel
getAccountModel account = 
  do
   models <- ask
   case getOne (models @= accountType account) of
    Just mmodel -> return mmodel    
    Nothing -> do
                let nmodel = basicModel $ accountType account
                modify $ insert $ nmodel
                return nmodel
                                
--Functions for operation on payments accounts


--Utils
basicModel accountType  = PaymentAccountModel {modelAccountType = accountType,
                                               modelPaymentForAccounts = PaymentForAccounts {
                                                                                  forAccount=free,
                                                                                  forSubaccount=free
                                                                          }
                                               }

free = Money 0
accountTypes::[AccountType]
accountTypes = [Private, Minimal , Medium , Maximal , Corp]
--Wrapper for all functions above
$(mkMethods '' PaymentAccountModels
                    [ 'getAccountModel   ])
$(mkMethods '' PaymentAccount 
                    [    ])
