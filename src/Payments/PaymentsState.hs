{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Payments.PaymentsState
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
-- Data storage structures for all payments in the system.
-- 'PaymentAccountModel' holds information abount payment models such as Private, Minimal, Corp etc.
-- 'UserPaymentPolicy' holds the user payment policy, with chosen account type and cutom and temporary changes.
-- 'UserPaymentAccount' holds money and free signatures for user. Last two are stored inside 'UserState.User'
--  while 'PaymentAccountModel' is undependent.
-----------------------------------------------------------------------------

module Payments.PaymentsState  (
         -- Basic types for payments models
         PaymentAccountModel(..)
       , Money(..)
       , PaymentAccountType(..)
       , PaymentForAccounts(..)
       , PaymentForSignature(..)
       , PaymentForSignedStorage(..)
       , PaymentForOtherStorage(..)
         -- User temporaty change of payment values
       , PaymentChange(..)
       -- Index of models to be used in main app
       , PaymentAccountModels
       --Structures for users payment accounts
       , UserPaymentPolicy(..)
       , UserPaymentAccount(..)
        -- Utils
       , emptyChange
       , basicPaymentPolicy
       , emptyPaymentAccount
       , free
       , freeTill
       , freeChange
       , extendFreeTmpChange
       , takeOneFreeSignature
       , requiresImmediatelyPayment
       -- Autogenerated data-functions constructors
       , GetPaymentModel(..)
       , GetPaymentModels(..)
       , UpdateAccountModel(..)
       -- | Payment schema and utils
       , mergeChanges
       , PaymentScheme
       , ($$)
       , paymentForAccounts
       , paymentForSignature
       , paymentForSignedStorage
       , paymentForOtherStorage

    ) where

import Control.Monad.Reader (ask)
import Control.Monad.State hiding (State)
import Happstack.Data.IxSet as IxSet
import Happstack.State
import Misc (allValues, mkTypeOf)
import MinutesTime
import Data.Maybe
import Data.Data

-- | Mail model for payments account
data PaymentAccountModel = PaymentAccountModel {
                                   modelAccountType :: PaymentAccountType,
                                   modelPaymentForAccounts :: PaymentForAccounts Money,
                                   modelPaymentForSignature :: PaymentForSignature Money,
                                   modelPaymentForSignedStorage :: PaymentForSignedStorage Money,
                                   modelPaymentForOtherStorage :: PaymentForOtherStorage Money
      }
    deriving (Eq, Ord, Show, Read)

instance Typeable PaymentAccountModel where typeOf _ = mkTypeOf "PaymentAccountModel"

-- | Temp change of payments
data PaymentChange = PaymentChange {
                                  changePaymentForAccounts :: PaymentForAccounts (Maybe Money),
                                  changePaymentForSignature :: PaymentForSignature (Maybe Money),
                                  changePaymentForSignedStorage :: PaymentForSignedStorage (Maybe Money),
                                  changePaymentForOtherStorage :: PaymentForOtherStorage (Maybe Money)
      }
    deriving (Eq, Ord, Show, Read, Typeable)


-- | Types of accounts
data PaymentAccountType = Private | Minimal | Medium | Maximal | Corp
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Money values wrapper
newtype Money = Money { money :: Integer }
    deriving (Eq, Ord, Show, Read, Typeable)

deriving instance Num Money
deriving instance Data Money

-- | Monthly payments for account and subaccounts
data PaymentForAccounts value = PaymentForAccounts {
                                 forAccount :: value,
                                 forSubaccount :: value
                               }
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Payments for each signature on document
data PaymentForSignature value = PaymentForSignature {
                                 forEmailSignature :: value,
                                 forElegSignature :: value,
                                 forMobileSignature :: value,
                                 forCreditCardSignature :: value,
                                 forIPadSignature :: value
                               }
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Payments for storage of signed documents
data PaymentForSignedStorage value = PaymentForSignedStorage {
     forAmazon :: value,
     forTrustWeaver :: value
    }
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Payments for storage of of unsigned templates and drafts
data PaymentForOtherStorage value  = PaymentForOtherStorage {
                                 forTemplate :: value,
                                 forDraft :: value
                               }
    deriving (Eq, Ord, Show, Read, Typeable)

--  | How the user should pay for signing etc
data UserPaymentPolicy =  UserPaymentPolicy {
               paymentaccounttype :: PaymentAccountType
             , custompaymentchange :: PaymentChange
             , temppaymentchange :: Maybe (MinutesTime,PaymentChange)
      }
    deriving (Eq, Ord, Show, Typeable)

-- | Info about free signatures left and money for user
data UserPaymentAccount0 = UserPaymentAccount0 {
               paymentaccountmoney0 :: Money
             , paymentaccountfreesignatures0 :: Int

      }
    deriving (Eq, Ord, Show, Read, Typeable)

data UserPaymentAccount = UserPaymentAccount {
               paymentAgreementRef :: Maybe String
             , paymentaccountfreesignatures :: Int

      }
    deriving (Eq, Ord, Show, Read, Typeable)




$(deriveSerialize ''PaymentAccountModel)
instance Version (PaymentAccountModel)

$(deriveSerialize ''PaymentChange)
instance Version (PaymentChange)

$(deriveSerialize ''PaymentAccountType)
instance Version (PaymentAccountType )

$(deriveSerialize ''Money )
instance Version (Money )

$(deriveSerialize ''PaymentForAccounts)
instance Version (PaymentForAccounts a)

$(deriveSerialize ''PaymentForSignature)
instance Version (PaymentForSignature a)

$(deriveSerialize ''PaymentForSignedStorage)
instance Version (PaymentForSignedStorage a)

$(deriveSerialize ''PaymentForOtherStorage)
instance Version (PaymentForOtherStorage a)

$(deriveSerialize ''UserPaymentPolicy)
instance Version UserPaymentPolicy

$(deriveSerialize ''UserPaymentAccount0)
instance Version UserPaymentAccount0

$(deriveSerialize ''UserPaymentAccount)
instance Version UserPaymentAccount where
    mode = extension 1 (Proxy :: Proxy UserPaymentAccount0)

instance Migrate UserPaymentAccount0 UserPaymentAccount  where
      migrate UserPaymentAccount0 {
               paymentaccountmoney0 = _
             , paymentaccountfreesignatures0  } =
                   UserPaymentAccount {
                       paymentAgreementRef = Nothing
                     , paymentaccountfreesignatures = paymentaccountfreesignatures0  }





deriving instance Bounded PaymentAccountType
deriving instance Enum PaymentAccountType

type PaymentAccountModels = IxSet PaymentAccountModel

instance Indexable PaymentAccountModel where
        empty = ixSet [ ixFun (\x -> [modelAccountType x] :: [PaymentAccountType])
                      ]

instance Component (PaymentAccountModels) where
  type Dependencies (PaymentAccountModels) = End
  initialValue = IxSet.empty

getPaymentModel :: PaymentAccountType -> Update PaymentAccountModels PaymentAccountModel
getPaymentModel accountType =
  do
   models <- ask
   case getOne (models @= accountType) of
    Just model -> return model
    Nothing -> do
                let nmodel = basicModel accountType
                modify $ insert $ nmodel
                return nmodel

getPaymentModels :: Update PaymentAccountModels [PaymentAccountModel]
getPaymentModels = mapM getPaymentModel (allValues :: [PaymentAccountType])

updateAccountModel :: PaymentAccountType -> PaymentAccountModel -> Update PaymentAccountModels ()
updateAccountModel accountType newmodel =  modify (updateIx accountType newmodel)

-- | Basic empty model, everythhing is free, used only to init database
basicModel :: PaymentAccountType -> PaymentAccountModel
basicModel accountType  = PaymentAccountModel {modelAccountType = accountType,
                                               modelPaymentForAccounts = PaymentForAccounts {
                                                                                  forAccount=free,
                                                                                  forSubaccount=free
                                                                          },
                                               modelPaymentForSignature = PaymentForSignature {
                                                                                  forEmailSignature=free,
                                                                                  forElegSignature=free,
                                                                                  forMobileSignature=free,
                                                                                  forCreditCardSignature=free,
                                                                                  forIPadSignature=free
                                                                          },
                                               modelPaymentForSignedStorage = PaymentForSignedStorage {
                                                                                  forAmazon=free,
                                                                                  forTrustWeaver=free
                                                                          },
                                               modelPaymentForOtherStorage = PaymentForOtherStorage {
                                                                                  forTemplate=free,
                                                                                  forDraft=free
                                                                          }
                                               }
-- | Initial, empty account change - all values are taken from account model
emptyChange :: PaymentChange
emptyChange =  PaymentChange {
                                  changePaymentForAccounts= PaymentForAccounts {
                                                                                  forAccount=Nothing,
                                                                                  forSubaccount=Nothing
                                                                          },
                                  changePaymentForSignature = PaymentForSignature {
                                                                                  forEmailSignature=Nothing,
                                                                                  forElegSignature=Nothing,
                                                                                  forMobileSignature=Nothing,
                                                                                  forCreditCardSignature=Nothing,
                                                                                  forIPadSignature=Nothing
                                                                          },
                                  changePaymentForSignedStorage = PaymentForSignedStorage {
                                                                                  forAmazon=Nothing,
                                                                                  forTrustWeaver=Nothing
                                                                          },
                                  changePaymentForOtherStorage = PaymentForOtherStorage {
                                                                                  forTemplate=Nothing,
                                                                                  forDraft=Nothing
                                                                          }
                        }
-- | Changes that sets everything free
freeChange :: PaymentChange
freeChange =  PaymentChange {
                                  changePaymentForAccounts= PaymentForAccounts {
                                                                                  forAccount=Just free,
                                                                                  forSubaccount=Just free
                                                                          },
                                  changePaymentForSignature = PaymentForSignature {
                                                                                  forEmailSignature=Just free,
                                                                                  forElegSignature=Just free,
                                                                                  forMobileSignature=Just free,
                                                                                  forCreditCardSignature=Just free,
                                                                                  forIPadSignature=Just free
                                                                          },
                                  changePaymentForSignedStorage = PaymentForSignedStorage {
                                                                                  forAmazon=Just free,
                                                                                  forTrustWeaver=Just free
                                                                          },
                                  changePaymentForOtherStorage = PaymentForOtherStorage {
                                                                                  forTemplate=Just free,
                                                                                  forDraft=Just free
                                                                          }
                        }
-- | Basic payments policy, Private account with no changes custom or temporary changes
basicPaymentPolicy :: UserPaymentPolicy
basicPaymentPolicy =  UserPaymentPolicy {
                                      paymentaccounttype = Private
                                    , custompaymentchange = emptyChange
                                    , temppaymentchange = Nothing
                                   }
emptyPaymentAccount :: UserPaymentAccount
emptyPaymentAccount = UserPaymentAccount {
                                      paymentAgreementRef = Nothing
                                    , paymentaccountfreesignatures = 0
                                    }
-- | No money
free :: Money
free = Money 0

-- | Policy change that is free tile a date
freeTill :: MinutesTime -> UserPaymentPolicy -> UserPaymentPolicy
freeTill freetill pp  = pp {temppaymentchange = Just (freetill, freeChange)}

extendFreeTmpChange :: MinutesTime -> Int -> UserPaymentPolicy -> UserPaymentPolicy
extendFreeTmpChange now days pp = case (temppaymentchange pp) of
                                    Nothing -> pp {temppaymentchange = Just ((days * 24 *60) `minutesAfter` now, freeChange)}
                                    Just (till,c) -> if (c == freeChange)
                                                           then  pp {temppaymentchange = Just ((days * 24 *60) `minutesAfter` till, c)}
                                                           else pp


-- | combines two payment changes to get only one. First change is preffered
mergeChanges :: PaymentChange -> PaymentChange -> PaymentChange
mergeChanges pc1 pc2 = let merge f = f pc1 `mplus` f pc2 in
                PaymentChange {
                  changePaymentForAccounts= PaymentForAccounts {
                    forAccount= merge (forAccount . changePaymentForAccounts) ,
                    forSubaccount=merge (forSubaccount . changePaymentForAccounts)
                   },
                  changePaymentForSignature = PaymentForSignature {
                    forEmailSignature=merge (forEmailSignature . changePaymentForSignature) ,
                    forElegSignature=merge (forElegSignature . changePaymentForSignature),
                    forMobileSignature=merge (forMobileSignature . changePaymentForSignature),
                    forCreditCardSignature=merge (forCreditCardSignature . changePaymentForSignature),
                    forIPadSignature=merge (forIPadSignature . changePaymentForSignature)
                   },
                  changePaymentForSignedStorage = PaymentForSignedStorage {
                    forAmazon=merge (forAmazon . changePaymentForSignedStorage),
                    forTrustWeaver=merge (forTrustWeaver . changePaymentForSignedStorage)
                   },
                  changePaymentForOtherStorage = PaymentForOtherStorage {
                    forTemplate= merge (forTemplate . changePaymentForOtherStorage),
                    forDraft= merge(forDraft . changePaymentForOtherStorage)
                   }
                 }

-- | Immediately payment required
requiresImmediatelyPayment :: UserPaymentPolicy -> Bool
requiresImmediatelyPayment upa = paymentaccounttype upa == Private

-- | This probably is a relict and we will have different solution for that
takeOneFreeSignature :: UserPaymentAccount -> UserPaymentAccount
takeOneFreeSignature pa = pa {paymentaccountfreesignatures  = (paymentaccountfreesignatures pa) - 1}

--Wrapper for all functions above
$(mkMethods '' PaymentAccountModels
                    [ 'getPaymentModel
                    , 'getPaymentModels
                    , 'updateAccountModel
                    ])


-- | This is util for extracting real values from user payment schemes
-- | Payment scheme can be accessed just like Model or change but no setters are provides
-- | Also when calling some fields we need to use $$ in our call.
-- | Example: forEmail $$ paymentForSignatur scheme
type PaymentScheme = (PaymentChange, PaymentAccountModel)

infixr 0  $$
($$) :: (forall a. m a -> a) -> ((forall a. m a -> a) -> b) -> b
($$) a f =  f a

extract :: (m (Maybe b), m b) -> (forall a. m a -> a) -> b
extract (ma,a) f = fromMaybe (f a) (f ma)

paymentForAccounts :: PaymentScheme -> (forall a. PaymentForAccounts  a -> a) -> Money
paymentForAccounts (a,b) = extract (changePaymentForAccounts a, modelPaymentForAccounts b)

paymentForSignature :: PaymentScheme -> (forall a. PaymentForSignature a -> a) -> Money
paymentForSignature (a,b) = extract (changePaymentForSignature a, modelPaymentForSignature b)

paymentForSignedStorage :: PaymentScheme -> (forall a. PaymentForSignedStorage a -> a) -> Money
paymentForSignedStorage (a,b) = extract (changePaymentForSignedStorage a, modelPaymentForSignedStorage b)

paymentForOtherStorage :: PaymentScheme -> (forall a. PaymentForOtherStorage a -> a) -> Money
paymentForOtherStorage (a,b) = extract (changePaymentForOtherStorage a, modelPaymentForOtherStorage b)

