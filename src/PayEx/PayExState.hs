{-# OPTIONS_GHC -Wall #-}
-- |Simple session support
module PayEx.PayExState
    (   PaymentId(..) 
      , PaymentPosition(..)
      , PaymentState(..) 
      , PaymentMethod(..) 
      , Payment(..)
      , Payments(..)
      , emptyPayment
      , paymentValue
      , isFailed
      --DB constructors
      , NewPayment(..)
      , GetPayment(..)
      , GetUserPayments(..)
      , UpdatePayment(..)
      , GetPaymentForDocumentSigning(..)
      , MergeForUser(..)
      , GetPaymentsThatNeedCheck(..))
    where

import Control.Monad.Reader (ask)
import Control.Monad.State hiding (State)
import Data.Generics
import Data.Maybe (isNothing,isJust, fromJust)
import Happstack.Data
import Happstack.Data.IxSet
import qualified Happstack.Data.IxSet as IxSet
import Happstack.State 
import User.UserState (UserID(..),FlashMessage,GetUserByUserID(GetUserByUserID), User, PaymentMethod(..))
import Doc.DocState
import MinutesTime
import Happstack.Server
import System.Random
import Payments.PaymentsState(Money(..))
import Misc
import Data.List (find)

newtype PaymentId = PaymentId {unPaymentId::Int}
    deriving (Eq, Ord, Typeable, Data)
data PaymentPosition = PaymentForSigning DocumentID |
                       PaymentForStorage DocumentID |
                       PaymentForAccount
                       deriving (Eq, Ord, Show, Typeable, Data)
data PaymentState =  Waiting | Send | Finished | Failed String PaymentState
                     deriving (Eq, Ord, Show, Typeable, Data)

data Payment = Payment { -- | Each payment needs to be identified uniquely 
                                paymentId::PaymentId,
                                -- | State of this payment  
                                paymentState::PaymentState,  
                                -- | When transaction should occur
                                paymentDate::MinutesTime,
                                -- | Transaction number returned by payEx  
                                transactionNumber::String,
                                -- | Guid that is returned by PayEx
                                orderRef::String,
                                --Url where user can make a payment  
                                redirectUrl::String,  
                                -- | Each payment is connected with a user
                                userId::UserID,
                                -- | We can charge user for many things durring one payment  
                                positions::[(PaymentPosition,Money)],
                                -- | This is set when we calculated value of this Payment, bacause prices can change in time
                                avaiblePaymentMethods::[PaymentMethod],
                                -- | Did we try to uer autopay functionality with this payment 
                                triedAutopay::Bool,
                                -- | When were we trying to check if transaction is finished
                                completeAttempts::[MinutesTime]
                       }        
                  deriving (Eq, Ord, Show)

instance Typeable Payment where typeOf _ = mkTypeOf "Payment"

deriving instance Data Payment


instance Show PaymentId where
  show = show . unPaymentId
  
instance Read PaymentId where
  readsPrec prec = let make (i,v) = (PaymentId i,v) 
                     in map make . readsPrec prec 
                     
$(deriveSerialize ''PaymentId)
instance Version PaymentId  

$(deriveSerialize ''PaymentPosition)
instance Version PaymentPosition 

$(deriveSerialize ''PaymentState)
instance Version PaymentState  

$(deriveSerialize ''Payment)
instance Version (Payment)
                         
  
$(inferIxSet "Payments" ''Payment 'noCalcs [''PaymentId, ''UserID, ''DocumentID])

instance Component (Payments) where
  type Dependencies (Payments) = End
  initialValue = IxSet.empty




-- | Global utils
paymentValue::Payment -> Money
paymentValue payment = sum $ map snd $ positions payment

isFailed::Payment -> Bool
isFailed (Payment{paymentState = (Failed _ _)}) = True
isFailed _ = False

isPaymentForSigningDocument ::DocumentID -> Payment -> Bool
isPaymentForSigningDocument docid payment = any ((==) (PaymentForSigning docid)) $ map fst $ positions payment

emptyPayment = Payment {  paymentId=PaymentId 0,
                          paymentState = Waiting ,
                          orderRef="",
                          transactionNumber="",  
                          redirectUrl="",
                          userId=UserID 0,
                          positions=[],
                          avaiblePaymentMethods=[],
                          paymentDate = MinutesTime 0 0,
                          triedAutopay = False,
                          completeAttempts = []  
                       }        

  

--DB FUNCTIONS  

-- |get the session data associated with the supplied SessionId
getPayment :: PaymentId -> Query Payments (Maybe (Payment))
getPayment paymentId = (return . getOne . (@= (paymentId :: PaymentId))) =<< ask

-- |start a new session with the supplied session data
-- returns: the SessionId
newPayment :: Payment -> Update Payments Payment
newPayment payment =
    do 
       payments <- ask 
       paymentId <- getUnique payments PaymentId
       let npayment = payment {paymentId = paymentId}
       modify $ insert $ npayment
       return npayment

updatePayment :: Payment -> Update Payments ()
updatePayment payment = modify (updateIx (paymentId payment) payment)

getUserPayments :: UserID -> Query Payments [Payment]
getUserPayments userid = do
                           payments <- ask                    
                           return $ toList (payments @= userid) 

getPaymentForDocumentSigning :: DocumentID -> Query Payments (Maybe Payment)
getPaymentForDocumentSigning docid = do
                                       payments <- fmap (toList . (@= docid)) ask
                                       return $ find ((==) Waiting . paymentState) $ filter (isPaymentForSigningDocument docid) payments


mergeForUser :: UserID -> Update Payments Payment
mergeForUser uid = do
                    payments <- fmap (filter (((==) Waiting) . paymentState) . toList . (@= uid)) $ ask
                    npayment <- newPayment emptyPayment {paymentState = Waiting, positions= join $ map positions payments, userId = uid}
                    sequence_ $ map (modify . delete) payments
                    return npayment


--This is 'don't have more time' solution. It is very very inefficient
getPaymentsThatNeedCheck :: MinutesTime -> Query Payments [Payment]
getPaymentsThatNeedCheck now = do
                            send <- fmap (filter (((==) Send) . paymentState) . toList) ask
                            return $ flip filter send $
                               \payment -> case (completeAttempts payment) of
                                            (lastattempt:rest) -> if (length rest > 20)
                                                          then False
                                                          else (now > 30 `minutesAfter` lastattempt)
                                            _ -> True
                                                
                                                      
                            

$(mkMethods ''Payments 
  [ 'getPayment
  , 'newPayment
  , 'getUserPayments 
  , 'updatePayment
  , 'getPaymentForDocumentSigning
  , 'mergeForUser
  , 'getPaymentsThatNeedCheck
  ])
