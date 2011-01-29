{-# OPTIONS_GHC -Wall #-}
-- |Simple session support
module PayEx.PayExState
    (   PaymentId(..) 
      , PaymentPosition(..)
      , PaymentState(..) 
      , PaymentMethod(..) 
      , Payment(..)
      , emptyPayment)
    where

import Control.Monad.Reader (ask)
import Control.Monad.State hiding (State)
import Data.Generics
import Data.Maybe (isNothing,isJust, fromJust)
import Happstack.Data
import Happstack.Data.IxSet
import qualified Happstack.Data.IxSet as IxSet
import Happstack.Server.HTTP.Types ()
import Happstack.State 
import UserState (UserID(..),FlashMessage,GetUserByUserID(GetUserByUserID), User)
import DocState
import MinutesTime
import Happstack.Server
import System.Random
import Payments.PaymentsState(Money(..))
import Misc

$( deriveAll [''Ord, ''Eq, ''Default, ''Show, ''Read]
   [d|
       newtype PaymentId = PaymentId {unPaymentId::Int}
       data PaymentPosition = PaymentForSigning DocumentID |
                              PaymentForStorage DocumentID |
                              PaymentForAccount
       data PaymentState =  Waiting | Captured | Finished | Failed
       data PaymentMethod = CreditCard | Invoice
       data Payment = Payment { -- | Each payment needs to be identified uniquely 
                                paymentId::PaymentId,
                                -- | Guid that is returned by PayEx
                                orderRef::String,
                                -- | Each payment is connected with a user
                                userId::UserID,
                                -- | We can charge user for many things durring one payment  
                                positions::[PaymentPosition],
                                -- | This is set when we calculated value of this Payment, bacause prices can change in time
                                value::Money ,
                                avaiblePaymentMethods::[PaymentMethod]  
                       }        
    
    |])


$(deriveSerialize ''PaymentId)
instance Version PaymentId  

$(deriveSerialize ''PaymentPosition)
instance Version PaymentPosition 

$(deriveSerialize ''PaymentState)
instance Version PaymentState  

$(deriveSerialize ''PaymentMethod)
instance Version PaymentMethod  

$(deriveSerialize ''Payment)
instance Version (Payment)
                         
  
$(inferIxSet "Payments" ''Payment 'noCalcs [''PaymentId])

instance Component (Payments) where
  type Dependencies (Payments) = End
  initialValue = IxSet.empty


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

$(mkMethods ''Payments 
  [ 'getPayment
  , 'newPayment

  ])

emptyPayment = Payment {  paymentId=PaymentId 0,
                          orderRef="",
                          userId=UserID 0,
                          positions=[],
                          value=Money 1,
                          avaiblePaymentMethods=[]
                       }        