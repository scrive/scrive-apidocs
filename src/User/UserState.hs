{-# OPTIONS_GHC -Wall #-}
module User.UserState 
    ( Email(..)
    , Friend(..)
    , Inviter(..)
    , InviteType(..)
    , InviteInfo(..)
    , LoginInfo(..)
    , DefaultMainSignatory(..)
    , ExternalUserID(..)
    , FlashType(..)
    , FlashMessage(..)
    , Password(..)
    , TrustWeaverStorage(..)
    , UserAccountType(..)
    , PaymentMethod(..)
    , UserAccountPlan(..)
    , SupervisorID(..)
    , User(..)
    , UserInfo(..)
    , UserSettings(..)
    , UserID(..)
    , Users
    , UserStats(..)
    , toFlashMsg
    , composeFullName
    , userfullname
    , createPassword
    , verifyPassword

    , AcceptTermsOfService(..)
    , AddUser(..)
    , ExportUsersDetailsToCSV(..)
    , GetAllUsers(..)
    , GetUserByEmail(..)
    , GetUserByUserID(..)
    , GetUserStats(..)
    , GetUserStatsByUser(..)
    , GetUserSubaccounts(..)
    , GetUserFriends(..)
    , SetUserInfo(..)
    , SetInviteInfo(..)
    , SetUserSettings(..)
    , SetUserPaymentAccount(..)
    , SetUserPaymentPolicyChange(..)
    , SetUserPassword(..)
    , GetUsersByFriendUserID(..)
    , AddViewerByEmail(..)
    , FreeUserFromPayments(..)
    , AddFreePaymentsForInviter(..)
    , RecordFailedLogin(..)
    , RecordSuccessfulLogin(..)
    , getUserPaymentSchema
    , takeImmediatelyPayment
) where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify,MonadState(..))
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (unlines) 
import Happstack.Data.IxSet as IxSet
import Data.Maybe(isJust,fromJust)
import Misc
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Codec.Utils (Octet)
import Data.Digest.SHA256 (hash)
import System.Random
import Data.List
import qualified Data.Set as Set
import Control.Applicative
import MinutesTime
import qualified Payments.PaymentsState as Payments
import Data.Data

newtype UserID = UserID { unUserID :: Int }
    deriving (Eq, Ord, Typeable)

deriving instance Data UserID

newtype ExternalUserID = ExternalUserID { unExternalUserID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
      -- Leaving FlashMessage declaration here is necessity
      -- Have to be used because of users versioning
      -- Can't be moved to Session where it belong (cycle references)
newtype Friend = Friend { unFriend :: Int }
    deriving (Eq, Ord, Typeable)
newtype Inviter = Inviter { unInviter :: Int }
    deriving (Eq, Ord, Typeable)
data InviteType = Viral | Admin
    deriving (Eq, Ord, Typeable)
data InviteInfo = InviteInfo 
          { userinviter :: Inviter
          , invitetime :: Maybe MinutesTime
          , invitetype :: Maybe InviteType
          }
    deriving (Eq, Ord, Typeable)
data LoginInfo = LoginInfo
          { lastsuccesstime :: Maybe MinutesTime
          , lastfailtime :: Maybe MinutesTime
          , consecutivefails :: Int
          }
    deriving (Eq, Ord, Typeable)
newtype DefaultMainSignatory = DefaultMainSignatory { unDMS :: Int }
    deriving (Eq, Ord, Typeable)
newtype FlashMessage0 = FlashMessage0 BS.ByteString
    deriving (Eq, Ord, Typeable)
newtype FlashMessage = FlashMessage { unFlashMessage :: (FlashType, String) }
    deriving (Eq, Ord, Typeable)
newtype Email = Email { unEmail :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
data Password = Password [Octet] [Octet] | NoPassword
    deriving (Eq, Ord, Typeable)
newtype SupervisorID = SupervisorID { unSupervisorID :: Int }
    deriving (Eq, Ord, Typeable)
data FlashType
        = SigningRelated
        | OperationDone
        | OperationFailed
        | Modal
    deriving (Eq, Ord, Typeable)
data TrustWeaverStorage = TrustWeaverStorage
          { storagetwenabled       :: Bool
          , storagetwname          :: BS.ByteString
          , storagetwsuperadmin    :: BS.ByteString
          , storagetwsuperadminpwd :: BS.ByteString
          , storagetwsectionpath   :: BS.ByteString
          }
    deriving (Eq, Ord, Typeable)
data UserAccountType = MainAccount | SubAccount
    deriving (Eq, Ord, Typeable)
data PaymentMethod = CreditCard | Invoice | Undefined
    deriving (Eq, Ord, Typeable)

deriving instance Data PaymentMethod

data UserAccountPlan = Basic
    deriving (Eq, Ord, Typeable)
data UserInfo0 = UserInfo0 {
            userfstname0                   :: BS.ByteString
          , usersndname0                   :: BS.ByteString
          , userpersonalnumber0            :: BS.ByteString
          , usercompanyname0               :: BS.ByteString
          , usercompanynumber0             :: BS.ByteString
          , useraddress0                   :: BS.ByteString 
          , userzip0                       :: BS.ByteString
          , usercity0                      :: BS.ByteString
          , usercountry0                   :: BS.ByteString
          , userphone0                     :: BS.ByteString
          , usermobile0                    :: BS.ByteString
          , useremail0                     :: Email 
          }       
                 deriving (Eq, Ord, Typeable)
          
data UserInfo = UserInfo {
            userfstname                   :: BS.ByteString
          , usersndname                   :: BS.ByteString
          , userpersonalnumber            :: BS.ByteString
          , usercompanyname               :: BS.ByteString
          , usercompanyposition           :: BS.ByteString
          , usercompanynumber             :: BS.ByteString
          , useraddress                   :: BS.ByteString 
          , userzip                       :: BS.ByteString
          , usercity                      :: BS.ByteString
          , usercountry                   :: BS.ByteString
          , userphone                     :: BS.ByteString
          , usermobile                    :: BS.ByteString
          , useremail                     :: Email 
          }        
    deriving (Eq, Ord, Typeable)

data UserSettings  = UserSettings {
               accounttype :: UserAccountType
             , accountplan :: UserAccountPlan
             , signeddocstorage :: Maybe TrustWeaverStorage
             , userpaymentmethod :: PaymentMethod
      }
    deriving (Eq, Ord, Typeable)
      
data User = User
          { userid                        :: UserID
          , userpassword                  :: Password
          , usersupervisor                :: Maybe SupervisorID
          , usercanhavesubaccounts        :: Bool
          , useraccountsuspended          :: Bool
          , userhasacceptedtermsofservice :: Maybe MinutesTime
          , userinfo                      :: UserInfo
          , usersettings                  :: UserSettings
          , userpaymentpolicy             :: Payments.UserPaymentPolicy
          , userpaymentaccount            :: Payments.UserPaymentAccount
          , userfriends                   :: [Friend]
          , userinviteinfo                :: Maybe InviteInfo
          , userlogininfo                 :: LoginInfo
          }
            deriving (Eq, Ord)

instance Typeable User where typeOf _ = mkTypeOf "User"

data User10 = User10
          { userid10                        :: UserID
          , userpassword10                  :: Password
          , usersupervisor10                :: Maybe SupervisorID
          , usercanhavesubaccounts10        :: Bool
          , useraccountsuspended10          :: Bool
          , userhasacceptedtermsofservice10 :: Maybe MinutesTime
          , userinfo10                      :: UserInfo
          , usersettings10                  :: UserSettings
          , userpaymentpolicy10             :: Payments.UserPaymentPolicy
          , userpaymentaccount10            :: Payments.UserPaymentAccount
          , userfriends10                   :: [Friend]
          , userinviteinfo10                :: Maybe InviteInfo
          }
    deriving (Eq, Ord, Typeable)

data User9 = User9
          { userid9                        :: UserID
          , userpassword9                  :: Password
          , usersupervisor9                :: Maybe SupervisorID
          , usercanhavesubaccounts9        :: Bool
          , useraccountsuspended9          :: Bool
          , userhasacceptedtermsofservice9 :: Maybe MinutesTime
          , userinfo9                      :: UserInfo
          , usersettings9                  :: UserSettings
          , userpaymentpolicy9             :: Payments.UserPaymentPolicy
          , userpaymentaccount9            :: Payments.UserPaymentAccount
          , userfriends9                   :: [Friend]
          , userinviter9                   :: Maybe Inviter
          }
    deriving (Eq, Ord, Typeable)
          
data User8 = User8
          { userid8                        :: UserID
          , userpassword8                  :: Password
          , usersupervisor8                :: Maybe SupervisorID
          , usercanhavesubaccounts8        :: Bool
          , useraccountsuspended8          :: Bool
          , userhasacceptedtermsofservice8 :: Maybe MinutesTime
          , userinfo8                      :: UserInfo
          , usersettings8                  :: UserSettings
          , userpaymentpolicy8             :: Payments.UserPaymentPolicy
          , userpaymentaccount8            :: Payments.UserPaymentAccount
          , userfriends8                   :: [Friend]
          -- should remove userdefaultmainsignatory in the next migration. just get rid of it.
          , userdefaultmainsignatory8      :: DefaultMainSignatory
          }
    deriving (Eq, Ord, Typeable)

data User7 = User7
          { userid7                        :: UserID
          , userpassword7                  :: Password
          , usersupervisor7                :: Maybe SupervisorID
          , usercanhavesubaccounts7        :: Bool
          , useraccountsuspended7          :: Bool
          , userhasacceptedtermsofservice7 :: Maybe MinutesTime
          , userinfo7                      :: UserInfo
          , usersettings7                  :: UserSettings
          , userpaymentpolicy7             :: Payments.UserPaymentPolicy
          , userpaymentaccount7            :: Payments.UserPaymentAccount
          }
    deriving (Eq, Ord, Typeable)
      
data User6 = User6
          { userid6                        :: UserID
          , userfullname6                  :: BS.ByteString
          , useremail6                     :: Email
          , usercompanyname6               :: BS.ByteString
          , usercompanynumber6             :: BS.ByteString
          , userinvoiceaddress6            :: BS.ByteString
          , userflashmessages6             :: [FlashMessage]
          , userpassword6                  :: Password
          , usersupervisor6                :: Maybe SupervisorID
          , usercanhavesubaccounts6        :: Bool
          , useraccountsuspended6          :: Bool
          , userhasacceptedtermsofservice6 :: Maybe MinutesTime
          }
    deriving (Eq, Ord, Typeable)

data User5 = User5
          { userid5                 :: UserID
          , userfullname5           :: BS.ByteString
          , useremail5              :: Email
          , usercompanyname5        :: BS.ByteString
          , usercompanynumber5      :: BS.ByteString
          , userinvoiceaddress5     :: BS.ByteString
          , userflashmessages5      :: [FlashMessage]
          , userpassword5           :: Password
          , usersupervisor5         :: Maybe SupervisorID
          , usercanhavesubaccounts5 :: Bool
          , useraccountsuspended5   :: Bool
          }
    deriving (Eq, Ord, Typeable)
          
data User4 = User4
          { userid4                 :: UserID
          , userfullname4           :: BS.ByteString
          , useremail4              :: Email
          , usercompanyname4        :: BS.ByteString
          , usercompanynumber4      :: BS.ByteString
          , userinvoiceaddress4     :: BS.ByteString
          , userflashmessages4      :: [FlashMessage]
          , userpassword4           :: BS.ByteString
          , usersupervisor4         :: Maybe SupervisorID
          , usercanhavesubaccounts4 :: Bool
          , useraccountsuspended4   :: Bool
          }
    deriving (Eq, Ord, Typeable)

data User3 = User3
          { userid3             :: UserID
          , userexternalids3    :: [ExternalUserID]
          , userfullname3       :: BS.ByteString
          , useremail3          :: Email
          , usercompanyname3    :: BS.ByteString
          , usercompanynumber3  :: BS.ByteString
          , userinvoiceaddress3 :: BS.ByteString
          , userflashmessages3  :: [FlashMessage]
          , userpassword3       :: Maybe BS.ByteString
          }
    deriving (Eq, Ord, Typeable)

data User2 = User2
          { userid2             :: UserID
          , userexternalids2    :: [ExternalUserID]
          , userfullname2       :: BS.ByteString
          , useremail2          :: BS.ByteString
          , usercompanyname2    :: BS.ByteString
          , usercompanynumber2  :: BS.ByteString
          , userinvoiceaddress2 :: BS.ByteString
          , userflashmessages2  :: [FlashMessage]
          }
    deriving (Eq, Ord, Typeable)

data User1 = User1
          { userid1             :: UserID
          , externaluserids1    :: [ExternalUserID]
          , fullname1           :: BS.ByteString
          , email1              :: BS.ByteString
          , usercompanyname1    :: BS.ByteString
          , usercompanynumber1  :: BS.ByteString
          , userinvoiceaddress1 :: BS.ByteString
          }
    deriving (Eq, Ord, Typeable)

data User0 = User0
          { userid0          :: UserID
          , externaluserids0 :: [ExternalUserID]
          , fullname0        :: BS.ByteString
          , email0           :: BS.ByteString
          }
    deriving (Eq, Ord, Typeable)

data UserStats = UserStats 
                       { usercount :: Int
                       , viralinvitecount :: Int
                       , admininvitecount :: Int
                       }
    deriving (Eq, Ord, Typeable)

deriving instance Data UserStats

deriving instance Show FlashType
deriving instance Show TrustWeaverStorage
deriving instance Show UserAccountType 
deriving instance Show PaymentMethod
deriving instance Show UserAccountPlan 
deriving instance Show UserInfo
deriving instance Show UserSettings
deriving instance Show User
deriving instance Show Email
deriving instance Show FlashMessage
deriving instance Show Password
deriving instance Show Friend
deriving instance Show Inviter
deriving instance Show InviteInfo
deriving instance Show InviteType
deriving instance Show LoginInfo
deriving instance Show DefaultMainSignatory
deriving instance Show UserStats

deriving instance Read FlashType
deriving instance Read FlashMessage
deriving instance Read TrustWeaverStorage

deriving instance Bounded UserAccountType
deriving instance Enum UserAccountType
deriving instance Read UserAccountType

deriving instance Bounded PaymentMethod
deriving instance Enum PaymentMethod
deriving instance Read PaymentMethod

deriving instance Bounded UserAccountPlan
deriving instance Enum UserAccountPlan
deriving instance Read UserAccountPlan

instance Migrate FlashMessage0 FlashMessage where
    migrate (FlashMessage0 msg) =
        toFlashMsg SigningRelated $ BS.toString msg

instance Migrate User0 User1 where
    migrate (User0
             { userid0
             , externaluserids0
             , fullname0
             , email0
             }) = User1
                { userid1 = userid0
                , externaluserids1 = externaluserids0
                , fullname1 = fullname0
                , email1 = email0
                , usercompanyname1 = BS.empty
                , usercompanynumber1 = BS.empty
                , userinvoiceaddress1 = BS.empty
                }

instance Migrate User1 User2 where
    migrate (User1
             { userid1
             , externaluserids1
             , fullname1
             , email1
             , usercompanyname1
             , usercompanynumber1
             , userinvoiceaddress1
             }) = User2
                { userid2 = userid1
                , userexternalids2 = externaluserids1
                , userfullname2 = fullname1
                , useremail2 = email1
                , usercompanyname2 = usercompanyname1
                , usercompanynumber2 = usercompanynumber1
                , userinvoiceaddress2 = userinvoiceaddress1
                , userflashmessages2 = []
                }

instance Migrate User2 User3 where
    migrate (User2
                { userid2
                , userexternalids2
                , userfullname2
                , useremail2
                , usercompanyname2
                , usercompanynumber2
                , userinvoiceaddress2
                , userflashmessages2
                }) = User3
                { userid3 = userid2
                , userexternalids3 = userexternalids2
                , userfullname3 = userfullname2
                , useremail3 = Email useremail2
                , usercompanyname3 = usercompanyname2
                , usercompanynumber3 = usercompanynumber2
                , userinvoiceaddress3 = userinvoiceaddress2
                , userflashmessages3 = userflashmessages2
                , userpassword3 = Nothing
                }

instance Migrate User3 User4 where
    migrate (User3
          { userid3
          -- , userexternalids3
          , userfullname3
          , useremail3
          , usercompanyname3
          , usercompanynumber3
          , userinvoiceaddress3
          , userflashmessages3
          , userpassword3
          }) = User4
          { userid4 = userid3
          , userfullname4 = userfullname3
          , useremail4 = useremail3
          , usercompanyname4 = usercompanyname3
          , usercompanynumber4 = usercompanynumber3
          , userinvoiceaddress4 = userinvoiceaddress3
          , userflashmessages4 = userflashmessages3
          , userpassword4 = maybe BS.empty id userpassword3
          , usersupervisor4 = Nothing
          , usercanhavesubaccounts4 = True
          , useraccountsuspended4 = False -- should probably have a reason and time here
          }

instance Migrate User4 User5 where
    migrate (User4
          { userid4
          , userfullname4
          , useremail4
          , usercompanyname4
          , usercompanynumber4
          , userinvoiceaddress4
          , userflashmessages4
          , userpassword4
          , usersupervisor4
          , usercanhavesubaccounts4
          , useraccountsuspended4
          }) = User5
          { userid5 = userid4
          , userfullname5 = userfullname4
          , useremail5 = useremail4
          , usercompanyname5 = usercompanyname4
          , usercompanynumber5 = usercompanynumber4
          , userinvoiceaddress5 = userinvoiceaddress4
          , userflashmessages5 = userflashmessages4
          , userpassword5 = unsafePerformIO $ createPassword userpassword4
          , usersupervisor5 = usersupervisor4
          , usercanhavesubaccounts5 = usercanhavesubaccounts4
          , useraccountsuspended5 = useraccountsuspended4
          }

instance Migrate User5 User6 where
    migrate (User5
             { userid5
             , userfullname5
             , useremail5   
             , usercompanyname5
             , usercompanynumber5
             , userinvoiceaddress5
             , userflashmessages5 
             , userpassword5      
             , usersupervisor5    
             , usercanhavesubaccounts5
             , useraccountsuspended5
             }) = User6
                { userid6                = userid5
                , userfullname6          = userfullname5
                , useremail6             = useremail5
                , usercompanyname6       = usercompanyname5
                , usercompanynumber6     = usercompanynumber5
                , userinvoiceaddress6    = userinvoiceaddress5
                , userflashmessages6     = userflashmessages5
                , userpassword6          = userpassword5
                , usersupervisor6        = usersupervisor5
                , usercanhavesubaccounts6= usercanhavesubaccounts5
                , useraccountsuspended6  = useraccountsuspended5
                , userhasacceptedtermsofservice6 = Nothing
                }
                
instance Migrate User6 User7 where
    migrate (User6
             { userid6
             , userfullname6
             , useremail6   
             , usercompanyname6
             , usercompanynumber6
             , userinvoiceaddress6
             , userpassword6      
             , usersupervisor6    
             , usercanhavesubaccounts6
             , useraccountsuspended6
             , userhasacceptedtermsofservice6
             }) = User7
                {  userid7                  =  userid6
                 , userpassword7            =  userpassword6 
                 , usersupervisor7          =  usersupervisor6   
                 , usercanhavesubaccounts7  =  usercanhavesubaccounts6 
                 , useraccountsuspended7    =  useraccountsuspended6      
                 , userhasacceptedtermsofservice7 = userhasacceptedtermsofservice6
                 , userinfo7 = UserInfo {
                                    userfstname = userfullname6          
                                  , usersndname = BS.empty
                                  , userpersonalnumber = BS.empty
                                  , usercompanyname = usercompanyname6
                                  , usercompanyposition = BS.empty
                                  , usercompanynumber  = usercompanynumber6
                                  , useraddress = userinvoiceaddress6
                                  , userzip = BS.empty
                                  , usercity  = BS.empty
                                  , usercountry = BS.empty
                                  , userphone = BS.empty
                                  , usermobile = BS.empty
                                  , useremail = useremail6   
                                   }
                , usersettings7  = UserSettings {
                                    accounttype = if (isNothing usersupervisor6)  then MainAccount else SubAccount
                                  , accountplan = Basic
                                  , signeddocstorage = Nothing
                                  , userpaymentmethod = Undefined
                                  }                   
                , userpaymentpolicy7 =  Payments.basicPaymentPolicy
                , userpaymentaccount7 = Payments.emptyPaymentAccount                  
      }       
      
instance Migrate User7 User8 where
    migrate (User7 
             { userid7 
             , userpassword7
             , usersupervisor7 
             , usercanhavesubaccounts7
             , useraccountsuspended7     
             , userhasacceptedtermsofservice7
             , userinfo7                   
             , usersettings7                  
             , userpaymentpolicy7
             , userpaymentaccount7  
             }) = User8 
                { userid8                         = userid7
                , userpassword8                   = userpassword7
                , usersupervisor8                 = usersupervisor7
                , usercanhavesubaccounts8         = usercanhavesubaccounts7
                , useraccountsuspended8           = useraccountsuspended7
                , userhasacceptedtermsofservice8  = userhasacceptedtermsofservice7
                , userinfo8                       = userinfo7
                , usersettings8                   = usersettings7
                , userpaymentpolicy8              = userpaymentpolicy7
                , userpaymentaccount8             = userpaymentaccount7
                , userfriends8                    = []
                , userdefaultmainsignatory8       = DefaultMainSignatory $ unUserID userid7
                }

instance Migrate User8 User9 where
    migrate (User8
               { userid8                     
                , userpassword8                
                , usersupervisor8               
                , usercanhavesubaccounts8        
                , useraccountsuspended8          
                , userhasacceptedtermsofservice8  
                , userinfo8                     
                , usersettings8                
                , userpaymentpolicy8             
                , userpaymentaccount8           
                , userfriends8                  
                , userdefaultmainsignatory8 = _       
                }) = User9 
                { userid9                         = userid8
                , userpassword9                   = userpassword8
                , usersupervisor9                 = usersupervisor8
                , usercanhavesubaccounts9         = usercanhavesubaccounts8
                , useraccountsuspended9           = useraccountsuspended8
                , userhasacceptedtermsofservice9  = userhasacceptedtermsofservice8
                , userinfo9                       = userinfo8
                , usersettings9                   = usersettings8
                , userpaymentpolicy9              = userpaymentpolicy8
                , userpaymentaccount9             = userpaymentaccount8
                , userfriends9                    = userfriends8
                , userinviter9                    = Nothing          
                }

instance Migrate User9 User10 where
    migrate (User9
               { userid9                     
                , userpassword9                
                , usersupervisor9               
                , usercanhavesubaccounts9        
                , useraccountsuspended9          
                , userhasacceptedtermsofservice9  
                , userinfo9                     
                , usersettings9                
                , userpaymentpolicy9             
                , userpaymentaccount9           
                , userfriends9                  
                , userinviter9       
                }) = User10 
                { userid10                         = userid9
                , userpassword10                   = userpassword9
                , usersupervisor10                 = usersupervisor9
                , usercanhavesubaccounts10         = usercanhavesubaccounts9
                , useraccountsuspended10           = useraccountsuspended9
                , userhasacceptedtermsofservice10  = userhasacceptedtermsofservice9
                , userinfo10                       = userinfo9
                , usersettings10                   = usersettings9
                , userpaymentpolicy10              = userpaymentpolicy9
                , userpaymentaccount10             = userpaymentaccount9
                , userfriends10                    = userfriends9
                , userinviteinfo10                 = fmap 
                                                       (\inviter ->  InviteInfo
                                                           { userinviter = inviter
                                                           , invitetime = Nothing
                                                           , invitetype = Nothing
                                                       })
                                                       userinviter9
                }

instance Migrate User10 User where
    migrate (User10
               { userid10                     
                , userpassword10                
                , usersupervisor10               
                , usercanhavesubaccounts10        
                , useraccountsuspended10          
                , userhasacceptedtermsofservice10  
                , userinfo10                     
                , usersettings10                
                , userpaymentpolicy10             
                , userpaymentaccount10           
                , userfriends10                  
                , userinviteinfo10       
                }) = User 
                { userid                         = userid10
                , userpassword                   = userpassword10
                , usersupervisor                 = usersupervisor10
                , usercanhavesubaccounts         = usercanhavesubaccounts10
                , useraccountsuspended           = useraccountsuspended10
                , userhasacceptedtermsofservice  = userhasacceptedtermsofservice10
                , userinfo                       = userinfo10
                , usersettings                   = usersettings10
                , userpaymentpolicy              = userpaymentpolicy10
                , userpaymentaccount             = userpaymentaccount10
                , userfriends                    = userfriends10
                , userinviteinfo                 = userinviteinfo10
                , userlogininfo                 = LoginInfo
                                                    { lastsuccesstime = Nothing
                                                    , lastfailtime = Nothing
                                                    , consecutivefails = 0
                                                    }
                }


toFlashMsg :: FlashType -> String -> FlashMessage
toFlashMsg type_ msg = FlashMessage (type_, msg)

composeFullName :: (BS.ByteString, BS.ByteString) -> BS.ByteString
composeFullName (fstname, sndname) =
    if BS.null sndname
       then fstname
       else fstname `BS.append` BS.fromString " " `BS.append` sndname

userfullname :: User -> BS.ByteString
userfullname u = composeFullName (userfstname $ userinfo u, usersndname $ userinfo u)

instance Migrate UserInfo0 UserInfo where
    migrate (UserInfo0 {
            userfstname0  
          , usersndname0       
          , userpersonalnumber0    
          , usercompanyname0    
          , usercompanynumber0  
          , useraddress0  
          , userzip0     
          , usercity0          
          , usercountry0   
          , userphone0          
          , usermobile0          
          , useremail0        
          }) = UserInfo {
            userfstname = userfstname0 
          , usersndname = usersndname0
          , userpersonalnumber = userpersonalnumber0
          , usercompanyname = usercompanyname0
          , usercompanyposition = BS.empty
          , usercompanynumber = usercompanynumber0
          , useraddress = useraddress0
          , userzip = userzip0
          , usercity = usercity0
          , usercountry = usercountry0
          , userphone = userphone0
          , usermobile = usermobile0
          , useremail = useremail0
          }

createPassword :: BS.ByteString -> IO Password
createPassword password = do
  salt <- makeSalt
  return $ Password salt (hashPassword password salt)
  
randomOctets :: Int -> IO [Octet]
randomOctets n = do
  randomGen <- newStdGen
  return $ take n $ map fromIntegral (randoms randomGen :: [Int])

makeSalt :: IO [Octet]
makeSalt = randomOctets 10

hashPassword :: BS.ByteString -> [Octet] -> [Octet]
hashPassword password salt =
  hash (salt ++ (BS.unpack password))

type Users = IxSet User

instance Indexable User where
        empty = ixSet [ ixFun (\x -> [userid x] :: [UserID])
                      , ixFun (\x -> [useremail $ userinfo x] :: [Email])
                      , ixFun (\x -> maybe [] return (usersupervisor x) :: [SupervisorID])
                      , ixFun userfriends
                      ]

$(deriveSerialize ''User0)
instance Version User0

$(deriveSerialize ''User1)
instance Version User1 where
    mode = extension 1 (Proxy :: Proxy User0)

$(deriveSerialize ''User2)
instance Version User2 where
    mode = extension 2 (Proxy :: Proxy User1)

$(deriveSerialize ''User3)
instance Version User3 where
    mode = extension 3 (Proxy :: Proxy User2)

$(deriveSerialize ''User4)
instance Version User4 where
    mode = extension 4 (Proxy :: Proxy User3)

$(deriveSerialize ''User5)
instance Version User5 where
    mode = extension 5 (Proxy :: Proxy User4)

$(deriveSerialize ''User6)
instance Version User6 where
    mode = extension 6 (Proxy :: Proxy User5)

$(deriveSerialize ''User7)
instance Version User7 where
    mode = extension 7 (Proxy :: Proxy User6) 

$(deriveSerialize ''User8)
instance Version User8 where
    mode = extension 8 (Proxy :: Proxy User7) 

$(deriveSerialize ''User9)
instance Version User9 where
    mode = extension 9 (Proxy :: Proxy User8)

$(deriveSerialize ''User10)
instance Version User10 where
    mode = extension 10 (Proxy :: Proxy User9)

$(deriveSerialize ''User)
instance Version User where
    mode = extension 11 (Proxy :: Proxy User10)

$(deriveSerialize ''TrustWeaverStorage )
instance Version TrustWeaverStorage

$(deriveSerialize ''UserAccountType )
instance Version UserAccountType 

$(deriveSerialize ''PaymentMethod)
instance Version PaymentMethod

$(deriveSerialize ''UserAccountPlan )
instance Version UserAccountPlan 

$(deriveSerialize ''UserInfo0)
instance Version UserInfo0

$(deriveSerialize ''UserInfo)
instance Version UserInfo where
    mode = extension 1 (Proxy :: Proxy UserInfo0)


$(deriveSerialize ''UserSettings)
instance Version UserSettings

$(deriveSerialize ''FlashType)
instance Version FlashType

$(deriveSerialize ''FlashMessage0)
instance Version FlashMessage0

$(deriveSerialize ''FlashMessage)
instance Version FlashMessage where
    mode = extension 1 (Proxy :: Proxy FlashMessage0)

$(deriveSerialize ''Email)
instance Version Email

$(deriveSerialize ''Password)
instance Version Password

$(deriveSerialize ''UserID)
instance Version UserID

$(deriveSerialize ''Friend)
instance Version Friend

$(deriveSerialize ''Inviter)
instance Version Inviter

$(deriveSerialize ''InviteInfo)
instance Version InviteInfo

$(deriveSerialize ''InviteType)
instance Version InviteType

$(deriveSerialize ''LoginInfo)
instance Version LoginInfo

$(deriveSerialize ''DefaultMainSignatory)
instance Version DefaultMainSignatory

$(deriveSerialize ''SupervisorID)
instance Version SupervisorID

$(deriveSerialize ''ExternalUserID)
instance Version ExternalUserID

$(deriveSerialize ''UserStats)
instance Version UserStats

instance Show ExternalUserID where
    showsPrec prec (ExternalUserID val) = showsPrec prec val

instance Read ExternalUserID where
    readsPrec prec = let make (i,v) = (ExternalUserID i,v) 
                     in map make . readsPrec prec 

instance Show UserID where
    showsPrec prec (UserID val) = showsPrec prec val

instance Read UserID where
    readsPrec prec = let make (i,v) = (UserID i,v) 
                     in map make . readsPrec prec 

instance FromReqURI UserID where
    fromReqURI = readM

instance Show SupervisorID where
    showsPrec prec (SupervisorID val) = showsPrec prec val

instance Read SupervisorID where
    readsPrec prec = let make (i,v) = (SupervisorID i,v) 
                     in map make . readsPrec prec 

instance FromReqURI SupervisorID where
    fromReqURI = readM

modifyUser :: UserID 
           -> (User -> Either String User) 
           -> Update Users (Either String User)
modifyUser uid action = do
  users <- ask
  case getOne (users @= uid) of
    Nothing -> return $ Left "no such user"
    Just user -> 
        case action user of
          Left message -> return $ Left message
          Right newuser -> 
              if userid newuser /= uid
                 then return $ Left "new user must have same id as old one"
              else do
                modify (updateIx uid newuser)
                return $ Right newuser

getUserByEmail :: Email -> Query Users (Maybe User)
getUserByEmail email = do
  users <- ask
  return $ getOne (users @= email)
    
getUserByUserID :: UserID -> Query Users (Maybe User)
getUserByUserID userid = do
  users <- ask
  return $ getOne (users @= userid)

getUsersByFriendUserID :: UserID -> Query Users [User]
getUsersByFriendUserID uid =
  return . toList . (@= (Friend $ unUserID uid)) =<< ask

getUserFriends :: UserID -> Query Users [User]
getUserFriends uid = do
  muser <- getUserByUserID uid
  case muser of
    Nothing -> return []
    Just user -> do
      mfriends <- sequence . map (getUserByUserID . UserID . unFriend) $ userfriends user
      return . map fromJust . filter isJust $ mfriends

getUserSubaccounts :: UserID -> Query Users (Set.Set User)
getUserSubaccounts userid = do
  users <- ask
  return $ toSet (users @= SupervisorID (unUserID userid))

addUser :: (BS.ByteString, BS.ByteString)
        -> BS.ByteString 
        -> Password
        -> Maybe UserID
        -> Update Users (Maybe User)
addUser (fstname, sndname) email passwd maybesupervisor = do
  users <- get
  if (IxSet.size (users @= Email email) /= 0)
   then return Nothing  -- "user with same email address exists"
   else do         
        userid <- getUnique users UserID
        let user = (User {  
                   userid                  =  userid
                 , userpassword            =  passwd
                 , usersupervisor          =  fmap (SupervisorID . unUserID) maybesupervisor
                 , usercanhavesubaccounts  =  True 
                 , useraccountsuspended    =  False  
                 , userhasacceptedtermsofservice = Nothing
                 , userinfo = UserInfo {
                                    userfstname = fstname
                                  , usersndname = sndname
                                  , userpersonalnumber = BS.empty
                                  , usercompanyname =  BS.empty
                                  , usercompanyposition =  BS.empty
                                  , usercompanynumber  =  BS.empty
                                  , useraddress =  BS.empty
                                  , userzip = BS.empty
                                  , usercity  = BS.empty
                                  , usercountry = BS.empty
                                  , userphone = BS.empty
                                  , usermobile = BS.empty
                                  , useremail =  Email email 
                                   }
                , usersettings  = UserSettings {
                                    accounttype = MainAccount 
                                  , accountplan = Basic
                                  , signeddocstorage = Nothing
                                  , userpaymentmethod = Undefined
                                  }                   
                , userpaymentpolicy =  Payments.basicPaymentPolicy
                , userpaymentaccount = Payments.emptyPaymentAccount 
              , userfriends = []
              , userinviteinfo = Nothing
              , userlogininfo = LoginInfo
                                { lastsuccesstime = Nothing
                                , lastfailtime = Nothing
                                , consecutivefails = 0
                                }
                 })             
        modify (updateIx (Email email) user)
        return $ Just user

getUserStats :: Query Users UserStats
getUserStats = do
  users <- ask
  return UserStats 
         { usercount = (size users)
         , viralinvitecount = length $ filterByInvite (isInviteType Viral) (toList users)
         , admininvitecount = length $ filterByInvite (isInviteType Admin) (toList users)
         }

getUserStatsByUser :: User -> Query Users UserStats
getUserStatsByUser user = do
  users <- ask
  let invitedusers = filterByInvite isInvitedByUser (toList users)
      isInvitedByUser :: InviteInfo -> Bool
      isInvitedByUser InviteInfo{userinviter} | (unInviter userinviter) == (unUserID . userid $ user) = True
      isInvitedByUser _ = False
  return UserStats 
         { usercount = 1 --sort of silly, but true
         , viralinvitecount = length $ filterByInvite (isInviteType Viral) invitedusers
         , admininvitecount = length $ filterByInvite (isInviteType Admin) invitedusers
         }

filterByInvite :: (InviteInfo -> Bool) -> [User] -> [User]
filterByInvite f users = filter ((maybe False f) . userinviteinfo) users

isInviteType :: InviteType -> InviteInfo -> Bool
isInviteType desiredtype InviteInfo{invitetype} | (isJust invitetype) && ((fromJust invitetype) == desiredtype) = True
isInviteType _ _ = False

getAllUsers :: Query Users [User]
getAllUsers = do
  users <- ask
  let usersSorted = sortBy compareuserfullname (toList users)
      compareuserfullname a b = compare (userfullname a) (userfullname b)
  return usersSorted

setUserPassword :: User -> Password -> Update Users ()
setUserPassword user@User{userid} newpassword = do
  modify (updateIx userid (user { userpassword = newpassword })) 
  return ()
  
verifyPassword :: Password -> BS.ByteString -> Bool
verifyPassword (Password salt hash1) password = hash1 == (hashPassword password salt)
verifyPassword _ _ = False        
                               

setInviteInfo :: Maybe User -> MinutesTime -> InviteType -> UserID -> Update Users ()
setInviteInfo minviter invitetime' invitetype' uid = do
    let mkInviteInfo user = InviteInfo
                            { userinviter = Inviter . unUserID . userid $ user
                            , invitetime = Just invitetime'
                            , invitetype = Just invitetype'
                            }
    _ <- modifyUser uid $ \user -> Right $ user {userinviteinfo = fmap mkInviteInfo minviter}
    return ()
        

setUserInfo :: UserID -> UserInfo -> Update Users (Either String User)
setUserInfo userid userinfo =
    modifyUser userid $ \user -> 
            Right $ user { userinfo = userinfo }                            

setUserSettings :: UserID -> UserSettings -> Update Users (Either String User)
setUserSettings userid usersettings =
    modifyUser userid $ \user -> 
            Right $ user { usersettings = usersettings }   


setUserPaymentAccount :: UserID -> Payments.UserPaymentAccount -> Update Users (Either String User)
setUserPaymentAccount userid userpaymentaccount =
    modifyUser userid $ \user -> 
            Right $ user {userpaymentaccount = userpaymentaccount}   


setUserPaymentPolicyChange :: UserID -> Payments.UserPaymentPolicy -> Update Users (Either String User)
setUserPaymentPolicyChange userid userpaymentpolicy =
    modifyUser userid $ \user -> 
            Right $ user {userpaymentpolicy = userpaymentpolicy}   
            
freeUserFromPayments :: User -> MinutesTime -> Update Users ()
freeUserFromPayments u freetill =  do
                                    _ <- modifyUser (userid u) $ \user -> 
                                      Right $ user {userpaymentpolicy = Payments.freeTill freetill (userpaymentpolicy user) }   
                                    return ()

{- |
    Records the details of a failed login.
-}
recordFailedLogin :: UserID -> MinutesTime -> Update Users (Either String User)
recordFailedLogin userid time = do
  modifyUser userid $ \user ->
                        Right $ user { userlogininfo = modifyLoginInfo $ userlogininfo user }
  where modifyLoginInfo logininfo =
            logininfo
            { lastfailtime = Just time
            , consecutivefails = (consecutivefails logininfo) + 1
            }   

{- |
    Records the details of a successful login.
-}
recordSuccessfulLogin :: UserID -> MinutesTime -> Update Users (Either String User)
recordSuccessfulLogin userid time = do
  modifyUser userid $ \user ->
                        Right $ user { userlogininfo = modifyLoginInfo $ userlogininfo user }
  where modifyLoginInfo logininfo =
            logininfo
            { lastsuccesstime = Just time
            , consecutivefails = 0
            }   

{- |
   Add a new viewer (friend) given the email address
 -}
addViewerByEmail :: UserID -> Email -> Update Users (Either String User)
addViewerByEmail uid vieweremail = do
  mms <- do users <- ask
            return $ getOne (users @= vieweremail)
  case mms of
    Just ms -> modifyUser uid $ \user ->
                                      Right $ user { userfriends = (Friend (unUserID $ userid ms) : (userfriends user)) }
    Nothing -> return $ Left $ "Användaren existerar ej: " ++ (BS.toString $ unEmail vieweremail)

acceptTermsOfService :: UserID -> MinutesTime -> Update Users (Either String User)
acceptTermsOfService userid minutestime = 
    modifyUser userid $ \user -> 
        Right $ user { userhasacceptedtermsofservice = Just minutestime }

addFreePaymentsForInviter ::MinutesTime -> User -> Update Users ()
addFreePaymentsForInviter now u = do
                           case (fmap userinviter $ userinviteinfo u) of
                            Nothing -> return ()   
                            Just (Inviter iid) -> do
                              users <- ask
                              let minviter = getOne (users @= (UserID iid))    
                              case minviter of
                                Nothing -> return ()   
                                Just inviter ->  do 
                                                 _<- modifyUser (userid inviter) $ \user -> 
                                                  Right $ user {userpaymentpolicy = Payments.extendFreeTmpChange now 7 (userpaymentpolicy user)}
                                                 return ()
                           
exportUsersDetailsToCSV :: Query Users BS.ByteString
exportUsersDetailsToCSV = do
  users <- ask
  let fields user = [userfullname user, unEmail $ useremail $ userinfo user]
      content = BS.intercalate (BS.fromString ",") <$> fields
  return $ BS.unlines $ content <$> (toList users)

instance Component Users where
  type Dependencies Users = End
  initialValue = IxSet.empty
  
-- create types for event serialization
$(mkMethods ''Users [ 'getUserByUserID
                    , 'getUserByEmail
                    , 'addUser
                    , 'getUserStats
                    , 'getUserStatsByUser
                    , 'getAllUsers
                    , 'setUserPassword
                    , 'setInviteInfo
                    , 'setUserInfo
                    , 'setUserSettings
                    , 'setUserPaymentAccount 
                    , 'setUserPaymentPolicyChange
                    , 'freeUserFromPayments
                    , 'recordFailedLogin
                    , 'recordSuccessfulLogin
                    , 'getUserSubaccounts
                    , 'getUsersByFriendUserID
                    , 'getUserFriends
                    , 'acceptTermsOfService
                    , 'exportUsersDetailsToCSV
                    , 'addViewerByEmail
                      -- the below should be only used carefully and by admins
                    , 'addFreePaymentsForInviter
                    ])

getUserPaymentSchema::User -> IO (Payments.PaymentScheme)
getUserPaymentSchema User{userpaymentpolicy } = do
                               now <- getMinutesTime
                               model <- update $ Payments.GetPaymentModel (Payments.paymentaccounttype userpaymentpolicy ) 
                               let paymentChange = case Payments.temppaymentchange userpaymentpolicy  of 
                                                     Nothing -> Payments.custompaymentchange  userpaymentpolicy 
                                                     Just (expires,tchange) -> 
                                                        if (now < expires)    
                                                        then Payments.custompaymentchange userpaymentpolicy 
                                                        else Payments.mergeChanges tchange (Payments.custompaymentchange userpaymentpolicy)
                               return $ (paymentChange,model)                                                                  

takeImmediatelyPayment::User -> Bool
takeImmediatelyPayment user = Payments.requiresImmediatelyPayment $ userpaymentpolicy user
