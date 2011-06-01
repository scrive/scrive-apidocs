{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
module User.UserState 
    ( Email(..)
    , Friend(..)
    , Inviter(..)
    , InviteType(..)
    , InviteInfo(..)
    , LoginInfo(..)
    , DefaultMainSignatory(..)
    , SignupMethod(..)
    , ExternalUserID(..)
    , Password(..)
    , TrustWeaverStorage(..)
    , UserAccountType(..)
    , PaymentMethod(..)
    , UserAccountPlan(..)
    , SupervisorID(..)
    , User(..)
    , UserInfo(..)
    , UserSettings(..)
    , DesignMode(..)
    , UserRecordStatus(..)
    , UserID(..)
    , Users
    , UserStats(..)
    , composeFullName
    , userfullname
    , isAbleToHaveSubaccounts
    , AcceptTermsOfService(..)
    , SetFreeTrialExpirationDate(..)
    , SetSignupMethod(..)
    , AddUser(..)
    , DeleteUser(..)
    , ExportUsersDetailsToCSV(..)
    , GetAllUsers(..)
    , GetUserByEmail(..)
    , GetUserByUserID(..)
    , GetUserStats(..)
    , GetUserStatsByUser(..)
    , GetUserSubaccounts(..)
    , GetUserRelatedAccounts(..)
    , GetUserFriends(..)
    , SetUserInfo(..)
    , SetInviteInfo(..)
    , SetUserSettings(..)
    , SetPreferredDesignMode(..)
    , SetUserPaymentAccount(..)
    , SetUserPaymentPolicyChange(..)
    , SetUserPassword(..)
    , SetUserSupervisor(..)
    , GetUsersByFriendUserID(..)
    , AddViewerByEmail(..)
    --, FreeUserFromPayments(..)
    --, AddFreePaymentsForInviter(..)
    , RecordFailedLogin(..)
    , RecordSuccessfulLogin(..)
    , getUserPaymentSchema
    , takeImmediatelyPayment
) where
import API.Service.ServiceState 
import Company.CompanyState
import Control.Applicative
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, MonadState(..))
import Data.Data
import Data.List
import Data.Maybe (isJust, fromJust, isNothing, fromMaybe)
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Happstack.Util.Common
import MinutesTime as MT
import Misc
import Payments.PaymentsState as Payments
import User.Password
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (unlines) 
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set

newtype UserID = UserID { unUserID :: Int }
    deriving (Eq, Ord, Typeable)

deriving instance Data UserID

data SignupMethod = AccountRequest | ViralInvitation | BySigning
    deriving (Eq, Ord, Show, Typeable)

newtype ExternalUserID = ExternalUserID { unExternalUserID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
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
newtype Email = Email { unEmail :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
newtype SupervisorID = SupervisorID { unSupervisorID :: Int }
    deriving (Eq, Ord, Typeable)
data TrustWeaverStorage = TrustWeaverStorage
          { storagetwenabled       :: Bool
          , storagetwname          :: BS.ByteString
          , storagetwsuperadmin    :: BS.ByteString
          , storagetwsuperadminpwd :: BS.ByteString
          , storagetwsectionpath   :: BS.ByteString
          }
    deriving (Eq, Ord, Typeable)

data UserAccountType0 = MainAccount | SubAccount
    deriving (Eq, Ord, Typeable)

data UserAccountType = PrivateAccount | CompanyAccount
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

data UserSettings0  = UserSettings0 {
               accounttype0 :: UserAccountType
             , accountplan0 :: UserAccountPlan
             , signeddocstorage0 :: Maybe TrustWeaverStorage
             , userpaymentmethod0 :: PaymentMethod
      }
    deriving (Eq, Ord, Typeable)

data UserSettings  = UserSettings {
               accounttype :: UserAccountType
             , accountplan :: UserAccountPlan
             , signeddocstorage :: Maybe TrustWeaverStorage
             , userpaymentmethod :: PaymentMethod
             , preferreddesignmode :: Maybe DesignMode
      }
    deriving (Eq, Ord, Typeable)

data DesignMode = BasicMode | AdvancedMode
    deriving (Eq, Ord, Typeable)

data UserRecordStatus = LiveUser | DeletedUser
    deriving (Eq, Ord, Typeable)

data User = User
          { userid                        :: !UserID
          , userpassword                  :: !Password
          , usersupervisor                :: !(Maybe SupervisorID)
          , useraccountsuspended          :: !Bool
          , userhasacceptedtermsofservice :: !(Maybe MinutesTime)
          , userfreetrialexpirationdate   :: !(Maybe MinutesTime)
          , usersignupmethod              :: !SignupMethod
          , userinfo                      :: !UserInfo
          , usersettings                  :: !UserSettings
          , userpaymentpolicy             :: !Payments.UserPaymentPolicy
          , userpaymentaccount            :: !Payments.UserPaymentAccount
          , userfriends                   :: ![Friend]
          , userinviteinfo                :: !(Maybe InviteInfo)
          , userlogininfo                 :: !LoginInfo
          , userservice                   :: !(Maybe ServiceID)
          , usercompany                   :: !(Maybe CompanyID)
          , userapikey                    :: !(Maybe MagicHash)
          , userrecordstatus              :: !UserRecordStatus
          }
            deriving (Eq, Ord)

instance Typeable User where typeOf _ = mkTypeOf "User"


data User15 = User15
          { userid15                        :: !UserID
          , userpassword15                  :: !Password
          , usersupervisor15                :: !(Maybe SupervisorID)
          , useraccountsuspended15          :: !Bool
          , userhasacceptedtermsofservice15 :: !(Maybe MinutesTime)
          , userfreetrialexpirationdate15   :: !(Maybe MinutesTime)
          , usersignupmethod15              :: !SignupMethod
          , userinfo15                      :: !UserInfo
          , usersettings15                  :: !UserSettings
          , userpaymentpolicy15             :: !Payments.UserPaymentPolicy
          , userpaymentaccount15            :: !Payments.UserPaymentAccount
          , userfriends15                   :: ![Friend]
          , userinviteinfo15                :: !(Maybe InviteInfo)
          , userlogininfo15                 :: !LoginInfo
          , userservice15                   :: !(Maybe ServiceID)
          , usercompany15                   :: !(Maybe CompanyID)
          , userapikey15                    :: !(Maybe MagicHash)
          }
            deriving (Eq, Ord, Typeable)

data User14 = User14
          { userid14                        :: UserID
          , userpassword14                  :: Password
          , usersupervisor14                :: Maybe SupervisorID
          , useraccountsuspended14          :: Bool
          , userhasacceptedtermsofservice14 :: Maybe MinutesTime
          , userfreetrialexpirationdate14   :: Maybe MinutesTime
          , usersignupmethod14              :: SignupMethod
          , userinfo14                      :: UserInfo
          , usersettings14                  :: UserSettings
          , userpaymentpolicy14             :: Payments.UserPaymentPolicy
          , userpaymentaccount14            :: Payments.UserPaymentAccount
          , userfriends14                   :: [Friend]
          , userinviteinfo14                :: Maybe InviteInfo
          , userlogininfo14                 :: LoginInfo
          , userservice14                   :: Maybe ServiceID
          , usercompany14                   :: Maybe CompanyID
          }
            deriving (Eq, Ord, Typeable)


data User13 = User13
          { userid13                        :: UserID
          , userpassword13                  :: Password
          , usersupervisor13                :: Maybe SupervisorID
          , useraccountsuspended13          :: Bool
          , userhasacceptedtermsofservice13 :: Maybe MinutesTime
          , userfreetrialexpirationdate13   :: Maybe MinutesTime
          , usersignupmethod13              :: SignupMethod
          , userinfo13                      :: UserInfo
          , usersettings13                  :: UserSettings
          , userpaymentpolicy13             :: Payments.UserPaymentPolicy
          , userpaymentaccount13            :: Payments.UserPaymentAccount
          , userfriends13                   :: [Friend]
          , userinviteinfo13                :: Maybe InviteInfo
          , userlogininfo13                 :: LoginInfo
          , userservice13                   :: Maybe ServiceID
          , userterminated13                :: Bool
          }
            deriving (Eq, Ord, Typeable)

data User12 = User12
          { userid12                        :: UserID
          , userpassword12                  :: Password
          , usersupervisor12                :: Maybe SupervisorID
          , useraccountsuspended12          :: Bool
          , userhasacceptedtermsofservice12 :: Maybe MinutesTime
          , userinfo12                      :: UserInfo
          , usersettings12                  :: UserSettings
          , userpaymentpolicy12             :: Payments.UserPaymentPolicy
          , userpaymentaccount12            :: Payments.UserPaymentAccount
          , userfriends12                   :: [Friend]
          , userinviteinfo12                :: Maybe InviteInfo
          , userlogininfo12                 :: LoginInfo
          }
            deriving (Eq, Ord, Typeable)

data User11 = User11
          { userid11                        :: UserID
          , userpassword11                  :: Password
          , usersupervisor11                :: Maybe SupervisorID
          , usercanhavesubaccounts11        :: Bool
          , useraccountsuspended11          :: Bool
          , userhasacceptedtermsofservice11 :: Maybe MinutesTime
          , userinfo11                      :: UserInfo
          , usersettings11                  :: UserSettings
          , userpaymentpolicy11             :: Payments.UserPaymentPolicy
          , userpaymentaccount11            :: Payments.UserPaymentAccount
          , userfriends11                   :: [Friend]
          , userinviteinfo11                :: Maybe InviteInfo
          , userlogininfo11                 :: LoginInfo
          }
    deriving (Eq, Ord, Typeable)

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


data UserStats = UserStats 
                       { usercount :: Int
                       , viralinvitecount :: Int
                       , admininvitecount :: Int
                       }
    deriving (Eq, Ord, Typeable)

deriving instance Data UserStats

deriving instance Show TrustWeaverStorage
deriving instance Show UserAccountType 
deriving instance Show PaymentMethod
deriving instance Show UserAccountPlan 
deriving instance Show UserInfo
deriving instance Show UserSettings
deriving instance Show DesignMode
deriving instance Show UserRecordStatus
deriving instance Show User
deriving instance Show Email
deriving instance Show Friend
deriving instance Show Inviter
deriving instance Show InviteInfo
deriving instance Show InviteType
deriving instance Show LoginInfo
deriving instance Show DefaultMainSignatory
deriving instance Show UserStats

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

instance Migrate UserAccountType0 UserAccountType where
    migrate _ = PrivateAccount

instance Migrate () User8 where
    migrate () = error "Cannot migrate to User8"

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

instance Migrate User10 User11 where
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
                }) = User11 
                { userid11                         = userid10
                , userpassword11                   = userpassword10
                , usersupervisor11                 = usersupervisor10
                , usercanhavesubaccounts11         = usercanhavesubaccounts10
                , useraccountsuspended11           = useraccountsuspended10
                , userhasacceptedtermsofservice11  = userhasacceptedtermsofservice10
                , userinfo11                       = userinfo10
                , usersettings11                   = usersettings10
                , userpaymentpolicy11              = userpaymentpolicy10
                , userpaymentaccount11             = userpaymentaccount10
                , userfriends11                    = userfriends10
                , userinviteinfo11                 = userinviteinfo10
                , userlogininfo11                 = LoginInfo
                                                    { lastsuccesstime = Nothing
                                                    , lastfailtime = Nothing
                                                    , consecutivefails = 0
                                                    }
                }

instance Migrate User11 User12 where
    migrate (User11
               { userid11                     
                , userpassword11                
                , usersupervisor11               
                , useraccountsuspended11          
                , userhasacceptedtermsofservice11  
                , userinfo11                     
                , usersettings11                
                , userpaymentpolicy11             
                , userpaymentaccount11           
                , userfriends11                  
                , userinviteinfo11
                , userlogininfo11       
                }) = User12 
                { userid12                         = userid11
                , userpassword12                   = userpassword11
                , usersupervisor12                 = usersupervisor11
                , useraccountsuspended12           = useraccountsuspended11
                , userhasacceptedtermsofservice12  = userhasacceptedtermsofservice11
                , userinfo12                       = userinfo11
                , usersettings12                   = usersettings11
                , userpaymentpolicy12              = userpaymentpolicy11
                , userpaymentaccount12             = userpaymentaccount11
                , userfriends12                    = userfriends11
                , userinviteinfo12                 = userinviteinfo11
                , userlogininfo12                  = userlogininfo11
                }

-- | This is kinda special. We reset payment changes (since the only changes there
-- are is the system are used to indicate whether a user has free trial or not) since
-- we want to treat free trial specially after it ends, so we need to distinguish
-- between "normal" payment change and free trial.
instance Migrate User12 User13 where
    migrate (User12
               { userid12                     
                , userpassword12                
                , usersupervisor12               
                , useraccountsuspended12          
                , userhasacceptedtermsofservice12  
                , userinfo12                     
                , usersettings12                
                , userpaymentpolicy12 = Payments.UserPaymentPolicy {temppaymentchange}
                , userfriends12                  
                , userinviteinfo12
                , userlogininfo12       
                }) = User13 
                { userid13                         = userid12
                , userpassword13                   = userpassword12
                , usersupervisor13                 = usersupervisor12
                , useraccountsuspended13           = useraccountsuspended12
                , userhasacceptedtermsofservice13  = userhasacceptedtermsofservice12
                , userfreetrialexpirationdate13    = Just freetrialexpirationdate
                , usersignupmethod13               = AccountRequest
                , userinfo13                       = userinfo12
                , usersettings13                   = usersettings12
                , userpaymentpolicy13              = Payments.initialPaymentPolicy
                , userpaymentaccount13             = Payments.emptyPaymentAccount {
                    paymentaccountfreesignatures = 100 -- for now we give them
                    -- a lot of free signatures because we don't handle the case
                    -- when they run out of them
                }
                , userfriends13                    = userfriends12
                , userinviteinfo13                 = userinviteinfo12
                , userlogininfo13                  = userlogininfo12
                , userservice13                    = Nothing
                , userterminated13                 = False
                }
                where
                    freetrialexpirationdate =
                        fromMaybe firstjuly (max firstjuly . fst <$> temppaymentchange)
                    firstjuly = fromJust $ parseMinutesTimeMDY "01-06-2011"

instance Migrate User13 User14 where
    migrate (User13
               {  userid13                   
                , userpassword13                
                , usersupervisor13               
                , useraccountsuspended13        
                , userhasacceptedtermsofservice13 
                , userfreetrialexpirationdate13   
                , usersignupmethod13         
                , userinfo13              
                , usersettings13       
                , userpaymentpolicy13      
                , userpaymentaccount13 
                , userfriends13       
                , userinviteinfo13     
                , userlogininfo13       
                , userservice13    
                }) = User14 
                { userid14                         = userid13
                , userpassword14                   = userpassword13
                , usersupervisor14                 = usersupervisor13
                , useraccountsuspended14           = useraccountsuspended13
                , userhasacceptedtermsofservice14  = userhasacceptedtermsofservice13
                , userfreetrialexpirationdate14    = userfreetrialexpirationdate13
                , usersignupmethod14               = usersignupmethod13
                , userinfo14                       = userinfo13
                , usersettings14                   = usersettings13
                , userpaymentpolicy14              = userpaymentpolicy13
                , userpaymentaccount14             = userpaymentaccount13
                , userfriends14                    = userfriends13
                , userinviteinfo14                 = userinviteinfo13
                , userlogininfo14                  = userlogininfo13
                , userservice14                    = userservice13
                , usercompany14                    = Nothing
                }

instance Migrate User14 User15 where
  migrate (User14 
                { userid14
                , userpassword14
                , usersupervisor14
                , useraccountsuspended14
                , userhasacceptedtermsofservice14
                , userfreetrialexpirationdate14
                , usersignupmethod14
                , userinfo14
                , usersettings14
                , userpaymentpolicy14
                , userpaymentaccount14
                , userfriends14
                , userinviteinfo14
                , userlogininfo14
                , userservice14
                , usercompany14
                }) = User15
                { userid15                         = userid14
                , userpassword15                   = userpassword14
                , usersupervisor15                 = usersupervisor14
                , useraccountsuspended15           = useraccountsuspended14
                , userhasacceptedtermsofservice15  = userhasacceptedtermsofservice14
                , userfreetrialexpirationdate15    = userfreetrialexpirationdate14
                , usersignupmethod15               = usersignupmethod14
                , userinfo15                       = userinfo14
                , usersettings15                   = usersettings14
                , userpaymentpolicy15              = userpaymentpolicy14
                , userpaymentaccount15             = userpaymentaccount14
                , userfriends15                    = userfriends14
                , userinviteinfo15                 = userinviteinfo14
                , userlogininfo15                  = userlogininfo14
                , userservice15                    = userservice14
                , usercompany15                    = usercompany14
                , userapikey15                     = Nothing
                }

instance Migrate User15 User where
  migrate (User15
                { userid15
                , userpassword15
                , usersupervisor15
                , useraccountsuspended15
                , userhasacceptedtermsofservice15
                , userfreetrialexpirationdate15
                , usersignupmethod15
                , userinfo15
                , usersettings15
                , userpaymentpolicy15
                , userpaymentaccount15
                , userfriends15
                , userinviteinfo15
                , userlogininfo15
                , userservice15
                , usercompany15
                , userapikey15
                }) = User
                { userid                         = userid15
                , userpassword                   = userpassword15
                , usersupervisor                 = usersupervisor15
                , useraccountsuspended           = useraccountsuspended15
                , userhasacceptedtermsofservice  = userhasacceptedtermsofservice15
                , userfreetrialexpirationdate    = userfreetrialexpirationdate15
                , usersignupmethod               = usersignupmethod15
                , userinfo                       = userinfo15
                , usersettings                   = usersettings15
                , userpaymentpolicy              = userpaymentpolicy15
                , userpaymentaccount             = userpaymentaccount15
                , userfriends                    = userfriends15
                , userinviteinfo                 = userinviteinfo15
                , userlogininfo                  = userlogininfo15
                , userservice                    = userservice15
                , usercompany                    = usercompany15
                , userapikey                     = userapikey15
                , userrecordstatus               = LiveUser
                }

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

instance Migrate UserSettings0 UserSettings where
    migrate (UserSettings0 {
            accounttype0
          , accountplan0
          , signeddocstorage0
          , userpaymentmethod0
          }) = UserSettings {
            accounttype = accounttype0
          , accountplan = accountplan0
          , signeddocstorage = signeddocstorage0
          , userpaymentmethod = userpaymentmethod0
          , preferreddesignmode = Nothing
          }

isAbleToHaveSubaccounts :: User -> Bool
isAbleToHaveSubaccounts user = isNothing $ usersupervisor user

type Users = IxSet User

instance Indexable User where
        empty = ixSet [ ixFun (\x -> [userid x] :: [UserID])
                      , ixFun (\x -> [useremail $ userinfo x] :: [Email])
                      , ixFun (\x -> maybe [] return (usersupervisor x) :: [SupervisorID])
                      , ixFun userfriends
                      , ixFun (\x -> [userservice x] :: [Maybe ServiceID])
                      , ixFun (\x -> [usercompany x] :: [Maybe CompanyID])
                      , ixFun (\x -> [userrecordstatus x] :: [UserRecordStatus])
                      ]


instance Version User8 where
    mode = extension 8 (Proxy :: Proxy ()) 

instance Version User9 where
    mode = extension 9 (Proxy :: Proxy User8)

instance Version User10 where
    mode = extension 10 (Proxy :: Proxy User9)

instance Version User11 where
    mode = extension 11 (Proxy :: Proxy User10)

instance Version User12 where
    mode = extension 12 (Proxy :: Proxy User11)

instance Version User13 where
    mode = extension 13 (Proxy :: Proxy User12)

instance Version User14 where
    mode = extension 14 (Proxy :: Proxy User13)

instance Version User15 where
    mode = extension 15 (Proxy :: Proxy User14)

instance Version User where
    mode = extension 16 (Proxy :: Proxy User15)
    
instance Version SignupMethod

instance Version TrustWeaverStorage

instance Version UserAccountType0

instance Version UserAccountType where
    mode = extension 2 (Proxy :: Proxy UserAccountType0)

instance Version PaymentMethod

instance Version UserAccountPlan 

instance Version UserInfo0

instance Version UserInfo where
    mode = extension 1 (Proxy :: Proxy UserInfo0)

instance Version UserSettings0

instance Version UserSettings where
    mode = extension 1 (Proxy :: Proxy UserSettings0)

instance Version DesignMode

instance Version UserRecordStatus

instance Version Email

instance Version UserID

instance Version Friend

instance Version Inviter

instance Version InviteInfo

instance Version InviteType

instance Version LoginInfo

instance Version DefaultMainSignatory

instance Version SupervisorID

instance Version ExternalUserID

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
  users <- askLive
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

{- |
    Cleans out all the deleted users
-}
queryUsers :: (Users -> a) -> Query Users a
queryUsers queryFunc = do
  users <- ask
  let liveusers = users @= LiveUser
  return $ queryFunc liveusers

askLive :: Update Users Users
askLive = do
  users <- ask
  return $ users @= LiveUser

getUserByEmail :: Maybe ServiceID  -> Email ->  Query Users (Maybe User)
getUserByEmail service email = queryUsers $ \users ->
  getOne (users @= email @= service)
    
getUserByUserID :: UserID -> Query Users (Maybe User)
getUserByUserID userid = queryUsers $ \users ->
  getOne (users @= userid)

getUsersByFriendUserID :: UserID -> Query Users [User]
getUsersByFriendUserID uid = queryUsers $ \users ->
  toList $ users @= (Friend $ unUserID uid)

getUserFriends :: UserID -> Query Users [User]
getUserFriends uid = do
  muser <- getUserByUserID uid
  case muser of
    Nothing -> return []
    Just user -> do
      mfriends <- sequence . map (getUserByUserID . UserID . unFriend) $ userfriends user
      return . map fromJust . filter isJust $ mfriends

getUserSubaccounts :: UserID -> Query Users [User]
getUserSubaccounts userid = do
  users <- ask
  return $ toSet (users @= SupervisorID (unUserID userid))

{- |
    Gets all the users that are related to the indicated user.
    They are related if they have the same supervisor,
    or are a supervisor, or are a subaccount (so if a parent, child or sibling).
-}
getUserRelatedAccounts :: UserID -> Query Users [User]
getUserRelatedAccounts userid = do
  muser <- getUserByUserID userid
  case muser of
    Nothing -> return []
    Just User{usersupervisor} -> queryUsers $ \users ->
      let subaccounts = users @= SupervisorID (unUserID userid)
          superaccounts = maybe IxSet.empty (\SupervisorID{unSupervisorID} -> users @= UserID unSupervisorID) usersupervisor
          siblingaccounts = maybe IxSet.empty (\supervisor -> users @= supervisor) usersupervisor in
      toList $ subaccounts ||| superaccounts ||| siblingaccounts

deleteUser :: UserID -> Update Users ()
deleteUser uid = do
  users <- askLive
  let muser = getOne (users @= uid)
  case muser of
    Nothing -> return ()
    Just _ -> do
      let deleteduser = blankUser { userid = uid,
                                    userrecordstatus = DeletedUser }
      modify (updateIx uid deleteduser)
      return ()

addUser :: (BS.ByteString, BS.ByteString)
        -> BS.ByteString 
        -> Password
        -> Maybe UserID
        -> Maybe ServiceID
        -> Maybe CompanyID
        -> Update Users (Maybe User)
addUser (fstname, sndname) email passwd maybesupervisor mservice mcompany = do
  allusers <- get
  liveusers <- askLive
  if (IxSet.size (liveusers @= mservice @= Email email) /= 0) -- a deleted user can re-register as a new user
   then return Nothing  -- "user with same email address exists"
   else do         
        userid <- getUnique allusers UserID --want userid to be unique even against deleted users
        let user = blankUser {  
                   userid                  =  userid
                 , userpassword            =  passwd
                 , usersupervisor          =  fmap (SupervisorID . unUserID) maybesupervisor
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
              , userservice = mservice
              , usercompany = mcompany
              }
        modify (updateIx userid user)
        return $ Just user

blankUser :: User
blankUser = User {  
                   userid                  =  UserID 0
                 , userpassword            =  NoPassword
                 , usersupervisor          =  Nothing 
                 , useraccountsuspended    =  False  
                 , userhasacceptedtermsofservice = Nothing
                 , userfreetrialexpirationdate = Nothing
                 , usersignupmethod = AccountRequest
                 , userinfo = UserInfo {
                                    userfstname = BS.empty
                                  , usersndname = BS.empty
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
                                  , useremail =  Email BS.empty 
                                   }
                , usersettings  = UserSettings {
                                    accounttype = PrivateAccount
                                  , accountplan = Basic
                                  , signeddocstorage = Nothing
                                  , userpaymentmethod = Undefined
                                  , preferreddesignmode = Nothing
                                  }                   
                , userpaymentpolicy = Payments.initialPaymentPolicy
                , userpaymentaccount = Payments.emptyPaymentAccount
              , userfriends = []
              , userinviteinfo = Nothing
              , userlogininfo = LoginInfo
                                { lastsuccesstime = Nothing
                                , lastfailtime = Nothing
                                , consecutivefails = 0
                                }
              , userservice = Nothing
              , usercompany = Nothing
              , userapikey = Nothing
              , userrecordstatus = LiveUser
              }


failure :: String -> Either String a
failure = Left

setUserSupervisor :: UserID -> UserID -> Update Users (Either String User)
setUserSupervisor userid supervisorid = do
    msupervisor <- (getOne . (@= supervisorid)) <$> askLive
    let supervisor = fromJust msupervisor
    modifyUser userid $ \user -> do -- Either String monad 
      let luseremail = BS.toString $ unEmail $ useremail $ userinfo user
          suseremail = BS.toString $ unEmail $ useremail $ userinfo supervisor
      when (userid == supervisorid) $ 
         failure "cannot be supervisor of yourself"
      when (isJust $ usersupervisor user) $
         failure "user already has a supervisor"
      when (isNothing $ msupervisor) $
         failure "supervisor id does not exist"
      when (dropWhile (/= '@') luseremail /= dropWhile (/= '@') suseremail) $
         failure $ "users domain names differ " ++ luseremail ++ " vs " ++ suseremail
      return $ user { usersupervisor = Just $ SupervisorID $ unUserID supervisorid}
  
getUserStats :: Query Users UserStats
getUserStats = queryUsers $ \users -> 
  let userList = toList users in
  UserStats 
         { usercount = length userList
         , viralinvitecount = length $ filterByInvite (isInviteType Viral) userList
         , admininvitecount = length $ filterByInvite (isInviteType Admin) userList
         }

getUserStatsByUser :: User -> Query Users UserStats
getUserStatsByUser user = queryUsers $ \users ->
  let invitedusers = filterByInvite isInvitedByUser (toList users)
      isInvitedByUser :: InviteInfo -> Bool
      isInvitedByUser InviteInfo{userinviter} | (unInviter userinviter) == (unUserID . userid $ user) = True
      isInvitedByUser _ = False in
  UserStats 
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
getAllUsers = queryUsers $ \users ->
  let usersSorted = sortBy compareuserfullname (toList users)
      compareuserfullname a b = compare (userfullname a) (userfullname b) in
  usersSorted

setUserPassword :: UserID -> Password -> Update Users (Either String User)
setUserPassword userid newpassword = do
    modifyUser userid $ \user ->
        Right $ user { userpassword = newpassword }

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

setPreferredDesignMode :: UserID -> Maybe DesignMode -> Update Users (Either String User)
setPreferredDesignMode userid designmode =
    modifyUser userid $ \user ->
            Right $ user { usersettings = (usersettings user){ preferreddesignmode = designmode } }


setUserPaymentAccount :: UserID -> Payments.UserPaymentAccount -> Update Users (Either String User)
setUserPaymentAccount userid userpaymentaccount =
    modifyUser userid $ \user -> 
            Right $ user {userpaymentaccount = userpaymentaccount}   


setUserPaymentPolicyChange :: UserID -> Payments.UserPaymentPolicy -> Update Users (Either String User)
setUserPaymentPolicyChange userid userpaymentpolicy =
    modifyUser userid $ \user -> 
            Right $ user {userpaymentpolicy = userpaymentpolicy}   
            
_freeUserFromPayments :: UserID -> MinutesTime -> Update Users ()
_freeUserFromPayments uid freetill =  do
                                    _ <- modifyUser uid $ \user -> 
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
  mms <- do users <- askLive
            return $ getOne (users @= vieweremail)
  case mms of
    Just ms -> modifyUser uid $ \user ->
                                      Right $ user { userfriends = (Friend (unUserID $ userid ms) : (userfriends user)) }
    Nothing -> return $ Left $ "Användaren existerar ej: " ++ (BS.toString $ unEmail vieweremail)

acceptTermsOfService :: UserID -> MinutesTime -> Update Users (Either String User)
acceptTermsOfService userid minutestime = 
    modifyUser userid $ \user -> 
        Right $ user {
              userhasacceptedtermsofservice = Just minutestime
            , userfreetrialexpirationdate  = Just $ (60*24*30) `minutesAfter` minutestime
        }

setFreeTrialExpirationDate :: UserID -> Maybe MinutesTime -> Update Users (Either String User)
setFreeTrialExpirationDate userid date = 
    modifyUser userid $ \user -> 
        Right $ user { userfreetrialexpirationdate = date }

setSignupMethod :: UserID -> SignupMethod -> Update Users (Either String User)
setSignupMethod userid signupmethod = 
    modifyUser userid $ \user -> 
        Right $ user { usersignupmethod = signupmethod }

_addFreePaymentsForInviter ::MinutesTime -> User -> Update Users ()
_addFreePaymentsForInviter now u = do
                           case (fmap userinviter $ userinviteinfo u) of
                            Nothing -> return ()   
                            Just (Inviter iid) -> do
                              users <- askLive
                              let minviter = getOne (users @= (UserID iid))    
                              case minviter of
                                Nothing -> return ()   
                                Just inviter ->  do 
                                                 _<- modifyUser (userid inviter) $ \user -> 
                                                  Right $ user {userpaymentpolicy = Payments.extendFreeTmpChange now 7 (userpaymentpolicy user)}
                                                 return ()
                           
exportUsersDetailsToCSV :: Query Users BS.ByteString
exportUsersDetailsToCSV = queryUsers $ \users ->
  let fields user = [userfullname user, unEmail $ useremail $ userinfo user]
      content = BS.intercalate (BS.fromString ",") <$> fields in
  BS.unlines $ content <$> (toList users)

  
getUserPaymentSchema::User -> IO (Payments.PaymentScheme)
getUserPaymentSchema User{userpaymentpolicy } = do
                               now <- getMinutesTime
                               model <- query $ Payments.GetPaymentModel (Payments.paymentaccounttype userpaymentpolicy ) 
                               let paymentChange = case Payments.temppaymentchange userpaymentpolicy  of 
                                                     Nothing -> Payments.custompaymentchange  userpaymentpolicy 
                                                     Just (expires,tchange) -> 
                                                        if (now < expires)    
                                                        then Payments.custompaymentchange userpaymentpolicy 
                                                        else Payments.mergeChanges tchange (Payments.custompaymentchange userpaymentpolicy)
                               return $ (paymentChange,model)                                                                  

takeImmediatelyPayment::User -> Bool
takeImmediatelyPayment user = Payments.requiresImmediatelyPayment $ userpaymentpolicy user

{- 

Template Haskell derivations should be kept at the end of the file

-}


-- create types for event serialization
$(mkMethods ''Users [ 'getUserByUserID
                    , 'getUserByEmail
                    , 'addUser
                    , 'deleteUser
                    , 'getUserStats
                    , 'getUserStatsByUser
                    , 'getAllUsers
                    , 'setUserPassword
                    , 'setInviteInfo
                    , 'setUserInfo
                    , 'setUserSettings
                    , 'setPreferredDesignMode
                    , 'setUserPaymentAccount
                    , 'setUserPaymentPolicyChange
                    --, 'freeUserFromPayments
                    , 'recordFailedLogin
                    , 'recordSuccessfulLogin
                    , 'getUserSubaccounts
                    , 'getUserRelatedAccounts
                    , 'getUsersByFriendUserID
                    , 'getUserFriends
                    , 'acceptTermsOfService
                    , 'setFreeTrialExpirationDate
                    , 'setSignupMethod
                    , 'exportUsersDetailsToCSV
                    , 'addViewerByEmail
                      -- the below should be only used carefully and by admins
                    --, 'addFreePaymentsForInviter
                    , 'setUserSupervisor
                    ])

$(deriveSerializeFor [ ''User
                     , ''User15
                     , ''User14
                     , ''User13
                     , ''User12
                     , ''User11
                     , ''User10
                     , ''User9
                     , ''User8

                     , ''SignupMethod
                     , ''TrustWeaverStorage
                     , ''UserAccountType
                     , ''UserAccountType0
                     , ''PaymentMethod
                     , ''UserInfo0
                     , ''UserStats
                     , ''Email
                     , ''InviteType
                     , ''LoginInfo
                     , ''InviteInfo
                     , ''Friend
                     , ''UserSettings
                     , ''UserSettings0
                     , ''DesignMode
                     , ''UserRecordStatus
                     , ''UserInfo
                     , ''SupervisorID
                     , ''UserID
                     , ''Inviter
                     , ''DefaultMainSignatory
                     , ''UserAccountPlan
                     ])

instance Component Users where
  type Dependencies Users = End
  initialValue = IxSet.empty
