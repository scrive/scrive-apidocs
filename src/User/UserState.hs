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
    , module User.OldPassword
    , TrustWeaverStorage(..)
    , UserAccountType(..)
    , PaymentMethod(..)
    , UserAccountPlan(..)
    , SupervisorID(..)
    , User(..)
    , UserInfo(..)
    , UserMailAPI(..)
    , UserSettings(..)
    , DesignMode(..)
    , UserID(..)
    , Users
    , UserStats(..)
    , composeFullName
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
    , GetUserFriends(..)
    , SetUserInfo(..)
    , SetUserMailAPI(..)
    , SetInviteInfo(..)
    , SetUserSettings(..)
    , SetPreferredDesignMode(..)
    , SetUserPaymentAccount(..)
    , SetUserPaymentPolicyChange(..)
    , SetUserPassword(..)
    , GetUsersByFriendUserID(..)
    , AddViewerByEmail(..)
    --, FreeUserFromPayments(..)
    --, AddFreePaymentsForInviter(..)
    , RecordFailedLogin(..)
    , RecordSuccessfulLogin(..)
    , GetCompanyAccounts(..)
    , SetUserCompany(..)
    , MakeUserACompanyAdmin(..)
    , getUserPaymentSchema
    , takeImmediatelyPayment
    , RequiresCompanyForMigration(..) --just for company migration
    , populateDBWithUsersIfEmpty
) where
import API.Service.ServiceState
import Company.CompanyState
import Control.Arrow (first)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Data
import Data.List
import Data.Maybe
import Database.HDBC
import DB.Classes
import DB.Derive
import DB.Types
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Happstack.Util.Common
import MinutesTime as MT
import Misc
import Payments.PaymentsState as Payments
import User.OldPassword
import User.OldLang
import User.OldSystemServer
import qualified AppLogger as Log
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS (unlines)
import qualified Data.ByteString.UTF8 as BS

newtype UserID = UserID { unUserID :: Int }
    deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''UserID)

deriving instance Data UserID

data SignupMethod = AccountRequest | ViralInvitation | BySigning
    deriving (Eq, Ord, Show, Typeable)
$(enumDeriveConvertible ''SignupMethod)

newtype ExternalUserID = ExternalUserID { unExternalUserID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
newtype Friend = Friend { unFriend :: Int }
    deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''Friend)
newtype Inviter = Inviter { unInviter :: Int }
    deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''Inviter)
data InviteType = Viral | Admin
    deriving (Eq, Ord, Typeable)
$(enumDeriveConvertible ''InviteType)
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
$(newtypeDeriveConvertible ''Email)
instance Read Email where
    readsPrec p s = first (Email . BS.fromString) <$> readsPrec p s
instance Show Email where
    show = show . unEmail


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

--stop using this, you can tell the account by checking usercompany on the user
data UserAccountType = PrivateAccount | CompanyAccount
    deriving (Eq, Ord, Typeable)

data PaymentMethod = CreditCard | Invoice | Undefined
    deriving (Eq, Ord, Typeable)
$(enumDeriveConvertible ''PaymentMethod)

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
          , usercompanyname               :: BS.ByteString --Don't use this anymore, use the one on company
          , usercompanyposition           :: BS.ByteString
          , usercompanynumber             :: BS.ByteString --Don't use this anymore, use the one on company
          , useraddress                   :: BS.ByteString --Don't use this anymore, use the one on company
          , userzip                       :: BS.ByteString --Don't use this anymore, use the one on company
          , usercity                      :: BS.ByteString --Don't use this anymore, use the one on company
          , usercountry                   :: BS.ByteString --Don't use this anymore, use the one on company
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

data UserSettings1  = UserSettings1 {
               accounttype1 :: UserAccountType
             , accountplan1 :: UserAccountPlan
             , signeddocstorage1 :: Maybe TrustWeaverStorage
             , userpaymentmethod1 :: PaymentMethod
             , preferreddesignmode1 :: Maybe DesignMode
      }
    deriving (Eq, Ord, Typeable)

data UserSettings2  = UserSettings2 {
               accounttype2 :: UserAccountType
             , accountplan2 :: UserAccountPlan
             , signeddocstorage2 :: Maybe TrustWeaverStorage
             , userpaymentmethod2 :: PaymentMethod
             , preferreddesignmode2 :: Maybe DesignMode
             , lang2 :: Lang
      }
    deriving (Eq, Ord, Typeable)

data UserSettings  = UserSettings {
               accounttype :: UserAccountType
             , accountplan :: UserAccountPlan
             , signeddocstorage :: Maybe TrustWeaverStorage
             , userpaymentmethod :: PaymentMethod
             , preferreddesignmode :: Maybe DesignMode
             , lang :: Lang
             , systemserver :: SystemServer
      }
    deriving (Eq, Ord, Typeable)
    
data DesignMode = BasicMode | AdvancedMode
    deriving (Eq, Ord, Typeable)
$(enumDeriveConvertible ''DesignMode)

{- |
    Deprecated, replaced with a simple flag called userdeleted, because
    it seemed like overkill
-}
data UserRecordStatus = LiveUser | DeletedUser
    deriving (Eq, Ord, Typeable)

data UserMailAPI = UserMailAPI {
      umapiKey          :: MagicHash
    , umapiDailyLimit   :: Int
    , umapiSentToday    :: Int
    , umapiLastSentDate :: Int
    } deriving (Eq, Ord, Show)

instance Typeable UserMailAPI where
    typeOf _ = mkTypeOf "UserMailAPI"

data User = User
          { userid                        :: !UserID
          , userpassword                  :: !Password
          , usersupervisor                :: !(Maybe SupervisorID) --don't use this anymore, it's been deprecated
          , useriscompanyadmin            :: !Bool
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
          , usermailapi                   :: !(Maybe UserMailAPI)
          , userdeleted                   :: !Bool
          }
            deriving (Eq, Ord)

instance Typeable User where typeOf _ = mkTypeOf "User"

data User17 = User17
          { userid17                        :: !UserID
          , userpassword17                  :: !Password
          , usersupervisor17                :: !(Maybe SupervisorID)
          , useraccountsuspended17          :: !Bool
          , userhasacceptedtermsofservice17 :: !(Maybe MinutesTime)
          , userfreetrialexpirationdate17   :: !(Maybe MinutesTime)
          , usersignupmethod17              :: !SignupMethod
          , userinfo17                      :: !UserInfo
          , usersettings17                  :: !UserSettings
          , userpaymentpolicy17             :: !Payments.UserPaymentPolicy
          , userpaymentaccount17            :: !Payments.UserPaymentAccount
          , userfriends17                   :: ![Friend]
          , userinviteinfo17                :: !(Maybe InviteInfo)
          , userlogininfo17                 :: !LoginInfo
          , userservice17                   :: !(Maybe ServiceID)
          , usercompany17                   :: !(Maybe CompanyID)
          , usermailapi17                   :: !(Maybe UserMailAPI)
          , userrecordstatus17              :: !UserRecordStatus
          } deriving (Eq, Ord, Typeable)

data User16 = User16
          { userid16                        :: !UserID
          , userpassword16                  :: !Password
          , usersupervisor16                :: !(Maybe SupervisorID)
          , useraccountsuspended16          :: !Bool
          , userhasacceptedtermsofservice16 :: !(Maybe MinutesTime)
          , userfreetrialexpirationdate16   :: !(Maybe MinutesTime)
          , usersignupmethod16              :: !SignupMethod
          , userinfo16                      :: !UserInfo
          , usersettings16                  :: !UserSettings
          , userpaymentpolicy16             :: !Payments.UserPaymentPolicy
          , userpaymentaccount16            :: !Payments.UserPaymentAccount
          , userfriends16                   :: ![Friend]
          , userinviteinfo16                :: !(Maybe InviteInfo)
          , userlogininfo16                 :: !LoginInfo
          , userservice16                   :: !(Maybe ServiceID)
          , usercompany16                   :: !(Maybe CompanyID)
          , userapikey16                    :: !(Maybe MagicHash)
          , userrecordstatus16              :: !UserRecordStatus
          }
            deriving (Eq, Ord, Typeable)

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
                    firstjuly = fromJust $ parseMinutesTimeDMY "01-06-2011"

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

instance Migrate User15 User16 where
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
                }) = User16
                { userid16                         = userid15
                , userpassword16                   = userpassword15
                , usersupervisor16                 = usersupervisor15
                , useraccountsuspended16           = useraccountsuspended15
                , userhasacceptedtermsofservice16  = userhasacceptedtermsofservice15
                , userfreetrialexpirationdate16    = userfreetrialexpirationdate15
                , usersignupmethod16               = usersignupmethod15
                , userinfo16                       = userinfo15
                , usersettings16                   = usersettings15
                , userpaymentpolicy16              = userpaymentpolicy15
                , userpaymentaccount16             = userpaymentaccount15
                , userfriends16                    = userfriends15
                , userinviteinfo16                 = userinviteinfo15
                , userlogininfo16                  = userlogininfo15
                , userservice16                    = userservice15
                , usercompany16                    = usercompany15
                , userapikey16                     = userapikey15
                , userrecordstatus16               = LiveUser
                }

instance Migrate User16 User17 where
  migrate (User16
                { userid16
                , userpassword16
                , usersupervisor16
                , useraccountsuspended16
                , userhasacceptedtermsofservice16
                , userfreetrialexpirationdate16
                , usersignupmethod16
                , userinfo16
                , usersettings16
                , userpaymentpolicy16
                , userpaymentaccount16
                , userfriends16
                , userinviteinfo16
                , userlogininfo16
                , userservice16
                , usercompany16
                , userrecordstatus16
                }) = User17
                { userid17                         = userid16
                , userpassword17                   = userpassword16
                , usersupervisor17                 = usersupervisor16
                , useraccountsuspended17           = useraccountsuspended16
                , userhasacceptedtermsofservice17  = userhasacceptedtermsofservice16
                , userfreetrialexpirationdate17    = userfreetrialexpirationdate16
                , usersignupmethod17               = usersignupmethod16
                , userinfo17                       = userinfo16
                , usersettings17                   = usersettings16
                , userpaymentpolicy17              = userpaymentpolicy16
                , userpaymentaccount17             = userpaymentaccount16
                , userfriends17                    = userfriends16
                , userinviteinfo17                 = userinviteinfo16
                , userlogininfo17                  = userlogininfo16
                , userservice17                    = userservice16
                , usercompany17                    = usercompany16
                , usermailapi17                    = Nothing
                , userrecordstatus17               = userrecordstatus16
                }

instance Migrate User17 User where
  migrate (User17
                { userid17
                , userpassword17
                , usersupervisor17
                , useraccountsuspended17
                , userhasacceptedtermsofservice17
                , userfreetrialexpirationdate17
                , usersignupmethod17
                , userinfo17
                , usersettings17
                , userpaymentpolicy17
                , userpaymentaccount17
                , userfriends17
                , userinviteinfo17
                , userlogininfo17
                , userservice17
                , usercompany17
                , usermailapi17
                , userrecordstatus17
                }) = User
                { userid                         = userid17
                , userpassword                   = userpassword17
                , usersupervisor                 = usersupervisor17
                , useriscompanyadmin             = False
                , useraccountsuspended           = useraccountsuspended17
                , userhasacceptedtermsofservice  = userhasacceptedtermsofservice17
                , userfreetrialexpirationdate    = userfreetrialexpirationdate17
                , usersignupmethod               = usersignupmethod17
                , userinfo                       = userinfo17
                , usersettings                   = usersettings17
                , userpaymentpolicy              = userpaymentpolicy17
                , userpaymentaccount             = userpaymentaccount17
                , userfriends                    = userfriends17
                , userinviteinfo                 = userinviteinfo17
                , userlogininfo                  = userlogininfo17
                , userservice                    = userservice17
                , usercompany                    = usercompany17
                , usermailapi                    = usermailapi17
                , userdeleted                    = userrecordstatus17 == DeletedUser
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

instance Migrate UserSettings0 UserSettings1 where
    migrate (UserSettings0 {
            accounttype0
          , accountplan0
          , signeddocstorage0
          , userpaymentmethod0
          }) = UserSettings1 {
            accounttype1 = accounttype0
          , accountplan1 = accountplan0
          , signeddocstorage1 = signeddocstorage0
          , userpaymentmethod1 = userpaymentmethod0
          , preferreddesignmode1 = Nothing
          }

instance Migrate UserSettings1 UserSettings2 where
    migrate (UserSettings1 {
            accounttype1
          , accountplan1
          , signeddocstorage1
          , userpaymentmethod1
          , preferreddesignmode1
          }) = UserSettings2 {
            accounttype2 = accounttype1
          , accountplan2 = accountplan1
          , signeddocstorage2 = signeddocstorage1
          , userpaymentmethod2 = userpaymentmethod1
          , preferreddesignmode2 = preferreddesignmode1
          , lang2 = LANG_SE
          }

instance Migrate UserSettings2 UserSettings where
    migrate (UserSettings2 {
            accounttype2
          , accountplan2
          , signeddocstorage2
          , userpaymentmethod2
          , preferreddesignmode2
          , lang2
          }) = UserSettings {
            accounttype = accounttype2
          , accountplan = accountplan2
          , signeddocstorage = signeddocstorage2
          , userpaymentmethod = userpaymentmethod2
          , preferreddesignmode = preferreddesignmode2
          , lang = lang2
          , systemserver = SkrivaPa
          }

type Users = IxSet User

instance Indexable User where
  empty = ixSet [ ixFun (\x -> [userid x] :: [UserID])
                , ixFun $ ifUserNotDeleted (\x -> [useremail $ userinfo x] :: [Email])
                , ixFun $ ifUserNotDeleted userfriends
                , ixFun $ ifUserNotDeleted (\x -> [userservice x] :: [Maybe ServiceID])
                , ixFun $ ifUserNotDeleted (\x -> (catMaybes [usercompany x]) :: [CompanyID])
                
                -- need to keep this hanging around until the company migration is done.  don't
                -- use it for anything else
                , ixFun (\x -> maybe [] return (usersupervisor x) :: [SupervisorID])
                ]
          where
            ifUserNotDeleted :: (User -> [a]) -> User -> [a]
            ifUserNotDeleted f doc
              | userdeleted doc = []
              | otherwise = f doc


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

instance Version User16 where
    mode = extension 16 (Proxy :: Proxy User15)

instance Version User17 where
    mode = extension 17 (Proxy :: Proxy User16)

instance Version User where
    mode = extension 18 (Proxy :: Proxy User17)

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

instance Version UserMailAPI

instance Version UserSettings0

instance Version UserSettings1 where
    mode = extension 1 (Proxy :: Proxy UserSettings0)

instance Version UserSettings2 where
    mode = extension 2 (Proxy :: Proxy UserSettings1)

instance Version UserSettings where
    mode = extension 3 (Proxy :: Proxy UserSettings2)

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
  users <- ask
  case getOne (users @= uid) of
    Nothing -> return $ Left "no such user"
    Just user | userdeleted user -> return $ Left "user has been deleted"
    Just user ->
        case action user of
          Left message -> return $ Left message
          Right newuser ->
              if userid newuser /= uid
                 then return $ Left "new user must have same id as old one"
              else do
                modify (updateIx uid newuser)
                return $ Right newuser

queryUsers :: (Users -> a) -> Query Users a
queryUsers queryFunc = do
  users <- ask
  return $ queryFunc users

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

{- |
    Fetches all of the accounts with a matching company id
    that this user can see.  If they're not an admin then this will
    be an empty list
-}
getCompanyAccounts :: UserID -> Query Users [User]
getCompanyAccounts userid = queryUsers $ \users ->
  let muser = getOne (users @= userid)
  in case (muser, muser >>= usercompany, fmap useriscompanyadmin muser) of
    (Just user, Just companyid, Just True) ->
      toList $ IxSet.delete user (users @= companyid)
    _ -> []
    
{- |
    Sets the user as a member of a company.
-}
setUserCompany :: UserID -> CompanyID -> Update Users (Either String User)
setUserCompany userid companyid = modifyUser userid $ \user -> 
  return $ user {
    usercompany = Just companyid,
    useriscompanyadmin = False
  }

deleteUser :: UserID -> Update Users (Either String User)
deleteUser uid = modifyUser uid $ \_ -> do
  return blankUser { userid = uid,
                     userdeleted = True }

addUser :: (BS.ByteString, BS.ByteString)
        -> BS.ByteString
        -> Password
        -> Bool
        -> Maybe ServiceID
        -> Maybe CompanyID
        -> SystemServer
        -> Update Users (Maybe User)
addUser (fstname, sndname) email passwd iscompanyadmin mservice mcompany sserver = do
  users <- ask
  if (IxSet.size (users @= Email email @= mservice ) /= 0) -- a deleted user can re-register as a new user
   then return Nothing  -- "user with same email address exists"
   else do
        userid <- getUnique users UserID --want userid to be unique even against deleted users
        let user = blankUser {
                   userid                  =  userid
                 , userpassword            =  passwd
                 , useriscompanyadmin      =  maybe False (const iscompanyadmin) mcompany
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
              , usersettings = (usersettings blankUser) {systemserver = sserver}
              }
        modify (updateIx userid user)
        return $ Just user

blankUser :: User
blankUser = User {
                   userid                  =  UserID 0
                 , userpassword            =  NoPassword
                 , usersupervisor          =  Nothing
                 , useriscompanyadmin = False
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
                                  , lang = Misc.defaultValue
                                  , systemserver = Misc.defaultValue
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
              , usermailapi = Nothing
              , userdeleted = False
              }

{- |
    This will make a user into a company admin.
    If the user doesn't exist, or if the user isn't a company user
    then a Left is returned.
-}
makeUserACompanyAdmin :: UserID -> Update Users (Either String User)
makeUserACompanyAdmin userid = modifyUser userid $ \user ->
  case usercompany user of
    Nothing -> Left "user doesn't belong to a company"
    Just _ -> return $ user { useriscompanyadmin = True }

filterOutDeletedUsers :: [User] -> [User]
filterOutDeletedUsers = filter (not . userdeleted)

getUserStats :: Query Users UserStats
getUserStats = queryUsers $ \users ->
  let userList = filterOutDeletedUsers $ toList users in
  UserStats
         { usercount = length userList
         , viralinvitecount = length $ filterByInvite (isInviteType Viral) userList
         , admininvitecount = length $ filterByInvite (isInviteType Admin) userList
         }

getUserStatsByUser :: User -> Query Users UserStats
getUserStatsByUser user = queryUsers $ \users ->
  let invitedusers = filterByInvite isInvitedByUser . filterOutDeletedUsers $ toList users
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
  let usersSorted = sortBy compareuserfullname . filterOutDeletedUsers $ toList users
      compareuserfullname a b = compare (userfullname a) (userfullname b) in
  usersSorted

setUserPassword :: UserID -> Password -> Update Users (Either String User)
setUserPassword userid newpassword = modifyUser userid $ \user ->
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

setUserMailAPI :: UserID -> Maybe UserMailAPI -> Update Users (Either String User)
setUserMailAPI userid musermailapi =
    modifyUser userid $ \user ->
            Right $ user { usermailapi = musermailapi }

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
  mms <- do users <- ask
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
                              users <- ask
                              let minviter = getOne (users @= (UserID iid))
                              case minviter of
                                Just inviter | (not $ userdeleted inviter) ->  do
                                                 _<- modifyUser (userid inviter) $ \user ->
                                                  Right $ user {userpaymentpolicy = Payments.extendFreeTmpChange now 7 (userpaymentpolicy user)}
                                                 return ()
                                _ -> return ()

exportUsersDetailsToCSV :: Query Users BS.ByteString
exportUsersDetailsToCSV = queryUsers $ \users ->
  let fields user = [userfullname user, unEmail $ useremail $ userinfo user]
      content = BS.intercalate (BS.fromString ",") <$> fields in
  BS.unlines $ content <$> (filterOutDeletedUsers $ toList users)


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

{- |
    Checks whether the given user should have an equivalent company made for them.
    To qualify for this the user should be live, and not have any company set on them already.
    Then they should either :
       * be a single user (so without a supervisor or subaccounts) who has a company name or number set on them
       * be a supervisor
-}
requiresCompanyForMigration :: UserID -> Query Users Bool
requiresCompanyForMigration userid = queryUsers $ \users ->
  let muser = getOne (users @= userid) in
  case muser of
    Just user ->
      let islive = not $ userdeleted user
          nocompany = isNothing $ usercompany user
          hascompanyinfo = BS.empty /= usercompanyname (userinfo user) 
                             || BS.empty /= usercompanynumber (userinfo user)
          issubaccount = isJust $ usersupervisor user
          issupervisor = not $ IxSet.null (users @= SupervisorID (unUserID userid))
          issingle = not issubaccount && not issupervisor
      in islive && nocompany && ((issingle && hascompanyinfo) || issupervisor)
    Nothing -> False

getAllUsersForDBMigration :: Query Users [User]
getAllUsersForDBMigration = ask >>= return . toList

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
                    , 'setUserMailAPI
                    , 'setUserSettings
                    , 'setPreferredDesignMode
                    , 'setUserPaymentAccount
                    , 'setUserPaymentPolicyChange
                    --, 'freeUserFromPayments
                    , 'recordFailedLogin
                    , 'recordSuccessfulLogin
                    , 'getUsersByFriendUserID
                    , 'getUserFriends
                    , 'acceptTermsOfService
                    , 'setFreeTrialExpirationDate
                    , 'setSignupMethod
                    , 'exportUsersDetailsToCSV
                    , 'addViewerByEmail
                    , 'getCompanyAccounts
                    , 'setUserCompany
                    , 'makeUserACompanyAdmin
                    --just for company migration
                    , 'requiresCompanyForMigration
                      -- the below should be only used carefully and by admins
                    --, 'addFreePaymentsForInviter
                    , 'getAllUsersForDBMigration
                    ])

$(deriveSerializeFor [ ''User
                     , ''User17
                     , ''User16
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
                     , ''UserMailAPI
                     , ''UserSettings
                     , ''UserSettings1
                     , ''UserSettings2
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

initialInsertUsersIntoPG :: DB ()
initialInsertUsersIntoPG = wrapDB $ \conn -> do
  users <- query GetAllUsersForDBMigration
  forM_ users $ \u -> do
    let (salt, hash) = case userpassword u of
         NoPassword -> (Nothing, Nothing)
         Password salt' hash' -> (Just $ B64.encode $ BS.pack salt', Just $ B64.encode $ BS.pack hash')
    _ <- run conn ("INSERT INTO users ("
      ++ "  id"
      ++ ", password"
      ++ ", salt"
      ++ ", is_company_admin"
      ++ ", account_suspended"
      ++ ", has_accepted_terms_of_service"
      ++ ", free_trial_expiration_date"
      ++ ", signup_method"
      ++ ", deleted) VALUES (?, decode(?, 'base64'), decode(?, 'base64'), ?, ?, to_timestamp(?), to_timestamp(?), ?, ?)") [
        toSql $ userid u
      , toSql hash
      , toSql salt
      , toSql $ useriscompanyadmin u
      , toSql $ useraccountsuspended u
      , toSql $ userhasacceptedtermsofservice u
      , toSql $ userfreetrialexpirationdate u
      , toSql $ usersignupmethod u
      , toSql $ userdeleted u
      ]
    _ <- run conn ("INSERT INTO user_login_infos ("
      ++ "  user_id"
      ++ ", last_success"
      ++ ", last_fail"
      ++ ", consecutive_fails) VALUES (?, to_timestamp(?), to_timestamp(?), ?)") [
        toSql $ userid u
      , toSql $ lastsuccesstime $ userlogininfo u
      , toSql $ lastfailtime $ userlogininfo u
      , toSql $ consecutivefails $ userlogininfo u
      ]
    _ <- run conn ("INSERT INTO user_infos ("
      ++ "  user_id"
      ++ ", first_name"
      ++ ", last_name"
      ++ ", personal_number"
      ++ ", company_position"
      ++ ", phone"
      ++ ", mobile"
      ++ ", email) VALUES (?, ?, ?, ?, ?, ?, ?, ?)") [
        toSql $ userid u
      , toSql $ userfstname $ userinfo u
      , toSql $ usersndname $ userinfo u
      , toSql $ userpersonalnumber $ userinfo u
      , toSql $ usercompanyposition $ userinfo u
      , toSql $ userphone $ userinfo u
      , toSql $ usermobile $ userinfo u
      , toSql $ useremail $ userinfo u
      ]
    when (isJust $ usermailapi u) $ do
      let mailapi = fromJust $ usermailapi u
      _ <- run conn ("INSERT INTO user_mail_apis ("
        ++ "  user_id"
        ++ ", key"
        ++ ", daily_limit"
        ++ ", sent_today"
        ++ ", last_sent_date) VALUES (?, ?, ?, ?, ?)") [
          toSql $ userid u
        , toSql $ umapiKey mailapi
        , toSql $ umapiDailyLimit mailapi
        , toSql $ umapiSentToday mailapi
        , toSql $ umapiLastSentDate mailapi
        ]
      return ()
    _ <- run conn ("INSERT INTO user_settings ("
      ++ "  user_id"
      ++ ", payment_method"
      ++ ", preferred_design_mode"
      ++ ", lang"
      ++ ", system_server) VALUES (?, ?, ?, ?, ?)") [
        toSql $ userid u
      , toSql $ userpaymentmethod $ usersettings u
      , toSql $ preferreddesignmode $ usersettings u
      , toSql $ lang $ usersettings u
      , toSql $ systemserver $ usersettings u
      ]
    when (isJust $ signeddocstorage $ usersettings u) $ do
      let tws = fromJust $ signeddocstorage $ usersettings u
      _ <- run conn ("INSERT INTO trust_weaver_storages ("
        ++ "  user_id"
        ++ ", enabled"
        ++ ", name"
        ++ ", superadmin"
        ++ ", superadmin_pwd"
        ++ ", section_path) VALUES (?, ?, ?, ?, ?, ?)") [
          toSql $ userid u
        , toSql $ storagetwenabled tws
        , toSql $ storagetwname tws
        , toSql $ storagetwsuperadmin tws
        , toSql $ storagetwsuperadminpwd tws
        , toSql $ storagetwsectionpath tws
        ]
      return ()
    _ <- run conn ("INSERT INTO user_payment_policies ("
      ++ "  user_id"
      ++ ", account_type) VALUES (?, ?)") [
        toSql $ userid u
      , toSql $ paymentaccounttype $ userpaymentpolicy u
      ]
    return ()

finalizeInsertUsersIntoPG :: DB ()
finalizeInsertUsersIntoPG = wrapDB $ \conn -> do
  users <- query GetAllUsersForDBMigration
  forM_ users $ \u -> do
    when (isJust $ userinviteinfo u) $ do
      let ii = fromJust $ userinviteinfo u
      _ <- run conn ("INSERT INTO user_invite_infos ("
        ++ "  user_id"
        ++ ", inviter_id"
        ++ ", invite_time"
        ++ ", invite_type) VALUES (?, ?, to_timestamp(?), ?)") [
          toSql $ userid u
        , toSql $ userinviter ii
        , toSql $ invitetime ii
        , toSql $ invitetype ii
        ]
      return ()
    forM_ (userfriends u) $ \f -> do
      _ <- run conn "INSERT INTO user_friends (user_id, friend_id) VALUES (?, ?)"
        [toSql $ userid u, toSql f]
      return ()
    return ()

populateDBWithUsersIfEmpty :: DB ()
populateDBWithUsersIfEmpty = do
  n <- wrapDB $ \conn -> do
    st <- prepare conn "SELECT COUNT(*) FROM user_infos"
    _ <- executeRaw st
    [n] <- fetchAllRows' st >>= return . map fromSql . join
    return (n::Int)
  when (n == 0) $ do
    Log.debug "No users in database, populating with values from happstack-state..."
    Log.debug "Copying users..."
    initialInsertUsersIntoPG
    Log.debug "Copying services..."
    insertServicesIntoPG
    Log.debug "Copying companies..."
    insertCompaniesIntoPG
    Log.debug "Finalizing users..."
    finalizeInsertUsersIntoPG
    Log.debug "Done."
