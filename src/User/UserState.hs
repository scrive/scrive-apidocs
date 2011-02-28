{-# OPTIONS_GHC -Wall #-}
module User.UserState 
    ( Email(..)
    , Friend(..)
    , Inviter(..)
    , DefaultMainSignatory(..)
    , ExternalUserID(..)
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
    , userfullname
    , createPassword
    , isPasswordStrong
    , verifyPassword

    , AcceptTermsOfService(..)
    , AddUser(..)
    , DeleteTermsOfService(..)
    , ExportUsersDetailsToCSV(..)
    , FragileDeleteUser(..)
    , GetAllUsers(..)
    , GetUserByEmail(..)
    , GetUserByUserID(..)
    , GetUserStats(..)
    , GetUserSubaccounts(..)
    , SetUserDetails(..)
    , SetUserInfo(..)
    , SetInviter(..)
    , SetUserSettings(..)
    , SetUserPaymentAccount(..)
    , SetUserPaymentPolicyChange(..)
    , SetUserPassword(..)
    , GetUsersByFriendUserID(..)
    , AddViewerByEmail(..)
    , GetUsersByUserIDs(..)
    , FreeUserFromPayments(..)
    , AddFreePaymentsForInviter(..)
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
import Data.Maybe(isJust,isNothing)
import Misc
import Control.Monad
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Codec.Utils (Octet)
import Data.Digest.SHA256 (hash)
import System.Random
import System.IO.Unsafe
import Data.List
import qualified Data.Set as Set
import Control.Applicative
import MinutesTime
import qualified Payments.PaymentsState as Payments

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
   
      newtype UserID = UserID { unUserID :: Int }
      newtype ExternalUserID = ExternalUserID { unExternalUserID :: BS.ByteString }
      -- Leaving FlashMessage declaration here is necessity
      -- Have to be used because of users versioning
      -- Can't be moved to Session where it belong (cycle references)
      newtype Friend = Friend { unFriend :: Int }
      newtype Inviter = Inviter { unInviter :: Int }
      newtype DefaultMainSignatory = DefaultMainSignatory { unDMS :: Int }
      newtype FlashMessage = FlashMessage { unFlashMessage :: BS.ByteString }  deriving Read       
      newtype Email = Email { unEmail :: BS.ByteString }
      data Password = Password [Octet] [Octet] | NoPassword
      newtype SupervisorID = SupervisorID { unSupervisorID :: Int }
      data TrustWeaverStorage = TrustWeaverStorage
          { storagetwenabled       :: Bool
          , storagetwname          :: BS.ByteString
          , storagetwsuperadmin    :: BS.ByteString
          , storagetwsuperadminpwd :: BS.ByteString
          , storagetwsectionpath   :: BS.ByteString
          }
      data UserAccountType = MainAccount | SubAccount
      data PaymentMethod = CreditCard | Invoice | Undefined
      data UserAccountPlan = Basic
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
      data UserSettings  = UserSettings {
               accounttype :: UserAccountType
             , accountplan :: UserAccountPlan
             , signeddocstorage :: Maybe TrustWeaverStorage
             , userpaymentmethod :: PaymentMethod
      }
      
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
          , userinviter                   :: Maybe Inviter
          }
          
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

      data User1 = User1
          { userid1             :: UserID
          , externaluserids1    :: [ExternalUserID]
          , fullname1           :: BS.ByteString
          , email1              :: BS.ByteString
          , usercompanyname1    :: BS.ByteString
          , usercompanynumber1  :: BS.ByteString
          , userinvoiceaddress1 :: BS.ByteString
          }

      data User0 = User0
          { userid0          :: UserID
          , externaluserids0 :: [ExternalUserID]
          , fullname0        :: BS.ByteString
          , email0           :: BS.ByteString
          }

   |])


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
deriving instance Show DefaultMainSignatory

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

instance Migrate User8 User where
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
                , userdefaultmainsignatory8       
                }) = User 
                { userid                         = userid8
                , userpassword                   = userpassword8
                , usersupervisor                 = usersupervisor8
                , usercanhavesubaccounts         = usercanhavesubaccounts8
                , useraccountsuspended           = useraccountsuspended8
                , userhasacceptedtermsofservice  = userhasacceptedtermsofservice8
                , userinfo                       = userinfo8
                , usersettings                   = usersettings8
                , userpaymentpolicy              = userpaymentpolicy8
                , userpaymentaccount             = userpaymentaccount8
                , userfriends                    = userfriends8
                , userinviter                    = Nothing
                }


userfullname :: User -> BS.ByteString
userfullname u = if (BS.null $ usersndname $ userinfo u) 
                  then (userfstname $ userinfo u) 
                  else (userfstname $ userinfo u) `BS.append` (BS.fromString " ") `BS.append` (usersndname $ userinfo u)


  
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
  
  
  
isPasswordStrong :: BS.ByteString -> Bool
isPasswordStrong password
    | length (BS.toString password) >= 6 = True
    | otherwise = False

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

$(inferIxSet "Users" ''User 'noCalcs [''UserID, ''Email, ''SupervisorID])

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
    
$(deriveSerialize ''User)
instance Version User where
    mode = extension 9 (Proxy :: Proxy User8)

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

$(deriveSerialize ''FlashMessage)
instance Version FlashMessage

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

$(deriveSerialize ''DefaultMainSignatory)
instance Version DefaultMainSignatory

$(deriveSerialize ''SupervisorID)
instance Version SupervisorID

$(deriveSerialize ''ExternalUserID)
instance Version ExternalUserID

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

getUsersByUserIDs :: [UserID] -> Query Users [User]
getUsersByUserIDs [] = return []
getUsersByUserIDs (u:us) = do
  muser1 <- getUserByUserID u
  users <- getUsersByUserIDs us
  case muser1 of
    Just user1 -> return $ [user1]
    Nothing -> return $ []

getUsersByFriendUserID :: UserID -> Query Users [User]
getUsersByFriendUserID uid = do
  users <- ask
  return $ filter (\u -> (unUserID uid) `elem` (map unFriend (userfriends u))) $ toList users

getUserSubaccounts :: UserID -> Query Users (Set.Set User)
getUserSubaccounts userid = do
  users <- ask
  return $ toSet (users @= SupervisorID (unUserID userid))

addUser :: BS.ByteString 
        -> BS.ByteString 
        -> Password
        -> Maybe UserID
        -> Update Users (Maybe User)
addUser fullname email passwd maybesupervisor = do
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
                                    userfstname = fullname       
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
              , userinviter = Nothing
                 })             
        modify (updateIx (Email email) user)
        return $ Just user

getUserStats :: Query Users Int
getUserStats = do
  users <- ask
  return (size users)


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
        
        
setUserDetails :: UserID
               -> BS.ByteString
               -> BS.ByteString
               -> BS.ByteString
               -> BS.ByteString
               -> BS.ByteString
               -> BS.ByteString
               -> Update Users (Either String User)
setUserDetails userid fname lname companyname companyposition companynumber invoiceaddress =
    modifyUser userid $ \user -> 
            Right $ user { userinfo = (userinfo user) { userfstname = fname
                                                      , usersndname = lname
                                                      , usercompanyname = companyname
                                                      , usercompanyposition = companyposition
                                                      , usercompanynumber = companynumber
                                                      , useraddress = invoiceaddress
                                                      }
                         }                            

setInviter :: Maybe User -> User -> Update Users ()
setInviter inviter u = do
                           _ <- modifyUser (userid u) $ \user -> 
                                   Right $ user { userinviter   = fmap (Inviter.  unUserID . userid) inviter }    
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


fragileDeleteUser :: UserID -> Update Users (Maybe User)
fragileDeleteUser userid = do
  users <- ask
  let maybeuser = getOne (users @= userid)
  when (isJust maybeuser) $
       modify (deleteIx userid)
  return maybeuser

acceptTermsOfService :: UserID -> MinutesTime -> Update Users (Either String User)
acceptTermsOfService userid minutestime = 
    modifyUser userid $ \user -> 
        Right $ user { userhasacceptedtermsofservice = Just minutestime }

addFreePaymentsForInviter ::MinutesTime -> User -> Update Users ()
addFreePaymentsForInviter now u = do
                           case (userinviter u) of
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
                           
-- for testing purposes
deleteTermsOfService :: UserID -> Update Users (Either String User)
deleteTermsOfService userid =
    modifyUser userid $ \user -> Right $ user { userhasacceptedtermsofservice = Nothing }

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
                    , 'getUsersByUserIDs
                    , 'addUser
                    , 'getUserStats
                    , 'getAllUsers
                    , 'setUserPassword
                    , 'setUserDetails
                    , 'setInviter
                    , 'setUserInfo
                    , 'setUserSettings
                    , 'setUserPaymentAccount 
                    , 'setUserPaymentPolicyChange
                    , 'freeUserFromPayments
                    , 'getUserSubaccounts
                    , 'getUsersByFriendUserID
                    , 'acceptTermsOfService
                    , 'exportUsersDetailsToCSV
                    , 'addViewerByEmail
                      -- the below should be only used carefully and by admins
                    , 'fragileDeleteUser
                    , 'deleteTermsOfService
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