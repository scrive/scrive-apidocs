{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeSynonymInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module UserState 
    ( Email(..)
    , ExternalUserID(..)
    , FlashMessage(..)
    , Password(..)
    , StorageType(..)
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
    , SetUserSettings(..)
    , SetUserPaymentAccount(..)
    , SetUserPaymentPolicyChange(..)
    , SetUserPassword(..)
) where
import Happstack.Data
import Happstack.State
import "mtl" Control.Monad.Reader (ask)
import "mtl" Control.Monad.State (modify,MonadState(..))
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
      newtype FlashMessage = FlashMessage BS.ByteString deriving Read       
      newtype Email = Email { unEmail :: BS.ByteString }
      data Password = Password [Octet] [Octet] | NoPassword
      newtype SupervisorID = SupervisorID { unSupervisorID :: Int }
      data StorageType = Amazon | TrustWeaver
      data UserAccountType = MainAccount | SubAccount
      data PaymentMethod = CreditCard | Invoice | Undefined
      data UserAccountPlan = Basic
      data UserInfo = UserInfo {
            userfstname                   :: BS.ByteString
          , usersndname                   :: BS.ByteString
          , userpersonalnumber            :: BS.ByteString
          , usercompanyname               :: BS.ByteString
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
             , signeddocstorage :: StorageType
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


deriving instance Show StorageType 
deriving instance Show UserAccountType 
deriving instance Show PaymentMethod
deriving instance Show UserAccountPlan 
deriving instance Show UserInfo
deriving instance Show UserSettings
deriving instance Show User
deriving instance Show Email
deriving instance Show FlashMessage
deriving instance Show Password

deriving instance Bounded StorageType 
deriving instance Enum StorageType 
deriving instance Read StorageType 

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
                
instance Migrate User6 User where
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
             }) = User
                {  userid                  =  userid6
                 , userpassword            =  userpassword6 
                 , usersupervisor          =  usersupervisor6   
                 , usercanhavesubaccounts  =  usercanhavesubaccounts6 
                 , useraccountsuspended    =  useraccountsuspended6      
                 , userhasacceptedtermsofservice = userhasacceptedtermsofservice6
                 , userinfo = UserInfo {
                                    userfstname = userfullname6          
                                  , usersndname = BS.empty
                                  , userpersonalnumber = BS.empty
                                  , usercompanyname = usercompanyname6
                                  , usercompanynumber  = usercompanynumber6
                                  , useraddress = userinvoiceaddress6
                                  , userzip = BS.empty
                                  , usercity  = BS.empty
                                  , usercountry = BS.empty
                                  , userphone = BS.empty
                                  , usermobile = BS.empty
                                  , useremail = useremail6   
                                   }
                , usersettings  = UserSettings {
                                    accounttype = if (isNothing usersupervisor6)  then MainAccount else SubAccount
                                  , accountplan = Basic
                                  , signeddocstorage = Amazon
                                  , userpaymentmethod = Undefined
                                  }                   
                , userpaymentpolicy =  Payments.basicPaymentPolicy
                , userpaymentaccount = Payments.emptyPaymentAccount                  
      }       
      
userfullname :: User -> BS.ByteString
userfullname u = if (BS.null $ usersndname $ userinfo u) 
                  then (userfstname $ userinfo u) 
                  else (userfstname $ userinfo u) `BS.append` (BS.fromString " ") `BS.append` (usersndname $ userinfo u)
  
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

$(deriveSerialize ''User)
instance Version User where
    mode = extension 7 (Proxy :: Proxy User6) 


$(deriveSerialize ''StorageType )
instance Version StorageType 

$(deriveSerialize ''UserAccountType )
instance Version UserAccountType 

$(deriveSerialize ''PaymentMethod)
instance Version PaymentMethod

$(deriveSerialize ''UserAccountPlan )
instance Version UserAccountPlan 

$(deriveSerialize ''UserInfo)
instance Version UserInfo

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

getUserSubaccounts :: UserID -> Query Users (Set.Set User)
getUserSubaccounts userid = do
  users <- ask
  return $ toSet (users @= SupervisorID (unUserID userid))

addUser :: BS.ByteString 
        -> BS.ByteString 
        -> Password
        -> Maybe UserID
        -> Update Users User
addUser fullname email passwd maybesupervisor = do
  users <- get
  when (IxSet.size (users @= Email email) /= 0)
     (error "user with same email address exists")
          
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
                                  , signeddocstorage = Amazon
                                  , userpaymentmethod = Undefined
                                  }                   
                , userpaymentpolicy =  Payments.basicPaymentPolicy
                , userpaymentaccount = Payments.emptyPaymentAccount  
                 })             
  modify (updateIx (Email email) user)
  return user

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
               -> Update Users (Either String User)
setUserDetails userid fullname companyname companynumber invoiceaddress =
    modifyUser userid $ \user -> 
            Right $ user { userinfo = (userinfo user) { 
                                                      userfstname = fullname
                                                    , usercompanyname = companyname
                                                    , usercompanynumber = companynumber
                                                    , useraddress = invoiceaddress
                                                    }
                         }                            

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
                    , 'addUser
                    , 'getUserStats
                    , 'getAllUsers
                    , 'setUserPassword
                    , 'setUserDetails
                    , 'setUserInfo
                    , 'setUserSettings
                    , 'setUserPaymentAccount 
                    , 'setUserPaymentPolicyChange
                    , 'getUserSubaccounts
                    , 'acceptTermsOfService
                    , 'exportUsersDetailsToCSV

                      -- the below should be only used carefully and by admins
                    , 'fragileDeleteUser
                    , 'deleteTermsOfService
                    ])

