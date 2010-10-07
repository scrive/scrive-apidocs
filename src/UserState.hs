{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeSynonymInstances, StandaloneDeriving #-}
module UserState where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify,MonadState(..))
import Happstack.State.ClockTime
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Happstack.Data.IxSet as IxSet
import Control.Applicative((<$>))
import Data.Data(Data(..))
import Data.Maybe(isNothing,isJust)
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
import Debug.Trace
import MinutesTime

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
                       
      data User = User
          { userid                        :: UserID
          , userfullname                  :: BS.ByteString
          , useremail                     :: Email
          , usercompanyname               :: BS.ByteString
          , usercompanynumber             :: BS.ByteString
          , userinvoiceaddress            :: BS.ByteString
          , userflashmessages             :: [FlashMessage]
          , userpassword                  :: Password
          , usersupervisor                :: Maybe SupervisorID
          , usercanhavesubaccounts        :: Bool
          , useraccountsuspended          :: Bool
          , userhasacceptedtermsofservice :: Maybe MinutesTime
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

deriving instance Show User
deriving instance Show Email
deriving instance Show FlashMessage
deriving instance Show Password

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
          , userexternalids3
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

instance Migrate User5 User where
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
             }) = User
                { userid                = userid5
                , userfullname          = userfullname5
                , useremail             = useremail5
                , usercompanyname       = usercompanyname5
                , usercompanynumber     = usercompanynumber5
                , userinvoiceaddress    = userinvoiceaddress5
                , userflashmessages     = userflashmessages5
                , userpassword          = userpassword5
                , usersupervisor        = usersupervisor5
                , usercanhavesubaccounts= usercanhavesubaccounts5
                , useraccountsuspended  = useraccountsuspended5
                , userhasacceptedtermsofservice = Nothing
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

$(deriveSerialize ''User)
instance Version User where
    mode = extension 6 (Proxy :: Proxy User5)

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
  let user = (User { userid = userid
                   , userfullname = fullname
                   , useremail = Email email
                   , usercompanyname = BS.empty
                   , usercompanynumber = BS.empty
                   , userinvoiceaddress = BS.empty
                   , userflashmessages = []
                   , userpassword = passwd
                   , usersupervisor = fmap (SupervisorID . unUserID) maybesupervisor
                   , usercanhavesubaccounts = True
                   , useraccountsuspended = False
                   , userhasacceptedtermsofservice = Nothing
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
  users <- ask
  modify (updateIx userid (user { userpassword = newpassword })) 
  return ()
  
verifyPassword :: Password -> BS.ByteString -> Bool
verifyPassword (Password salt hash) password
    | hash == generatedHash = True
    | otherwise = False
    where
        generatedHash = hashPassword password salt

setUserDetails :: User 
               -> BS.ByteString
               -> BS.ByteString
               -> BS.ByteString
               -> BS.ByteString
               -> Update Users User
setUserDetails user1 fullname companyname companynumber invoiceaddress = do
  users <- ask
  let Just user = getOne (users @= userid user1)
  let newuser = user { userfullname = fullname
                     , usercompanyname = companyname
                     , usercompanynumber = companynumber
                     , userinvoiceaddress = invoiceaddress
                     }
  modify (updateIx (userid user) newuser)
  return newuser
  

fragileDeleteUser :: UserID -> Update Users (Maybe User)
fragileDeleteUser userid = do
  users <- ask
  let maybeuser = getOne (users @= userid)
  when (isJust maybeuser) $
       modify (deleteIx userid)
  return maybeuser

acceptTermsOfService :: UserID -> MinutesTime -> Update Users ()
acceptTermsOfService userid minutestime = do
  users <- ask
  case getOne (users @= userid) of
    Just user -> modify (updateIx userid (user { userhasacceptedtermsofservice = Just minutestime })) 
  return ()

-- for testing purposes
deleteTermsOfService :: UserID -> Update Users ()
deleteTermsOfService userid = do
  users <- ask
  case getOne (users @= userid) of
    Just user -> modify (updateIx userid (user { userhasacceptedtermsofservice = Nothing }))
  return ()

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
                    , 'getUserSubaccounts
                    , 'acceptTermsOfService

                      -- the below should be only used carefully and by admins
                    , 'fragileDeleteUser
                    , 'deleteTermsOfService
                    ])

