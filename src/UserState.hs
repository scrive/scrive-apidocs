{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeSynonymInstances, StandaloneDeriving #-}
module UserState where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify,MonadState(..))
import Happstack.State.ClockTime
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Happstack.Data.IxSet as IxSet
import Control.Applicative((<$>))
import Data.Data(Data(..))
import Data.Maybe(isNothing)
import Misc
import Control.Monad
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Data.List


$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
   
      newtype UserID = UserID { unUserID :: Int }
      newtype ExternalUserID = ExternalUserID { unExternalUserID :: BS.ByteString }
      newtype FlashMessage = FlashMessage BS.ByteString
      newtype Email = Email { unEmail :: BS.ByteString }
      newtype SupervisorID = SupervisorID { unSupervisorID :: Int }
                       
      data User = User
          { userid                 :: UserID
          , userfullname           :: BS.ByteString
          , useremail              :: Email
          , usercompanyname        :: BS.ByteString
          , usercompanynumber      :: BS.ByteString
          , userinvoiceaddress     :: BS.ByteString
          , userflashmessages      :: [FlashMessage]
          , userpassword           :: BS.ByteString
          , usersupervisor         :: Maybe SupervisorID
          , usercanhavesubaccounts :: Bool
          , useraccountsuspended   :: Bool
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

instance Migrate User3 User where
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
          }) = User
          { userid = userid3
          , userfullname = userfullname3
          , useremail = useremail3
          , usercompanyname = usercompanyname3
          , usercompanynumber = usercompanynumber3
          , userinvoiceaddress = userinvoiceaddress3
          , userflashmessages = userflashmessages3
          , userpassword = maybe (BS.empty) (id) userpassword3
          , usersupervisor = Nothing
          , usercanhavesubaccounts = True
          , useraccountsuspended = False -- should probably have a reason and time here
          }


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

$(deriveSerialize ''User)
instance Version User where
    mode = extension 4 (Proxy :: Proxy User3)

$(deriveSerialize ''FlashMessage)
instance Version FlashMessage

$(deriveSerialize ''Email)
instance Version Email

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

getUserSubaccounts :: UserID -> Query Users [User]
getUserSubaccounts userid = do
  users <- ask
  return $ toList (users @= SupervisorID (unUserID userid))

addUser :: BS.ByteString 
        -> BS.ByteString 
        -> BS.ByteString 
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

getUserFlashMessages :: UserID -> Update Users [FlashMessage]
getUserFlashMessages userid = do
  users <- ask
  case getOne (users @= userid) of
    Nothing -> return []
    Just (user@User{ userflashmessages }) -> 
        do
          modify (updateIx userid (user { userflashmessages = []})) 
          return userflashmessages


addUserFlashMessage :: UserID -> FlashMessage -> Update Users ()
addUserFlashMessage userid msg= do
  users <- ask
  case getOne (users @= userid) of
    Nothing -> return ()
    Just (user@User{ userflashmessages }) -> 
        do
          modify (updateIx userid (user { userflashmessages = msg : userflashmessages })) 
          return ()

setUserPassword :: User -> BS.ByteString -> Update Users ()
setUserPassword user@User{userid} newpassword = do
  users <- ask
  modify (updateIx userid (user { userpassword = newpassword })) 
  return ()

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
  

  

instance Component Users where
  type Dependencies Users = End
  initialValue = IxSet.empty
  
-- create types for event serialization
$(mkMethods ''Users [ 'getUserByUserID
                    , 'getUserByEmail
                    , 'addUser
                    , 'getUserStats
                    , 'getAllUsers
                    , 'getUserFlashMessages
                    , 'addUserFlashMessage
                    , 'setUserPassword
                    , 'setUserDetails
                    , 'getUserSubaccounts
                    ])

