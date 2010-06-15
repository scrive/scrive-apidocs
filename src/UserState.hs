{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeSynonymInstances #-}
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



$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
   
      newtype UserID = UserID Int
      newtype ExternalUserID = ExternalUserID BS.ByteString                  
      newtype FlashMessage = FlashMessage BS.ByteString
                       
      data User = User
          { userid             :: UserID
          , userexternalids    :: [ExternalUserID]
          , userfullname       :: BS.ByteString
          , useremail          :: BS.ByteString
          , usercompanyname    :: BS.ByteString
          , usercompanynumber  :: BS.ByteString
          , userinvoiceaddress :: BS.ByteString
          , userflashmessages  :: [FlashMessage]
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

instance Migrate User1 User where
    migrate (User1
             { userid1
             , externaluserids1
             , fullname1
             , email1
             , usercompanyname1
             , usercompanynumber1
             , userinvoiceaddress1
             }) = User
                { userid = userid1
                , userexternalids = externaluserids1
                , userfullname = fullname1
                , useremail = email1
                , usercompanyname = usercompanyname1
                , usercompanynumber = usercompanynumber1
                , userinvoiceaddress = userinvoiceaddress1
                , userflashmessages = []
                }



$(inferIxSet "Users" ''User 'noCalcs [''UserID, ''ExternalUserID])

$(deriveSerialize ''User0)
instance Version User0

$(deriveSerialize ''User1)
instance Version User1 where
    mode = extension 1 (Proxy :: Proxy User0)

$(deriveSerialize ''User)
instance Version User where
    mode = extension 2 (Proxy :: Proxy User1)

$(deriveSerialize ''FlashMessage)
instance Version FlashMessage

$(deriveSerialize ''UserID)
instance Version UserID

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

findUserByExternalUserID :: ExternalUserID -> Query Users (Maybe User)
findUserByExternalUserID externaluserid = do
  users <- ask
  return $ getOne (users @= externaluserid)
    
findUserByUserID :: UserID -> Query Users (Maybe User)
findUserByUserID userid = do
  users <- ask
  return $ getOne (users @= userid)

addUser :: ExternalUserID -> BS.ByteString 
        -> BS.ByteString -> Update Users User
addUser externaluserid fullname email = do
  users <- get
  userid <- getUnique users UserID
  let user = (User { userid = userid
                   , userexternalids = [externaluserid]
                   , userfullname = fullname
                   , useremail = email
                   , usercompanyname = BS.empty
                   , usercompanynumber = BS.empty
                   , userinvoiceaddress = BS.empty
                   , userflashmessages = []
                   })
  put (insert user users)
  return user

getUserStats :: Query Users Int
getUserStats = do
  users <- ask
  return (size users)


getAllUsers :: Query Users [User]
getAllUsers = do
  users <- ask
  return (toList users)

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

instance Component Users where
  type Dependencies Users = End
  initialValue = IxSet.empty
  
-- create types for event serialization
$(mkMethods ''Users [ 'findUserByUserID
                    , 'findUserByExternalUserID
                    , 'addUser
                    , 'getUserStats
                    , 'getAllUsers
                    , 'getUserFlashMessages
                    , 'addUserFlashMessage
                    ])

