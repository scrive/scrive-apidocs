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
import Happstack.Data.IxSet as IxSet
import Control.Applicative((<$>))
import Data.Data(Data(..))
import Data.Maybe(isNothing)
import Misc
import Control.Monad
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common

-- |perform insert only if test is True
testAndInsert :: (Indexable a b,
                  Ord a,
                  Data a,
                  MonadState (IxSet a) m) =>
                 (IxSet a -> Bool) -> a -> m Bool
testAndInsert test a =
    maybeModify $ \ixset ->
        if test ixset
          then Just (insert a ixset)
          else Nothing

-- this should be sent upstream to mtl
maybeModify :: (MonadState s m) => (s -> Maybe s) -> m Bool
maybeModify f =
    do state <- get
       case f state of
         Nothing -> return False
         (Just state') -> 
             do put state' 
                return True

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
   
      newtype UserID = UserID Int
      newtype ExternalUserID = ExternalUserID BS.ByteString                  
                       
      data User = User
          { userid          :: UserID
          , externaluserids :: [ExternalUserID]
          , fullname        :: BS.ByteString
          , email           :: BS.ByteString
          }

   |])

$(inferIxSet "Users" ''User 'noCalcs [''UserID, ''ExternalUserID])

$(deriveSerialize ''User)
instance Version User

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
  let user = (User userid [externaluserid] fullname email)
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


instance Component Users where
  type Dependencies Users = End
  initialValue = IxSet.empty
  
-- create types for event serialization
$(mkMethods ''Users ['findUserByUserID, 'findUserByExternalUserID, 
                     'addUser, 'getUserStats, 'getAllUsers])

