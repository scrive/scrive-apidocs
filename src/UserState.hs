{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeSynonymInstances
    #-}
module UserState where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify,MonadState(..))
import Happstack.State.ClockTime
import Data.ByteString
import Happstack.Data.IxSet as IxSet
import Control.Applicative((<$>))
import Data.Data(Data(..))
import Data.Maybe(isNothing)
import Misc

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

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
   
      newtype UserID = UserID Int
      newtype ExternalUserID = ExternalUserID ByteString                  
                       
      data User = User
          { userid          :: UserID
          , externaluserids :: [ExternalUserID]
          , fullname        :: ByteString
          , email           :: ByteString
          }

   |])

$(inferIxSet "Users" ''User 'noCalcs [''UserID, ''ExternalUserID])

$(deriveSerialize ''User)
instance Version User

$(deriveSerialize ''UserID)
instance Version UserID

$(deriveSerialize ''ExternalUserID)
instance Version ExternalUserID


findUserByExternalUserID :: ExternalUserID -> Query Users (Maybe User)
findUserByExternalUserID externaluserid = do
  users <- ask
  return $ getOne (users @= externaluserid)
    
findUserByUserID :: UserID -> Query Users (Maybe User)
findUserByUserID userid = do
  users <- ask
  return $ getOne (users @= userid)

addUser :: ExternalUserID -> ByteString -> ByteString -> Update Users User
addUser externaluserid fullname email = do
  users <- get
  userid <- getUnique users UserID
  let user = (User userid [externaluserid] fullname email)
  put (insert user users)
  return user

instance Component Users where
  type Dependencies Users = End
  initialValue = IxSet.empty
  
-- create types for event serialization
$(mkMethods ''Users ['findUserByUserID, 'findUserByExternalUserID, 'addUser])

