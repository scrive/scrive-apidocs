{-# OPTIONS_GHC -fno-warn-name-shadowing #-} -- ghc-7.4.1 workaround
module ActionSchedulerState (
      SchedulerData(..)
    , ActionID(..)
    , ActionType(..)
    , ActionTypeID(..)
    , InactiveAccountState(..)
    , ActionImportance(..)
    , Action(..)
    , Actions
    , GetAction(..)
    , GetExpiredActions(..)
    , NewAction(..)
    , UpdateActionType(..)
    , UpdateActionEvalTime(..)
    , DeleteAction(..)
    , actionTypeID
    , actionImportance
    , checkTypeID
    , checkValidity
    , newAccountCreated
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State
import Control.Monad.Reader
import Crypto.RNG (CryptoRNG, random)
import Data.Typeable
import Happstack.Data.IxSet
import Happstack.State
import qualified Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common

import System.Random (randomR, StdGen)
import System.Random.CryptoRNG ()
import Doc.DocStateData
import MagicHash (MagicHash)
import Misc
import MinutesTime
import User.Model

data SchedulerData a b = SchedulerData {
      sdAppConf      :: a
    , sdTemplates    :: b
}

newtype ActionID = ActionID Integer
    deriving (Eq, Ord, Typeable)

instance Show ActionID where
    show (ActionID aid) = show aid

instance FromReqURI ActionID where
    fromReqURI s = ActionID <$> readM s

data ActionType = PasswordReminder {
                      prUserID         :: UserID
                    , prRemainedEmails :: Int
                    , prToken          :: MagicHash
                }
                | ViralInvitationSent { {- This can be dropped with next migration -}
                      visEmail          :: Email
                    , visTime           :: MinutesTime
                    , visInviterID      :: UserID
                    , visRemainedEmails :: Int
                    , visToken          :: MagicHash
                }
                | AccountCreated {
                      acUserID :: UserID
                    , acToken  :: MagicHash
                } --AccountCreatedBySigning is no longer needed
                | AccountCreatedBySigning {
                      acbsState         :: InactiveAccountState
                    , acbsUserID        :: UserID
                    , acbsDocLinkDataID :: (DocumentID, SignatoryLinkID)
                    , acbsToken         :: MagicHash
                }
                | RequestEmailChange {
                      recUser :: UserID
                    , recNewEmail :: Email
                    , recToken :: MagicHash
                }
                | DummyActionType
                  deriving (Eq, Ord, Show, Typeable)

data InactiveAccountState = NothingSent
                          | ReminderSent
                            deriving (Eq, Ord, Show, Typeable)

-- | Used for comparing action types since we can't compare type constructors
data ActionTypeID = PasswordReminderID
                  | ViralInvitationSentID {- This can be dropped with next migration -}
                  | AccountCreatedID
                  | AccountCreatedBySigningID
                  | RequestEmailChangeID
                  | DummyActionTypeID
                    deriving (Eq, Ord, Show, Typeable)

-- | Convert ActionType to its type identifier
actionTypeID :: ActionType -> ActionTypeID
actionTypeID (PasswordReminder _ _ _) = PasswordReminderID
actionTypeID (ViralInvitationSent _ _ _ _ _) = ViralInvitationSentID
actionTypeID (AccountCreated _ _) = AccountCreatedID
actionTypeID (AccountCreatedBySigning _ _ _ _) = AccountCreatedBySigningID
actionTypeID (RequestEmailChange _ _ _) = RequestEmailChangeID
actionTypeID DummyActionType = DummyActionTypeID

-- | Determines how often we should check if there's an action to evaluate
data ActionImportance = UrgentAction
                      | LeisureAction
                        deriving (Eq, Ord, Show, Typeable)

actionImportance :: ActionType -> ActionImportance
actionImportance (PasswordReminder _ _ _) = LeisureAction
actionImportance (ViralInvitationSent _ _ _ _ _) = LeisureAction
actionImportance (AccountCreated _ _) = LeisureAction
actionImportance (AccountCreatedBySigning _ _ _ _) = LeisureAction
actionImportance (RequestEmailChange _ _ _) = LeisureAction
actionImportance DummyActionType = LeisureAction

data Action = Action {
      actionID       :: ActionID
    , actionType     :: ActionType
    , actionEvalTime :: MinutesTime
    } deriving (Eq, Ord, Show)

instance Typeable Action where
    typeOf _ = mkTypeOf "Action"

$(deriveSerialize ''ActionID)
instance Version ActionID

$(deriveSerialize ''ActionImportance)
instance Version ActionImportance

$(deriveSerialize ''Action)
instance Version Action

type Actions = IxSet Action

instance Indexable Action where
    empty = ixSet
        [ ixFun (\a -> [actionID a])
        , ixFun (\a -> [actionEvalTime a])
        , ixFun (\a -> [actionImportance $ actionType a])
        , ixFun (\a -> [actionTypeID $ actionType a])
        , ixFun (\a -> case actionType a of
                            ViralInvitationSent email _ _ _ _ -> [email]
                            _                                 -> [])
        , ixFun (\a -> case actionType a of
                            PasswordReminder uid _ _          -> [uid]
                            ViralInvitationSent _ _ uid _ _   -> [uid]
                            AccountCreated uid _              -> [uid]
                            AccountCreatedBySigning _ uid _ _ -> [uid]
                            RequestEmailChange uid _ _        -> [uid]
                            _                                 -> [])
        ]

instance Component Actions where
    type Dependencies Actions = End
    initialValue = IxSet.empty

-- | Get action by its ID
getAction :: ActionID -> Query Actions (Maybe Action)
getAction aid = return . getOne . (@= aid) =<< ask

-- | Get expired actions
getExpiredActions :: ActionImportance -> MinutesTime -> Query Actions [Action]
getExpiredActions imp now = return . IxSet.toList . (@<= now) . (@= imp) =<< ask

-- | Insert new action
newAction :: StdGen -> ActionType -> MinutesTime -> Update Actions Action
newAction rng atype time = do
    actions <- ask
    let (aid,rng') = first ActionID $ randomR (0, 1000000000) rng
    case getOne $ actions @= aid of
         Just _  -> newAction rng' atype time
         Nothing -> do
             let action = Action aid atype time
             modify $ IxSet.updateIx aid action
             return action

-- | Update action's type. Returns Nothing if there is no action with given id.
updateActionType :: ActionID -> ActionType -> Update Actions (Maybe Action)
updateActionType aid atype = do
    actions <- ask
    case getOne $ actions @= aid of
         Nothing     -> return Nothing
         Just action -> do
             let new_action = action { actionType = atype }
             modify $ IxSet.updateIx aid new_action
             return $ Just new_action

-- | Update action's expiration date. Returns Nothing
-- if there is no action with given id.
updateActionEvalTime :: ActionID -> MinutesTime -> Update Actions (Maybe Action)
updateActionEvalTime aid time = do
    actions <- ask
    case getOne $ actions @= aid of
         Nothing     -> return Nothing
         Just action -> do
             let new_action = action { actionEvalTime = time }
             modify $ IxSet.updateIx aid new_action
             return $ Just new_action

-- | Delete existing action. Returns Nothing if there is
-- no action with given id, otherwise deleted action.
deleteAction :: ActionID -> Update Actions (Maybe Action)
deleteAction aid = do
    actions <- ask
    case getOne $ actions @= aid of
         Nothing     -> return Nothing
         Just action -> do
             modify $ IxSet.deleteIx aid
             return $ Just action

$(mkMethods ''Actions
  [ 'getAction
  , 'getExpiredActions
  , 'newAction
  , 'updateActionType
  , 'updateActionEvalTime
  , 'deleteAction
  ])

-- | Check if action is of given type
checkTypeID :: ActionTypeID -> Maybe Action -> Maybe Action
checkTypeID atypeid maction = maction >>= \action ->
    if actionTypeID (actionType action) == atypeid
       then Just action
       else Nothing

-- | Check if action hasn't expired yet
checkValidity :: MinutesTime -> Maybe Action -> Maybe Action
checkValidity now maction = maction >>= \action ->
    if now < actionEvalTime action
       then Just action
       else Nothing

-- | Create new 'account created' action
newAccountCreated :: (MonadIO m, CryptoRNG m) => User -> m Action
newAccountCreated user = do
    hash <- random
    rng <- random
    now <- getMinutesTime
    let action = AccountCreated {
          acUserID = userid user
        , acToken  = hash
    }
    update $ NewAction rng action $ (24*60) `minutesAfter` now

-- Migrations and old stuff --

$(deriveSerialize ''InactiveAccountState)
instance Version InactiveAccountState where
  mode = extension 1 (Proxy :: Proxy ())

instance Migrate () InactiveAccountState where
  migrate = error "Can't migrate to InactiveAccountState"

$(deriveSerialize ''ActionType)
instance Version ActionType where
  mode = extension 3 (Proxy :: Proxy ())

instance Migrate () ActionType where
  migrate _ = error "Can't migrate to ActionType"
