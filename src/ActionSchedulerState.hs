{-# OPTIONS_GHC -Wall #-}

module ActionSchedulerState (
      SchedulerData(..)
    , ActionID
    , ActionType(..)
    , ActionTypeID(..)
    , InactiveAccountState(..)
    , ActionImportance(..)
    , Action(..)
    , Actions
    , GetAction(..)
    , GetExpiredActions(..)
    , GetPasswordReminder(..)
    , GetViralInvitationByEmail(..)
    , NewAction(..)
    , UpdateActionType(..)
    , UpdateActionEvalTime(..)
    , DeleteAction(..)
    , actionTypeID
    , actionImportance
    , checkTypeID
    , checkValidity
    , newPasswordReminder
    , newViralInvitationSent
    , newAccountCreated
    , newAccountCreatedBySigning
    , newEmailSendoutAction
    ) where

import Control.Applicative ((<$>))
import Control.Monad.State (modify)
import Control.Monad.Reader (ask)
import Data.Typeable
import Data.Word
import Happstack.Data.IxSet (Indexable, IxSet, getOne, ixFun, ixSet, (@=), (@<=), (@>))
import Happstack.State
import System.Random (randomIO)
import qualified Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP (FromReqURI(..))
import Happstack.Util.Common (readM)

import Doc.DocState
import Misc
import MinutesTime
import Mails.SendMail
import User.UserState

data SchedulerData a b c = SchedulerData {
      sdAppConf   :: a
    , sdMailer    :: b
    , sdTemplates :: c
}

newtype ActionID = ActionID Integer
    deriving (Eq, Ord, Typeable)

instance Show ActionID where
    show (ActionID aid) = show aid

instance FromReqURI ActionID where
    fromReqURI s = ActionID <$> readM s

data ActionType = TrustWeaverUpload {
                      twuOwner :: String
                    , twuDocID :: DocumentID
                }
                | AmazonUpload {
                      auDocID  :: DocumentID
                    , uaFileID :: FileID
                }
                | PasswordReminder {
                      prUserID         :: UserID
                    , prRemainedEmails :: Int
                    , prToken          :: MagicHash
                }
                | ViralInvitationSent {
                      visEmail          :: Email
                    , visTime           :: MinutesTime
                    , visInviterID      :: UserID
                    , visRemainedEmails :: Int
                    , visToken          :: MagicHash
                }
                | AccountCreated {
                      acUserID :: UserID
                    , acToken  :: MagicHash
                }
                | AccountCreatedBySigning {
                      acbsState         :: InactiveAccountState
                    , acbsUserID        :: UserID
                    , acbsDocLinkDataID :: (DocumentID, SignatoryLinkID)
                    , acbsToken         :: MagicHash
                }
                | EmailSendout {
                      esMail :: Mail
                }
                  deriving (Eq, Ord, Show, Typeable)

data InactiveAccountState = JustCreated
                          | FirstReminderSent
                          | SecondReminderSent
                          | ThirdReminderSent
                            deriving (Eq, Ord, Show, Typeable)

-- | Used for comparing action types since we can't compare type constructors
data ActionTypeID = TrustWeaverUploadID
                  | AmazonUploadID
                  | PasswordReminderID
                  | ViralInvitationSentID
                  | AccountCreatedID
                  | AccountCreatedBySigningID
                  | EmailSendoutID
                    deriving (Eq, Ord, Show, Typeable)

-- | Convert ActionType to its type identifier
actionTypeID :: ActionType -> ActionTypeID
actionTypeID (TrustWeaverUpload _ _) = TrustWeaverUploadID
actionTypeID (AmazonUpload _ _) = AmazonUploadID
actionTypeID (PasswordReminder _ _ _) = PasswordReminderID
actionTypeID (ViralInvitationSent _ _ _ _ _) = ViralInvitationSentID
actionTypeID (AccountCreated _ _) = AccountCreatedID
actionTypeID (AccountCreatedBySigning _ _ _ _) = AccountCreatedBySigningID
actionTypeID (EmailSendout _) = EmailSendoutID

-- | Determines how often we should check if there's an action to evaluate
data ActionImportance = UrgentAction
                      | LeisureAction
                      | EmailSendoutAction
                        deriving (Eq, Ord, Show, Typeable)

actionImportance :: ActionType -> ActionImportance
actionImportance (TrustWeaverUpload _ _) = UrgentAction
actionImportance (AmazonUpload _ _) = UrgentAction
actionImportance (PasswordReminder _ _ _) = LeisureAction
actionImportance (ViralInvitationSent _ _ _ _ _) = LeisureAction
actionImportance (AccountCreated _ _) = LeisureAction
actionImportance (AccountCreatedBySigning _ _ _ _) = LeisureAction
actionImportance (EmailSendout _) = EmailSendoutAction

data Action = Action {
      actionID       :: ActionID
    , actionType     :: ActionType
    , actionEvalTime :: MinutesTime
    } deriving (Eq, Ord, Show)

instance Typeable Action where
    typeOf _ = mkTypeOf "Action"

$(deriveSerialize ''ActionID)
instance Version ActionID

$(deriveSerialize ''ActionType)
instance Version ActionType

$(deriveSerialize ''InactiveAccountState)
instance Version InactiveAccountState

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

-- | Get password reminder action by user id
getPasswordReminder :: UserID -> Query Actions (Maybe Action)
getPasswordReminder uid =
    return . getOne . (@= uid) . (@= PasswordReminderID) =<< ask

-- | Get viral invitation action by invited person's email address
getViralInvitationByEmail :: Email -> Query Actions (Maybe Action)
getViralInvitationByEmail email =
    return . getOne . (@= email) . (@= ViralInvitationSentID) =<< ask

-- | Insert new action
newAction :: ActionType -> MinutesTime -> Update Actions Action
newAction atype time = do
    actions <- ask
    aid <- ActionID <$> getRandomR (0, 1000000000)
    case getOne $ actions @= aid of
         Just _  -> newAction atype time
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
  , 'getPasswordReminder
  , 'getViralInvitationByEmail
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

-- | Create new 'password remainder' action
newPasswordReminder :: User -> IO Action
newPasswordReminder user = do
    hash <- randomIO
    now <- getMinutesTime
    let action = PasswordReminder {
          prUserID         = userid user
        , prRemainedEmails = 9
        , prToken          = hash
    }
    update $ NewAction action $ (12*60) `minutesAfter` now

-- | Create new 'invitation sent' action
newViralInvitationSent :: Email -> UserID -> IO Action
newViralInvitationSent email inviterid = do
    hash <- randomIO
    now <- getMinutesTime
    let action = ViralInvitationSent {
          visEmail          = email
        , visTime           = now
        , visInviterID      = inviterid
        , visRemainedEmails = 9
        , visToken          = hash
    }
    update $ NewAction action $ (7*24*60) `minutesAfter` now

-- | Create new 'account created' action
newAccountCreated :: User -> IO Action
newAccountCreated user = do
    hash <- randomIO
    now <- getMinutesTime
    let action = AccountCreated {
          acUserID = userid user
        , acToken  = hash
    }
    update $ NewAction action $ (24*60) `minutesAfter` now

-- | Create new 'account created by signing' action
newAccountCreatedBySigning :: User -> (DocumentID, SignatoryLinkID) -> IO Action
newAccountCreatedBySigning user doclinkdata = do
    hash <- randomIO
    now <- getMinutesTime
    let action = AccountCreatedBySigning {
          acbsState         = JustCreated
        , acbsUserID        = userid user
        , acbsDocLinkDataID = doclinkdata
        , acbsToken         = hash
    }
    update $ NewAction action $ (24*60) `minutesAfter` now

-- | Create new 'email sendout' action
newEmailSendoutAction :: Mail -> IO ()
newEmailSendoutAction mail = do
    now <- getMinutesTime
    let action = EmailSendout {
        esMail = mail
    }
    _ <- update $ NewAction action $ now
    return ()
