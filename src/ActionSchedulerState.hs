module ActionSchedulerState (
      SchedulerData(..)
    , ActionID(..)
    , ActionType(..)
    , ActionTypeID(..)
    , ActionBackdoorInfo(..)
    , InactiveAccountState(..)
    , ActionImportance(..)
    , Action(..)
    , Actions
    , GetAction(..)
    , GetExpiredActions(..)
    , GetPasswordReminder(..)
    , GetViralInvitationByEmail(..)
    , GetBackdoorInfoByEmail(..)
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

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.Ord
import Data.Typeable
import Happstack.Data.IxSet
import Happstack.State
import System.Random
import qualified Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common

import DB.Types
import Doc.DocState
import Misc
import MinutesTime
import Mails.MailsData
import User.Model
import File.FileID

data SchedulerData a b c = SchedulerData {
      sdAppConf      :: a
    , sdMailer       :: b
    , sdTemplates    :: c
    , sdMailEnforcer :: MVar ()
}

newtype ActionID = ActionID Integer
    deriving (Eq, Ord, Typeable)

instance Show ActionID where
    show (ActionID aid) = show aid

instance FromReqURI ActionID where
    fromReqURI s = ActionID <$> readM s

data ActionBackdoorInfo =
  ActionBackdoorInfo { bdContent :: BS.ByteString
                     } deriving (Eq, Ord, Show, Typeable)

data ActionType0 = TrustWeaverUpload0 {
                      twuOwner0 :: String
                    , twuDocID0 :: DocumentID
                }
                | AmazonUpload0 {
                      auDocID0  :: DocumentID
                    , uaFileID0 :: FileID
                }
                | PasswordReminder0 {
                      prUserID0         :: UserID
                    , prRemainedEmails0 :: Int
                    , prToken0          :: MagicHash
                }
                | ViralInvitationSent0 {
                      visEmail0          :: Email
                    , visTime0           :: MinutesTime
                    , visInviterID0      :: UserID
                    , visRemainedEmails0 :: Int
                    , visToken0          :: MagicHash
                }
                | AccountCreated0 {
                      acUserID0 :: UserID
                    , acToken0  :: MagicHash
                }
                | AccountCreatedBySigning0 {
                      acbsState0         :: InactiveAccountState
                    , acbsUserID0        :: UserID
                    , acbsDocLinkDataID0 :: (DocumentID, SignatoryLinkID)
                    , acbsToken0         :: MagicHash
                }
                | EmailSendout0 {
                      esMail0 :: Mail
                }
                | SentEmailInfo0 {
                      seiEmail0            :: Email
                    , seiMailInfo0         :: MailInfo
                    , seiEventType0        :: SendGridEventType
                    , seiLastModification0 :: MinutesTime
                }
                  deriving (Eq, Ord, Show, Typeable)

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
                | SentEmailInfo {
                      seiEmail            :: Email
                    , seiMailInfo         :: MailInfo
                    , seiEventType        :: SendGridEventType
                    , seiLastModification :: MinutesTime
                    , seiBackdoorInfo     :: Maybe ActionBackdoorInfo
                }
                  deriving (Eq, Ord, Show, Typeable)

instance Migrate ActionType0 ActionType where
  migrate (TrustWeaverUpload0
             { twuOwner0
             , twuDocID0
             }) = TrustWeaverUpload {
                    twuOwner = twuOwner0
                  , twuDocID = twuDocID0
                  }
  migrate (AmazonUpload0
             { auDocID0
             , uaFileID0
             }) = AmazonUpload {
                    auDocID = auDocID0
                  , uaFileID = uaFileID0
                  }
  migrate (PasswordReminder0
             { prUserID0
             , prRemainedEmails0
             , prToken0
             }) = PasswordReminder {
                    prUserID = prUserID0
                  , prRemainedEmails = prRemainedEmails0
                  , prToken = prToken0
                  }
  migrate (ViralInvitationSent0
             { visEmail0
             , visTime0
             , visInviterID0
             , visRemainedEmails0
             , visToken0
             }) = ViralInvitationSent {
                    visEmail = visEmail0
                  , visTime = visTime0
                  , visInviterID = visInviterID0
                  , visRemainedEmails = visRemainedEmails0
                  , visToken = visToken0
                  }
  migrate (AccountCreated0
             { acUserID0
             , acToken0
             }) = AccountCreated {
                    acUserID = acUserID0
                  , acToken = acToken0
                  }
  migrate (AccountCreatedBySigning0
             { acbsState0
             , acbsUserID0
             , acbsDocLinkDataID0
             , acbsToken0
             }) = AccountCreatedBySigning {
                    acbsState = acbsState0
                  , acbsUserID = acbsUserID0
                  , acbsDocLinkDataID = acbsDocLinkDataID0
                  , acbsToken = acbsToken0
                  }
  migrate (EmailSendout0
             { esMail0
             }) = EmailSendout {
                    esMail = esMail0
                  }
  migrate (SentEmailInfo0
             { seiEmail0
             , seiMailInfo0
             , seiEventType0
             , seiLastModification0
             }) = SentEmailInfo {
                    seiEmail = seiEmail0
                  , seiMailInfo = seiMailInfo0
                  , seiEventType = seiEventType0
                  , seiLastModification = seiLastModification0
                  , seiBackdoorInfo = Nothing
                  }

data InactiveAccountState = NothingSent
                          | ReminderSent
                            deriving (Eq, Ord, Show, Typeable)

-- | Used for comparing action types since we can't compare type constructors
data ActionTypeID = TrustWeaverUploadID
                  | AmazonUploadID
                  | PasswordReminderID
                  | ViralInvitationSentID
                  | AccountCreatedID
                  | AccountCreatedBySigningID
                  | EmailSendoutID
                  | SentEmailInfoID
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
actionTypeID (SentEmailInfo _ _ _ _ _) = SentEmailInfoID

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
actionImportance (SentEmailInfo _ _ _ _ _) = LeisureAction

data Action = Action {
      actionID       :: ActionID
    , actionType     :: ActionType
    , actionEvalTime :: MinutesTime
    } deriving (Eq, Ord, Show)

instance Typeable Action where
    typeOf _ = mkTypeOf "Action"

$(deriveSerialize ''ActionID)
instance Version ActionID

$(deriveSerialize ''ActionType0)
instance Version ActionType0

$(deriveSerialize ''ActionType)
instance Version ActionType where
    mode = extension 1 (Proxy :: Proxy ActionType0)

$(deriveSerialize ''ActionBackdoorInfo)
instance Version ActionBackdoorInfo

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
                            SentEmailInfo email _ _ _ _       -> [email]
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

{- |
    Gets the backdoor information for the last email sent.  This can be used
    for running tests.  It shouldn't be used for normal functionality.
-}
getBackdoorInfoByEmail :: Email -> Query Actions (Maybe ActionBackdoorInfo)
getBackdoorInfoByEmail email = do
  return . listToMaybe . catMaybes . map getBackdoorInfo . reverse . sortBy (comparing actionEvalTime) . toList . (@= email) =<< ask
  where
    getBackdoorInfo action =
      case actionType action of
        SentEmailInfo{seiBackdoorInfo} -> seiBackdoorInfo
        _ -> Nothing

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
  , 'getBackdoorInfoByEmail
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

-- | Create new 'password reminder' action
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
          acbsState         = NothingSent
        , acbsUserID        = userid user
        , acbsDocLinkDataID = doclinkdata
        , acbsToken         = hash
    }
    update $ NewAction action $ (24 * 60) `minutesAfter` now

-- | Create new 'email sendout' action
newEmailSendoutAction :: Mail -> IO ()
newEmailSendoutAction mail = do
    now <- getMinutesTime
    let action = EmailSendout {
        esMail = mail
    }
    _ <- update $ NewAction action $ now
    return ()

-- Migrations and old stuff --

data InactiveAccountState0 = JustCreated
                          | FirstReminderSent
                          | SecondReminderSent
                          | ThirdReminderSent
                            deriving (Eq, Ord, Show, Typeable)

$(deriveSerialize ''InactiveAccountState0)
instance Version InactiveAccountState0

$(deriveSerialize ''InactiveAccountState)
instance Version InactiveAccountState where
    mode = extension 1 (Proxy :: Proxy InactiveAccountState0)

instance Migrate InactiveAccountState0 InactiveAccountState where
    migrate JustCreated = NothingSent
    migrate FirstReminderSent = ReminderSent
    migrate SecondReminderSent = ReminderSent
    migrate ThirdReminderSent = ReminderSent
