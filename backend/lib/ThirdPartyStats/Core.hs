{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Backend API for logging events to be sent off to a third party.
module ThirdPartyStats.Core (
    EventProperty (..),
    PropValue (..),
    ProcRes (..),
    EventProcessor(..),
    NumEvents (..),
    EventName (..),
    EventType (..),
    AsyncEvent,
    someProp,
    numProp,
    stringProp,
    asyncLogEvent,
    asyncProcessEvents,
    (@@),
    catEventProcs
  ) where

import Control.Monad.IO.Class
import Data.Binary as B
import Data.Int
import Data.String
import Data.Time.Clock.POSIX
import Log
import Test.QuickCheck (Arbitrary(..), Gen, frequency, oneof, suchThat)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Semigroup as SG
import qualified Data.Text as T

import DB
import Doc.DocumentID (DocumentID, unsafeDocumentID)
import IPAddress
import MinutesTime ()
import Text.JSON.Orphans ()
import User.Email
import User.UserID (UserID, unsafeUserID)
import UserGroup.Types

-- | The various types of values a property can take.
data PropValue
  = PVNumber      Double
  | PVString      Text
  | PVBool        Bool
  | PVUTCTime UTCTime
    deriving (Show, Eq)

instance Binary PropValue where
  put (PVNumber  d) = putWord8 0 >> put d
  put (PVString  s) = putWord8 1 >> put s
  put (PVBool    b) = putWord8 2 >> put b
  put (PVUTCTime t) = putWord8 3 >> put t

  get = do
    tag <- getWord8
    case tag of
      0 -> PVNumber <$> B.get
      1 -> PVString <$> B.get
      2 -> PVBool <$> B.get
      3 -> PVUTCTime <$> B.get
      n -> fail $ "Couldn't parse PropValue constructor tag: " <> show n

-- | Type class to keep the user from having to wrap stuff in annoying data
--   constructors.
class SomeProperty a where
  someProp :: PropName -> a -> EventProperty

instance SomeProperty String where
  someProp s = SomeProp s . PVString . T.pack

instance SomeProperty Text where
  someProp s = SomeProp s . PVString

instance SomeProperty Double where
  someProp s = SomeProp s . PVNumber

instance SomeProperty Bool where
  someProp s = SomeProp s . PVBool

instance SomeProperty UTCTime where
  someProp s = SomeProp s . PVUTCTime


-- | Create a named property with a Double value.
--   This function just fixes the property type of someProp to avoid
--   unnecessary type annotations due to numeric literals being overloaded.
numProp :: PropName -> Double -> EventProperty
numProp = someProp

-- | Create a named property with a String value.
--   This function just fixes the property type of someProp to avoid
--   unnecessary type annotations in the presence of overloaded strings.
stringProp :: PropName -> Text -> EventProperty
stringProp = someProp

-- | Distinguish between the event processor functions to use.
data EventType
  = EventMixpanel
  | EventPlanhat
  deriving (Eq, Show)

instance Binary EventType where
  put EventMixpanel = putWord8 0
  put EventPlanhat  = putWord8 1

  get = do
    tag <- getWord8
    case tag of
      0 -> return EventMixpanel
      1 -> return EventPlanhat
      n -> fail $ "Couldn't parse EventType constructor tag: " <> show n

-- | Makes type signatures on functions involving event names look nicer.
data EventName
  = SetUserProps
  | NamedEvent Text
  | SetCompanyProps
    deriving (Show, Eq)
type PropName = Text

instance IsString EventName where
  fromString = NamedEvent . T.pack

instance Binary EventName where
  put SetUserProps      = putWord8 0
  put SetCompanyProps   = putWord8 1
  put (NamedEvent name) = putWord8 255 >> put name

  get = do
    tag <- getWord8
    case tag of
      0   -> return SetUserProps
      1   -> return SetCompanyProps
      255 -> NamedEvent <$> B.get
      t   -> fail $ "Unable to parse EventName constructor tag: " <> show t


-- | Represents a property on an event.
data EventProperty
  = MailProp      Email
  | IPProp        IPAddress
  | NameProp      Text
  | UserIDProp    UserID
  | TimeProp      UTCTime
  | DocIDProp     DocumentID
  | UserGroupIDProp UserGroupID
  | FirstNameProp Text
  | LastNameProp  Text
  | SomeProp      PropName PropValue
    deriving (Show, Eq)


-- | The scheme here is to have generic properties at ID 255 and give any
--   special properties IDs starting at 0. Obviously.
instance Binary EventProperty where
  put (MailProp        mail) = putWord8 0 >> put (unEmail mail)
  put (IPProp          ip  ) = putWord8 1 >> put ip
  put (NameProp        name) = putWord8 2 >> put name
  put (UserIDProp      uid ) = putWord8 3 >> put uid
  put (TimeProp        t   ) = putWord8 4 >> put t
  put (DocIDProp       did ) = putWord8 5 >> put did
  put (UserGroupIDProp cid ) = putWord8 6 >> put cid
  put (FirstNameProp   n   ) = putWord8 7 >> put n
  put (LastNameProp    n   ) = putWord8 8 >> put n
  put (SomeProp name val   ) = putWord8 255 >> put name >> put val

  get = do
    tag <- getWord8
    case tag of
      0   -> MailProp . Email <$> B.get
      1   -> IPProp <$> B.get
      2   -> NameProp <$> B.get
      3   -> UserIDProp <$> B.get
      4   -> TimeProp <$> B.get
      5   -> DocIDProp <$> B.get
      6   -> UserGroupIDProp <$> B.get
      7   -> FirstNameProp <$> B.get
      8   -> LastNameProp <$> B.get
      255 -> SomeProp <$> B.get <*> B.get
      n   -> fail $ "Couldn't parse EventProperty constructor tag: " <> show n


-- | Data type representing an asynchronous event.
data AsyncEvent = AsyncEvent EventName [EventProperty] EventType deriving (Show, Eq)

instance Binary AsyncEvent where
  put (AsyncEvent ename eprops etype) = put ename >> put eprops >> put etype
  get = AsyncEvent <$> B.get <*> B.get <*> B.get


-- | Denotes how many events should be processed.
data NumEvents = All | NoMoreThan Integer


-- | Indicates how the processing of an event fared.
data ProcRes
  = OK             -- ^ Processing succeeded, we're done with this event.
  | PutBack        -- ^ Processing failed, but may succeed if retried later.
  | Failed Text  -- ^ Processing failed permanently, discard event and
                   --   log the failure.
  | Ignored Text -- ^ Processing ignored and regarded as done after logging

newtype EventProcessor m =
  EventProcessor
  { unEventProcessor :: EventName -> [EventProperty] -> m ProcRes }

-- | Combine two event processors into one. The first event processor is always
--   executed. If it fails, the second is not. Unfortunately, there is no way
--   to rollback an event processor that already succeeded, so if the second
--   event processor returns PutBack, the aggregate event processor will return
--   Failed rather than PutBack.
--   The operator is kind of ugly, but all the good ones were already taken by
--   Arrow, Applicative, etc.
(@@) :: Monad m => EventProcessor m -> EventProcessor m -> EventProcessor m
EventProcessor a @@ EventProcessor b = EventProcessor $ \evt props -> do
  res <- a evt props
  case res of
    OK -> do
      res' <- b evt props
      case res' of
        PutBack ->
          return
            (Failed $ "PutBack after one or more event " <> "processors already succeded!"
            )
        x -> return x
    PutBack -> return PutBack
    x       -> return x

-- | Concatenate a list of event processors.
catEventProcs :: Monad m => [EventProcessor m] -> EventProcessor m
catEventProcs = mconcat

instance Monad m => SG.Semigroup (EventProcessor m) where
  (<>) = (@@)

instance Monad m => Monoid (EventProcessor m) where
  mempty  = EventProcessor $ \_ _ -> return OK
  mappend = (SG.<>)
  mconcat = catEventProcs

-- | Remove a number of events from the queue and process them.
asyncProcessEvents
  :: (MonadIO m, MonadLog m, MonadDB m)
  => (EventType -> Maybe (EventProcessor m))
                      -- ^ Event processing function mapper.
  -> NumEvents
                      -- ^ Max events to process.
  -> m ()
asyncProcessEvents getEventProcessor numEvts = do
  (evts, lastEvt) <- fetchEvents
  mapM_ processEvent evts
  deleteEvents lastEvt
  where
    processEvent (AsyncEvent ename eprops etype) = do
      result <- case getEventProcessor etype of
        Nothing      -> return . Ignored $ "No event processor defined"
        Just process -> unEventProcessor process ename eprops
      case result of
        PutBack    -> asyncLogEvent ename eprops etype
        Failed msg -> logInfo "Event processing failed" $ object
          ["event_name" .= show ename, "event_type" .= show etype, "reason" .= msg]
        Ignored msg -> logInfo "Event processing ignored " $ object
          ["event_name" .= show ename, "event_type" .= show etype, "reason" .= msg]
        _ -> return ()

    decoder (evts, max_seq) (seqnum, evt) =
      return (decode (BL.fromChunks [evt]) : evts, max seqnum max_seq)

    -- Delete all events with a sequence number less than or equal to lastEvt.
    deleteEvents lastEvt = do
      runQuery_ . sqlDelete "async_event_queue" $ do
        sqlWhere $ "sequence_number <=" <?> lastEvt

    -- Fetch events from database and turn them into a pair of
    -- (events, highest sequence number in fetched list).
    fetchEvents = do
      runQuery_ . sqlSelect "async_event_queue" $ do
        sqlResult "sequence_number"
        sqlResult "event::bytea"
        sqlOrderBy "sequence_number ASC"
        case numEvts of
          NoMoreThan n -> sqlLimit n
          _            -> return ()
      foldlDB decoder ([], 0 :: Int64)


-- | Send a message off to the async queue for later processing.
--   The event must have a name and zero or more named properties, some of
--   which, such as email address or signup time, may be "special", in the
--   sence that third party services may be able to do interesting things to
--   them since they have more contextual knowledge about them.
--
--   "Special" properties should be identified using the proper `EventProperty`
--   constructor, whereas others may be arbitrarily named using `someProp`.
asyncLogEvent :: (MonadDB m) => EventName -> [EventProperty] -> EventType -> m ()
asyncLogEvent ename eprops etype = do
  runQuery_ . sqlInsert "async_event_queue" $ do
    sqlSet "event" . mkBinary $ AsyncEvent ename eprops etype
  where mkBinary = B.concat . BL.toChunks . encode

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

instance Arbitrary EventName where
  arbitrary = frequency
    [ (1, return SetUserProps)
    , (2, return SetCompanyProps)
    , (8, NamedEvent <$> arbitraryText)
    ]

instance Arbitrary EventType where
  arbitrary = oneof [return EventMixpanel, return EventPlanhat]


instance Arbitrary PropValue where
  arbitrary = oneof
    [ PVNumber <$> arbitrary
    , PVString <$> arbitraryText
    , PVBool <$> arbitrary
    , PVUTCTime . posixSecondsToUTCTime . fromInteger <$> arbitrary
    ]

-- Note that IP addresses are completely arbitrary 32 bit words here!
instance Arbitrary EventProperty where
  arbitrary = frequency
    [ (1, MailProp <$> email)
    , (1, IPProp . unsafeIPAddress <$> arbitrary)
    , (1, NameProp <$> arbitraryText)
    , (1, LastNameProp <$> arbitraryText)
    , (1, FirstNameProp <$> arbitraryText)
    , (1, UserIDProp . unsafeUserID <$> arbitrary)
    , (1, TimeProp . posixSecondsToUTCTime . fromInteger <$> arbitrary)
    , (1, DocIDProp . unsafeDocumentID <$> arbitrary)
    , (1, UserGroupIDProp . unsafeUserGroupID <$> arbitrary)
    , (5, SomeProp <$> arbitraryText <*> arbitrary)
    ]
    where
      email = do
        acct     <- arbitraryText `suchThat` ((> 5) . T.length)
        domain   <- arbitraryText `suchThat` ((> 5) . T.length)
        toplevel <- oneof (map return ["com", "net", "org", "nu", "se"])
        return $! Email $! T.concat [acct, "@", domain, ".", toplevel]

instance Arbitrary AsyncEvent where
  arbitrary = AsyncEvent <$> arbitrary <*> arbitrary <*> arbitrary
