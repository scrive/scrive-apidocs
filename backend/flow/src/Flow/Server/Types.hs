module Flow.Server.Types
  ( Account(..)
  , AppM
  , FlowConfiguration(..)
  , FlowContext(..)
  , InstanceUser(..)
  , InstanceUserHTML(..)
  , RunLogger
  , aesonOptions
  )
where

import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Happstack.Server (Request, Response)
import Log.Monad (LogT)
import Servant
import Servant.Server.Experimental.Auth
import qualified Text.StringTemplates.TemplatesLoader as TL

import AccessControl.Types
import Auth.Session.SessionID
import EventStream.Kinesis
import Flow.Id
import Flow.Names
import Flow.OrphanInstances ()
import Folder.Types (Folder)
import KontraMonad (Kontrakcja)
import User.Types.User (User)
import UserGroup.Internal (UserGroup)

data FlowConfiguration = FlowConfiguration
    { dbConnectionPool :: forall m . (MonadBase IO m, MonadMask m) => ConnectionSourceM m
    , flowPort :: Int
    , cryptoRNG :: CryptoRNGState
    , context :: FlowContext
    }

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

type InternalM = KinesisT (CryptoRNGT (DBT (LogT Handler)))
type AppM = ReaderT FlowContext InternalM

data FlowContext = FlowContext
  { handleWithKontra :: (forall m. Kontrakcja m => m Response) -> Request -> InternalM Response
  , mainDomainUrl :: Text
  , cdnBaseUrl :: Maybe Text
  , production :: Bool
  , templates :: TL.GlobalTemplates
  , kinesisStream :: Maybe KinesisConf
  }

data Account = Account
    { user :: User
    , userGroup :: UserGroup
    , folder :: Folder
    , roles :: [AccessRole]
    -- TODO move this into a separate context type
    , baseUrl :: Text
    }
  deriving (Generic, Show)

instance ToJSON Account where
  toJSON Account {..} = object
    [ "user_id" .= view #id user
    , "user_group_id" .= view #id userGroup
    , "folder_id" .= view #id folder
    ]

type instance AuthServerData (AuthProtect "account") = Account

-- TODO: Get rid of the InstanceUser. We just need to pass the SessionID to
-- authenticated endpoints. UserName/InstanceId should be URL parameters
-- to those endpoints.
data InstanceUser = InstanceUser
    { userName :: UserName
    , instanceId :: InstanceId
    , sessionId :: SessionID
    }
  deriving (Generic, Show)

instance ToJSON InstanceUser where
  toJSON = genericToJSON aesonOptions

type instance AuthServerData (AuthProtect "instance-user") = InstanceUser

newtype InstanceUserHTML = InstanceUserHTML InstanceUser

type instance AuthServerData (AuthProtect "instance-user-html") = InstanceUserHTML

type RunLogger = forall m a . LogT m a -> m a
