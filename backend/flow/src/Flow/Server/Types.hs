{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
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

import AccessControl.Types
import Flow.Id
import Flow.Names
import Flow.OrphanInstances ()
import Folder.Types (Folder)
import KontraMonad (Kontrakcja)
import User.Types.User (User)
import UserGroup.Internal (UserGroup)

data FlowConfiguration = FlowConfiguration
    { dbConnectionPool :: forall m . (MonadBase IO m, MonadMask m)
        => ConnectionSourceM m
    , flowPort :: Int
    , cryptoRNG :: CryptoRNGState
    , context :: FlowContext
    }

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

data FlowContext = FlowContext
  { handleWithKontra :: (forall m. Kontrakcja m => m Response) -> Request -> CryptoRNGT (DBT (LogT Handler)) Response
  , mainDomainUrl :: Text
  , cdnBaseUrl :: Maybe Text
  , production :: Bool
  }

type AppM = ReaderT FlowContext (CryptoRNGT (DBT (LogT Handler)))

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

data InstanceUser = InstanceUser
    { userName :: UserName
    , instanceId :: InstanceId
    }
  deriving (Generic, Show)

instance ToJSON InstanceUser where
  toJSON = genericToJSON aesonOptions

type instance AuthServerData (AuthProtect "instance-user") = InstanceUser

newtype InstanceUserHTML = InstanceUserHTML InstanceUser

type instance AuthServerData (AuthProtect "instance-user-html") = InstanceUserHTML

type RunLogger = forall m a . LogT m a -> m a
