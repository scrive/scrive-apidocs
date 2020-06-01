{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Flow.Server.Types where

import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Log.Monad (LogT)
import Servant
import Servant.Server.Experimental.Auth

import Flow.OrphanInstances ()
import User.UserID (UserID)
import UserGroup.Internal (UserGroupID)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

data FlowConfiguration = FlowConfiguration
    { dbConnectionPool :: forall m . (MonadBase IO m, MonadMask m)
        => ConnectionSourceM m
    , flowPort :: Int
    }

type AppM = ReaderT FlowConfiguration (LogT (DBT Handler))

data Account = Account
    { userId :: UserID
    , userGroupId :: UserGroupID
    }
  deriving (Generic, Show)

instance ToJSON Account where
  toJSON = genericToJSON aesonOptions

type instance AuthServerData (AuthProtect "oauth") = Account
