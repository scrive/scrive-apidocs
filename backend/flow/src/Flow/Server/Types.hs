{-# LANGUAGE StrictData #-}

module Flow.Server.Types where

import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Data.Aeson
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Log.Monad (LogT)
import Servant
import Servant.Server.Experimental.Auth

import AccessControl.Types
import Flow.OrphanInstances ()
import Folder.Types (Folder)
import User.Types.User (User)
import UserGroup.Internal (UserGroup)

data FlowConfiguration = FlowConfiguration
    { dbConnectionPool :: forall m . (MonadBase IO m, MonadMask m)
        => ConnectionSourceM m
    , flowPort :: Int
    }

type AppM = ReaderT FlowConfiguration (LogT (DBT Handler))

data Account = Account
    { user :: User
    , userGroup :: UserGroup
    , folder :: Folder
    , roles :: [AccessRole]
    }
  deriving (Generic, Show)

instance ToJSON Account where
  toJSON Account {..} = object
    [ "user_id" .= view #id user
    , "user_group_id" .= view #id userGroup
    , "folder_id" .= view #id folder
    ]

type instance AuthServerData (AuthProtect "oauth-or-cookies") = Account
