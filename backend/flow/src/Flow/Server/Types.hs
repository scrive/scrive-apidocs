{-# LANGUAGE StrictData #-}

module Flow.Server.Types where

import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Data.Either.Combinators (mapLeft)
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Log.Monad (LogT)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Server.Experimental.Auth
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

import AccessControl.Types
import Flow.Id
import Flow.OrphanInstances ()
import Flow.Names
import Folder.Types (Folder)
import User.Types.User (User)
import UserGroup.Internal (UserGroup)

data FlowConfiguration = FlowConfiguration
    { dbConnectionPool :: forall m . (MonadBase IO m, MonadMask m)
        => ConnectionSourceM m
    , flowPort :: Int
    , cryptoRNG :: CryptoRNGState
    }

type AppM = ReaderT FlowConfiguration (CryptoRNGT (LogT (DBT Handler)))

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

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

type instance AuthServerData (AuthProtect "account") = Account

data InstanceUser = InstanceUser
    { userName :: UserName
    , instanceId :: InstanceId
    }
  deriving (Generic, Show)

instance ToJSON InstanceUser where
  toJSON = genericToJSON aesonOptions

type instance AuthServerData (AuthProtect "instance-user") = InstanceUser

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ t = BSL.fromStrict $ T.encodeUtf8 t

instance MimeUnrender HTML Text where
  mimeUnrender _ bs = mapLeft show (T.decodeUtf8' $ BSL.toStrict bs)

type Get302 contentTypes a = Verb 'GET 302 contentTypes a

type Host = Text
