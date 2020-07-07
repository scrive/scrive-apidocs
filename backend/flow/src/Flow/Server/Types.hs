{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server.Types where

import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Data.ByteString
import Data.CaseInsensitive
import Data.Either.Combinators (mapLeft)
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Happstack.Server (Request, Response)
import Log.Monad (LogT)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Server.Experimental.Auth
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

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
    , handleWithKontra :: (forall m. Kontrakcja m => m Response) -> Request -> CryptoRNGT (DBT (LogT Handler)) Response
    }

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

newtype FlowContext = FlowContext
  { handleWithKontra :: (forall m. Kontrakcja m => m Response) -> Request -> CryptoRNGT (DBT (LogT Handler)) Response
  }

type AppM = ReaderT FlowContext (CryptoRNGT (DBT (LogT Handler)))

data Account = Account
    { user :: User
    , userGroup :: UserGroup
    , folder :: Folder
    , roles :: [AccessRole]
    -- TODO remove this and implement it by adding Header input to the `startInstance` handler.
    -- It is needed because oauth and cookie headers have to be passed to the document starting API.
    , headers :: [(CI ByteString, ByteString)]
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
