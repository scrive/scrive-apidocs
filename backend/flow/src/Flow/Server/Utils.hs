module Flow.Server.Utils
  ( mkAccessLinks
  , mkBaseUrl
  , mkInstanceOverviewUrl
  ) where

import Data.Map (Map)
import Servant.API
import qualified Data.Map as Map
import qualified Data.Text as T

import Auth.MagicHash
import Flow.Core.Type.Url
import Flow.Id
import Flow.Names
import Flow.Routes.Types

mkAccessLinks :: Text -> InstanceId -> [(UserName, MagicHash)] -> Map UserName Url
mkAccessLinks baseUrl instanceId = Map.fromList . fmap (\(u, h) -> (u, mkAccessLink u h))
  where
    mkAccessLink userName hash = Url $ T.intercalate
      "/"
      [ baseUrl
      , flowPath
      , "overview"
      , toUrlPiece instanceId
      , toUrlPiece userName
      , toUrlPiece hash
      ]

-- | We use the base URL for both Flow and Kontrakcja, since the services
-- are meant to be run behind a proxy, with their ports hidden.
--
-- To make this happen locally, you can run a simple
-- nginx server (see `flow-nginx.conf`).
--
-- We don't use a proxy in the automated tests, so there
-- it's up to clever approaches / hacks to make things work
-- when interacting with both services at the same time.
mkBaseUrl :: Text -> Bool -> Maybe Host -> Text
mkBaseUrl mainDomainUrl isSecure = \case
  Nothing   -> mainDomainUrl
  Just host -> protocol <> host
  where protocol = if isSecure then "https://" else "http://"

mkInstanceOverviewUrl :: InstanceId -> UserName -> Text
mkInstanceOverviewUrl instanceId userName = "/" <> T.intercalate
  "/"
  [flowPath, "overview", toUrlPiece instanceId, toUrlPiece userName]
