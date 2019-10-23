module PadApplication.API (
    padApplicationAPI
  , apiCallGetPadInfo
  ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Unjson as Unjson
import Database.PostgreSQL.PQTypes.SQL.Builder
import Happstack.Server.Types
import Happstack.StaticRouting
import Text.JSON.Gen hiding (object)
import Text.JSON.Types (JSValue(JSNull))

import API.Monad.V1
import AppView
import BrandedDomain.BrandedDomain
import DB.Query
import Happstack.Fields
import Kontra
import PadApplication.Types
import Routing
import Theme.Model
import Theme.View
import User.Types.User
import UserGroup.Model
import UserGroup.Types
import qualified API.V2 as V2

padApplicationAPI :: Route (Kontra Response)
padApplicationAPI = dir "api" $ choice
  [ dir "frontend" $ padApplicationAPIV2
  , padApplicationAPIV1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ padApplicationAPIV1
  , dir "v2" $ padApplicationAPIV2
  ]

padApplicationAPIV1 :: Route (Kontra Response)
padApplicationAPIV1 = choice
  [ dir "checkclient" $ hPostNoXTokenHttp $ toK0 $ apiCallCheckClient
  , dir "padclienttheme" $ hGet $ toK0 $ apiCallGetPadClientTheme
  ]

padApplicationAPIV2 :: Route (Kontra Response)
padApplicationAPIV2 =
  choice [dir "pad" $ dir "info" $ hGet $ toK0 $ apiCallGetPadInfo, padApplicationAPIV1]

apiCallCheckClient :: Kontrakcja m => m Response
apiCallCheckClient = api $ do
  mclient <- getField "client"
  when (isNothing mclient) $ do
    throwM . SomeDBExtraException $ serverError "No client description"
  runJSONGenT $ do
    value "valid"           True
    value "upgrade_warning" False
    value "valid_until"     JSNull
    value "upgrade_url"     JSNull
    value "message"         JSNull

apiCallGetPadClientTheme :: Kontrakcja m => m Response
apiCallGetPadClientTheme = api $ do
  ctx          <- getContext
  (user, _, _) <- getAPIUserWithAnyPrivileges
  ug           <- dbQuery . UserGroupGetByUserID . userid $ user
  theme <- dbQuery $ GetTheme $ fromMaybe (get (bdSignviewTheme . ctxbrandeddomain) ctx)
                                          (get (uguiSignviewTheme . ugUI) ug)
  simpleAesonResponse $ Unjson.unjsonToJSON'
    (Options { pretty = True, indent = 2, nulls = True })
    unjsonTheme
    theme

apiCallGetPadInfo :: Kontrakcja m => m Response
apiCallGetPadInfo = V2.api $ do
  (user, _, _) <- getAPIUserWithAnyPrivileges
  ugwp         <- dbQuery . UserGroupGetWithParentsByUserID . userid $ user
  return $ V2.Ok $ object
    [ "app_mode" .= (padAppModeText . get ugsPadAppMode . ugwpSettings $ ugwp)
    , "e_archive_enabled" .= (get ugsPadEarchiveEnabled . ugwpSettings $ ugwp)
    ]
