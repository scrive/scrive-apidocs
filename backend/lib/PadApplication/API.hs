module PadApplication.API (
    padApplicationAPI
  ) where

import Control.Monad.Catch
import Data.Unjson as Unjson
import Database.PostgreSQL.PQTypes.SQL.Builder
import Happstack.Server.Types
import Happstack.StaticRouting
import Text.JSON.Gen
import Text.JSON.Types (JSValue(JSNull))

import API.Monad.V1
import AppView
import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Company.Model
import DB.Query
import Happstack.Fields
import Kontra
import KontraPrelude
import Routing
import Theme.Model
import Theme.View
import User.Utils

padApplicationAPI :: Route (Kontra Response)
padApplicationAPI = dir "api" $ choice
  [ dir "frontend" $ padApplicationAPI'
  , padApplicationAPI' -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ padApplicationAPI'
  , dir "v2" $ padApplicationAPI'
  ]

padApplicationAPI' :: Route (Kontra Response)
padApplicationAPI' = choice
  [ dir "checkclient"     $ hPostNoXTokenHttp $ toK0 $ apiCallCheckClient
  , dir "padclienttheme"     $ hGet $ toK0 $ apiCallGetPadClientTheme
  ]


apiCallCheckClient :: Kontrakcja m => m Response
apiCallCheckClient = api $ do
    mclient  <- getField "client"
    when (isNothing mclient) $ do
      throwM . SomeDBExtraException $ serverError "No client description"
    runJSONGenT $ do
      value "valid" True
      value "upgrade_warning" False
      value "valid_until" JSNull
      value "upgrade_url" JSNull
      value "message" JSNull

apiCallGetPadClientTheme :: Kontrakcja m => m Response
apiCallGetPadClientTheme = api $ do
  ctx <- getContext
  (user, _ , _) <- getAPIUserWithAnyPrivileges
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  theme <- dbQuery $ GetTheme $ fromMaybe (bdSignviewTheme $ ctxbrandeddomain ctx) (companySignviewTheme $ companyui)
  simpleAesonResponse $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonTheme theme
