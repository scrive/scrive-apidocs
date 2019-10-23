module User.APILog.API (
    apiLogAPI
  , apiLogGetList
  , apiLogGetItem
  ) where

import Control.Monad.Catch
import Happstack.Server.Types
import Happstack.StaticRouting

import API.V2.User
import DB
import Kontra
import Routing
import User.APILog.Model
import User.Model
import qualified API.V2 as V2

apiLogAPI :: Route (Kontra Response)
apiLogAPI = dir "api" $ dir "frontend" $ dir "apilog" $ choice
  [ dir "list" $ hGet $ toK0 $ apiLogGetList
  , param $ dir "get" $ hGet $ toK1 $ apiLogGetItem
  ]

apiLogGetList :: Kontrakcja m => m Response
apiLogGetList = V2.api $ do
  (user, _) <- getAPIUserWithAnyPrivileges
  clis      <- dbQuery . GetCallLogList $ userid user
  return $ V2.Ok (unjsonCallLogListForAPI, clis)

apiLogGetItem :: Kontrakcja m => CallLogID -> m Response
apiLogGetItem clid = V2.api $ do
  (user, _) <- getAPIUserWithAnyPrivileges
  cli       <- dbQuery $ GetCallLogItem clid
  when (cliUserID cli /= userid user) $ throwM $ SomeDBExtraException $ V2.serverError
    "Cannot access this call log"
  return $ V2.Ok (unjsonCallLogItem, cli)
