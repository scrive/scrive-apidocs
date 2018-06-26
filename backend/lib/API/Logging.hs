module API.Logging where

import Data.Aeson
import Log

import API.APIVersion
import Kontra
import Log.Identifier
import OAuth.Util
import User.Data.User
import User.Utils
import UserGroup.Data

logUserCompanyIPAndApiVersion :: Kontrakcja m => APIVersion -> m a -> m a
logUserCompanyIPAndApiVersion apiversion acc = do
  userandcompanyids <- getMaybeAPIUserWithAnyPrivileges >>= \case
    Nothing -> return []
    Just user -> do
      ug <- getUserGroupForUser user
      return
        [ identifier_ $ userid user
        , identifier_ . get ugID $ ug
        ]
  ctx <- getContext
  let apiversionandip = [identifier_ apiversion, "ip" .= show (get ctxipnumber ctx)]
  localData (userandcompanyids ++ apiversionandip) $ do
    logInfo_ "API call"
    acc
