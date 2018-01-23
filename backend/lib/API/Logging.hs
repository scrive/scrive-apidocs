module API.Logging where

import Data.Aeson
import Log

import API.APIVersion
import Company.Model
import Kontra
import Log.Identifier
import OAuth.Util
import User.Data.User
import User.Utils

logUserCompanyIPAndApiVersion :: Kontrakcja m => APIVersion -> m a -> m a
logUserCompanyIPAndApiVersion apiversion acc = do
  userandcompanyids <- getMaybeAPIUserWithAnyPrivileges >>= \case
    Nothing -> return []
    Just user -> do
      company <- getCompanyForUser user
      return
        [ identifier_ $ userid user
        , identifier_ $ companyid company
        ]
  ctx <- getContext
  let apiversionandip = [identifier_ apiversion, "ip" .= show (get ctxipnumber ctx)]
  localData (userandcompanyids ++ apiversionandip) acc
