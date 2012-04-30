module OAuth.View where

import MagicHash
import Misc
import Data.Int
import Templates.Templates
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.UTF8 as BS
import OAuth.Model
import qualified Templates.Fields as F
import User.Model
import User.UserView

import Control.Monad

pagePrivilegesConfirm :: TemplatesMonad m
                      => [APIPrivilege]
                      -> String -- email
                      -> String -- company name
                      -> APIToken
                      -> m String  
pagePrivilegesConfirm privileges email companyname token = do
     renderTemplate "pagePrivilegesConfirm" $ do
         F.value "email" email
         F.value "isDocumentCreate" $ APIDocCreate `elem` privileges
         F.value "companyname" companyname
         F.value "token" $ show token

pagePrivilegesConfirmNoUser :: TemplatesMonad m
                            => [APIPrivilege]
                            -> String -- company name
                            -> APIToken
                            -> m String  
pagePrivilegesConfirmNoUser privileges companyname token = do
     renderTemplate "pagePrivilegesConfirmNoUser" $ do
         F.value "isDocumentCreate" $ APIDocCreate `elem` privileges
         F.value "companyname" companyname
         F.value "token" $ show token

showAPIDashboard :: TemplatesMonad m
                    => User
                    -> [(APIToken, MagicHash)]
                    -> [(Int64, String, [APIPrivilege])]
                    -> Maybe (APIToken, MagicHash, APIToken, MagicHash)
                    -> m String
showAPIDashboard user apitokens apiprivileges mpersonaltoken = do
  renderTemplate "apiDashboard" $ do
    menuFields user
    F.objects "apitokens" $ for apitokens $ \(tok, mh) -> do
      F.value "token" $ show tok
      F.value "secret" $ show mh
    when (not $ null $ apiprivileges) $ do
      F.objects "apiprivileges" $ for apiprivileges $ \(tokenid, name, ps) -> do
        F.value "tokenid" $ show tokenid
        F.value "name" name
        F.valueM "privileges" $ mapM privilegeDescription ps
    case mpersonaltoken of
      Nothing -> return ()
      Just (apitoken, apisecret, tok, mh) ->
        F.object "personaltoken" $ do
          F.value "apitoken" $ show apitoken
          F.value "apisecret" $ show apisecret
          F.value "token" $ show tok 
          F.value "secret" $ show mh
          
privilegeDescription :: TemplatesMonad m => APIPrivilege -> m String
privilegeDescription APIDocCreate = return "Create a document on your behalf."
privilegeDescription APIPersonal  = return "This is a personal access token."