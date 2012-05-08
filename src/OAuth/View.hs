module OAuth.View where

import MagicHash
import Misc
import Data.Int
import Templates.Templates
import OAuth.Model
import qualified Templates.Fields as F
import User.Model
import User.UserView
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import Text.JSON

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

showAPIDashboard :: TemplatesMonad m => User -> m String
showAPIDashboard user = do
  renderTemplate "apiDashboard" $ do
    menuFields user
          
privilegeDescription :: TemplatesMonad m => APIPrivilege -> m String
privilegeDescription APIDocCreate = return "Create a document on your behalf."
privilegeDescription APIPersonal  = return "This is a personal access token."

jsonFromPersonalToken :: (APIToken, MagicHash, APIToken, MagicHash) -> JSValue
jsonFromPersonalToken (apitoken, apisecret, tok, mh) = 
  runJSONGen $ do
    J.value "apitoken"     $ show apitoken
    J.value "apisecret"    $ show apisecret
    J.value "accesstoken"  $ show tok
    J.value "accesssecret" $ show mh

jsonFromAPIToken :: (APIToken, MagicHash) -> JSValue
jsonFromAPIToken (apitoken, apisecret) =
  runJSONGen $ do
    J.value "apitoken"  $ show apitoken
    J.value "apisecret" $ show apisecret
    
jsonFromGrantedPrivilege :: (Int64, String, [APIPrivilege]) -> [(APIPrivilege, String)] -> [JSValue]
jsonFromGrantedPrivilege (tokenid, name, ps) ds =
  [runJSONGen $ do
      J.value "tokenid" $ show tokenid
      J.value "name" name
      J.value "privilege" ""] ++
  (for ps $ \p -> do
      runJSONGen $ do
        J.value "tokenid" $ show tokenid
        J.value "name" name
        J.value "privilegedescription" $ lookup p ds
        J.value "privilege" $ show p)
      