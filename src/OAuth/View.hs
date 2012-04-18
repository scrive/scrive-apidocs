module OAuth.View where

import Templates.Templates
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.UTF8 as BS
import OAuth.Model
import qualified Templates.Fields as F

pagePrivilegesConfirm :: TemplatesMonad m
                      => [APIPrivilege]
                      -> String -- company name
                      -> APIToken
                      -> m String  
pagePrivilegesConfirm privileges companyname token = do
     renderTemplate "pagePrivilegesConfirm" $ do
         F.value "isDocumentCreate" $ APIDocCreate `elem` privileges
         F.value "companyname" companyname
         F.value "token" $ show token
