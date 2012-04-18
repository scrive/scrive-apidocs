module OAuth.View where

import Templates.Templates
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.UTF8 as BS
import OAuth.Model
import qualified Templates.Fields as F

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

