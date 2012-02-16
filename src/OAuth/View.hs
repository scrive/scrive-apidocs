module OAuth.View where

import Templates.Templates
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.UTF8 as BS
import OAuth.Model

pagePrivilegesConfirm :: TemplatesMonad m
                      => [APIPrivilege]
                      -> String -- company name
                      -> APIToken
                      -> m String  
pagePrivilegesConfirm privileges companyname token = do
     renderTemplateFM "pagePrivilegesConfirm" $ do
         field "isDocumentCreate" $ APIDocCreate `elem` privileges
         field "companyname" companyname
         field "token" $ show token
