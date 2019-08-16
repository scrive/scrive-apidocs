{-# LANGUAGE ExtendedDefaultRules #-}
module OAuth.View where

import Data.Int
import Happstack.Server.SimpleHTTP
import Text.JSON
import Text.JSON.Gen hiding (value)
import Text.StringTemplates.Templates
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
import Kontra
import MagicHash
import OAuth.Model
import Templates (renderTextTemplate)

pagePrivilegesConfirm :: Kontrakcja m
                      => Context
                      -> [APIPrivilege]
                      -> String -- company name
                      -> APIToken
                      -> m Response
pagePrivilegesConfirm ctx privileges companyname token = do
     ad <- getAnalyticsData
     response <- renderTextTemplate "pagePrivilegesConfirm" $ do
         F.value "isDocumentCreate" $ APIDocCreate `elem` privileges
         F.value "isDocumentSend" $ APIDocSend `elem` privileges
         F.value "isDocumentCheck" $ APIDocCheck `elem` privileges
         F.value "companyname" companyname
         F.value "token" $ show token
         standardPageFields ctx Nothing ad
     simpleHtmlResponse response

privilegeDescription :: TemplatesMonad m => APIPrivilege -> m String
privilegeDescription APIDocCreate = renderTemplate_ "_apiConfiramtionCreatePersmission"
privilegeDescription APIDocCheck  = renderTemplate_ "_apiConfiramtionReadPermission"
privilegeDescription APIDocSend   = renderTemplate_ "_apiConfiramtionSendPermission"
privilegeDescription APIPersonal  = return "Personal access token."

jsonFromAPIToken :: (APIToken, MagicHash) -> JSValue
jsonFromAPIToken (apitoken, apisecret) =
  runJSONGen $ do
    J.value "apitoken"  $ show apitoken
    J.value "apisecret" $ show apisecret

jsonFromGrantedPrivilege :: (Int64, String, [APIPrivilege]) -> [(APIPrivilege, String)] -> [JSValue]
jsonFromGrantedPrivilege (tokenid, name, ps) ds =
    for ps $ \p -> do
      runJSONGen $ do
        J.value "tokenid" $ show tokenid
        J.value "name" name
        J.value "privilegedescription" $ lookup p ds
        J.value "privilege" $ show p
