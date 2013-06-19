{-# LANGUAGE ExtendedDefaultRules #-}
module OAuth.View where

import MagicHash
import Utils.Prelude
import Data.Int
import Text.StringTemplates.Templates
import OAuth.Model
import qualified Text.StringTemplates.Fields as F
import User.Model
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import Text.JSON
import AppView
import Happstack.Server.SimpleHTTP
import Kontra
import qualified Static.Resources as SR


pagePrivilegesConfirm :: Kontrakcja m
                      => Context
                      -> [APIPrivilege]
                      -> String -- company name
                      -> APIToken
                      -> m Response
pagePrivilegesConfirm ctx privileges companyname token = do
     rsp <- renderTemplate "pagePrivilegesConfirm" $ do
         F.value "isDocumentCreate" $ APIDocCreate `elem` privileges
         F.value "isDocumentSend" $ APIDocSend `elem` privileges
         F.value "isDocumentCheck" $ APIDocCheck `elem` privileges
         F.value "companyname" companyname
         F.value "token" $ show token
         F.value "staticResources" $ SR.htmlImportList "systemPage" (ctxstaticresources ctx)
         contextInfoFields ctx
     simpleHtmlResonseClrFlash rsp

showAPIDashboard :: TemplatesMonad m => User -> m String
showAPIDashboard user = do
  renderTemplate "apiDashboard" $ do
      F.value "iscompanyadmin" $ useriscompanyadmin user
      F.value "seessubscriptiondashboard" $ useriscompanyadmin user || (usercompany user) == Nothing

privilegeDescription :: TemplatesMonad m => APIPrivilege -> m String
privilegeDescription APIDocCreate = renderTemplate_ "_apiConfiramtionCreatePersmission"
privilegeDescription APIDocCheck  = renderTemplate_ "_apiConfiramtionReadPermission"
privilegeDescription APIDocSend   = renderTemplate_ "_apiConfiramtionSendPermission"
privilegeDescription APIPersonal  = return "Personal access token."


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
