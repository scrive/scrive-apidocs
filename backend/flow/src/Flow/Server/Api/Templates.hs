{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server.Api.Templates where

import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Time
import Data.Either.Combinators
import Log.Class
import Servant
import qualified Data.Text as T

import AccessControl.Check
import AccessControl.Types
import Flow.Error
import Flow.Guards
import Flow.HighTongue
import Flow.Id
import Flow.Model.Types
import Flow.OrphanInstances ()
import Flow.Process
import Flow.Routes.Api as Api
import Flow.Server.Api.Common
import Flow.Server.Api.Templates.Start
import Flow.Server.Types
import qualified Flow.Model as Model

accountEndpoints :: ServerT (AuthProtect "account" :> TemplateApi) AppM
accountEndpoints account =
  createTemplate account
    :<|> deleteTemplate account
    :<|> getTemplate account
    :<|> patchTemplate account
    :<|> listTemplates account
    :<|> commitTemplate account
    :<|> startTemplate account

createTemplate :: Account -> CreateTemplate -> AppM GetCreateTemplate
createTemplate account@Account {..} CreateTemplate {..} = do
  logInfo_ "Creating template"
  guardUserHasPermission account [canDo CreateA $ FlowTemplateR (folder ^. #id)]
  id <- Model.insertTemplate $ InsertTemplate name process (user ^. #id) (folder ^. #id)
  pure $ GetCreateTemplate { id }

-- TODO: Committed templates shouldn't be deleted.
deleteTemplate :: Account -> TemplateId -> AppM NoContent
deleteTemplate account id = do
  logInfo_ "Deleting template"
  template <- selectTemplate id
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo DeleteA $ FlowTemplateR fid]
  Model.deleteTemplate id
  pure NoContent

getTemplate :: Account -> TemplateId -> AppM GetTemplate
getTemplate account templateId = do
  logInfo_ "Getting template"
  template <- selectTemplate templateId
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo ReadA $ FlowTemplateR fid]
  pure $ toGetTemplate template

toGetTemplate :: Template -> GetTemplate
toGetTemplate t = GetTemplate { id        = t ^. #id
                              , name      = t ^. #name
                              , process   = t ^. #process
                              , committed = t ^. #committed
                              , folderId  = t ^. #folderId
                              }

patchTemplate :: Account -> TemplateId -> PatchTemplate -> AppM GetTemplate
patchTemplate account templateId PatchTemplate {..} = do
  logInfo_ "Patching template"
  template <- selectTemplate templateId
  when (isJust $ template ^. #committed) throwTemplateAlreadyCommittedError
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo UpdateA $ FlowTemplateR fid]
  let ut = UpdateTemplate templateId name process Nothing
  updated <- fromMaybeM throwTemplateNotFoundError $ Model.updateTemplate ut
  pure $ toGetTemplate updated

commitTemplate :: Account -> TemplateId -> AppM NoContent
commitTemplate account id = do
  logInfo_ "Committing template"
  now      <- liftIO currentTime
  template <- selectTemplate id
  when (isJust $ template ^. #committed) throwTemplateAlreadyCommittedError
  let templateDSL = template ^. #process
  -- We're currently not storing the machine, so we throw it away.
  checkDSL templateDSL
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo UpdateA $ FlowTemplateR fid]
  void . Model.updateTemplate $ UpdateTemplate id Nothing Nothing (Just now)
  pure NoContent

validateTemplate :: Process -> AppM NoContent
validateTemplate templateDSL = do
  logInfo_ "Validating templateDSL"
  checkDSL templateDSL
  pure NoContent

-- TODO: Currently, there's no way to get more than a singleton list of validation
-- TODO: errors. This allows the Error module to be prettier, all this should be
-- TODO: improved later.
checkDSL :: Process -> AppM ()
checkDSL templateDSL = do
  when (T.null $ fromProcess templateDSL)
    .  throwDSLValidationError
    $  "No template DSL text available. "
    <> "Please update the template with non-empty process and try again."
  whenLeft (decodeHighTongue templateDSL) $ \case
    -- TODO: Improve error messages.
    []      -> throwDSLValidationError "Unknown validation error"
    err : _ -> throwDSLValidationError $ error_message err

listTemplates :: Account -> AppM [GetTemplate]
listTemplates account@Account {..} = do
  logInfo_ "Listing templates"
  templates <- Model.selectTemplatesByUserID $ user ^. #id
  let fids = view #folderId <$> templates
  guardUserHasPermission account [ canDo ReadA $ FlowTemplateR fid | fid <- fids ]
  pure $ fmap toGetTemplate templates
