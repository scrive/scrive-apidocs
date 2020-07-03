{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Flow.Server.Handlers.Instance where

import Control.Monad.Except
import Control.Monad.Extra (fromMaybeM)
import Crypto.RNG
import Data.Aeson
import Data.Either.Combinators (rightToMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Log.Class
import Servant
import Web.Cookie
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import AccessControl.Check
import AccessControl.Types
import Auth.MagicHash
import Auth.Session
import Doc.DocumentID (DocumentID, unsafeDocumentID)
import Flow.Aggregator as Aggregator
import Flow.Api as Api
import Flow.Engine
import Flow.Error
import Flow.Guards
import Flow.Id
import Flow.Machinize as Machinize
import Flow.Model.Types
import Flow.Names
import Flow.OrphanInstances ()
import Flow.Server.Cookies
import Flow.Server.Types
import User.UserID (unsafeUserID)
import qualified Auth.Model as AuthModel
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model
import qualified Flow.Model.Types as Model
import qualified Flow.Transducer as Transducer

startInstance :: Account -> TemplateId -> InstanceToTemplateMapping -> AppM StartTemplate
startInstance Account {..} templateId InstanceToTemplateMapping {..} = do
  logInfo_ "Starting instance"
  -- TODO: Check permissions create instance..
  -- TODO: Check permissions to the template.
  -- TODO: Validate mapping...
  -- TODO: Replace value type with enum???
  -- TODO: Model instance state inside database somehow!
  -- TODO: Check the template is committed
  -- TODO: Check the flow users are unique

  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId
  machine  <- compile $ template ^. #process
  let ii = InsertInstance templateId $ Transducer.initialState machine
  id <- Model.insertFlowInstance ii

  insertFlowInstanceKeyValues id documents StoreDocumentId
  insertFlowInstanceKeyValues id users     StoreUserId
  insertFlowInstanceKeyValues id messages  StoreMessage

  -- Generate magic hashes for invitation links
  -- TODO get rid of unsafe
  usersWithHashes <- zip (fmap unsafeName $ Map.keys users) <$> replicateM (length users) random
  mapM_ (uncurry $ Model.insertInstanceAccessToken id) usersWithHashes

  -- TODO: This is for initial debugging/testing only. Remove.
  mapM_ (logInvitationLink id) usersWithHashes

  pure $ StartTemplate { id }
  where
    logInvitationLink instanceId (userName, hash) = do
      logInfo_ $ "Invitation link: /flow/overview/" <> T.intercalate
        "/"
        [toUrlPiece instanceId, toUrlPiece userName, toUrlPiece hash]
    -- TODO: this probably needs to be moved to Model module.
    insertFlowInstanceKeyValues
      :: InstanceId -> Map.Map Text a -> (a -> Model.StoreValue) -> AppM ()
    insertFlowInstanceKeyValues instanceId keyValues f =
      mapM_ (\(k, v) -> Model.insertFlowInstanceKeyValue instanceId k $ f v)
        $ Map.toList keyValues

getInstance :: Account -> InstanceId -> AppM GetInstance
getInstance account instanceId = do
  logInfo_ "Getting instance"
  -- TODO: Authorize user.
  -- TODO: Model instance state inside database somehow!
  flowInstance <- checkInstancePerms account instanceId ReadA
  documents    <-
    Map.fromList
    .   fmap (fmap unsafeDocumentID)
    <$> Model.selectInstanceKeyValues instanceId Document
  users <-
    Map.fromList
    .   fmap (fmap unsafeUserID)
    <$> Model.selectInstanceKeyValues instanceId User
  messages <-
    Map.fromList
    .   fmap (fmap identity)
    <$> Model.selectInstanceKeyValues instanceId Message
  pure $ GetInstance { id                 = instanceId
                     , templateId         = flowInstance ^. #templateId
                     , templateParameters = InstanceToTemplateMapping { .. }
                     , state = InstanceState { availableActions = [], history = [] }
                     }

listInstances :: Account -> AppM [GetInstance]
listInstances account@Account {..} = do
  is <- Model.selectInstancesByUserID $ user ^. #id
  let iids = map (view #id) is
  mapM (getInstance account) iids

getInstanceView :: InstanceUser -> InstanceId -> AppM GetInstanceView
getInstanceView user@InstanceUser {..} instanceId' = do
  logInfo "Getting instance view"
    $ object ["instance_id" .= instanceId', "instance_user" .= user]

  when (instanceId /= instanceId') $ throwAuthenticationError AccessControlError

  documents <-
    Map.fromList
    .   fmap (fmap unsafeDocumentID)
    <$> Model.selectInstanceKeyValues instanceId Document

  fullInstance <- fromMaybeM (throwError err404) $ Model.selectFullInstance instanceId
  let aggrState = instanceToAggregator fullInstance
  machine <- compile $ fullInstance ^. #template % #process

  case mAllowedEvents machine aggrState of
    Just allowedEvents ->
      pure
        $ let -- Actions
              userAllowedEvents =
                Set.filter (\e -> eventInfoUser e == userName) allowedEvents
              userActions = catMaybes . Set.toList $ Set.map
                (\e -> do
                  userAction <- toApiUserAction $ eventInfoAction e
                  -- TODO Use DocumentName and get rid of `fromName`
                  docId      <- Map.lookup (fromName $ eventInfoDocument e) documents
                  pure $ InstanceUserAction userAction docId
                )
                userAllowedEvents

              -- Document state
              userReceivedEvents = Set.filter (\e -> eventInfoUser e == userName)
                $ Aggregator.receivedEvents (aggrState :: AggregatorState)
              userDocs          = Set.map eventInfoDocument userReceivedEvents
              userDocsWithState = catMaybes . Set.toList $ Set.map
                (toDocumentOverview userReceivedEvents documents)
                userDocs
          in  GetInstanceView
                { id      = instanceId
                , state   = InstanceUserState { documents = userDocsWithState }
                , actions = userActions
                }
    _ -> do
      logAttention "GetInstanceView: Invalid userId or broken state for Flow instance "
        $ object ["instance_id" .= instanceId, "instance_user" .= user]
      throwError
        $ err500 { errBody = "Could not reconstruct the state of the Flow process." }

  where
    mAllowedEvents machine aggregatorState =
      rightToMaybe $ Aggregator.getAllowedEvents <$> Transducer.getState
        machine
        (currentState aggregatorState)

    toApiUserAction :: Machinize.UserAction -> Maybe Api.InstanceEventAction
    toApiUserAction Machinize.Approval  = Just Api.Approve
    toApiUserAction Machinize.Signature = Just Api.Sign
    toApiUserAction Machinize.View      = Just Api.View
    toApiUserAction Machinize.Rejection = Just Api.Reject
    toApiUserAction (Machinize.Field _) = Nothing
    toApiUserAction Machinize.Timeout   = Nothing

    toDocumentOverview
      :: Set EventInfo -> Map Text DocumentID -> DocumentName -> Maybe DocumentOverview
    toDocumentOverview userReceivedEvents docMap docName =
      -- TODO Use DocumentName and get rid of `fromName`
      (`DocumentOverview` docState) <$> Map.lookup (fromName docName) docMap

      where
        docEvents = Set.filter (\e -> eventInfoDocument e == docName) userReceivedEvents
        docHasAction action =
          Set.filter (\e -> eventInfoAction e == action) docEvents /= Set.empty
        docState | docHasAction Machinize.Approval = Approved
                 | docHasAction Machinize.Signature = Signed
                 | docHasAction Machinize.View = Viewed
                 | docHasAction Machinize.Rejection = Rejected
                 | otherwise                   = Started

-- brittany-disable-next-binding
instanceOverviewMagicHash
  :: InstanceId
  -> UserName
  -> MagicHash
  -> Maybe Cookies'
  -> Maybe Host
  -> IsSecure
  -> AppM
       ( Headers
           '[ Header "Location" Text
            , Header "Set-Cookie" SetCookie
            , Header "Set-Cookie" SetCookie
            ]
           NoContent
       )
instanceOverviewMagicHash instanceId userName hash mCookies mHost isSecure = do
  _ <-
    fromMaybeM (throwAuthenticationError InvalidInstanceAccessTokenError)
      $ Model.verifyInstanceAccessToken instanceId userName hash

  mSessionId <- case getAuthCookies of
    Just authCookies -> AuthModel.getSessionIDByCookies authCookies
    Nothing          -> pure Nothing

  -- If we don't have an existing Kontrakcja session - start a new one
  -- and add sessionId and xtoken cookies to the response.
  -- TODO: It would be better to let Kontrakcja handle creating the session
  (sessionId, maybeAddCookieHeaders) <- case mSessionId of
    Just sessionId -> pure (sessionId, noHeader . noHeader)
    Nothing        -> do
      newAuthCookies <- AuthModel.insertNewSession (cookieDomain mHost)
      pure ( cookieSessionID (authCookieSession newAuthCookies)
           , addAuthCookieHeaders (isSecure == Secure) newAuthCookies
           )

  -- The Flow user's access token has been verified so insert an "instance session"
  -- which is used for cookie authentication in subsequent calls.
  Model.upsertInstanceSession sessionId instanceId userName

  let response = addHeader redirectUrl $ maybeAddCookieHeaders NoContent
  pure response

  where
    getAuthCookies = do
      Cookies' cookies <- mCookies
      readAuthCookies cookies
    redirectUrl = "/flow/overview/"
      <> T.intercalate "/" [toUrlPiece instanceId, toUrlPiece userName]

-- Instance overview page
-- TODO: Implement the overview page
instanceOverview :: InstanceUser -> InstanceId -> UserName -> AppM Text
instanceOverview InstanceUser {..} instanceId' _ = do
  when (instanceId /= instanceId') $ throwAuthenticationError AccessControlError

  return "<html><body><h1> Flow overview page </h1></body></html>"

checkInstancePerms :: Account -> InstanceId -> AccessAction -> AppM Model.Instance
checkInstancePerms account instanceId action = do
  flowInstance <- fromMaybeM (throwError err404) $ Model.selectInstance instanceId
  let tid = flowInstance ^. #templateId
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate tid
  guardUserHasPermission account [canDo action . FlowTemplateR $ template ^. #folderId]
  pure flowInstance
