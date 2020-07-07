{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server.Handlers.Instance where

import Control.Monad.Except
import Control.Monad.Extra (fromMaybeM)
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Data.Either.Combinators (rightToMaybe)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics
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
import DB.Query
import Doc.DocumentID (DocumentID)
import Doc.Model.Query
import Doc.SignatoryLinkID
import Doc.Types.Document
import Flow.Aggregator as Aggregator
import Flow.Api as Api hiding (documents)
import Flow.DocumentChecker as DocumentChecker
import Flow.DocumentStarting
import Flow.Engine
import Flow.Error
import Flow.Guards
import Flow.Id
import Flow.Machinize as Machinize
import Flow.Model.Types
import Flow.Model.Types.FlowUserId as FlowUserId
import Flow.Names
import Flow.OrphanInstances ()
import Flow.Server.Cookies
import Flow.Server.Types
import qualified Auth.Model as AuthModel
import qualified Flow.Api as Api
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model
import qualified Flow.Model.Types as Model
import qualified Flow.Transducer as Transducer
import qualified Flow.VariableCollector as Collector

startInstance :: Account -> TemplateId -> InstanceKeyValues -> AppM StartTemplate
startInstance account templateId keyValues = do
  logInfo_ "Starting instance"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo CreateA $ FlowTemplateR fid]

  when (isNothing $ template ^. #committed) throwTemplateNotCommittedError

  tongue <- parseTongue $ template ^. #process
  let variables = Collector.collectVariables tongue
  reportVariables $ validateVariables variables keyValues

  let documentMapping = keyValues ^. #documents
  let documentIds     = Map.elems documentMapping

  when (hasDuplicates documentMapping) $ throwTemplateCannotBeStartedError
    "Invalid parameters"
    "Document ID cannot be associated with multiple DSL document variables."

  Model.selectDocumentIdsAssociatedWithSomeInstance documentIds
    >>= reportAssociatedDocuments

  let userMapping = keyValues ^. #users
  when (hasDuplicates userMapping) $ throwTemplateCannotBeStartedError
    "Invalid parameters"
    "User ID cannot be associated with multiple DSL user variables."

  documents <- mapM (dbQuery . GetDocumentByDocumentID) documentIds
  reportSettings $ checkDocumentSettingsConsistency documents

  machine <- translate tongue
  let ii = InsertInstance templateId $ Transducer.initialState machine
  id <- Model.insertFlowInstance ii

  -- The ordering of operations here is crucial.
  Model.insertFlowInstanceKeyValues id keyValues

  -- 1. Documents have to be started after storing the key values
  -- so that notifications are not sent out.
  forM_ documentIds $ startDocument account

  -- 2. Instance signatories can only be inserted after documents are started
  -- because that code calls `ResetSignatoryDetails`, which would break
  -- our foreign keys.
  let userMatchingResult = matchUsersWithSignatories documents variables keyValues
  logInfo "Matching result" userMatchingResult
  if validate userMatchingResult
    then do
      let links = Set.toList . Set.map createPair $ matched userMatchingResult
      Model.insertInstanceSignatories id links
    else
      throwTemplateCannotBeStartedError "DSL users do not match documents' signatories."
        $ toJSON userMatchingResult

  -- For the MVP we provide Flow app link,
  -- and so don't have to run the engine

  -- Generate magic hashes for invitation links
  usersWithHashes <- zip (Map.keys userMapping) <$> replicateM (length userMapping) random
  mapM_ (uncurry $ Model.insertInstanceAccessToken id) usersWithHashes

  -- TODO: This is for initial debugging/testing only. Remove.
  mapM_ (logInvitationLink id) usersWithHashes

  -- TODO return an app link
  pure $ StartTemplate { id }
  where
    logInvitationLink instanceId (userName, hash) = do
      logInfo_ $ "Invitation link: /flow/overview/" <> T.intercalate
        "/"
        [toUrlPiece instanceId, toUrlPiece userName, toUrlPiece hash]

    validate MatchingResult {..} = null unmatchedFlowUserIds && null unmatchedSignatories
    createPair :: UserMatch -> (UserName, SignatoryLinkID)
    createPair UserMatch {..} = (name, DocumentChecker.id signatory)
      where name = fromJust . getUserName flowUserId $ keyValues ^. #users

    -- Assuming we allow only one UserName per user.
    getUserName :: FlowUserId -> Map UserName FlowUserId -> Maybe UserName
    getUserName flowUserId userMap =
      case Map.keys $ Map.filter (== flowUserId) userMap of
        []      -> Nothing
        (x : _) -> Just x

reportAssociatedDocuments :: [DocumentID] -> AppM ()
reportAssociatedDocuments docIds =
  unless (null docIds)
    . throwTemplateCannotBeStartedError
        "Some of the documents are already being used in other Flow instances."
    $ object ["associated_document_ids" .= docIds]

matchUsersWithSignatories
  :: [Document] -> Collector.FlowVariables -> InstanceKeyValues -> MatchingResult
matchUsersWithSignatories documents variables keyValues = matchUsers flowUserIdDocRoles
                                                                     signatoryDocRoles
  where
    signatoryDocRoles  = Set.unions $ fmap documentSignatories documents
    namedRoles         = Collector.documentUserAssociation variables
    flowUserIdDocRoles = Set.map resolveNamedRole namedRoles
    resolveNamedRole DocRoleFor {..} = DocRoleFor role user' document'
      where
        -- The variables are already validated at this point, so this is safe.
        user'     = fromJust . Map.lookup user $ keyValues ^. #users
        document' = fromJust . Map.lookup document $ keyValues ^. #documents

data Variables = Variables
  { users :: Set UserName
  , documents :: Set DocumentName
  , messages :: Set MessageName
  } deriving (Generic)

instance ToJSON Variables where
  toEncoding = genericToEncoding aesonOptions

data VariableErrors = VariableErrors
  { undefinedVariables :: Variables
  , unknownParameters :: Variables
  } deriving (Generic)

instance ToJSON VariableErrors where
  toEncoding = genericToEncoding aesonOptions

validateVariables :: Collector.FlowVariables -> InstanceKeyValues -> VariableErrors
validateVariables variables keyValues = VariableErrors
  { undefinedVariables = diff templateVars parameterVars
  , unknownParameters  = diff parameterVars templateVars
  }
  where
    toSet :: Ord a => Optic' A_Lens is InstanceKeyValues (Map a b) -> Set a
    toSet f = Set.fromList . Map.keys $ keyValues ^. f
    parameterVars = Variables (toSet #users) (toSet #documents) (toSet #messages)
    templateVars  = Variables (Collector.users variables)
                              (Collector.documents variables)
                              (Collector.messages variables)
    diff v1 v2 = Variables (users v1 Set.\\ users v2)
                           (documents v1 Set.\\ documents v2)
                           (messages v1 Set.\\ messages v2)

nonEmptyVariables :: Variables -> Bool
nonEmptyVariables Variables {..} =
  not null users || not null documents || not null messages

reportVariables :: VariableErrors -> AppM ()
reportVariables uv@VariableErrors {..} =
  when (nonEmptyVariables undefinedVariables || nonEmptyVariables unknownParameters)
    . throwTemplateCannotBeStartedError
        "Provided parameters do not match template variables."
    $ toJSON uv

hasDuplicates :: Ord v => Map k v -> Bool
hasDuplicates m = length l /= length (Set.fromList l) where l = Map.elems m

data DocumentField
  = DaysToSign
  | DaysToRemind
  | TimeoutTime
  | AutoRemindTime
  deriving (Eq, Generic, Ord)

instance ToJSON DocumentField where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = snakeCase }

checkDocumentSettingsConsistency :: [Document] -> Set DocumentField
checkDocumentSettingsConsistency docs = Set.filter (\f -> unique f > 1) fields
  where
    fields = Set.fromList [DaysToSign, DaysToRemind, TimeoutTime, AutoRemindTime]
    unique = \case
      DaysToSign     -> uniqueOf documentdaystosign
      DaysToRemind   -> uniqueOf documentdaystoremind
      TimeoutTime    -> uniqueOf documenttimeouttime
      AutoRemindTime -> uniqueOf documentautoremindtime
    uniqueOf :: Ord a => (Document -> a) -> Int
    uniqueOf f = length . Set.fromList $ fmap f docs

reportSettings :: Set DocumentField -> AppM ()
reportSettings fields =
  unless (null fields)
    . throwTemplateCannotBeStartedError
        "Some settings are not consistent across all documents."
    $ toJSON fields

getInstance :: Account -> InstanceId -> AppM GetInstance
getInstance account instanceId = do
  logInfo_ "Getting instance"
  flowInstance <- checkInstancePerms account instanceId ReadA
  keyValues    <- Model.selectInstanceKeyValues instanceId
  pure $ GetInstance { id                 = instanceId
                     , templateId         = flowInstance ^. #templateId
                     , templateParameters = keyValues
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

  keyValues    <- Model.selectInstanceKeyValues instanceId
  fullInstance <- fromMaybeM (throwError err404) $ Model.selectFullInstance instanceId
  let aggrState = instanceToAggregator fullInstance
  machine <- compile $ fullInstance ^. #template % #process

  case mAllowedEvents machine aggrState of
    Just allowedEvents ->
      pure
        $ let
              -- Actions
              userAllowedEvents =
                Set.filter (\e -> eventInfoUser e == userName) allowedEvents
              userActions = catMaybes . Set.toList $ Set.map
                (\e -> do
                  userAction <- toApiUserAction $ eventInfoAction e
                  docId      <- Map.lookup (eventInfoDocument e) (keyValues ^. #documents)
                  pure $ InstanceUserAction userAction docId
                )
                userAllowedEvents

              -- Document state
              userReceivedEvents = Set.filter (\e -> eventInfoUser e == userName)
                $ Aggregator.receivedEvents (aggrState :: AggregatorState)
              userDocs          = Set.map eventInfoDocument userReceivedEvents
              userDocsWithState = catMaybes . Set.toList $ Set.map
                (toDocumentOverview userReceivedEvents $ keyValues ^. #documents)
                userDocs
          in  GetInstanceView
                { id      = instanceId
                , state   = InstanceUserState { Api.documents = userDocsWithState }
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
      :: Set EventInfo
      -> Map DocumentName DocumentID
      -> DocumentName
      -> Maybe DocumentOverview
    toDocumentOverview userReceivedEvents docMap docName =
      (`DocumentOverview` docState) <$> Map.lookup docName docMap

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
