{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server.Api.Templates.Start where

import Control.Monad.Extra (fromMaybeM)
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics
import Log.Class
import qualified Data.Map as Map
import qualified Data.Set as Set

import AccessControl.Check
import AccessControl.Types
import DB (dbQuery)
import Doc.DocumentID (DocumentID)
import Doc.Model.Query
import Doc.SignatoryLinkID
import Doc.Types.Document
import Flow.DocumentChecker as DocumentChecker
import Flow.DocumentStarting
import Flow.Engine
import Flow.Error
import Flow.Guards
import Flow.HighTongue
import Flow.Id
import Flow.Model.Types
import Flow.Model.Types.FlowUserId as FlowUserId
import Flow.OrphanInstances ()
import Flow.Routes.Api hiding (documents)
import Flow.Server.Types
import Flow.Server.Utils
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model
import qualified Flow.VariableCollector as Collector

startTemplate :: Account -> TemplateId -> InstanceKeyValues -> AppM GetInstance
startTemplate account templateId keyValues = do
  logInfo_ "Starting instance"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo CreateA $ FlowTemplateR fid]

  when (isNothing $ template ^. #committed) throwTemplateNotCommittedError

  tongue <- decodeHighTongueM $ template ^. #process
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

  -- TODO: remove head...
  let stateId = stageName . head $ stages tongue
  let ii      = InsertInstance templateId stateId
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
  -- -- TODO get rid of unsafe
  -- usersWithHashes <-
  --   zip (unsafeName <$> Map.keys users) <$> replicateM (length users) random
  mapM_ (uncurry $ Model.insertInstanceAccessToken id) usersWithHashes

  let accessLinks = mkAccessLinks (baseUrl account) id usersWithHashes

  let templateParameters = keyValues
  -- TODO add a proper instance state
  let state = InstanceState { availableActions = [], history = [] }
  pure $ GetInstance { .. }
  where

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

