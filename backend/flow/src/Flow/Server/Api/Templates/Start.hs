module Flow.Server.Api.Templates.Start where

import Control.Monad.Except
import Control.Monad.Reader
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Data.Char
import Data.List.Extra hiding (head)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple.Extra
import GHC.Generics
import Log.Class
import Servant
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.StringTemplates.Templates as TM

import AccessControl.Check
import AccessControl.Types
import DB (dbQuery)
import Doc.DocumentID (DocumentID)
import Doc.Model.Query
import Doc.SignatoryLinkID
import Doc.Types.Document
import Flow.ActionConsumers hiding (Notify, users)
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
import Flow.Routes.Types
import Flow.Server.Api.Common
import Flow.Server.Types
import Flow.Server.Utils
import User.Lang
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model
import qualified Flow.VariableCollector as Collector

startTemplate :: Account -> TemplateId -> CreateInstance -> AppM GetInstance
startTemplate account templateId (CreateInstance title keyValues) = do
  logInfo_ "Starting instance"
  template <- selectTemplate templateId
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo CreateA $ FlowTemplateR fid]

  when (isNothing $ template ^. #committed) throwTemplateNotCommittedError

  tongue <- decodeHighTongueM $ template ^. #process
  let variables = Collector.collectVariables tongue
  reportVariables $ validateVariables variables keyValues

  let documentMapping = keyValues ^. #documents
      documentIds     = Map.elems documentMapping
  validDocumentIds <- Model.selectDocumentIdsByDocumentIds documentIds
  reportMissingDocuments . Set.toList $ Set.difference (Set.fromList documentIds)
                                                       (Set.fromList validDocumentIds)

  when (hasDuplicates documentMapping) $ throwTemplateCannotBeStartedError
    TemplateStartBadRequest
    "Invalid parameters"
    "Document ID cannot be associated with multiple DSL document variables."

  Model.selectDocumentIdsAssociatedWithSomeInstance documentIds
    >>= reportAssociatedDocuments

  let userMapping = keyValues ^. #users
  when (hasDuplicates userMapping) $ throwTemplateCannotBeStartedError
    TemplateStartBadRequest
    "Invalid parameters"
    "User ID cannot be associated with multiple DSL user variables."

  documents <- mapM (dbQuery . GetDocumentByDocumentID) documentIds
  reportSettings $ checkDocumentSettingsConsistency documents

  now <- liftIO currentTime
  let initialStage = head $ stages tongue
      actions      = stageActions initialStage
      stateId      = stageName initialStage
      started      = now
      lastEvent    = now
  id <- Model.insertFlowInstance $ InsertInstance templateId title stateId started

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
      throwTemplateCannotBeStartedError TemplateStartConflict
                                        "DSL users do not match documents' signatories."
        $ toJSON userMatchingResult

  let notificationErrors =
        validateNotificationSettings actions userMapping (matched userMatchingResult)
  reportNotificationErrors notificationErrors

  -- For the MVP we provide Flow app link,
  -- and so don't have to run the engine

  -- Generate magic hashes for invitation links
  usersWithHashes <- zip (Map.keys userMapping) <$> replicateM (length userMapping) random
  mapM_ (uncurry $ Model.insertInstanceAccessToken id) usersWithHashes

  let accessLinks = mkAccessLinks (baseUrl account) id usersWithHashes

  let templateParameters = keyValues
  -- TODO add a proper instance state
  let state = InstanceState { availableActions = [] }
  let status             = InProgress

  templates' <- asks templates
  TM.runTemplatesT (T.unpack $ codeFromLang LANG_EN, templates')
    . forM_ actions
    $ \Notify {..} -> notifyAction (Url $ baseUrl account) id notifyUsers notifyMethods
        >>= uncurry3 consumeNotifyAction

  pure $ GetInstance { .. }
  where

    validate :: MatchingResult -> Bool
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

reportNotificationErrors :: MonadError ServerError m => Set MissingContactDetail -> m ()
reportNotificationErrors errors =
  unless (null errors)
    . throwTemplateCannotBeStartedError
        TemplateStartConflict
        "Required contact details are not configured on document signatories"
    $ object ["missing_contact_details" .= errors]

reportMissingDocuments :: MonadError ServerError m => [DocumentID] -> m ()
reportMissingDocuments docIds =
  unless (null docIds)
    . throwTemplateCannotBeStartedError TemplateStartBadRequest
                                        "Some of the documents do not exist"
    $ object ["missing_document_ids" .= docIds]

reportAssociatedDocuments :: MonadError ServerError m => [DocumentID] -> m ()
reportAssociatedDocuments docIds =
  unless (null docIds)
    . throwTemplateCannotBeStartedError
        TemplateStartConflict
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

reportVariables :: MonadError ServerError m => VariableErrors -> m ()
reportVariables uv@VariableErrors {..} =
  when (nonEmptyVariables undefinedVariables || nonEmptyVariables unknownParameters)
    . throwTemplateCannotBeStartedError
        TemplateStartBadRequest
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

reportSettings :: MonadError ServerError m => Set DocumentField -> m ()
reportSettings fields =
  unless (null fields)
    . throwTemplateCannotBeStartedError
        TemplateStartConflict
        "Some settings are not consistent across all documents."
    $ toJSON fields

data RequiredMethods = RequiredMethods
  { emailRequired :: Bool
  , smsRequired :: Bool
  } deriving (Show)

instance Semigroup RequiredMethods where
  RequiredMethods ae as <> RequiredMethods be bs = RequiredMethods (ae || be) (as || bs)

data MissingMethod
  = MissingEmail
  | MissingPhoneNumber
  deriving (Eq, Generic, Ord, Show)

instance ToJSON MissingMethod where
  toJSON = \case
    MissingEmail       -> toJSON ("email" :: T.Text)
    MissingPhoneNumber -> toJSON ("phone_number" :: T.Text)

data MissingContactDetail = MissingContactDetail
  { userName :: [UserName]
  , missingMethod :: MissingMethod
  } deriving (Eq, Generic, Ord, Show)

instance ToJSON MissingContactDetail where
  toJSON = genericToJSON aesonOptions

data DetailedRequirement = DetailedRequirement
  { userName :: UserName
  , requirement :: RequiredMethods
  , signatory :: Signatory
  } deriving (Show)

validateNotificationSettings
  :: [SystemAction]
  -> Map UserName FlowUserId
  -> Set UserMatch
  -> Set MissingContactDetail
validateNotificationSettings actions userMap userMatches = Set.fromList errors
  where
    errors :: [MissingContactDetail]
    errors =
      let groupedPairs = groupSort . concat $ map checkUser detailedRequirements
      in  [ MissingContactDetail (nubOrd usernames) missingMethods
          | (missingMethods, usernames) <- groupedPairs
          ]

    checkUser :: DetailedRequirement -> [(MissingMethod, UserName)]
    checkUser DetailedRequirement {..} = (, userName) <$> missingMethod
      where
        emailError =
          emailRequired requirement && isAbsent (DocumentChecker.email signatory)
        smsError =
          smsRequired requirement && isAbsent (DocumentChecker.phoneNumber signatory)
        missingMethod = case (emailError, smsError) of
          (True , True ) -> [MissingEmail, MissingPhoneNumber]
          (True , False) -> [MissingEmail]
          (False, True ) -> [MissingPhoneNumber]
          (False, False) -> []
        isAbsent = maybe True $ T.all isSpace

    detailedRequirements :: [DetailedRequirement]
    detailedRequirements = map annotate $ Map.toList userRequirements

    annotate :: (UserName, RequiredMethods) -> DetailedRequirement
    annotate (userName, requirement) = DetailedRequirement { .. }
      where
        userId    = fromJust $ Map.lookup userName userMap
        signatory = DocumentChecker.signatory . fromJust $ find
          (\um -> flowUserId um == userId)
          userMatches

    userRequirements :: Map UserName RequiredMethods
    userRequirements = foldl' groupUsers Map.empty actions

    groupUsers
      :: Map UserName RequiredMethods -> SystemAction -> Map UserName RequiredMethods
    groupUsers reqs action = case action of
      Notify userNames methods -> foldl' (addUser $ asRequired methods) reqs userNames

    asRequired :: SystemActionMethods -> RequiredMethods
    asRequired (Methods email sms) = RequiredMethods (isJust email) (isJust sms)

    addUser
      :: RequiredMethods
      -> Map UserName RequiredMethods
      -> UserName
      -> Map UserName RequiredMethods
    addUser methods reqs userName = Map.alter (addMethods methods) userName reqs

    addMethods :: Semigroup a => a -> Maybe a -> Maybe a
    addMethods methods = \case
      Nothing         -> Just methods
      Just oldMethods -> Just $ oldMethods <> methods
