module Flow.DocumentChecker
  ( FlowUserIdDocRole
  , MatchingResult(..)
  , Signatory(..)
  , SignatoryConsistencyError(..)
  , SignatoryDocRole
  , UserMatch(..)
  , aggregateSignatories
  , documentSignatories
  , docRolesMatch
  , flowUserIdMatchesSignatory
  , groupFlowUserIds
  , matchUsers
  )
where

import Data.Aeson
import Data.Aeson.Casing
import Data.Bifunctor
import Data.Functor
import Data.Set (Set)
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Text as Text

import Doc.DocumentID
import Doc.SignatoryLinkID
import Doc.Types.Document
import Doc.Types.SignatoryField
import Doc.Types.SignatoryLink
import Flow.Model.Types.FlowUserId
import Flow.VariableCollector
import User.UserID

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

type FlowUserIdDocRole = DocRoleFor FlowUserId DocumentID

-- | This is a simplified version of the `SignatoryLink`.
-- We only keep the fields we need in Flow.
data Signatory = Signatory
  { id          :: SignatoryLinkID
  , email       :: Maybe Text
  , phoneNumber :: Maybe Text
  , userId      :: Maybe UserID
  } deriving (Eq, Generic, Ord, Show)
type SignatoryDocRole = DocRoleFor Signatory DocumentID

instance ToJSON Signatory where
  toEncoding = genericToEncoding aesonOptions

-- | Association between a Flow user, as specified
-- by a Flow process, and a signatory, as specified
-- in a document.
data UserMatch = UserMatch
  { flowUserId :: FlowUserId
  , signatory  :: Signatory
  } deriving (Eq, Generic, Ord, Show)

instance ToJSON UserMatch where
  toEncoding = genericToEncoding aesonOptions

-- | Result of matching Flow users with the signatories
-- in the documents associated with that Flow.
data MatchingResult = MatchingResult
  { matched              :: Set UserMatch
  , unmatchedFlowUserIds   :: Set FlowUserIdDocRole
  , unmatchedSignatories :: Set SignatoryDocRole
  } deriving (Eq, Generic,  Show)

instance ToJSON MatchingResult where
  toEncoding = genericToEncoding aesonOptions

-- | Extract user document roles from a Flow process.
-- signatoryRequirements :: HighTongue -> Set FlowUserIdDocRole

-- Forwarded roles only occur in a running process,
-- so do not concern us here when checking documents for starting.
signatoryLinkRoleToSignatoryRole :: SignatoryRole -> Role
signatoryLinkRoleToSignatoryRole = \case
  SignatoryRoleViewer                -> Viewer
  SignatoryRoleSigningParty          -> SigningParty
  SignatoryRoleApprover              -> Approver
  SignatoryRoleForwardedSigningParty -> SigningParty
  SignatoryRoleForwardedApprover     -> Approver

-- | Extract signatory document roles from a document.
documentSignatories :: Document -> Set SignatoryDocRole
documentSignatories doc =
  Set.fromList . map (sigDocRole . signatoryInfo) $ documentsignatorylinks doc
  where sigDocRole (sig, role) = DocRoleFor role sig (documentid doc)

-- | Convert document signatory to a signatory document role.
signatoryInfo :: SignatoryLink -> (Signatory, Role)
signatoryInfo sigLink = (Signatory { .. }, role)
  where
    id          = signatorylinkid sigLink
    fields      = signatoryfields sigLink
    -- Text.strip is here because it's in SignatoryField.getEmail,
    -- suggesting that emails might be unsanitized in the DB.
    email       = getFieldByIdentity EmailFI fields >>= fieldTextValue <&> Text.strip
    phoneNumber = getFieldByIdentity MobileFI fields >>= fieldTextValue
    userId      = maybesignatory sigLink
    role        = signatoryLinkRoleToSignatoryRole $ signatoryrole sigLink

-- | Tries to match the flow users described in a Flow process
-- with the signatories in the associated documents.
-- It returns both the matching pairs, as well as all the unmatched
-- users on both the Flow side and the Document side.
--
-- Note: the implementation uses a naive quadratic algorithm that should be fine
-- until we have hundreds of users signing dozens of documents in a single process.
matchUsers :: Set FlowUserIdDocRole -> Set SignatoryDocRole -> MatchingResult
matchUsers fudrs sdrs = MatchingResult { .. }
  where
    matched              = Set.fromList $ map toUserMatch matchedDocRoles
    toUserMatch          = uncurry UserMatch . bimap user user
    unmatchedFlowUserIds = Set.difference fudrs . Set.fromList $ map fst matchedDocRoles
    unmatchedSignatories = Set.difference sdrs . Set.fromList $ map snd matchedDocRoles
    matchedDocRoles      = mapMaybe matchUser $ Set.toList fudrs

    matchUser :: FlowUserIdDocRole -> Maybe (FlowUserIdDocRole, SignatoryDocRole)
    matchUser fudr = (fudr, ) <$> find (docRolesMatch fudr) (Set.toList sdrs)

docRolesMatch :: FlowUserIdDocRole -> SignatoryDocRole -> Bool
docRolesMatch fudr sdr =
  flowUserIdMatchesSignatory (user fudr) (user sdr)
    && (document fudr == document sdr)
    && (role fudr == role sdr)

flowUserIdMatchesSignatory :: FlowUserId -> Signatory -> Bool
flowUserIdMatchesSignatory flowUserId Signatory {..} = case flowUserId of
  Email       email'       -> Just email' == email
  PhoneNumber phoneNumber' -> Just phoneNumber' == phoneNumber
  UserId      userId'      -> Just userId' == userId

data SignatoryConsistencyError
  = InconsistentEmail (Set Text)
  | InconsistentPhoneNumber (Set Text)
  | InconsistentUserId (Set UserID)
  | NoSignatories

groupFlowUserIds :: Set UserMatch -> [(FlowUserId, Set Signatory)]
groupFlowUserIds =
  map toOutput
    . groupBy (\x y -> flowUserId x == flowUserId y)
    . sortOn flowUserId
    . Set.toList
  where toOutput l = (flowUserId $ head l, Set.fromList $ map signatory l)

-- | This function verifies that the signatory contact/user data
-- is consistent across several records, i.e. no kind of contact/user datum
-- (email, say) has multiple values. If this is the case, it creates
-- an aggregate signatory record.
--
-- The purpose is to check that signatory records coming from several
-- documents (such as those associated with a Flow) are consistent
-- with each other.
aggregateSignatories :: Set Signatory -> Either SignatoryConsistencyError Signatory
aggregateSignatories sigs
  | Set.null sigs = Left NoSignatories
  | otherwise = do
    e   <- tryUnique email InconsistentEmail
    pn  <- tryUnique phoneNumber InconsistentPhoneNumber
    uid <- tryUnique userId InconsistentUserId
    pure $ Signatory (id . head $ Set.toList sigs) e pn uid
  where
    unique :: Ord a => (Signatory -> Maybe a) -> Set a
    unique f = Set.fromList . mapMaybe f $ Set.toList sigs
    tryUnique f mkErr = case Set.toList (unique f) of
      []  -> pure Nothing
      [x] -> pure $ Just x
      _   -> Left . mkErr $ unique f
