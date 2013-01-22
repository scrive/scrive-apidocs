module ThirdPartyStats.Utils (
    extractUID, extractDocID
  ) where
import User.UserID (UserID)
import Doc.DocumentID (DocumentID)
import ThirdPartyStats.Core
import Data.List (partition)

-- | Separate the user ID property from the rest, if present.
--   More than one UID is not OK either.
extractUID :: [EventProperty] -> Maybe (UserID, [EventProperty])
extractUID props =
    case partition isUIDProp props of
      ([UserIDProp uid], props') -> Just (uid, props')
      _                          -> Nothing
  where
    isUIDProp (UserIDProp _) = True
    isUIDProp _              = False

-- | Separate the doc ID property from the rest, if present.
--   More than one doc ID is not OK either.
extractDocID :: [EventProperty] -> Maybe (DocumentID, [EventProperty])
extractDocID props =
    case partition isUIDProp props of
      ([DocIDProp uid], props') -> Just (uid, props')
      _                         -> Nothing
  where
    isUIDProp (DocIDProp _) = True
    isUIDProp _             = False
