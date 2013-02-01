module ThirdPartyStats.Utils (
    extractUID, extractDocID, extractCompanyID
  ) where
import User.UserID (UserID)
import Doc.DocumentID (DocumentID)
import Company.CompanyID (CompanyID)
import ThirdPartyStats.Core
import Data.List (partition)
import Data.Maybe (isJust)

-- | Separate the user ID property from the rest, if present.
--   More than one UID is not OK either.
extractUID :: [EventProperty] -> Maybe (UserID, [EventProperty])
extractUID = extract uidProp
  where
    uidProp (UserIDProp x) = Just x
    uidProp _              = Nothing

-- | Separate the doc ID property from the rest, if present.
--   More than one doc ID is not OK either.
extractDocID :: [EventProperty] -> Maybe (DocumentID, [EventProperty])
extractDocID = extract didProp
  where
    didProp (DocIDProp x) = Just x
    didProp _             = Nothing

-- | Separate the doc ID property from the rest, if present.
--   More than one doc ID is not OK either.
extractCompanyID :: [EventProperty] -> Maybe (CompanyID, [EventProperty])
extractCompanyID = extract cidProp
  where
    cidProp (CompanyIDProp x) = Just x
    cidProp _                 = Nothing

-- | Extract a certain property from the rest. There has to be exactly one
--   instance of this property, or the result will be Nothing.
extract :: (EventProperty -> Maybe a) -> [EventProperty] -> Maybe (a, [EventProperty])
extract fromprop props =
    case partition predicate props of
      ([prop], props') | Just prop' <- fromprop prop ->
        Just (prop', props')
      _ ->
        Nothing
  where
    predicate = isJust . fromprop
