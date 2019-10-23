module ThirdPartyStats.Utils (
    extractUID
  ) where

import ThirdPartyStats.Core
import User.UserID (UserID)

-- | Separate the user ID property from the rest, if present.
--   More than one UID is not OK either.
extractUID :: [EventProperty] -> Maybe (UserID, [EventProperty])
extractUID = extract uidProp
  where
    uidProp (UserIDProp x) = Just x
    uidProp _              = Nothing

-- | Extract a certain property from the rest. There has to be exactly one
--   instance of this property, or the result will be Nothing.
extract :: (EventProperty -> Maybe a) -> [EventProperty] -> Maybe (a, [EventProperty])
extract fromprop props = case partition predicate props of
  ([prop], props') | Just prop' <- fromprop prop -> Just (prop', props')
  _ -> Nothing
  where predicate = isJust . fromprop
