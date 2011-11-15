module Auth.UserAuthorization where

import User.Model
import MinutesTime
import Auth.UserPrivilege
import Data.Semantic

-- | Instances of this class determine whether an Authorization can
-- act on the User
class UserAuthorization a where
  canUser :: a -> UserID -> MinutesTime -> UserPrivilege -> Bool
  canUser _ _ _ _ = False

-- standard recursive instances
instance UserAuthorization a => UserAuthorization (Maybe a) where
  canUser Nothing _ _ _ = False
  canUser (Just a) x y z = canUser a x y z
  
instance UserAuthorization a => UserAuthorization (Either x a) where
  canUser (Left _) _ _ _ = False
  canUser (Right a) x y z = canUser a x y z
  
instance (UserAuthorization a, UserAuthorization b) => UserAuthorization (Or a b) where
  canUser (Or a b) x y z = canUser a x y z || canUser b x y z
