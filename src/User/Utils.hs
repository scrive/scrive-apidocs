module User.Utils where

import User.Model
import DB.Classes
import Company.Model

{- |
    This looks up the company for the given user, if the user doesn't
    have a company then it returns Nothing.
-}
getCompanyForUser :: DBMonad m => User -> m (Maybe Company)
getCompanyForUser = maybe (return Nothing) (runDBQuery . GetCompany) . usercompany

-- | Version to be executed within DB monad (currently used in tests only)
getCompanyForUser' :: User -> DB (Maybe Company)
getCompanyForUser' = maybe (return Nothing) (dbQuery . GetCompany) . usercompany
