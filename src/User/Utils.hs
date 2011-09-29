module User.Utils where

import User.Model
import KontraMonad
import DB.Classes
import Company.Model

{- |
    This looks up the company for the given user, if the user doesn't
    have a company then it returns Nothing.
-}
getCompanyForUser :: Kontrakcja m => User -> m (Maybe Company)
getCompanyForUser user = maybe (return Nothing) (runDBQuery . GetCompany) (usercompany user)
