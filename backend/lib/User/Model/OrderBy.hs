module User.Model.OrderBy (
    UserOrderBy(..)
  , userOrderByToSQL
  , userOrderByAscDescToSQL
  ) where

import DB

data UserOrderBy
  = UserOrderByName
  | UserOrderByEmail
  | UserOrderByAccountCreationDate

-- | Convert UserOrderBy enumeration into proper SQL order by statement
userOrderByToSQL :: UserOrderBy -> SQL
userOrderByToSQL UserOrderByName = "(users.first_name || ' ' || users.last_name)"
userOrderByToSQL UserOrderByEmail = "users.email"
userOrderByToSQL UserOrderByAccountCreationDate = "users.has_accepted_terms_of_service"

userOrderByAscDescToSQL :: AscDesc UserOrderBy -> SQL
userOrderByAscDescToSQL (Asc x@UserOrderByAccountCreationDate) =
  userOrderByToSQL x <+> "ASC NULLS FIRST "
userOrderByAscDescToSQL (Desc x@UserOrderByAccountCreationDate) =
  userOrderByToSQL x <+> "DESC NULLS LAST "
userOrderByAscDescToSQL (Asc  x) = userOrderByToSQL x
userOrderByAscDescToSQL (Desc x) = userOrderByToSQL x <+> "DESC"

