module User.Model.Filter (
    UserFilter(..)
  , userFilterToSQL
  ) where

import DB
import KontraPrelude

data UserFilter
  = UserFilterByString String             -- ^ Contains the string in name, email or anywhere

userFilterToSQL :: UserFilter -> SQL
userFilterToSQL (UserFilterByString string) =
  sqlConcatAND $ map (\wordpat -> sqlConcatOR [
      "users.first_name ILIKE" <?> wordpat
    , "users.last_name ILIKE" <?> wordpat
    , "users.email ILIKE" <?> wordpat
    , "translate(users.phone,'-+ .,()','') ILIKE translate(" <?> wordpat <+> ",'-+ .,()','')"
    , "translate(users.personal_number,'-+ .,()','') ILIKE translate(" <?> wordpat <+> ",'-+ .,()','')"
    ]) sqlwordpat
  where
      sqlwordpat = map (\word -> "%" ++ concatMap escape word ++ "%") (words string)
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]
