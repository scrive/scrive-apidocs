module User.Model.Filter (
    UserFilter(..)
  , userFilterToSQL
  ) where

import DB

data UserFilter
  = UserFilterByString String             -- ^ Contains the string in name, email or anywhere
  | UserFilterWithAnyDocumentRetentionPolicy

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
userFilterToSQL UserFilterWithAnyDocumentRetentionPolicy =
  sqlConcatOR . map (\idle_setting -> "users." <> idle_setting <+> "IS NOT NULL") $
    [ "idle_doc_timeout_preparation"
    , "idle_doc_timeout_closed"
    , "idle_doc_timeout_canceled"
    , "idle_doc_timeout_timedout"
    , "idle_doc_timeout_rejected"
    , "idle_doc_timeout_error"
    ]
