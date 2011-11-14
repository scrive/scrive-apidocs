-- | Privileges specific to a User (Creating docs, changing settings,
-- etc).

module Auth.UserPrivilege where

data UserPrivilege = DocumentCreate -- ^ Create a new document
               deriving (Show, Eq)
