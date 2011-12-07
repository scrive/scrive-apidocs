
module Auth.Privilege where

import Auth.DocumentPrivilege
import Auth.UserPrivilege
import Misc

-- | Privilege tokens; actions should fall into one of these
-- Privileges. Please think about granularity before adding a new one.
data Privilege = DocumentPrivilege DocumentPrivilege
               | UserPrivilege     UserPrivilege
                 deriving (Show, Eq)
                          
-- An informal standard of User Privileges being 2xx, doc privileges
-- in 1xx
instance SafeEnum Privilege where
  fromSafeEnum (UserPrivilege   DocumentCreate) = 200
  fromSafeEnum (DocumentPrivilege DocumentSend) = 100
  fromSafeEnum (DocumentPrivilege DocumentSign) = 101
  fromSafeEnum (DocumentPrivilege DocumentView) = 102
  fromSafeEnum (DocumentPrivilege DocumentEdit) = 103
  
  toSafeEnum 200 = Just (UserPrivilege   DocumentCreate)
  toSafeEnum 100 = Just (DocumentPrivilege DocumentSend)
  toSafeEnum 101 = Just (DocumentPrivilege DocumentSign)
  toSafeEnum 102 = Just (DocumentPrivilege DocumentView)
  toSafeEnum 103 = Just (DocumentPrivilege DocumentEdit)
  toSafeEnum _   = Nothing
