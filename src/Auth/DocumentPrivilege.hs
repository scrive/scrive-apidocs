-- | Privileges that have to do with a specific, existing document

module Auth.DocumentPrivilege where

data DocumentPrivilege = DocumentSend -- ^ Send invitations/reminders/etc
                       | DocumentView -- ^ View a document
                       | DocumentEdit
               deriving (Show, Eq)
