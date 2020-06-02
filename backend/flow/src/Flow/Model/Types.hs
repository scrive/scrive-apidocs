{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Flow.Model.Types where

import GHC.Generics (Generic)

import Doc.DocumentID (DocumentID)
import Flow.Id
import User.UserID (UserID)
import UserGroup.Internal (UserGroupID)

data InsertTemplate = InsertTemplate
    { name :: Text
    , process :: Text
    , userId :: UserID
    , userGroupId :: UserGroupID
    }
  deriving (Show, Eq, Generic)

data Instance = Instance
    { id :: InstanceId
    , templateId :: TemplateId
    }

data StoreValue
    = StoreDocumentId DocumentID
    | StoreUserId UserID
    | StoreEmail Text
    | StorePhoneNumber Text
    | StoreMessage Text
  deriving (Show, Eq, Generic)

data StoreValueType
    = Document
    | User
    | Email
    | PhoneNumber
    | Message

storeValueTypeToText :: StoreValueType -> Text
storeValueTypeToText Document    = "document"
storeValueTypeToText User        = "user"
storeValueTypeToText Email       = "email"
storeValueTypeToText PhoneNumber = "phone_number"
storeValueTypeToText Message     = "message"
