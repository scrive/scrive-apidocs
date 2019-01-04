module UserGroup.JSON (
    unjsonUserGroupWithChildren
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Time
import Data.Unjson

import Chargeable.Model (GetNumberOfDocumentsStartedThisMonth(..))
import DB
import FeatureFlags.Model
import User.Model.Query (UserGroupGetUsers(..))
import UserGroup.Types
import UserGroup.Types.PaymentPlan

ugWithChildrenToJson (UserGroupWithChildren ug children) = object [
    "group" .= object
      [ "name" .= get ugName ug
      , identifier $ get ugID ug
      ]
  , "children" .= map ugWithChildrenToJson children
  ]
