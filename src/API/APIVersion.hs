module API.APIVersion
 ( APIVersion(..)
 ) where

data APIVersion =
  Frontend -- Access from our own JS frontend
  -- Below: access to published API versions from clients
 |  V1
  deriving (Eq, Show)
