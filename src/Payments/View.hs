module Payments.View where

import MagicHash
import Misc
import Data.Int
import Templates.Templates
import OAuth.Model
import qualified Templates.Fields as F
import User.Model
import User.UserView
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import Text.JSON

showSubscriptionDashboard :: TemplatesMonad m => User -> m String
showSubscriptionDashboard user = do
  renderTemplate "subscriptionDashboard" $ do
    menuFields user
