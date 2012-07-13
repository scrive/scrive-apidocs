module Payments.View where

--import Data.Int
--import Text.JSON
--import Text.JSON.Gen hiding (value)
--import qualified Text.JSON.Gen as J

--import MagicHash
--import Misc
--import OAuth.Model
import Templates.Templates
import User.Model
import User.UserView
--import qualified Templates.Fields as F

showSubscriptionDashboard :: TemplatesMonad m => User -> m String
showSubscriptionDashboard user = do
  renderTemplate "subscriptionDashboard" $ do
    menuFields user


