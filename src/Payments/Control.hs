module Payments.Control where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Text.JSON (JSValue(..), toJSObject, showJSON)

import ActionQueue.Core
import ActionQueue.EmailChangeRequest
import ActionQueue.PasswordReminder
import ActionQueue.UserAccountRequest
import AppView
import Crypto.RNG
import DB hiding (update, query)
import Doc.Action
import Company.Model
import Control.Logic
import InputValidation
import Kontra
import KontraLink
import MagicHash (MagicHash)
import Mails.SendMail
import MinutesTime
import Misc
import Redirect
import Templates.Templates
import User.Model
import User.UserView
import Util.FlashUtil
import Util.MonadUtils
import Util.HasSomeUserInfo
import qualified Log
import Stats.Control
import User.Action
import User.Utils
import User.History.Model
import ScriveByMail.Model
import Payments.View
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import Text.JSON

handleSubscriptionDashboard :: Kontrakcja m => m (Either KontraLink Response)
handleSubscriptionDashboard = checkUserTOSGet $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  showSubscriptionDashboard user >>= renderFromBody kontrakcja
  
handleSubscriptionDashboardInfo :: Kontrakcja m => m (Either KontraLink JSValue)
handleSubscriptionDashboardInfo = checkUserTOSGet $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  runJSONGenT $ return ()