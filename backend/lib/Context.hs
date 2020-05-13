module Context
  ( Context(Context)
  , contextUser
  , anonymiseContext
  , contextToMailContext
  ) where

import Context.Internal
import MailContext
import User.Types.User (User)
import qualified MailContext.Internal

-- | Get a user from `Context` (user takes precedence over pad user).
contextUser :: Context -> Maybe User
contextUser ctx = ctx ^. #maybeUser <|> ctx ^. #maybePadUser

contextToMailContext :: Context -> MailContext
contextToMailContext ctx = MailContext { lang               = ctx ^. #lang
                                       , brandedDomain      = ctx ^. #brandedDomain
                                       , time               = ctx ^. #time
                                       , mailNoreplyAddress = ctx ^. #mailNoreplyAddress
                                       }
