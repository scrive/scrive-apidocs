module Context
  ( I.Context
  , contextUser
  , I.anonymiseContext
  , contextToMailContext
  ) where

import User.Types.User (User)
import qualified Context.Internal as I
import qualified MailContext.Internal as I

-- | Get a user from `Context` (user takes precedence over pad user).
contextUser :: I.Context -> Maybe User
contextUser ctx = ctx ^. #maybeUser <|> ctx ^. #maybePadUser

contextToMailContext :: I.Context -> I.MailContext
contextToMailContext ctx = I.MailContext
  { lang               = ctx ^. #lang
  , brandedDomain      = ctx ^. #brandedDomain
  , time               = ctx ^. #time
  , mailNoreplyAddress = ctx ^. #mailNoreplyAddress
  }
