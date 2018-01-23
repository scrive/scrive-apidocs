module Context (
    Context
  , ctxDomainUrl
  , getContextUser
  , anonymousContext
  , contextToMailContext
  , module Context.Labels
  ) where

import BrandedDomain.BrandedDomain
import Context.Internal
import Context.Labels
import MailContext.Internal (MailContext(..))
import User.Data.User (User)

ctxDomainUrl :: Context -> String
ctxDomainUrl = get (bdUrl . ctxbrandeddomain)

-- | Get a user from `Context` (user takes precedence over pad user).
getContextUser :: Context -> Maybe User
getContextUser ctx = get ctxmaybeuser ctx `mplus` get ctxmaybepaduser ctx

contextToMailContext :: Context -> MailContext
contextToMailContext ctx = MailContext {
    _mctxlang                 = get ctxlang ctx
  , _mctxcurrentBrandedDomain = get ctxbrandeddomain ctx
  , _mctxtime                 = get ctxtime ctx
  , _mctxmailNoreplyAddress   = get ctxmailnoreplyaddress ctx
  }
