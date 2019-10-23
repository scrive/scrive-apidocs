module Context (
    Context
  , ctxDomainUrl
  , getContextUser
  , anonymiseContext
  , contextToMailContext
  , module Context.Labels
  ) where

import Data.Label

import BrandedDomain.BrandedDomain
import Context.Internal
import Context.Labels
import MailContext.Internal (MailContext(..))
import User.Types.User (User)

ctxDomainUrl :: Context :-> Text
ctxDomainUrl = bdUrl . ctxbrandeddomain

-- | Get a user from `Context` (user takes precedence over pad user).
getContextUser :: Context -> Maybe User
getContextUser ctx = get ctxmaybeuser ctx `mplus` get ctxmaybepaduser ctx

contextToMailContext :: Context -> MailContext
contextToMailContext ctx = MailContext
  { _mctxlang                 = get ctxlang ctx
  , _mctxcurrentBrandedDomain = get ctxbrandeddomain ctx
  , _mctxtime                 = get ctxtime ctx
  , _mctxmailNoreplyAddress   = get ctxmailnoreplyaddress ctx
  }
