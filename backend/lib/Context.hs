module Context (
    Context
  , ctxDomainUrl
  , getContextUser
  , anonymiseContext
  , contextToMailContext
  ) where

import Optics

import Context.Internal
import Context.Labels ()
import MailContext.Internal (MailContext(..))
import User.Types.User (User)

ctxDomainUrl :: Lens' Context Text
ctxDomainUrl = #ctxBrandedDomain % #bdUrl

-- | Get a user from `Context` (user takes precedence over pad user).
getContextUser :: Context -> Maybe User
getContextUser ctx = ctxMaybeUser ctx `mplus` ctxMaybePadUser ctx

contextToMailContext :: Context -> MailContext
contextToMailContext ctx = MailContext
  { mctxLang                 = ctxLang ctx
  , mctxCurrentBrandedDomain = ctxBrandedDomain ctx
  , mctxTime                 = ctxTime ctx
  , mctxMailNoreplyAddress   = ctxMailNoreplyAddress ctx
  }
