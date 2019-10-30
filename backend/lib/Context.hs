{-# LANGUAGE TemplateHaskell #-}
module Context
  ( I.Context
  , ctxDomainUrl
  , contextUser
  , I.anonymiseContext
  , contextToMailContext
  ) where

import Optics

import User.Types.User (User)
import qualified Context.Internal as I
import qualified MailContext.Internal as I

ctxDomainUrl :: Lens' I.Context Text
ctxDomainUrl = #ctxBrandedDomain % #bdUrl

-- | Get a user from `Context` (user takes precedence over pad user).
contextUser :: AffineFold I.Context User
contextUser = (#ctxMaybeUser % _Just) `afailing` (#ctxMaybePadUser % _Just)

contextToMailContext :: I.Context -> I.MailContext
contextToMailContext ctx = I.MailContext
  { mctxLang                 = ctx ^. #ctxLang
  , mctxCurrentBrandedDomain = ctx ^. #ctxBrandedDomain
  , mctxTime                 = ctx ^. #ctxTime
  , mctxMailNoreplyAddress   = ctx ^. #ctxMailNoreplyAddress
  }
