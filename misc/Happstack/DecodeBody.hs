module Happstack.DecodeBody where

import Control.Monad.IO.Class
import Happstack.Server
import Happstack.Server.Internal.MessageWrap

-- | Sane variant of 'decodeBody' that doesn't use 'WebMonad'.
withDecodedBody :: (FilterMonad Response m, ServerMonad m, MonadIO m)
                => BodyPolicy
                -> m Response
                -> m Response
withDecodedBody bp action = do
  rq <- askRq
  (_, me) <- bodyInput bp rq
  case me of
    Just e  -> requestEntityTooLarge (toResponse e)
    Nothing -> action
