module Util.KontraLinkUtils where

import Context
import KontraLink
import KontraMonad

getHomeOrUploadLink :: KontraMonad m => m KontraLink
getHomeOrUploadLink = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Just _ -> return LinkUpload
    Nothing -> return $ LinkHome (ctxlocale ctx)

