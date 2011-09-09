module Util.KontraLinkUtils where

import Context
import KontraLink
import KontraMonad

getHomeOrUploadLink :: KontraMonad m => m KontraLink
getHomeOrUploadLink = do
  Context{ctxmaybeuser, ctxregion, ctxlang} <- getContext
  case ctxmaybeuser of
    Just _ -> return LinkUpload
    Nothing -> return $ LinkHome ctxregion ctxlang 

