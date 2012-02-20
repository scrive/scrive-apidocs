--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Utils
-- Copyright : (c) 2006-2009, Galois, Inc. 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability : provisional
-- Portability: portable
--
-- Extracting content from MIME values and types.
-- 
--------------------------------------------------------------------
module Codec.MIME.Utils
  ( 
    getByAttachmentName
  , charset
  )  where

import Codec.MIME.Type
import Codec.MIME.Decode
import Data.Maybe
import Data.List ( find )
import qualified Data.ByteString.UTF8 as BS

{-
-- | Given a parameter name, locate it within a MIME value,
-- returning the corresponding (sub) MIME value.
-- (this function never worked but was unused! -- Eric)
findMultipartNamed :: String -> MIMEValue -> Maybe MIMEValue
findMultipartNamed nm mv =
 case mime_val_content mv of
   Multi ms  -> msum (map (findMultipartNamed nm) ms)
   Single {} -> do
      cd <- mime_val_disp mv
      find (withDispName nm) (dispParams cd)
      return mv
 where withDispName a (Name b) = a == b
       withDispName _ _ = False
-}


getByAttachmentName :: String -> [(Type, BS.ByteString)] -> Maybe (Type, BS.ByteString)
getByAttachmentName name ps =
  find byname ps
    where byname p = case lookup "name" (mimeParams $ fst p) of
            Just n' -> name == decodeWords n'
            _       -> False

charset :: Type -> String
charset mimetype = fromMaybe "us-ascii" $ lookup "charset" (mimeParams mimetype)

