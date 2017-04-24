{-# OPTIONS_GHC -Wwarn #-}
module Happstack.Server.RqDataExtra where

import Codec.Text.IConv (convert)
import Control.Monad.Error (strMsg)
import Data.String.Utils (split, strip)
import Happstack.Server
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL

import KontraPrelude

-- Just like Happstack's lookBS, but also returns charset of input if it was provided in a multipart chunk
lookBSWithCharset :: (Functor m, Monad m, HasRqData m) => String -> m (Maybe String, BSL.ByteString)
lookBSWithCharset key = do
  i <- lookInput key
  case inputValue i of
    Left _ -> rqDataError $ (strMsg $ "myLook: " ++ key ++ " is a file.")
    Right bs -> return $ (lookup "charset" $ ctParameters $ inputContentType i, bs)

-- Just like Happstack's getDataFn, but result is wrapped in Maybe instead of Either.
-- If value was found in a multipart chunk provided with its own Content-Type (including charset)
-- it gets decoded using that. Otherwise value is decoded using Charset found in request's Content-Type header.
-- Or UTF-8 in the worst case.
getDataFnWithDecode' :: (HasRqData m, ServerMonad m) => RqData (Maybe String, BSL.ByteString) -> m (Maybe String)
getDataFnWithDecode' fun = do
  mCharset <- rqCharset <$> askRq
  -- defaultEncoding is request's Content-Type charset if present of UTF-8
  let defaultEncoding = fromMaybe "UTF-8" mCharset
  x <- getDataFn fun
  case x of
    Left _ -> return Nothing
    Right (mencoding, bs) ->
      -- decode using embedded multipart charset encoding or fallback to defaultEncoding
      let s = convert (fromMaybe defaultEncoding mencoding) "UTF-8" bs
      in return $ Just $ BSL.toString s

 where -- Return charset from request's Content-Type Header
       rqCharset :: Request -> Maybe String
       rqCharset rq = lookup "charset" =<< (parseCTHeaderParams <$> BS.unpack <$> getHeader "content-type" rq)

       -- parse content-type header params according to RFC 1341
       -- e.g. "aplication/video; charset=ISO-8859-1" -> [("charset", "ISO-8859-1")]
       parseCTHeaderParams :: String -> [(String, String)]
       parseCTHeaderParams s = case split ";" s of
                                 [] -> [] -- can't happen - split always returns non-empty list
                                 [_contentType] -> []
                                 (_contentType:params) -> catMaybes $ map (parseCTHeaderParam . strip) params

       -- parse something like "charset=ISO-8859-1"
       parseCTHeaderParam :: String -> Maybe (String, String)
       parseCTHeaderParam s = case split "=" s of
                                [] -> Nothing
                                [_] -> Just (s, "")
                                [k, v] -> Just (k, v)
                                _ -> Nothing
