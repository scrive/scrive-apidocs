{-# LANGUAGE ViewPatterns #-}

module Cookies ( addCookie
               , addHttpOnlyCookie
               , lookCookieNames
               , lookCookieValue
               , lookCookieValues
               ) where

import Control.Monad.IO.Class
import Happstack.Server hiding (addCookie, lookCookieValue)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Happstack.Server as HS

import Utils.List (rStripFrom)

-- | Add a non-http only cookie
addCookie :: (MonadIO m, FilterMonad Response m) => Bool -> CookieLife -> Cookie -> m ()
addCookie issecure life cookie =
  HS.addCookie life $ cookie { secure = issecure, httpOnly = False }

addHttpOnlyCookie
  :: (MonadIO m, FilterMonad Response m) => Bool -> CookieLife -> Cookie -> m ()
addHttpOnlyCookie issecure life cookie = do
  HS.addCookie life $ cookie { secure = issecure, httpOnly = True }

{-
  This parses cookies from `cookie-string` (see RFC 6265).

  Instead of following official syntax to the letter, split on semicolons
  and try to parse each `cookie-pair`, but if one pair fails to parse,
  still return values of other proper cookie-pairs. Thanks to this, when
  some domain adds a broken cookie, it will not break all other cookies.
-}
parseCookieHeader :: Text -> [(Text, Text)]
parseCookieHeader = catMaybes . map cookieFromPair . cookiePairs
  where -- split cookie-string on semicolons to get a list of cookie-pairs
        -- e.g. "key1=val1; key2=val2" -> ["key1=val1", "key2=val2"]
    cookiePairs :: Text -> [Text]
    cookiePairs = map T.strip . T.splitOn ";"

    -- parse cookie-pair string to a cookie key/value pair
    -- e.g. "key=val" -> Just ("key", "val")
    cookieFromPair :: Text -> Maybe (Text, Text)
    cookieFromPair cookiePair = case T.splitOn "=" cookiePair of
      [] -> Nothing -- no '=' - no cookie
      (cookieName : cookieValueElems) -> do
        -- cookie value can contain '=' so rejoin it
        let cookieValueString = T.intercalate "=" cookieValueElems
        cookieValue <- parseCookieValue cookieValueString
        return (T.toLower $ cookieName, cookieValue)

    -- parse `cookie-value` (from RFC 6265)
    -- not as strict - there's no need to
    parseCookieValue :: Text -> Maybe Text
    parseCookieValue (T.unpack -> '"': rest)
      | "\"" `isSuffixOf` rest = Just $ T.pack $ unquote $ rStripFrom "\"" rest
      | otherwise              = Nothing
    parseCookieValue s = Just s

    -- unquote characters, just like quotedPair in Happstack.Server.Internal.cookiesParser[quotedPair]
    unquote :: String -> String
    unquote ""                = ""
    unquote "\\"              = ""
    unquote ('\\' : c : rest) = c : unquote rest
    unquote (c        : rest) = c : unquote rest

-- To protect against overload attack, we limit number of session cookies supported to 10.
lookCookieValues :: Text -> Headers -> [Text]
lookCookieValues name headers =
  let
    mCookieHeader      = M.lookup "cookie" headers
    cookieHeaderValues = map TE.decodeUtf8 $ maybe [] hValue mCookieHeader
    cookiesWithNames   = concatMap parseCookieHeader cookieHeaderValues
    cookies =
      take 10 $ map snd $ filter (\c -> (fst c) == (T.toLower name)) cookiesWithNames
  in
    cookies

lookCookieNames :: Headers -> [Text]
lookCookieNames headers =
  let mCookieHeader      = M.lookup "cookie" headers
      cookieHeaderValues = map TE.decodeUtf8 $ maybe [] hValue mCookieHeader
      cookiesWithNames   = concatMap parseCookieHeader cookieHeaderValues
  in  map fst cookiesWithNames

lookCookieValue :: Text -> Headers -> Maybe Text
lookCookieValue name = listToMaybe . lookCookieValues name
