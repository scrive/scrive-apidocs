module Cookies ( addCookie
               , addHttpOnlyCookie
               , lookCookieNames
               , lookCookieValue
               , lookCookieValues
               ) where

import Control.Monad.IO.Class
import Data.Char
import Data.List.Split
import Data.String.Utils (strip)
import Happstack.Server hiding (addCookie, lookCookieValue)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Happstack.Server as HS

import Utils.List (rStripFrom)

-- | Add a non-http only cookie
addCookie :: (MonadIO m, FilterMonad Response m) => Bool -> CookieLife -> Cookie -> m ()
addCookie issecure life cookie =
  HS.addCookie life $ cookie { secure = issecure, httpOnly = False }

addHttpOnlyCookie :: (MonadIO m, FilterMonad Response m) => Bool -> CookieLife -> Cookie -> m ()
addHttpOnlyCookie issecure life cookie = do
  HS.addCookie life $ cookie { secure = issecure, httpOnly = True }

{-
  This parses cookies from `cookie-string` (see RFC 6265).

  Instead of following official syntax to the letter, split on semicolons
  and try to parse each `cookie-pair`, but if one pair fails to parse,
  still return values of other proper cookie-pairs. Thanks to this, when
  some domain adds a broken cookie, it will not break all other cookies.
-}
parseCookieHeader :: String -> [(String, String)]
parseCookieHeader = catMaybes . map cookieFromPair . cookiePairs
  where -- split cookie-string on semicolons to get a list of cookie-pairs
        -- e.g. "key1=val1; key2=val2" -> ["key1=val1", "key2=val2"]
        cookiePairs :: String -> [String]
        cookiePairs = map strip . splitOn ";"

        -- parse cookie-pair string to a cookie key/value pair
        -- e.g. "key=val" -> Just ("key", "val")
        cookieFromPair :: String -> Maybe (String, String)
        cookieFromPair cookiePair = case splitOn "=" cookiePair of
                                      [] -> Nothing -- no '=' - no cookie
                                      (cookieName:cookieValueElems) -> do
                                        -- cookie value can contain '=' so rejoin it
                                        let cookieValueString = intercalate "=" cookieValueElems
                                        cookieValue <- parseCookieValue cookieValueString
                                        return (map toLower cookieName, cookieValue)

        -- parse `cookie-value` (from RFC 6265)
        -- not as strict - there's no need to
        parseCookieValue :: String -> Maybe String
        parseCookieValue ('"': rest) | "\"" `isSuffixOf` rest = Just $ unquote $ rStripFrom "\"" rest
                                     | otherwise = Nothing
        parseCookieValue s = Just s

        -- unquote characters, just like quotedPair in Happstack.Server.Internal.cookiesParser[quotedPair]
        unquote :: String -> String
        unquote ""   = ""
        unquote "\\" = ""
        unquote ('\\':c:rest) = c : unquote rest
        unquote (c:rest) = c : unquote rest

-- To protect against overload attack, we limit number of session cookies supported to 10.
lookCookieValues :: String -> Headers -> [String]
lookCookieValues name headers =
  let mCookieHeader = M.lookup "cookie" headers
      cookieHeaderValues = map BS.unpack $ maybe [] hValue mCookieHeader
      cookiesWithNames = concatMap parseCookieHeader cookieHeaderValues
      cookies = take 10 $ map snd $ filter (\c -> (fst c) == (map toLower name)) cookiesWithNames
  in  cookies

lookCookieNames :: Headers -> [String]
lookCookieNames headers =
  let mCookieHeader = M.lookup "cookie" headers
      cookieHeaderValues = map BS.unpack $ maybe [] hValue mCookieHeader
      cookiesWithNames = concatMap parseCookieHeader cookieHeaderValues
  in map fst cookiesWithNames

lookCookieValue :: String -> Headers -> Maybe String
lookCookieValue name = listToMaybe . lookCookieValues name
