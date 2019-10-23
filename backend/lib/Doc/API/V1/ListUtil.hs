-----------------------------------------------------------------------------
-- |
-- Module      :  ListUtil
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Utils for API V1 lists. No used anywhere else anymore - should be removed when V1 is dropped.
-----------------------------------------------------------------------------
module Doc.API.V1.ListUtil(
              PagedList(..)
            , ListParams
            , emptyListParams
            , getListParams
            , pagingParamsJSON
            , listParamsSearching
            , listParamsSorting
            , listParamsOffset
            , listParamsLimit
            , listParamsFilters
          ) where

import Control.Monad.Identity
import Control.Monad.Trans
import Happstack.Server hiding (simpleHTTP)
import Network.HTTP.Base (urlEncode)
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen
import Text.JSON.String (runGetJSON)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T

import Happstack.Fields
import Utils.Prelude

-- This part is responsible for sorting,searching and paging documents lists
data PagedList a = PagedList
  { list       :: [a]
  , pageSize   :: Int
  , params     :: ListParams
  , listLength :: Int
  } deriving Show

data ListParams = ListParams
  { sorting :: [Text]
  , search  :: Maybe Text
  , filters :: [(Text, Text)]
  , offset  :: Int
  , limit   :: Int
  }
    deriving (Eq)

listParamsSorting :: ListParams -> [Text]
listParamsSorting = sorting

listParamsSearching :: ListParams -> Text
listParamsSearching params = fromMaybe "" (search params)

listParamsOffset :: ListParams -> Int
listParamsOffset = offset

listParamsLimit :: ListParams -> Int
listParamsLimit = limit

listParamsFilters :: ListParams -> [(Text, Text)]
listParamsFilters = filters

instance Show ListParams where
  show params = intercalate "&" $ off <> lim <> srch <> srt
    where
      off   = ["offset=" <> (toUrl $ show $ offset params)]
      lim   = ["limit=" <> (toUrl $ show $ limit params)]
      srch  = map ((<>) "search=") $ maybeToList $ (toUrl . T.unpack) <$> search params
      srt   = map ((<>) "sorting=") $ (toUrl . T.unpack) <$> sorting params
      toUrl = urlEncode . BS8.unpack . BS.fromString

emptyListParams :: ListParams
emptyListParams =
  ListParams { sorting = [], search = Nothing, filters = [], offset = 0, limit = 1000 }

{- New version working with JSON interface-}
getListParams :: (ServerMonad m, Functor m, HasRqData m, MonadIO m) => m ListParams
getListParams = do
  offset' <- readField "offset"
  limit'  <- readField "limit"
  search  <- getField "textfilter"
  filters <- do
    eja <- liftM (runGetJSON readJSArray) $ T.unpack <$> getField' "selectfilter"
    return $ case eja of
      Left _ -> []
      Right ja ->
        fromMaybe [] $ runIdentity $ withJSValue ja $ fromJSValueCustomMany $ do
          n <- fromJSValueField "name"
          v <- fromJSValueField "value"
          return $ liftM2 (\x y -> (T.pack x, T.pack y)) n v
  sorting         <- getField "sort"
  sortingReversed <- maybeToBool <$> fmap (== "true") <$> getField "sortReversed"
  let sorting' = if (sortingReversed) then sorting else (<> "REV") <$> sorting

  return ListParams
         -- REVIEW: I am assuming constants below stem from emptyListParams.
                    { offset = fromMaybe (offset emptyListParams) offset'
                    , limit = fromMaybe (limit emptyListParams) limit'
                    , search = search
                    , filters = filters
                    , sorting = maybeToList sorting'
                    }

pagingParamsJSON :: PagedList a -> JSValue
pagingParamsJSON (PagedList { pageSize, params, listLength }) = runJSONGen $ do
  value "pageCurrent" $ offset params `div` pageSize
  value "itemMin" $ offset params
  value "itemMax" $ listLength - 1
  value "maxNextPages" $ (limit params) `div` pageSize
  value "pageSize" $ pageSize
