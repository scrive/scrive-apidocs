{-# LANGUAGE OverlappingInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ListUtil
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Datatype for sorting,searching and filtering params for all pages with lists of elements.
-- Used for every user-visible element table in our system. We support only operations on a server.
--
-- HOW To
-- 1) Define sorting rules, searchin rules and page size with types matching
--      type SortingFunction a = (String -> a -> a -> Ordering)
--      type SearchingFunction a = (String -> a -> Bool)
--      type PageSize = Int
-- 2) Define local aplication of 'listSortSearchPage' to them
-- 3) Use getListParams to get current request params for sorting etc. It uses some constant names for search params
-- 4) Call 2) 'listSortSearchPage' to get change input list to PagedList
--      PagedList is part of list that You want to show to the user with some extra info.
--      Basic list is avaible by call 'list'
-- 5) Pass this list to templates.
-- 6) You may also want to pagedListFields. Then You will be avaible to use some utils (like paging) from listutil.st
--    It requires a currentlink field to be set.
--
-- For example look in subaccounts list
-----------------------------------------------------------------------------
module ListUtil(
              PagedList(..)
            , ListParams
            , emptyListParams
            , getListParamsNew
            , getListParamsForSearch
            , listSortSearchPage
            , SortingFunction
            , SearchingFunction
            , PageSize
            , viewComparing
            , viewComparingRev
            , pagingParamsJSON

            , listParamsSearching
            , listParamsSorting
            , listParamsPage
          ) where
import Control.Applicative ((<$>))
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Ord
import Misc
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Happstack.Server hiding (simpleHTTP)
import Network.HTTP.Base (urlEncode)
import Text.JSON

-- This part is responsible for sorting,searching and paging documents lists
data PagedList a = PagedList{
        list::[a]
      , pageSize :: Int
      , params::ListParams
    } deriving Show

data ListParams = ListParams {
      sorting      :: [String]
    , search       :: Maybe String
    , page         :: Int }
    deriving (Eq)

listParamsSorting :: ListParams -> [String]
listParamsSorting = sorting

listParamsSearching :: ListParams -> String
listParamsSearching params = fromMaybe "" (search params)

listParamsPage :: ListParams -> Int
listParamsPage = page

instance Show ListParams where
    show params = intercalate "&" $ pg ++ srch ++ srt
        where
        pg =  ["page=" ++ (toUrl  $ show $ page params)]
        srch = map ((++) "search=") $ maybeToList $ toUrl  <$> search params
        srt = map ((++) "sorting=") $ toUrl  <$> sorting params
        toUrl = urlEncode . BS8.unpack . BS.fromString

emptyListParams :: ListParams
emptyListParams = ListParams {sorting=[], search=Nothing, page = 1}

{- New version working with JSON interface-}
getListParamsNew :: (ServerMonad m,Functor m,HasRqData m,MonadIO m) => m ListParams
getListParamsNew = do
    page <- readField "page"
    search <- getField "filter"
    sorting <- getField "sort"
    sortingReversed <- joinB <$> fmap (== "true") <$> getField "sortReversed"
    let sorting'  = if (sortingReversed)
                     then sorting
                     else (++ "REV") <$> sorting
    return ListParams {
          page     = 1 + (fromMaybe 0 page)
        , search  = search
        , sorting  = maybeToList sorting' }

pagingParamsJSON :: PagedList a -> JSValue
pagingParamsJSON (PagedList{list,pageSize,params}) = JSObject $ toJSObject [
    ("pageCurrent", showJSON $ (page params) - 1),
    ("itemMin",showJSON $ minElementIndex),
    ("itemMax",showJSON $ minElementIndex + length list - 1),
    ("pageSize", showJSON $ pageSize)
    ]
    where
    minElementIndex = pageSize * (page params - 1) 


getListParamsForSearch :: (ServerMonad m,Functor m,HasRqData m,MonadIO m)  => m ListParams
getListParamsForSearch = do
    search <- getField "search"
    return $ emptyListParams { search  = search}


{- | Applying  params to the list -}
type SortingFunction a = (String -> a -> a -> Ordering)
type SearchingFunction a = (String -> a -> Bool)
type PageSize = Int

listSortSearchPage ::
    SortingFunction a ->
    SearchingFunction a ->
    PageSize ->
    ListParams ->
    [a] -> PagedList a

listSortSearchPage sortFunc searchFunc pageSize params list =
    let
        searched = doSearching searchFunc (search params) list
        sorted = doSorting sortFunc (sorting params) searched
        paged = doPaging pageSize (page params)  sorted
    in  PagedList {list=paged , params = params, pageSize = pageSize}

doSorting::SortingFunction a -> [String] -> [a] -> [a]
doSorting sortFunc  = sortBy . compareList .  map sortFunc
    where compareList l a1 a2 = foldMap (\f -> f a1 a2) l


class ViewOrd a where
    viewCompare:: a -> a -> Ordering

instance ViewOrd String where
    viewCompare = comparing (map toUpper)

instance (Ord a) => ViewOrd a where
    viewCompare = compare

viewComparing:: (ViewOrd a) => (b -> a) -> b -> b -> Ordering
viewComparing f a1 a2 = viewCompare (f a1) (f a2)

viewComparingRev:: (ViewOrd a) => (b -> a) -> b -> b -> Ordering
viewComparingRev f a1 a2 = viewCompare (f a2) (f a1)

doSearching::SearchingFunction a -> Maybe String -> [a] -> [a]
doSearching _ Nothing = id
doSearching searchFunc (Just s) = filter (searchFunc s)

doPaging:: Int -> Int -> [a] -> [a]
doPaging pageSize page = (take pageSize) . (drop $ (page-1)*pageSize)
