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
            , getListParams
            , getListParamsForSearch
            , pagedListFields
            , listSortSearchPage
            , SortingFunction
            , SearchingFunction
            , PageSize
            , viewComparing
            , viewComparingRev
            , pagingParamsJSON
          ) where
import Control.Applicative ((<$>))
import Control.Monad.Trans
import Control.Monad
import Data.List
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Ord
import Misc
import Templates.Templates
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Happstack.Server hiding (simpleHTTP)
import Network.HTTP.Base (urlEncode)
import Text.JSON

-- This part is responsible for sorting,searching and paging documents lists
data PagedList a = PagedList{
        list::[a]
      , totalCount::Int
      , pageSize :: Int
      , params::ListParams
    } deriving Show

data ListParams = ListParams {
      sorting      :: [String]
    , search       :: Maybe String
    , page         :: Int }
    deriving (Eq)

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
pagingParamsJSON (PagedList{list,pageSize,totalCount,params}) = JSObject $ toJSObject [
    ("pageMax",showJSON totalPages ),
    ("pageCurrent", showJSON $ (page params) - 1),
    ("itemMin",showJSON $ minElementIndex),
    ("itemMax",showJSON $ minElementIndex + length list - 1),
    ("itemTotal",showJSON $ totalCount)
    ]
    where
    totalPages =  (totalCount -1) `div` pageSize 
    minElementIndex = pageSize * (page params - 1) 


{- | Getting sorting , paging and filtering params-}
getListParams :: (ServerMonad m,Functor m,HasRqData m,MonadIO m) => m ListParams
getListParams = do
    page <- readField "page"
    search <- getField "search"
    sorting <- getFields "sorting"
    return ListParams {
        page     = fromMaybe 1 page
        , search  = search
        -- This take 1 is disabling sort by many columns functionality. There is no good spec for it now
        , sorting  = take 1 $ reverse $ nub $ clearSortList sorting }
    where
    clearSortList (s:ss) =
        if (any (\s' -> isPrefixOf s s' || isPrefixOf s' s) ss)
         then  clearSortList ss
         else s:(clearSortList ss)
    clearSortList [] = []

getListParamsForSearch :: (ServerMonad m,Functor m,HasRqData m,MonadIO m)  => m ListParams
getListParamsForSearch = do
    search <- getField "search"
    return $ emptyListParams { search  = search}


{- Standard fields-}
pagedListFields :: (Functor m, MonadIO m) => PagedList a -> Fields m
pagedListFields (PagedList{list,pageSize,totalCount,params}) = do
    fieldF "params" $ do
        fieldF "sorting" $ mapM_ (\sp -> field sp sp) $ sorting params
        field "search" $ join $ nothingIfEmpty <$> search params
    fieldFL "pages" $ for [1..totalPages] $ \n -> do
        field "nr" $ show $ n
        field "current" $ n == (page params)
    fieldF "elements" $ do
        field "min" $ show $ minElementIndex
        field "max" $ show $ minElementIndex + length list - 1
        field "total" $ show totalCount
        field "totalPages" $ show $ totalPages
        field "none" $ null list
        field "single" $ length list == 1
        field "firstPageAvaible" $ page params > 1 && not (null list)
        field "lastPageAvaible" $ page params < totalPages && not (null list)
        field "nextPage" $ show $ page params + 1
        field "lastPage" $ show $ page params - 1

    where
    totalPages = (totalCount -1) `div` pageSize + 1
    minElementIndex = pageSize * (page params - 1) +1

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
    in  PagedList {list=paged , params = params, totalCount = length searched, pageSize = pageSize}

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
