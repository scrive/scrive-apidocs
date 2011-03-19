{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ListUtil
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Datatype for sorting,searching and filtering params for all pages with lists of elements
--
-----------------------------------------------------------------------------
module ListUtil(
              PagedList(..)
            , ListParams
            , emptyListParams
            , getListParams
            , getListParamsForSearch
            , pagedListFields
            , listSortSearchPage
            , SortingFunction 
            , SearchingFunction 
            , PageSize
            , rcomparing
            , comparing
          ) where
import Control.Applicative ((<$>))
import Control.Monad.Trans
import Control.Monad
import Data.Data
import Data.List 
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Ord
import Doc.DocState
import MinutesTime
import Misc
import Templates.Templates
import Templates.TemplatesUtils
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Data.List (isInfixOf,sortBy)
import Data.Monoid
import Happstack.Server hiding (simpleHTTP)
import Templates.Templates
import Network.HTTP.Base (urlEncode)

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

instance Show ListParams where
    show params = intercalate "&" $ pg ++ srch ++ srt
        where
        pg =  ["page=" ++ (toUrl  $ show $ page params)]
        srch = map ((++) "search=") $ maybeToList $ toUrl  <$> search params
        srt = map ((++) "sorting=") $ toUrl  <$> sorting params        
        toUrl = urlEncode . BS8.unpack . BS.fromString

emptyListParams = ListParams {sorting=[], search=Nothing, page = 1}

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
pagedListFields:: PagedList a-> Fields
pagedListFields (PagedList{list,pageSize,totalCount,params}) = do
    field "params" $ do
        field "sorting" $ mapM_ (\sp -> field sp sp) $ sorting params
        field "search" $ join $ nothingIfEmpty <$> search params
    field "pages" $ for [1..totalPages] $ \n -> do
        field "nr" $ show $ n
        field "current" $ n == (page params)
    field "elements" $ do
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
          
rcomparing::(Ord a) => (b -> a) -> b -> b -> Ordering        
rcomparing f c1 c2 = comparing f c2 c1

doSearching::SearchingFunction a -> Maybe String -> [a] -> [a]
doSearching searchFunc Nothing = id
doSearching searchFunc (Just s) = filter (searchFunc s)

doPaging:: Int -> Int -> [a] -> [a]
doPaging pageSize page = (take pageSize) . (drop $ (page-1)*pageSize) 