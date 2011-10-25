{-# LANGUAGE OverlappingInstances, FunctionalDependencies, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
    FlexibleContexts #-}
module Happstack.StaticRouting.Internal where

-- | Support for static routing tables in Happstack.  The routing
-- tables are order independent as long as:
--
-- * if any two handlers overlap, one of them handles a more specific
-- path than the other.  The more specific handler is then tried
-- first.
--
-- Routing tables are constructed from 'dir', 'path', 'remainingPath',
-- 'choice', and (for now) 'param'.
--
-- A routing table is compiled by using 'compile'.  The result is an
-- overlap report, and a prefix tree that is used to efficiently
-- dispatch requests by means of 'dispatch'.

import Happstack.Server(askRq, rqPaths, rqMethod, localRq, ServerMonad,
  HasRqData, methodM, look, FromReqURI)
import qualified Happstack.Server as H
import Control.Monad(msum, MonadPlus, mzero, mplus)
import Control.Monad.IO.Class(MonadIO)
import Control.Arrow(first, second)
import qualified Data.ListTrie.Map as Trie
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(intercalate)

-- | Static routing tables consisting of handlers of type 'a'
data Route a =
    Dir Segment (Route a)
  | Handler EndSegment a
  | Param String (Route a)
  | Choice [Route a]

newtype Segment =
    StringS String
  deriving (Show, Eq, Ord)

type EndSegment = (Maybe Int, H.Method)

class Path m a b c | a -> m b c where
  pathHandler :: (m b -> m c) -> a -> m c
  arity :: a -> Int

instance (FromReqURI d, ServerMonad m, MonadPlus m, Path m a b c) => Path m (d -> a) b c where
  pathHandler w f = H.path (pathHandler w . f)
  arity f = 1 + arity (f undefined)

instance Path m (m b) b c where
  pathHandler w m = w m
  arity _ = 0

dir :: String -> Route a -> Route a
dir = Dir . StringS

-- | Combine several routes into one
choice :: [Route a] -> Route a
choice = Choice

-- | Expect the given method, and exactly 'n' more segments, where 'n' is the arity of the handler
path :: Path m h a b => H.Method -> (m a -> m b) -> h -> Route (m b)
path m w h = Handler (Just (arity h),m) (pathHandler w h)

-- | Expect zero or more segments
remainingPath :: H.Method -> h -> Route h
remainingPath m = Handler (Nothing,m)

-- | DEPRECATED. Expect a specific parameter to be present
param :: String -> Route a -> Route a
param = Param

-- | Extract handler monad from a route
extract :: (ServerMonad m, MonadPlus m, HasRqData m, MonadIO m) => Route (m a) -> m a
extract = f Nothing where
  f p (Dir (StringS s) r) = H.dir s (f p r)
  f (Just p) (Param p' _) = error $ "extract: cannot handle more than one param: "++ show(p,p')
  f _ (Param p r) = f (Just p) r
  f p (Handler (_,m) a) = doParam p >> methodM m >> a
  f p (Choice rs) = msum (map (f p) rs)

doParam :: (MonadIO m, HasRqData m, ServerMonad m, MonadPlus m) =>
           Maybe String -> m ()
doParam Nothing = return ()
doParam (Just p) = H.getDataFn (look p) >>= either (const mzero) (const (return ()))

type Param = Maybe String
newtype RouteTree a =
  R { unR :: Trie.TrieMap Map Segment (Map EndSegment (Map Param [a])) }

type Segments = ([Segment],EndSegment)

routeTree :: (ServerMonad m, MonadPlus m) => Route (m a) -> RouteTree (m a)
routeTree r = R $ foldr (\(((ps,es),mp),m) ->
                          Trie.insertWith (Map.unionWith (Map.unionWith (++)))
                              ps
                              (Map.singleton es (Map.singleton mp [m])))
                  Trie.empty (flatten r)

flatten :: (ServerMonad m, MonadPlus m) => Route (m a) -> [((Segments, Param), m a)]
flatten = f Nothing where
  f p (Dir s r) = map (first (first (first (s:)))) (f p r)
  f (Just p) (Param p' _) = error $ "flatten: cannot handle more than one param: "++show (p,p')
  f _ (Param p' r) = f (Just p') r
  f p (Handler e a) = [((([], e),p), a)]
  f p (Choice rs) = concatMap (f p) rs

-- | Compile routes, also return possible overlap report.  If the
-- overlap report is 'Nothing', the routing table is order independent.
compile :: (MonadIO m, HasRqData m, ServerMonad m, MonadPlus m) =>
           Route (m a) -> (m a, Maybe String)
compile r = ( dispatch t
            , if null os then Nothing
              else Just $ "Overlapping handlers: \n"++ showOverlaps os
            )
  where t = routeTree r
        os = overlaps True t

maybezero :: MonadPlus m => Maybe a -> (a -> m b) -> m b
maybezero = flip (maybe mzero)

-- | Dispatch a request given a route.  Give priority to more specific paths
-- in case of overlap.
dispatch :: (MonadIO m, HasRqData m, ServerMonad m, MonadPlus m) =>
            RouteTree (m a) -> m a
dispatch (R t) = do
  let m = Trie.children1 t
  rq  <- askRq
  let ps = rqPaths rq
  (case ps of
      p:xs -> maybezero (Map.lookup (StringS p) m)
                        (localRq (\newRq -> newRq{rqPaths = xs}) . dispatch . R)
      []   -> mzero) 
    -- most specific: match against a given next segment 
    `mplus`
    (maybezero (Trie.lookup [] t) $ \em ->
     maybezero (Map.lookup (Just (length ps), rqMethod rq) em) dispatchParams
    -- or else a 'path' taking a given specific number of remaining segments
     `mplus`
     maybezero (Map.lookup (Nothing, rqMethod rq) em) dispatchRemainingPath
     -- least specific: a 'remainingPath' taking any number of remaining segments
     )

dispatchParams :: (MonadIO m, HasRqData m, ServerMonad m, MonadPlus m) =>
                  Map (Maybe String) [m a] -> m a
dispatchParams m = msum [doParam p >> msum hs | (p,hs) <- Map.assocs m]

dispatchRemainingPath :: MonadPlus m => Map k [m a] -> m a
dispatchRemainingPath m = msum (map msum (Map.elems m))

-- | All paths with more than one handler
overlaps :: forall a . Bool -> -- ^ Only include order-dependent paths (exclude paths overlapping with more specific paths)
            RouteTree a -> [[([Segment],[(EndSegment,[(Param,[a])])])]]
overlaps eso t = (filter (not . null) $
                  map (filter (not . null . snd) . (:[]) . second f) $
                  flattenTree t) ++
                 if eso then [] else  pathOverlaps t
  where f :: [(EndSegment,[(Param,[a])])] -> [(EndSegment,[(Param,[a])])]
        f l = (filter (not . null . snd) . map (second g)) l
        g l@((Nothing,_):_:_) | not eso = l  -- non-parameter handler overlaps with parameter handlers
        g l = filter ((>1) . length . snd) l -- more than one handler for this path

pathOverlaps :: RouteTree a -> [[([Segment], [(EndSegment, [(Param, [a])])])]]
pathOverlaps (R t) =
  (case Trie.lookup [] t of
    Just em -> filter ((>1) . length) $ map f (Map.keys em)
      where f es1 = filter ((>0) . length . snd)
                    [ (ks, filter (lengthOverlaps es1 (length ks)) as)
                    | (ks,as) <- flattenTree (R t) ]
            lengthOverlaps (a1,m1) ksl ((a2,m2),_) =
              m1 == m2 && case (a1,a2) of
                            (Nothing,_) -> True
                            (Just i1,Nothing) -> i1 >= ksl
                            (Just i1,Just i2) -> i1 == ksl + i2

    Nothing -> []) ++
  [ [ (k:ks,a) | (ks,a) <- l ]
  | (k,t') <- Map.assocs (Trie.children1 t)
  , l <- pathOverlaps (R t') ]

showOverlaps :: [[([Segment],[(EndSegment,[(Param,[a])])])]] -> String
showOverlaps = unlines . intercalate [[]] . map (concatMap showPath)

showPath :: ([Segment],[(EndSegment,[(Param,[a])])]) -> [String]
showPath (ss,es) =
  [ intercalate "/" (map showSegment ss++
                     showArity i)++
                     showParam p++showMethod m++
                     showHandlers hs
  | ((i,m),ps) <- es, (p,hs) <- ps ]
  where showParam Nothing  = ""
        showParam (Just p) = "?"++p++"=*"
        showMethod H.GET = ""
        showMethod m = " ("++show m++")"
        showSegment (StringS s) = s
        showArity Nothing = ["**"]
        showArity (Just i) = replicate i "*"
        showHandlers hs | length hs == 1 = ""
                        | otherwise      = " -> "++show (length hs)

-- | Convert to a list of path-value pairs
flattenTree :: RouteTree a -> [([Segment],[(EndSegment,[(Param,[a])])])]
flattenTree (R t) =
  (case Trie.lookup [] t of
     Just em -> [([], map (second Map.assocs) (Map.assocs em))]
     Nothing -> []) ++
  [ (k:ks,a) | (k,t') <- Map.assocs (Trie.children1 t)
             , (ks,a) <- flattenTree (R t') ]
