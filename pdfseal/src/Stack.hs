
module Stack where
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Control.Monad
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Batch

type Stack s a = STRef s (Int,STArray s Int a)

empty :: ST s (Stack s a)
empty = do
    a <- newArray_ (0,0)
    newSTRef (0,a)

size :: Stack s a -> ST s Int
size stack = do
    (i,a) <- readSTRef stack
    return i

clear :: Stack s a -> ST s ()
clear x = modifySTRef x (\(i,a) -> (0,a))

push :: a -> Stack s a -> ST s ()
push v x = do
    (i,a) <- readSTRef x
    (l,h) <- getBounds a
    a2 <- if (h-l+1>i)
             then return a
             else do
                 a2 <- newArray_ (0,(h-l+1)*2)
                 sequence_ [ readArray a i >>= writeArray a2 i | i <- range (l,h) ]
                 return a2
    writeArray a2 i v
    writeSTRef x (i+1,a2)

pushlist :: [a] -> Stack s a -> ST s ()
pushlist v x = mapM_ (\a -> push a x) v

index :: Int -> Stack s a -> ST s a
index n x = do
    (i,a) <- readSTRef x
    readArray a (i-1-n)

pop :: Stack s a -> ST s (Maybe a)
pop x = do
    (i,a) <- readSTRef x
    if i>0
        then do
            writeSTRef x (i-1,a)
            v <- readArray a (i-1)
            return (Just v)
        else return Nothing

peekpoplist :: Bool -> Int -> Stack s a -> ST s ([a])
peekpoplist domod n x = do
    (i,a) <- readSTRef x
    let newi = max 0 (i-n) `min` i
    when domod (writeSTRef x (newi,a))
    v <- sequence [readArray a k | k <- [i-1,(i-2) .. newi]]
    return v

peekpoplistreverse :: Bool -> Int -> Stack s a -> ST s ([a])
peekpoplistreverse domod n x = do
    (i,a) <- readSTRef x
    let newi = max 0 (i-n) `min` i
    when domod (writeSTRef x (newi,a))
    v <- sequence [readArray a k | k <- [newi .. i-1]]
    return v

poplist :: Int -> Stack s a -> ST s ([a])
poplist n x = peekpoplist True n x

peeklist :: Int -> Stack s a -> ST s ([a])
peeklist n x = peekpoplist False n x

poplistreverse :: Int -> Stack s a -> ST s ([a])
poplistreverse n x = peekpoplistreverse True n x

peeklistreverse :: Int -> Stack s a -> ST s ([a])
peeklistreverse n x = peekpoplistreverse False n x

top :: Stack s a -> ST s a
top x = Stack.index 0 x

find :: (a -> Bool) -> Stack s a -> ST s Int
find pred x = do
    (i,a) <- readSTRef x
    find' (i-1) (i-1) a
    where find' b idx array = do
                 g <- readArray array idx
                 if pred g
                     then return (b-idx)
                     else find' b (idx-1) array

prop_empty_size_zero :: Bool
prop_empty_size_zero = runST (do
    stack <- empty
    s <- size stack
    return (s==0))

prop_push_size :: [Int] -> Bool
prop_push_size list = runST (do
    stack <- empty
    mapM_ (\x -> push x stack) list
    s <- size stack
    return (s==length list))

prop_pushlist_size :: [Int] -> Bool
prop_pushlist_size list = runST (do
    stack <- empty
    pushlist list stack
    s <- size stack
    return (s==length list))

prop_index :: [Int] -> Bool
prop_index list = runST (do
    stack <- empty
    pushlist list stack
    d <- zipWithM (\x idx -> do
                       g <- Stack.index (length list - idx - 1) stack
                       return (g==x)) list [0..]
    return (and d))

prop_find :: [Int] -> Bool
prop_find list = runST (do
    stack <- empty
    pushlist list stack
    d <- zipWithM (\x idx -> do
                       g <- Stack.find (==x) stack
                       return (g<=idx)) (reverse list) [0..]
    return (and d))

prop_pop :: [Int] -> Bool
prop_pop list = runST (do
    stack <- empty
    pushlist list stack
    case list of
        [] -> do
            v <- pop stack
            return (v==Nothing)
        _ -> do
            v <- pop stack
            return (v==Just (last list))
            )

prop_top :: [Int] -> Bool
prop_top list = runST (do
    stack <- empty
    pushlist list stack
    case list of
        [] -> do
            return (True)
        _ -> do
            v <- top stack
            return (v==last list)
            )

prop_poplist :: Int -> [Int] -> Bool
prop_poplist n list = runST (do
    stack <- empty
    pushlist list stack
    l <- poplist n stack
    return (reverse l `isSuffixOf` list)
    )

prop_poplist_size :: Int -> [Int] -> Bool
prop_poplist_size n list = runST (do
    stack <- empty
    pushlist list stack
    l <- poplist n stack
    sz <- size stack
    return (length list - length l == sz)
    )


propall = runTests "Stack" defOpt
    [ run prop_empty_size_zero
    , run prop_push_size
    , run prop_pushlist_size
    , run prop_index
    , run prop_pop
    , run prop_top
    , run prop_poplist
    , run prop_poplist_size
    , run prop_find
    ]