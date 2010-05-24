
module BitStream where
import Data.Bits
import Data.List

testBits :: Bits a => [Int] -> a -> [Bool]
testBits ls v = map (testBit v) ls

unpackAllBE :: Bits a => a -> [Bool]
unpackAllBE v = unpackBE (bitSize v) v

unpackAllLE :: Bits a => a -> [Bool]
unpackAllLE v = unpackLE (bitSize v) v

unpackBE :: Bits a => Int -> a -> [Bool]
unpackBE = unpackBE' 0

unpackLE :: Bits a => Int -> a -> [Bool]
unpackLE = unpackLE' 0

forloop f c b = take c (z f) where z f = f : z (f+b)

unpackBE' :: Bits a => Int -> Int -> a -> [Bool]
unpackBE' b c v
    | c > 0 = testBits (forloop (b+c-1) c (-1)) v
    | otherwise = []

unpackLE' :: Bits a => Int -> Int -> a -> [Bool]
unpackLE' b c v
    | c > 0 = testBits (forloop (b) c (1)) v
    | otherwise = []

packUniv :: Bits a => Int -> [Int] -> [Bool] -> [a]
packUniv s sx [] = []
packUniv s sx l = w
    where (a,b) = splitAt s l
          w = p : packUniv s sx b
          kt = zipWith (\a b -> if a then (\x -> setBit x b) else id) a sx
          p = foldl' (.) id kt $ 0

packBE :: Bits a => [Bool] -> [a]
packBE l = res
    where res = packUniv s [(s-1),(s-2)..0] l
          s = bitSize (head res)

packLE :: Bits a => [Bool] -> [a]
packLE l = res
    where res = packUniv s [0..(s-1)] l
          s = bitSize (head res)


{-
   Scales unsigned bit values so that (2^n-1)==(2^m-1)
-}
scaleBE :: Bits a => Int -> Int -> a -> [Bool]
scaleBE n m k = take m (cycle (unpackBE n k))

scaleLE :: Bits a => Int -> Int -> a -> [Bool]
scaleLE n m k = take m (cycle (unpackLE n k))
