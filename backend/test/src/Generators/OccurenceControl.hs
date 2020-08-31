-- | 'OccurenceControl' allows to create random generators while controlling
-- the occurence of some event, typically the presence of an error in the data
-- structure.
--
-- 'runOccurenceControl' takes the desired probability of such an event *not*
-- happening. We can therefore have generators that do sometimes generate
-- erroneous values but not virtually all the time as is the case with a more
-- naive approach.

module Generators.OccurenceControl
  ( OccurenceControl
  , runOccurenceControl
  , decide
  , decide'
  , decideWeighted
  , decideWeighted'
  , listOC
  , liftGen
  , testGeneratorHelper
  ) where

import Control.Applicative.Free as Free
import Test.QuickCheck hiding (sample)

import TestingUtil
import TestKontra

-- Problem: When generating complex structures such as documents, it is deemed
-- invalid as soon as one part is invalid. Parts of the structure have a
-- probability of being valid. If the structure contains a lot of such parts,
-- the probability of it being valid quickly goes down. For example, with 5 parts
-- having each a probability of 90% of being valid, the whole structure only
-- has a probability of 59% of being valid. With 30 parts, it is only 4%.
--
-- Solution: It would be nice to be able to at least control the probability of
-- not having any errors in some structure. This is what OccurenceControl is
-- doing.
--
-- We also want different kinds of errors to have the same probability of
-- occuring (modulo weights.) Thus,
--
-- * When calling a generator for a substructure, we can't decide in advance
-- the probability of errors in it because we don't know the number of errors
-- that are possible. (Otherwise, we would have a leaky abstraction.)
-- * When generating a list of substrucures, we need to "fix" the probabilities
-- of errors such that errors in these substructures won't be overrepresented.

data OccurenceControlF a where
  Decision ::Int -> Gen a -> Gen a -> OccurenceControlF a
  ListOf ::Int -> Int -> OccurenceControl a -> OccurenceControlF [a]
  LiftGen ::Gen a -> OccurenceControlF a

type OccurenceControl = Free.Ap OccurenceControlF

getTotalWeight :: OccurenceControl a -> Double
getTotalWeight = getSum . runAp_ counter
  where
    counter :: OccurenceControlF a -> Sum Double
    counter = \case
      Decision w _ _   -> Sum $ fromIntegral w
      ListOf   _ _ sub -> runAp_ counter sub
      LiftGen _        -> Sum 0

getGenerator :: Double -> Double -> OccurenceControl a -> Gen a
getGenerator probtot wtot = runAp (interpret 1)
  where
    interpret
      :: Double -- ^ Normalising constant for lists.
      -> OccurenceControlF a
      -> Gen a
    interpret norm = \case
      Decision w t f -> do
        -- QuickCheck takes weights instead of probabilities so we multiply
        -- by a constant and round.
        let prob = probtot ** (norm * fromIntegral w / wtot)
            wf   = round @Double @Int $ 1000000 * prob
            wt   = 1000000 - wf
        decision <- frequency [(wt, pure True), (wf, pure False)]
        if decision then t else f

      ListOf l u sub -> do
        k <- choose (l, u)
        let norm' = norm / fromIntegral k
        replicateM k $ runAp (interpret norm') sub

      LiftGen gen -> gen

-- | Run an 'OccurenceControl' such that the occurence of the event *not*
-- happening has the given probability.
runOccurenceControl :: Double -> OccurenceControl a -> Gen a
runOccurenceControl prob program =
  -- First, it gets the total weight so that the probability of each decision
  -- site can be calculated. It then interpret the program and extracts a
  -- generator.
  getGenerator prob (getTotalWeight program) program

-- | Decide whether the event should occur and call the first program if it
-- should, the second one otherwise.
decide :: Gen a -> Gen a -> OccurenceControl a
decide = decideWeighted 1

decide' :: (Bool -> Gen a) -> OccurenceControl a
decide' = decideWeighted' 1

-- | Decide whether the event should occur and call the first program if it
-- should, the second one otherwise.
--
-- It takes a weight so that some errors can be made more likely than others.
decideWeighted :: Int -> Gen a -> Gen a -> OccurenceControl a
decideWeighted w t f = liftAp $ Decision w t f

decideWeighted' :: Int -> (Bool -> Gen a) -> OccurenceControl a
decideWeighted' w f = decideWeighted w (f True) (f False)

-- | Generate a list of a random number of elements within the given range
-- such that the probability of a certain kind of errors happening in any of
-- them stays the same regardless of the number of elements.
listOC
  :: Int -- ^ Lower limit.
  -> Int -- ^ Upper limit.
  -> OccurenceControl a
  -> OccurenceControl [a]
listOC l u f = liftAp $ ListOf l u f

liftGen :: Gen a -> OccurenceControl a
liftGen = liftAp . LiftGen

-- | Helper to test that a generator makes the event occur as expected.
--
-- Some discrepancy is to be expected due to the probabilistic nature of
-- generators but also to some acceptable imperfections of the generator.
-- The acceptable delta can be passed to control how "perfect" we want the
-- test result to be.
testGeneratorHelper
  :: Show a -- FIXME: remove this
  => Double -- ^ Acceptable delta in percentage, e.g. 0.15.
  -> OccurenceControl a -- ^ Generator under test.
  -> (a -> Bool) -- ^ Did the event occur?
  -> TestEnv ()
testGeneratorHelper deltaPercentage generator checkOcc = do
  let h               = 100
      n               = 2 * h
      acceptableDelta = round @Double @Int $ fromIntegral n * deltaPercentage

      leftOccurenceCases =
        foldl' (\acc k -> acc + choose' n k) 0 [0 .. (h - acceptableDelta - 1)]
      occurenceCases = 2 * leftOccurenceCases
      totalCases     = 2 ^ n
      prob           = occurenceCases / totalCases

  sample <- rand 10 . replicateM n $ runOccurenceControl 0.5 generator
  let noOccurenceCount = length $ filter (not . checkOcc) sample
      lowerCount       = min noOccurenceCount (n - noOccurenceCount)
      delta            = h - lowerCount

  assertBool
    (  "Acceptable delta ("
    ++ show delta
    ++ " <= "
    ++ show acceptableDelta
    ++ ", no occurence count: "
    ++ show noOccurenceCount
    ++ " out of "
    ++ show n
    ++ ", prob of failure: "
    ++ show prob
    ++ ")"
    )
    (delta <= acceptableDelta)

  where
    choose' :: Int -> Int -> Double
    choose' n k = foldl' (\acc i -> acc * fromIntegral (n - i)) 1 [0 .. (k - 1)]
      / foldl' (\acc i -> acc * fromIntegral i) 1 [1 .. k]
