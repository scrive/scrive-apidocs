module EvidencePackage.EvidenceOfTime (
    EvidenceOfTime(..)
  , evidenceOfTimeHTML
  , generateEvidenceOfTimeData
) where

import Control.Arrow (second)
import Data.Decimal (realFracToDecimal)
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Process
import System.Random
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Function as SF
import qualified Statistics.Sample as S
import qualified Text.StringTemplates.Fields as F

import MinutesTime
import qualified HostClock.Model as HC

data EvidenceOfTime = EvidenceOfTime {
    eotDistribution :: !NormalDistribution
  , eotGraph        :: !BS.ByteString
  }

-- | Given the document title, list of clock error estimates, and clock error
-- statistics graph (in SVG, to be directly injected in HTML page) returns the
-- Evidence of Time HTML
evidenceOfTimeHTML
  :: TemplatesMonad m => String -> [HC.ClockErrorEstimate] -> EvidenceOfTime -> m String
evidenceOfTimeHTML title clockErrors EvidenceOfTime {..} = do
  let startTime = minimum (map HC.time clockErrors)
      endTime   = maximum (map HC.time clockErrors)
      absoluteCDF dist v = cumulative dist v - cumulative dist (-v)
      showCDFInPercent dist v =
        (++ "%") (show . realFracToDecimal 3 $ 100 * absoluteCDF dist v)
  renderTemplate "evidenceOfTime" $ do
    F.value "documenttitle" title
    F.value "graph_image" eotGraph
    F.value "EvidenceOfTimeMean" $ HC.showClockError 2 (mean eotDistribution)
    F.value "EvidenceOfTimeStddev" $ HC.showClockError 2 (stdDev eotDistribution)
    F.value "EvidenceOfTimeLT25" $ showCDFInPercent eotDistribution 0.0025
    F.value "EvidenceOfTimeLT5" $ showCDFInPercent eotDistribution 0.005
    F.value "EvidenceOfTimeLT10" $ showCDFInPercent eotDistribution 0.01
    F.value "EvidenceOfTimeStartDate" $ formatTimeUTC startTime ++ " UTC"
    F.value "EvidenceOfTimeEndDate" $ formatTimeUTC endTime ++ " UTC"
    F.objects "entries" . for clockErrors $ \entry -> do
      F.value "offset" . HC.showClockError 1 $ HC.offset entry
      F.value "offset_time" . formatTimeUTC $ HC.time entry

-- | Given intervals, a temporary file path for input and output of data and
-- SVG, and a list of `Double` representing clock errors, returns evidence of
-- time data, i.e. estimated distribution and SVG graph.
generateEvidenceOfTimeData :: Int -> FilePath -> FilePath -> [Double] -> IO EvidenceOfTime
generateEvidenceOfTimeData intervals inputFilePath outputFilePath offsets' = do
  let (dist, distData) = computeDist intervals (-0.01, 0.01) offsets
      offsets          = V.fromList offsets'
  writeFile inputFilePath ""
  V.forM_ distData $ \(x, emp_x, est_x, err) ->
    appendFile inputFilePath $ (unwords . map show $ [x, emp_x, est_x, err]) ++ "\n"
  void . system $ smconcat
    [ "gnuplot -e"
    , "\""
    , "inputfile='" ++ inputFilePath ++ "';"
    , "outputfile='" ++ outputFilePath ++ "'"
    , "\""
    , "evidence-package/samples.p"
    ]
  graph <- BS.readFile outputFilePath
  return EvidenceOfTime { eotDistribution = dist, eotGraph = graph }

computeDist
  :: Int
  -> (Double, Double)
  -> V.Vector Double
  -> (NormalDistribution, V.Vector (Double, Double, Double, Double))
computeDist intervals (mins, maxs) samples =
  (estimatedDist, fillData `V.map` empiricalCdf)
  where
    sortedSamples :: V.Vector Double
    sortedSamples = SF.sort samples

    fillData :: (Double, Double) -> (Double, Double, Double, Double)
    fillData (v, emp) = (v, scaledEmp, est, abs $ scaledEmp - est)
      where
        scaledEmp = 100 * emp
        est       = 100 * cumulative estimatedDist v

    estimatedDist :: NormalDistribution
    estimatedDist = go (mkStdGen 1337) 0 (normalDistr initialMu initialSigma)
      where
        -- Initial estimation using maximum likelihood method.
        (initialMu, initialSigma) = second sqrt $ S.meanVariance samples

        -- Enhance accuracy of initial estimation using randomized algorithm in
        -- case data set contains significant outliers. It exploits the fact
        -- that the search space is small and we land near optimal solution
        -- anyway, so the only mechanism to prevent us from getting stuck in a
        -- local minimum that is not a global one is phi that limits convergence
        -- of parameters to zero.
        --
        -- Note: searched neighborhood is reduced proportionally to the number
        -- of times a better solution was not found to increase the probability
        -- of finding small improvements.
        --
        -- Note: random number generator with constant seed is used for
        -- deterministic results between runs.
        go :: StdGen -> Int -> NormalDistribution -> NormalDistribution
        go _ 50 dist = dist
        go g n dist =
          let phi               = 1.5 * (fromIntegral n + 1)
              mu                = mean dist
              sigma             = stdDev dist
              (deltaMu   , g' ) = randomR (-mu / phi, mu / phi) g
              (deltaSigma, g'') = randomR (-sigma / phi, sigma / phi) g'
              newDist           = normalDistr (mu + deltaMu) (sigma + deltaSigma)
          in  if estimationError newDist < estimationError dist
                then go g'' 0 newDist
                else go g'' (n + 1) dist

        -- Compute euclidean norm of the vector of errors, i.e. its distance
        -- from the zero vector which represents perfect estimation. The greater
        -- the distance, the greater the error.
        estimationError :: NormalDistribution -> Double
        estimationError dist = sqrt . V.sum $ V.map
          (\(v, emp) -> (emp - cumulative dist v) ** 2)
          empiricalCdf

    empiricalCdf :: V.Vector (Double, Double)
    empiricalCdf = computeEmpirical `V.map` V.enumFromStepN mins step intervals
      where
        computeEmpirical :: Double -> (Double, Double)
        computeEmpirical v = (v, cdf v)

        cdf :: Double -> Double
        cdf v = fromIntegral (V.length $ V.takeWhile (<= v) sortedSamples) / n
          where
            n :: Double
            n = fromIntegral $ V.length samples

        step :: Double
        step = (maxs - mins) / fromIntegral (intervals - 1)
