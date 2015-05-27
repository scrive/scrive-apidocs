module EvidencePackage.EvidenceOfTime (
    evidenceOfTimeHTML
  , generateEvidenceOfTimeGraph
) where

import Control.Monad.Reader
import Data.Decimal (realFracToDecimal)
import Data.Vector.Unboxed (Vector)
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.FilePath
import System.Process
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Function as SF
import qualified Text.StringTemplates.Fields as F

import KontraPrelude
import MinutesTime
import qualified HostClock.Model as HC

-- | Given the document title, list of clock error estimates, and clock error
-- statistics graph (in SVG, to be directly injected in HTML page) returns the
-- Evidence of Time HTML
evidenceOfTimeHTML :: TemplatesMonad m => String -> [HC.ClockErrorEstimate] -> BS.ByteString -> m String
evidenceOfTimeHTML title clockErrors graph = do
  let offsets   = V.fromList (map HC.offset clockErrors)
      norm      = normalFromSample offsets
      startTime = $minimum (map HC.time clockErrors)
      endTime   = $maximum (map HC.time clockErrors)
      absoluteCDF dist v = cumulative dist v - cumulative dist (-v)
      showCDFInPercent dist v = (++ "%") (show $ realFracToDecimal 3 $ 100 * absoluteCDF dist v)
  renderTemplate "evidenceOfTime" $ do
    F.value "documenttitle" title
    F.value "graph_image" graph
    F.value "EvidenceOfTimeMean"   $ HC.showClockError 2 (mean norm)
    F.value "EvidenceOfTimeStddev" $ HC.showClockError 2 (stdDev norm)
    F.value "EvidenceOfTimeLT25" $ showCDFInPercent norm 0.0025
    F.value "EvidenceOfTimeLT5"  $ showCDFInPercent norm 0.005
    F.value "EvidenceOfTimeLT10" $ showCDFInPercent norm 0.01
    F.value "EvidenceOfTimeStartDate" $ formatTimeUTC startTime ++ " UTC"
    F.value "EvidenceOfTimeEndDate"   $ formatTimeUTC endTime   ++ " UTC"
    F.objects "entries" $ for clockErrors $ \entry -> do
      F.value "offset" $ HC.showClockError 1 $ HC.offset entry
      F.value "offset_time" $ formatTimeUTC $ HC.time entry

-- | Given intervals, a temporary file path for input and output of data and
-- SVG, and a list of `Double` representing clock errors, returns an SVG graph
generateEvidenceOfTimeGraph :: MonadIO m => Int -> FilePath -> FilePath -> [Double] -> m BS.ByteString
generateEvidenceOfTimeGraph intervals inputFilePath outputFilePath offsets' = do
  let dist = computeDist intervals (V.minimum offsets, V.maximum offsets) offsets
      offsets = V.fromList offsets'
  liftIO $ writeFile inputFilePath ""
  V.forM_ dist $ \(x, emp_x, est_x, err) ->
    liftIO $ appendFile inputFilePath $ (intercalate " " . map show $ [x, emp_x, est_x, err]) ++ "\n"
  _ <- liftIO $ system $ "gnuplot -e \"inputfile='" ++ inputFilePath
                      ++ "'; outputfile='" ++ outputFilePath
                      ++ "\" evidence-package/samples.p"
  graph <- liftIO $ BS.readFile outputFilePath
  return graph

computeDist :: Int
            -> (Double, Double)
            -> Vector Double
            -> Vector (Double, Double, Double, Double)
computeDist intervals (mins, maxs) samples =
  f `V.map` V.enumFromStepN mins step intervals
  where
    f :: Double -> (Double, Double, Double, Double)
    f v = let emp = 100 * empirical v
              est = 100 * cumulative dist v
          in (v, emp, est, abs $ emp - est)

    empirical :: Double -> Double
    empirical v = fromIntegral (V.length $ V.takeWhile (<= v) sorted_samples) / n

    dist = normalFromSample samples

    sorted_samples :: Vector Double
    sorted_samples = SF.sort samples

    n, step :: Double
    n = fromIntegral $ V.length samples
    step = (maxs - mins) / fromIntegral (intervals-1)
