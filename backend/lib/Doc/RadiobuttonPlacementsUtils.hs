module Doc.RadiobuttonPlacementsUtils (
    -- Checking and fixing sizes
      radiobuttonPlacementHasValidRadiobuttonRatio
    -- Tools for sealing
    , RadiobuttonImagesMapping
    , readRadiobuttonImagesMapping
    , getRadiobuttonImage
  ) where

import qualified Data.ByteString.Char8 as BS

import Doc.Types.SignatoryField

smallSize :: Double
smallSize = 0.014736 -- ~ 14px / 1040px

mediumSize :: Double
mediumSize = 0.021052 -- ~ 20px / 1040px

largeSize :: Double
largeSize = 0.025263 -- ~ 24px / 1040px

validRadiobuttonRatios :: [Double]
validRadiobuttonRatios = [smallSize, mediumSize, largeSize]

-- Comparing doubles can be tricky - this is why we use veryCloseTo instead of ==
veryCloseTo :: Double -> Double -> Bool
veryCloseTo v1 v2 = abs (v1 - v2) < 0.000001

-- Checking validity
radiobuttonPlacementHasValidRadiobuttonRatio :: FieldPlacement -> Bool
radiobuttonPlacementHasValidRadiobuttonRatio fp =
  (placementfsrel fp == 0)
    && (placementhrel fp == 0)
    && any (veryCloseTo (placementwrel fp)) validRadiobuttonRatios

-- Finding right image for radiobutton for sealing.
-- We pick image based on size (Double) and it being checked (Bool)
newtype RadiobuttonImagesMapping = RadiobuttonImagesMapping { unRadiobuttonImagesMapping :: Double -> Bool -> BS.ByteString }

readRadiobuttonImagesMapping :: IO RadiobuttonImagesMapping
readRadiobuttonImagesMapping = do
  let readRadiobuttonImage s = BS.readFile $ "files/images/radiobuttons/" ++ s
  smallSelected     <- readRadiobuttonImage "small-selected.png"
  smallNotSelected  <- readRadiobuttonImage "small-not-selected.png"
  mediumSelected    <- readRadiobuttonImage "medium-selected.png"
  mediumNotSelected <- readRadiobuttonImage "medium-not-selected.png"
  largeSelected     <- readRadiobuttonImage "large-selected.png"
  largeNotSelected  <- readRadiobuttonImage "large-not-selected.png"
  let radiobuttonMappingFunction size selected
        | size == smallSize && selected = smallSelected
        | size == smallSize  = smallNotSelected
        | size == mediumSize && selected = mediumSelected
        | size == mediumSize = mediumNotSelected
        | size == largeSize && selected = largeSelected
        | size == largeSize  = largeNotSelected
        |
        -- If no size matches, fallback to small size
          selected           = smallSelected
        | otherwise          = smallNotSelected
  return $ RadiobuttonImagesMapping radiobuttonMappingFunction


getRadiobuttonImage :: RadiobuttonImagesMapping -> Double -> Bool -> BS.ByteString
getRadiobuttonImage = unRadiobuttonImagesMapping
