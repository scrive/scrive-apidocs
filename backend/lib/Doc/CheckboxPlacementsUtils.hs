module Doc.CheckboxPlacementsUtils (
    -- Checking and fixing sizes
      checkboxPlacementHasValidCheckboxRatio
    , fixCheckboxPlacementRatioIfInvalid
    -- Tools for sealing
    , CheckboxImagesMapping
    , readCheckboxImagesMapping
    , getCheckboxImage
  ) where

import qualified Data.ByteString.Char8 as BS

import Doc.Types.SignatoryField

-- Valid sizes. Exact size is synched with frontend.
defaultCheckboxRatio :: Double
defaultCheckboxRatio = smallSize

smallSize :: Double
smallSize = 0.011538 -- ~ 12px / 1040px

mediumSize :: Double
mediumSize = 0.021153 -- ~ 22px / 1040px

largeSize :: Double
largeSize = 0.0423076 -- ~ 44px / 1040px

validCheckboxRatios :: [Double]
validCheckboxRatios = [smallSize, mediumSize, largeSize]

-- Comparing doubles can be tricky - this is why we use veryCloseTo instead of ==
veryCloseTo :: Double -> Double -> Bool
veryCloseTo v1 v2 = abs (v1 - v2) < 0.000001

-- Checking validity (API v2) and fixing checkbox placements (API v1)
checkboxPlacementHasValidCheckboxRatio :: FieldPlacement -> Bool
checkboxPlacementHasValidCheckboxRatio fp =
  (placementfsrel fp == 0)
    && (placementhrel fp == 0)
    && any (veryCloseTo (placementwrel fp)) validCheckboxRatios

fixCheckboxPlacementRatioIfInvalid :: FieldPlacement -> FieldPlacement
fixCheckboxPlacementRatioIfInvalid fp
  | checkboxPlacementHasValidCheckboxRatio fp = fp
  | otherwise = fp { placementwrel  = defaultCheckboxRatio
                   , placementhrel  = 0
                   , placementfsrel = 0
                   }

-- Finding right image for checkbox for sealing.
-- We pick image based on size (Double) and it being checked (Bool)
newtype CheckboxImagesMapping = CheckboxImagesMapping (Double -> Bool -> BS.ByteString)

readCheckboxImagesMapping :: IO CheckboxImagesMapping
readCheckboxImagesMapping = do
  let readCheckboxImage s = BS.readFile $ "files/images/checkboxes/" ++ s
  smallChecked     <- readCheckboxImage "small-checked.png"
  smallNotChecked  <- readCheckboxImage "small-not-checked.png"
  mediumChecked    <- readCheckboxImage "medium-checked.png"
  mediumNotChecked <- readCheckboxImage "medium-not-checked.png"
  largeChecked     <- readCheckboxImage "large-checked.png"
  largeNotChecked  <- readCheckboxImage "large-not-checked.png"
  let checkboxMappingFunction size checked | size == smallSize && checked = smallChecked
                                           | size == smallSize  = smallNotChecked
                                           | size == mediumSize && checked = mediumChecked
                                           | size == mediumSize = mediumNotChecked
                                           | size == largeSize && checked = largeChecked
                                           | size == largeSize  = largeNotChecked
                                           |
        -- If no size matches, fallback to small size
                                             checked            = smallChecked
                                           | otherwise          = smallNotChecked
  return $ CheckboxImagesMapping $ checkboxMappingFunction


getCheckboxImage :: CheckboxImagesMapping -> Double -> Bool -> BS.ByteString
getCheckboxImage (CheckboxImagesMapping checkboxMappingFunction) =
  checkboxMappingFunction
