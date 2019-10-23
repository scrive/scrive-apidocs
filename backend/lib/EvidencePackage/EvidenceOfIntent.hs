module EvidencePackage.EvidenceOfIntent (
  evidenceOfIntentHTML
) where

import Text.StringTemplates.Templates
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Doc.DocStateData
import Doc.SignatoryIdentification (SignatoryIdentifierMap, signatoryIdentifier)
import MinutesTime
import Utils.Image
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

-- | Generate evidence of intent in self-contained HTML for inclusion as attachment in PDF.
evidenceOfIntentHTML
  :: TemplatesMonad m
  => SignatoryIdentifierMap
  -> Text
  -> [(SignatoryLink, SignatoryScreenshots.SignatoryScreenshots)]
  -> m Text
evidenceOfIntentHTML sim title l = T.pack <$> do
  emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
  renderTemplate "evidenceOfIntent" $ do
    F.value "documenttitle" title
    let values Nothing  = return ()
        values (Just s) = do
          F.value "time" $ formatTimeUTC (Screenshot.time s) ++ " UTC"
          F.value "image" $ imgEncodeRFC2397 $ Screenshot.image s
          F.value "length" $ BS.length $ Screenshot.image s
    F.objects "entries" $ for l $ \(sl, entry) -> do
      F.value "signatory" $ signatoryIdentifier sim (signatorylinkid sl) $ T.pack
        emptyNamePlaceholder
      F.value "ip" $ show . signipnumber <$> maybesigninfo sl
      F.object "first" $ values (SignatoryScreenshots.first entry)
      F.object "signing" $ values (SignatoryScreenshots.signing entry)
      F.object "reference" $ values (SignatoryScreenshots.getReferenceScreenshot entry)
