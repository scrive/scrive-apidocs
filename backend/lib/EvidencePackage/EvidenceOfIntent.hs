module EvidencePackage.EvidenceOfIntent (
  evidenceOfIntentHTML
) where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import Doc.DocStateData
import Doc.SignatoryIdentification (SignatoryIdentifierMap, signatoryIdentifier)
import KontraPrelude
import MinutesTime
import Utils.Image
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

-- | Generate evidence of intent in self-contained HTML for inclusion as attachment in PDF.
evidenceOfIntentHTML :: TemplatesMonad m => SignatoryIdentifierMap -> String -> [(SignatoryLink, SignatoryScreenshots.SignatoryScreenshots)] -> m String
evidenceOfIntentHTML sim title l = do
  emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
  renderTemplate "evidenceOfIntent" $ do
    F.value "documenttitle" title
    let values Nothing = return ()
        values (Just s) = do
          F.value "time" $ formatTimeUTC (Screenshot.time s) ++ " UTC"
          F.value "image" $ imgEncodeRFC2397 $ Screenshot.image s
    F.objects "entries" $ for l $ \(sl, entry) -> do
      F.value "signatory"  $ signatoryIdentifier sim (signatorylinkid sl) emptyNamePlaceholder
      F.value "ip"         $ show . signipnumber <$> maybesigninfo sl
      F.object "first"     $ values (SignatoryScreenshots.first entry)
      F.object "signing"   $ values (SignatoryScreenshots.signing entry)
      F.object "reference" $ values (SignatoryScreenshots.getReferenceScreenshot entry)
