module EvidencePackage.EvidenceLog (
   htmlDocFromEvidenceLog
 ,finalizeEvidenceText
) where

import Data.Decimal (realFracToDecimal)
import Data.Word (Word8)
import Text.StringTemplates.Templates
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Doc.DocStateData
import Doc.SignatoryIdentification (SignatoryIdentifierMap, siLink, signatoryIdentifier)
import EvidenceLog.Model
import KontraPrelude
import MinutesTime
import Text.XML.Content (cdata)
import Text.XML.DirtyContent (XMLContent, renderXMLContent, substitute)
import Util.SignatoryLinkUtils
import Utils.Prelude
import qualified HostClock.Model as HC

-- | Generating text of Evidence log that is attached to PDF. It should be complete
htmlDocFromEvidenceLog :: TemplatesMonad m => String -> SignatoryIdentifierMap -> [DocumentEvidenceEvent] -> HC.ClockErrorStatistics -> m String
htmlDocFromEvidenceLog title sim elog ces = do
  emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
  renderTemplate "htmlevidencelog" $ do
    F.value "documenttitle" title
    F.value "ce_max"       $ showClockError 1 <$> HC.max ces
    F.value "ce_mean"      $ showClockError 1 <$> HC.mean ces
    F.value "ce_std_dev"   $ showClockError 1 <$> HC.std_dev ces
    F.value "ce_collected" $ HC.collected ces
    F.value "ce_missed"    $ HC.missed ces
    F.objects "entries" $ for (filter (not . htmlSkipedEvidenceType . evType) elog) $ \entry -> do
      F.value "time" $ formatTimeUTC (evTime entry) ++ " UTC"
                       ++ maybe "" (\e -> " ±" ++ showClockError 0 e)
                                   (HC.maxClockError (evTime entry) <$> evClockErrorEstimate entry)
      F.value "ces_time" $ maybe "" ((++" UTC") . formatTimeUTC . HC.time)
                                    (evClockErrorEstimate entry)
      F.value "ip"   $ show <$> evIP4 entry
      F.value "text" $ T.unpack $ renderXMLContent $ finalizeEvidenceText sim entry emptyNamePlaceholder

showClockError :: Word8 -> Double -> String
showClockError decimals e = show (realFracToDecimal decimals (e * 1000)) ++ " ms"

htmlSkipedEvidenceType :: EvidenceEventType -> Bool
htmlSkipedEvidenceType (Obsolete OldDocumentHistory) = True
htmlSkipedEvidenceType _ = False

finalizeEvidenceText :: SignatoryIdentifierMap -> DocumentEvidenceEvent -> String -> XMLContent
finalizeEvidenceText sim event emptyNamePlaceholder =
  substitute (Map.fromList [ (("span",n), cdata (T.pack v))
                           | (n,Just v) <- [ ("actor", ((\slid -> signatoryIdentifier sim slid emptyNamePlaceholder) =<< evSigLink event) `mplus` Just (evActor event))
                                           , ("signatory", (\slid -> signatoryIdentifier sim slid emptyNamePlaceholder) =<< evAffectedSigLink event)
                                           , ("author", (\slid -> signatoryIdentifier sim slid emptyNamePlaceholder) =<< authorSigLinkID) ] ]) (evText event)
  where
    authorSigLinkID = signatorylinkid <$> getAuthorSigLink (catMaybes (map siLink (Map.elems sim)))
