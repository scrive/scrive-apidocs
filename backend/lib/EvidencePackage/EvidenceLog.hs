module EvidencePackage.EvidenceLog (
   htmlDocFromEvidenceLog
 ,finalizeEvidenceText
) where

import Text.StringTemplates.Templates
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Doc.DocStateData
import Doc.SignatoryIdentification
  ( SignatoryIdentifierMap, siLink, signatoryIdentifier )

import EvidenceLog.Model
import MinutesTime
import Text.XML.Content (cdata)
import Text.XML.DirtyContent (XMLContent, renderXMLContent, substitute)
import qualified HostClock.Model as HC

-- | Generating text of Evidence log that is attached to PDF. It should be complete
htmlDocFromEvidenceLog
  :: TemplatesMonad m
  => Text
  -> SignatoryIdentifierMap
  -> [DocumentEvidenceEvent]
  -> m String
htmlDocFromEvidenceLog title sim elog = do
  emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
  renderTemplate "htmlevidencelog" $ do
    F.value "documenttitle" title
    F.objects "entries"
      $ for (filter (not . htmlSkipedEvidenceType . evType) elog)
      $ \entry -> do
          F.value "time" $ formatTimeUTC (evTime entry) ++ " UTC" ++ maybe
            ""
            (\e -> " ±" ++ HC.showClockError 0 e)
            (HC.maxClockError (evTime entry) <$> evClockErrorEstimate entry)
          F.value "ces_time" $ maybe ""
                                     ((++ " UTC") . formatTimeUTC . HC.time)
                                     (evClockErrorEstimate entry)
          F.value "ip" $ show <$> evIP4 entry
          F.value "ua" $ evClientName entry
          F.value "text" $ T.unpack $ renderXMLContent $ finalizeEvidenceText
            sim
            entry
            (T.pack emptyNamePlaceholder)

htmlSkipedEvidenceType :: EvidenceEventType -> Bool
htmlSkipedEvidenceType (Obsolete OldDocumentHistory) = True
htmlSkipedEvidenceType _ = False

finalizeEvidenceText
  :: SignatoryIdentifierMap -> DocumentEvidenceEvent -> Text -> XMLContent
finalizeEvidenceText sim event emptyNamePlaceholder = substitute
  (Map.fromList
    [ (("span", n), cdata v)
    | (n, Just v) <-
      [ ( "actor"
        , (   (\slid -> signatoryIdentifier sim slid emptyNamePlaceholder)
          =<< evSigLink event
          )
          `mplus` Just (evActor event)
        )
      , ( "signatory"
        , (\slid -> signatoryIdentifier sim slid emptyNamePlaceholder)
          =<< evAffectedSigLink event
        )
      , ( "author"
        , (\slid -> signatoryIdentifier sim slid emptyNamePlaceholder) =<< authorSigLinkID
        )
      ]
    ]
  )
  (evText event)
  where
    authorSigLinkID = signatorylinkid
      <$> find (signatoryisauthor) (catMaybes (map siLink (Map.elems sim)))
