module ScreenshotReview where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (liftIO)
import KontraPrelude

import EvidenceLog.Model (GetEvidenceLog(..), DocumentEvidenceEvent(..))
import DB (dbQuery, MonadDB)
import Interactive (run)
import Doc.DocStateData (Document(..))
import Doc.SignatoryLinkID (SignatoryLinkID)
import Doc.Model.Query (GetRandomSignatoriesThatSignedRecently(..), GetSignatoryScreenshots(..), GetDocumentBySignatoryLinkID(..))
import Doc.Screenshot (Screenshot(..))
import Doc.SignatoryScreenshots (SignatoryScreenshots(..))
import qualified Data.ByteString.UTF8 as BS
import Text.StringTemplates.Templates (renderTemplate)
import MinutesTime (currentTime, daysBefore)
import qualified Text.StringTemplates.Fields as F
import qualified Data.ByteString.RFC2397 as RFC2397

getImagesFromScreenshots :: (MonadDB m, MonadThrow m) => (SignatoryLinkID, SignatoryScreenshots) -> m [(Bool, String, BS.ByteString)]
getImagesFromScreenshots (slid, ss) = do
  doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
  elogEvents <- dbQuery $ GetEvidenceLog $ documentid doc
  let sigElogEvents = filter ((== Just slid) . evSigLink) elogEvents
      Just (Just userAgent) = evClientName <$> find (isJust . evClientName) sigElogEvents
  return $ catMaybes $ map (aux userAgent) [ (True,) <$> first ss
                                           , (False,) <$> signing ss
                                           ]
    where aux :: String -> Maybe (Bool, Screenshot) -> Maybe (Bool, String, BS.ByteString)
          aux _ Nothing = Nothing
          aux ua (Just (b, Screenshot _ img)) = Just (b, ua, img)

main :: IO ()
main = run $ do
  now <- currentTime
  let lastWeek = 7 `daysBefore` now
  ids <- dbQuery $ GetRandomSignatoriesThatSignedRecently 5 lastWeek
  screenshots :: [(SignatoryLinkID, SignatoryScreenshots)] <- dbQuery $ GetSignatoryScreenshots ids
  images <- sequence $ map getImagesFromScreenshots screenshots
  let serialize (first', userAgent, image) = do
        F.value "image" $ BS.toString $ RFC2397.encode "image/jpeg" image
        F.value "userAgent" userAgent
        F.value "first" first'
  renderedElems <- mapM (renderTemplate "screenshotReviewElem" . serialize) $ concat images
  result <- renderTemplate "screenshotReview" $ F.value "content" $ concat renderedElems
  liftIO $ writeFile "/tmp/screenshots.html" result
