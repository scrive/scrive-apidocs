module Doc.Anchors (
    recalcuateAnchoredFieldPlacements
  , getAnchorPositions
  ) where

import Control.Monad.Error
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Exit
import Text.JSON.FromJSValue
import Text.JSON.Gen
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Map as Map hiding (map)
import qualified Text.JSON as J
import qualified Text.JSON.Pretty as J (pp_value)
import qualified Text.JSON.String as J

import DB
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model
import File.Model
import File.Storage
import KontraMonad
import KontraPrelude
import Utils.Directory
import Utils.IO
import qualified Log

getAnchorPositions :: (Monad m, MonadBaseControl IO m,Log.MonadLog m,MonadIO m) => BS.ByteString -> [PlacementAnchor] -> m (Map.Map PlacementAnchor (Int,Double,Double))
getAnchorPositions _pdfcontent [] = return Map.empty
getAnchorPositions pdfcontent anchors = do
  withSystemTempDirectory' ("find-text-") $ \tmppath -> do
    let inputpath = tmppath ++ "/input.pdf"
        config = runJSONGen $ do
          value "input" inputpath
          objects "matches" (map anchorToJS anchors)
        anchorToJS anc = do
          value "text" (placementanchortext anc)
          value "index" (placementanchorindex anc)
          case placementanchorpages anc of
            Nothing -> return ()
            Just pages -> value "pages" pages
        configpath = tmppath ++ "/find-texts.json"

    liftIO $ BS.writeFile inputpath pdfcontent
    liftIO $ BSL.writeFile configpath (BSL.fromString $ show $ J.pp_value (toJSValue config))

    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode' "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "find-texts", configpath] (BSL.empty)

    case code of
      ExitSuccess -> do
        stdoutjs <- either (\e -> do Log.attention "scrivepdftools/scrivepdftools.jar find-texts did not produce valid json" $ do
                                       value "message" e
                                       value "stdout" (BSL.toString stdout)
                                     fail "scrivepdftools/scrivepdftools.jar find-texts did not produce valid json"
                                  ) return $
                    J.runGetJSON J.readJSValue (BSL.toString stdout)

        let matches :: Maybe (Maybe [Maybe (PlacementAnchor, (Int,Double,Double))])
            matches = withJSValue stdoutjs $ fromJSValueFieldCustom "matches" $ fromJSValueCustomMany $ ((do
              text                 <- fromJSValueField "text"
              index                <- fromMaybe (Just 1) <$> fromJSValueField "index"
              pages                <- fmap Just $ fromJSValueField "pages"
              page                 <- fromJSValueField "page"
              coords               <- fromJSValueField "coords"
              let coordx = fst <$> coords
              let coordy = snd <$> coords
              return (Just ((,) <$> (PlacementAnchor <$> text <*> index <*> pages)
                      <*> ((,,) <$> page <*> coordx <*> coordy)))))
        case matches of
          Just (Just realMatches) -> do
            return (Map.fromList (catMaybes realMatches))
          _ -> do
            return Map.empty
      ExitFailure _ -> do
        Log.attention_ $ BSL.toString stderr
        return Map.empty


recalcuateAnchoredFieldPlacements :: (Kontrakcja m,DocumentMonad m) => FileID -> FileID -> m ()
recalcuateAnchoredFieldPlacements oldfileid newfileid | oldfileid == newfileid = do
    return ()
recalcuateAnchoredFieldPlacements oldfileid newfileid = do
  doc <- theDocument
  -- Algo:
  -- 1. Enumerate all anchors.
  -- 2. Calculate all anchors.
  -- 3. Iterate over fields, move a field based on anchor information.
  -- 4. Update.
  -- Note: Check if there are any anchors, if none skip all of this.

  let anchors = [ anc | sig <- documentsignatorylinks doc,
                        fld <- signatoryfields sig,
                        plc <- fieldPlacements fld,
                        anc <- placementanchors plc ]

  when (not (null anchors)) $ do
    oldfilecontents <- getFileIDContents oldfileid
    newfilecontents <- getFileIDContents newfileid
    oldAnchorPositions <- getAnchorPositions oldfilecontents anchors
    newAnchorPositions <- getAnchorPositions newfilecontents anchors

    -- oldAnchorPositions and newAnchorPositions are maps from anchors
    -- to extracted positions. Note that it is possible that not every
    -- anchor was found in the documents. In that case we just do not
    -- move a field and that should be good enough as debugging
    -- information.
    let maybeMoveFieldPlacement :: FieldPlacement -> Maybe FieldPlacement
        maybeMoveFieldPlacement plc = do
          -- find first anchor that was found
          (_oldAnchorPosPage,oldAnchorPosX,oldAnchorPosY) <-
            msum (map (\anchor -> Map.lookup anchor oldAnchorPositions) (placementanchors plc))
          (newAnchorPosPage,newAnchorPosX,newAnchorPosY) <-
            msum (map (\anchor -> Map.lookup anchor newAnchorPositions) (placementanchors plc))
          return (plc { placementxrel = placementxrel plc - oldAnchorPosX + newAnchorPosX,
                        placementyrel = placementyrel plc - oldAnchorPosY + newAnchorPosY,
                        placementpage = newAnchorPosPage })
    forM_ [fld | sig <- documentsignatorylinks doc,
                 fld <- signatoryfields sig ] $ \fld -> do
      let plcpairs :: [(FieldPlacement, Maybe FieldPlacement)]
          plcpairs = map (\p -> (p,maybeMoveFieldPlacement p)) (fieldPlacements fld)
      when (any (isJust . snd) plcpairs) $ do
        let newplc = map (\(k,v) -> fromMaybe k v) plcpairs
        dbUpdate $ SetFieldPlacements (fieldID fld) newplc
        return ()

  return ()
