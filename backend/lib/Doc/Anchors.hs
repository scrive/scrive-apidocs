module Doc.Anchors (
    recalculateAnchoredFieldPlacements
  , getAnchorPositions
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Int
import Log
import System.Exit
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Text.JSON.FromJSValue
import Text.JSON.Gen hiding (object)
import qualified Control.Monad.Fail as MF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.Map as Map hiding (map)
import qualified Data.Ord as Ord
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
import Log.Utils
import Utils.Directory

getAnchorPositions
  :: (MF.MonadFail m, Monad m, MonadBaseControl IO m, MonadLog m, MonadIO m)
  => BS.ByteString
  -> [PlacementAnchor]
  -> m (Map.Map PlacementAnchor (Int32, Double, Double))
getAnchorPositions _pdfcontent []      = return Map.empty
getAnchorPositions pdfcontent  anchors = do
  withSystemTempDirectory' "find-text-" $ \tmppath -> do
    let inputpath = tmppath ++ "/input.pdf"
        config    = runJSONGen $ do
          value "input" inputpath
          objects "matches" (map anchorToJS anchors)
        anchorToJS anc = do
          value "text"  (placementanchortext anc)
          value "index" (placementanchorindex anc)
        configpath    = tmppath ++ "/find-texts.json"
        configContent = BSL.fromString . show $ J.pp_value (toJSValue config)
    logInfo "Temp file write" $ object
      [ "bytes_written" .= BS.length pdfcontent
      , "originator" .= ("getAnchorPositions" :: Text)
      ]
    liftIO $ BS.writeFile inputpath pdfcontent
    logInfo "Temp file write" $ object
      [ "bytes_written" .= BSL.length configContent
      , "originator" .= ("getAnchorPositions" :: Text)
      ]
    liftIO $ BSL.writeFile configpath configContent

    (code, stdout, stderr) <- liftIO $ do
      readProcessWithExitCode
        "java"
        ["-jar", "scrivepdftools/scrivepdftools.jar", "find-texts", configpath]
        BSL.empty

    case code of
      ExitSuccess -> do
        stdoutjs <-
          either
              (\e -> do
                logAttention
                    "scrivepdftools/scrivepdftools.jar find-texts did not produce valid json"
                  $ object ["message" .= e, "stdout" `equalsExternalBSL` stdout]
                fail
                  "scrivepdftools/scrivepdftools.jar find-texts did not produce valid json"
              )
              return
            $ J.runGetJSON J.readJSValue (BSL.toString stdout)

        let matches :: Maybe (Maybe [Maybe (PlacementAnchor, (Int32, Double, Double))])
            matches =
              withJSValue stdoutjs
                . fromJSValueFieldCustom "matches"
                $ fromJSValueCustomMany
                    (do
                      text   <- fromJSValueField "text"
                      index  <- fromMaybe (Just 1) <$> fromJSValueField "index"
                      page   <- fromJSValueField "page"
                      coords <- fromJSValueField "coords"
                      let coordx = fst <$> coords
                      let coordy = snd <$> coords
                      return
                        (Just
                          (   (,)
                          <$> (PlacementAnchor <$> text <*> index)
                          <*> ((,,) <$> page <*> coordx <*> coordy)
                          )
                        )
                    )
        case matches of
          Just (Just realMatches) -> do
            return (Map.fromList (catMaybes realMatches))
          _ -> do
            return Map.empty
      ExitFailure _ -> do
        logAttention "Error while running scrivepdftools"
          $ object ["stderr" `equalsExternalBSL` stderr]
        return Map.empty

anchorSorter
  :: Maybe (Int32, Double, Double) -> Maybe (Int32, Double, Double) -> Ord.Ordering
anchorSorter (Just (a1page, _, _)) (Just (a2page, _, _)) | a1page < a2page = Ord.LT
anchorSorter (Just (a1page, _, _)) (Just (a2page, _, _)) | a1page > a2page = Ord.GT
anchorSorter (Just (a1page, a1yrel, _)) (Just (a2page, a2yrel, _))
  | a1page == a2page && a1yrel <= a2yrel = Ord.LT
anchorSorter (Just (a1page, a1yrel, _)) (Just (a2page, a2yrel, _))
  | a1page == a2page && a1yrel > a2yrel = Ord.GT
anchorSorter (Just _) _        = Ord.GT
anchorSorter _        (Just _) = Ord.LT
anchorSorter Nothing  Nothing  = Ord.EQ

recalculateAnchoredFieldPlacements
  :: (Kontrakcja m, DocumentMonad m) => FileID -> FileID -> m ()
recalculateAnchoredFieldPlacements oldfileid newfileid | oldfileid == newfileid = do
  return ()
recalculateAnchoredFieldPlacements oldfileid newfileid = do
  doc <- theDocument
  -- Algo:
  -- 1. Enumerate all anchors.
  -- 2. Calculate all anchors.
  -- 3. Iterate over fields, move a field based on anchor information.
  -- 4. Update.
  -- Note: Check if there are any anchors, if none skip all of this.

  let anchors =
        [ anc
        | sig <- documentsignatorylinks doc
        , fld <- signatoryfields sig
        , plc <- fieldPlacements fld
        , anc <- placementanchors plc
        ]

  unless (null anchors) $ do
    oldfilecontents    <- getFileIDContents oldfileid
    newfilecontents    <- getFileIDContents newfileid
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
          (_oldAnchorPosPage, oldAnchorPosX, oldAnchorPosY) <- msum
            (map (`Map.lookup` oldAnchorPositions) (placementanchors plc))
          (newAnchorPosPage, newAnchorPosX, newAnchorPosY) <- maximumBy
            anchorSorter
            (map (`Map.lookup` newAnchorPositions) (placementanchors plc))

          return
            (plc { placementxrel = placementxrel plc - oldAnchorPosX + newAnchorPosX
                 , placementyrel = placementyrel plc - oldAnchorPosY + newAnchorPosY
                 , placementpage = newAnchorPosPage
                 }
            )
    forM_ [ fld | sig <- documentsignatorylinks doc, fld <- signatoryfields sig ]
      $ \fld -> do
          let plcpairs :: [(FieldPlacement, Maybe FieldPlacement)]
              plcpairs = map (\p -> (p, maybeMoveFieldPlacement p)) (fieldPlacements fld)

          when (any (isJust . snd) plcpairs) $ do
            let newplc = map (uncurry fromMaybe) plcpairs
            dbUpdate $ SetFieldPlacements (fieldID fld) newplc
            return ()
