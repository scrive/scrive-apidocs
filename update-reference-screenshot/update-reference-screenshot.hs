{-

Run this program with a document ID pointing to a signed document.
The program will then extract the signing screenshot from the database
for the first (and only) non-author signatory and write that to
src/Doc/ReferenceScreenshot.hs

Thereafter, the extracted screenshot will be used as a reference
screen shot in future evidence-of-intent attachments in signed
documents.

The database configuration will be picked from "kontrakcja.conf".

-}

import AppConf (dbConfig)
import Configuration (readConfig)
import Control.Monad.Trans (liftIO, MonadIO)
import DB (dbQuery, MonadDB)
import DB.PostgreSQL (withPostgreSQL)
import Data.Maybe (listToMaybe)
import Doc.DocStateData (documentsignatorylinks, signatorylinkid)
import Doc.DocumentID (unsafeDocumentID)
import Doc.Model (GetDocumentByDocumentID (..), GetSignatoryScreenshots(..))
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import MinutesTime (MinutesTime, toSeconds)
import System.Environment (getProgName, getArgs)
import Util.SignatoryLinkUtils (isAuthor, hasSigned)
import Utils.Read (maybeRead)

usage :: MonadIO m => m a
usage = do
  progName <- liftIO getProgName
  fail $ "Usage: " ++ progName ++ " <documentid>"

main :: IO ()
main = do
  appConf <- do
    appname <- getProgName
    args <- getArgs
    readConfig putStrLn appname args "kontrakcja.conf"
  withPostgreSQL (dbConfig appConf) $ updateReference


updateReference :: (MonadIO m, MonadDB m) => m ()
updateReference = do
  args <- liftIO getArgs
  case args of
    [s] -> do
      docid <- maybe usage (return . unsafeDocumentID) $ maybeRead s
      doc <- maybe (fail $ "Cannot find document: " ++ show docid) return
         =<< dbQuery (GetDocumentByDocumentID docid)
      let ps = [ p | p <- documentsignatorylinks doc
                   , hasSigned p, not (isAuthor p) ]
      case ps of
        [p] -> do
          ss <- maybe (fail "No screenshots found for signing party")
                      (return . snd)
           =<< fmap listToMaybe (dbQuery (GetSignatoryScreenshots
                                            [signatorylinkid p]))
          signing <- maybe
                      (fail "Signing screenshot not part of the screenshots")
                      return
                      (SignatoryScreenshots.signing ss)
          let filename = "src/Doc/ReferenceScreenshot.hs"
          liftIO $ writeFile filename $ "\
            \-- This file is generated from update-reference-screenshot.\n\
            \-- Do not edit manually.\n\
            \module Doc.ReferenceScreenshot where\n\
            \\n\
            \import Doc.Screenshot(Screenshot(..))\n\
            \import MinutesTime(MinutesTime, fromSeconds)\n\
            \\n\
            \referenceScreenshot :: (MinutesTime, Screenshot)\n\
            \referenceScreenshot = " ++ show' signing ++ "\n\
            \"
          liftIO $ putStrLn $ "Wrote new screenshot to " ++ filename
        _   -> fail
                "Expected precisely one signing party in addition to the author"
    _ -> usage


show' :: (MinutesTime, Screenshot.Screenshot) -> String
show' (t, s) = "(fromSeconds " ++ show (toSeconds t) ++ ", "++ show s ++ ")"