{- How to update reference screenshots from server running in current directory:

1. Create a template with Adam Author, Dave Desktop and Mike Mobile.  (Last names must match.)
2. Instantiate and sign, using desktop view for Dave, and mobile view for Mike.
3. Check that the screenshots look OK in the evidence-of-intent attachment.
4. Run this script with the document number <docid> that was just signed:
   $ cabal repl
   > :l scripts/updateReferenceScreenshots.hs
   > :main <docid>
5. Repeat step 2.
6. Check that the *reference* screenshots match the screenshots.
7. Add and commit files/reference_screenshots.

-}

import Control.Monad (forM_, unless)
import Control.Monad.Trans (liftIO)
import DB (dbQuery)
import Doc.DocStateData (documentsignatorylinks, signatorylinkid)
import Doc.DocumentID (DocumentID, unsafeDocumentID)
import Doc.Model (GetDocumentByDocumentID(..), GetSignatoryScreenshots(..))
import Doc.SignatoryScreenshots (signing, validReferenceName, referencePath)
import Interactive (run)
import System.Environment (getArgs)
import Text.JSON (showJSValue)
import Text.JSON.ToJSValue (toJSValue)
import Util.HasSomeUserInfo (getLastName)

main :: IO ()
main = do
  liftIO getArgs >>= \case
    [docid] -> updateScreenshots (unsafeDocumentID (read docid))
    _ -> fail "Usage from within ghci:\n\
              \:main <docid> -- update reference screenshots using given document"

updateScreenshots :: DocumentID -> IO ()
updateScreenshots docid = do
  let l = [ ("Author", "author")
          , ("Mobile", "mobile")
          , ("Desktop", "standard")
          ]
  forM_ l $ updateScreenshot docid True
  forM_ l $ updateScreenshot docid False

updateScreenshot :: DocumentID -> Bool -> (String, String) -> IO ()
updateScreenshot docid dryRun (lastName, refname) | validReferenceName refname = run $ do
  d <- dbQuery $ GetDocumentByDocumentID docid
  case [sl | sl <- documentsignatorylinks d, getLastName sl == lastName] of
    [s] -> do
      [(_,ss)] <- dbQuery $ GetSignatoryScreenshots [signatorylinkid s]
      case signing ss of
        Just fs -> do
          let p = referencePath refname
          unless dryRun $ do
            liftIO $ writeFile p(showJSValue (toJSValue fs) "")
            liftIO $ putStrLn $ "Wrote reference screenshot to " ++ p
        Nothing -> fail $ "Cannot find signing screenshot for signatory with last name: " ++ show lastName
    _ -> fail $ "Cannot find signing screenshot for signatory with last name: " ++ show lastName
updateScreenshot _ _ (_, refname) = fail $ "Invalid reference name: " ++ show refname
