module Doc.DigitalSignature
  ( addDigitalSignature
  , extendDigitalSignature
  ) where

import Control.Monad.Catch
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG (CryptoRNG)
import Data.Default (def)
import Log
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Data.ByteString as BS

import ActionQueue.Scheduler (SchedulerData, getGlobalTemplates, sdAppConf)
import Amazon (AmazonMonad)
import AppConf (guardTimeConf)
import DB (dbUpdate)
import Doc.API.Callback.Model (triggerAPICallbackIfThereIsOne)
import Doc.Data.Document (documentsealedfile)
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils (fileFromMainFile)
import Doc.Model (AppendExtendedSealedFile(..), AppendSealedFile(..))
import Doc.SealStatus (SealStatus(..))
import File.File (filename)
import File.Model (NewFile(..))
import File.Storage (getFileContents)
import GuardTime (GuardTimeConf, GuardTimeConfMonad, getGuardTimeConf)
import KontraPrelude
import Log.Identifier
import Templates (runTemplatesT)
import Util.Actor (systemActor)
import Utils.Directory (withSystemTempDirectory')
import qualified GuardTime as GT

addDigitalSignature :: (CryptoRNG m, MonadIO m, MonadMask m, MonadLog m, MonadBaseControl IO m, DocumentMonad m, AmazonMonad m, GuardTimeConfMonad m, TemplatesMonad m) => m ()
addDigitalSignature = theDocumentID >>= \did ->
  withSystemTempDirectory' ("DigitalSignature-" ++ show did ++ "-") $ \tmppath -> do
  Just file <- fileFromMainFile =<< (documentsealedfile <$>theDocument)
  content <- getFileContents file
  let mainpath = tmppath </> "main.pdf"
  liftIO $ BS.writeFile mainpath content
  now <- currentTime
  gtconf <- getGuardTimeConf
  -- GuardTime signs in place
  code <- GT.digitallySign gtconf mainpath
  (newfilepdf, status) <- case code of
    ExitSuccess -> do
      vr <- GT.verify gtconf mainpath
      case vr of
           GT.Valid gsig -> do
                res <- liftIO $ BS.readFile mainpath
                logInfo "GuardTime verification result" $ logObject_ vr
                logInfo_ "GuardTime signed successfully"
                return (res, Guardtime (GT.extended gsig) (GT.privateGateway gsig))
           _ -> do
                res <- liftIO $ BS.readFile mainpath
                logAttention "GuardTime verification after signing failed for document" $ logObject_ vr
                return (res, Missing)
    ExitFailure c -> do
      res <- liftIO $ BS.readFile mainpath
      logAttention "GuardTime failed" $ object [
          "code" .= c
        ]
      return (res, Missing)
  when (status /= Missing) $ do
    logInfo_ "Adding new sealed file to DB"
    sealedfileid <- dbUpdate $ NewFile (filename file) newfilepdf
    logInfo "Finished adding sealed file to DB, adding to document" $ object [
        identifier_ sealedfileid
      ]
    dbUpdate $ AppendSealedFile sealedfileid status $ systemActor now

-- | Extend a document: replace the digital signature with a keyless one.  Trigger callbacks.
extendDigitalSignature :: (MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLog m, MonadReader SchedulerData m, CryptoRNG m, DocumentMonad m, AmazonMonad m) => m Bool
extendDigitalSignature = do
  Just file <- fileFromMainFile =<< (documentsealedfile <$>theDocument)
  did <- theDocumentID
  withSystemTempDirectory' ("ExtendSignature-" ++ show did ++ "-") $ \tmppath -> do
    content <- getFileContents file
    let sealedpath = tmppath </> "sealed.pdf"
    liftIO $ BS.writeFile sealedpath content
    now <- currentTime
    gtconf <- asks (guardTimeConf . sdAppConf)
    templates <- getGlobalTemplates
    res <- runTemplatesT (def, templates) $ digitallyExtendFile now gtconf sealedpath (filename file)
    when res $ triggerAPICallbackIfThereIsOne =<< theDocument -- Users that get API callback on document change, also get information about sealed file being extended.
    return res

    -- Here, we have the option of notifying signatories of the
    -- extended version.  However: customers that choose to create an
    -- account will be able to access the extended version in their
    -- archive, and we consider that sufficient at the moment.
    --
    -- We may consider putting in an option in a user's account
    -- settings where the user can opt in to get extended versions
    -- mailed.
    --
    -- We can also provide a service /extend (similar to /verify), by
    -- which signatories can get their documents extended.  Or the
    -- /verify service can detect and provide an extended version if
    -- the verified document was extensible.

digitallyExtendFile :: (TemplatesMonad m, MonadThrow m, CryptoRNG m, MonadLog m, MonadIO m, DocumentMonad m)
                    => UTCTime -> GuardTimeConf -> FilePath -> String -> m Bool
digitallyExtendFile ctxtime ctxgtconf pdfpath pdfname = do
  code <- GT.digitallyExtend ctxgtconf pdfpath
  mr <- case code of
    ExitSuccess -> do
      vr <- GT.verify ctxgtconf pdfpath
      case vr of
           GT.Valid gsig | GT.extended gsig -> do
                res <- liftIO $ BS.readFile pdfpath
                logInfo "GuardTime verification result" $ logObject_ vr
                logInfo_ "GuardTime extended successfully"
                return $ Just (res, Guardtime (GT.extended gsig) (GT.privateGateway gsig))
           _ -> do
                logInfo "GuardTime verification after extension failed" $ logObject_ vr
                return Nothing
    ExitFailure c -> do
      logAttention "GuardTime failed for document" $ object [
          "code" .= c
        ]
      return Nothing
  case mr of
    Nothing -> return False
    Just (extendedfilepdf, status) -> do
      logInfo_ "Adding new extended file to DB"
      sealedfileid <- dbUpdate $ NewFile pdfname extendedfilepdf
      logInfo "Finished adding extended file to DB, adding to document" $ object [
          identifier_ sealedfileid
        ]
      dbUpdate $ AppendExtendedSealedFile sealedfileid status $ systemActor ctxtime
      return True
