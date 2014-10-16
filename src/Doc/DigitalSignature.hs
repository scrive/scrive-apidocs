module Doc.DigitalSignature
  ( addDigitalSignature
  , extendDigitalSignature
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Data.ByteString as BS

import ActionQueue.Scheduler (SchedulerData, getGlobalTemplates, sdAppConf)
import Amazon (AmazonMonad)
import AppConf (guardTimeConf)
import Crypto.RNG (CryptoRNG)
import DB (Binary(..), dbUpdate)
import Doc.API.Callback.Model (triggerAPICallbackIfThereIsOne)
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils (documentsealedfileM)
import Doc.Model (AppendSealedFile(..), AppendExtendedSealedFile(..))
import Doc.SealStatus (SealStatus(..))
import File.File (filename)
import File.Model (NewFile(..))
import File.Storage (getFileContents)
import GuardTime (GuardTimeConf, GuardTimeConfMonad, getGuardTimeConf)
import MinutesTime
import Templates (runTemplatesT)
import Util.Actor (systemActor)
import Utils.Default (defaultValue)
import Utils.Directory (withSystemTempDirectory')
import qualified GuardTime as GT
import qualified Log

addDigitalSignature :: (CryptoRNG m, MonadIO m, MonadThrow m, Log.MonadLog m, MonadBaseControl IO m, DocumentMonad m, AmazonMonad m, GuardTimeConfMonad m, TemplatesMonad m) => m ()
addDigitalSignature = theDocumentID >>= \did ->
  withSystemTempDirectory' ("DigitalSignature-" ++ show did ++ "-") $ \tmppath -> do
  Just file <- theDocument >>= documentsealedfileM
  content <- getFileContents file
  let mainpath = tmppath </> "main.pdf"
  liftIO $ BS.writeFile mainpath content
  now <- currentTime
  gtconf <- getGuardTimeConf
  -- GuardTime signs in place
  code <- GT.digitallySign gtconf mainpath
  (newfilepdf, status) <- first Binary <$> case code of
    ExitSuccess -> do
      vr <- GT.verify gtconf mainpath
      case vr of
           GT.Valid gsig -> do
                res <- liftIO $ BS.readFile mainpath
                Log.mixlog_ $ "GuardTime verification result: " ++ show vr
                Log.mixlog_ $ "GuardTime signed successfully #" ++ show did
                return (res, Guardtime (GT.extended gsig) (GT.privateGateway gsig))
           _ -> do
                res <- liftIO $ BS.readFile mainpath
                Log.attention_ $ "GuardTime verification after signing failed for document #" ++ show did ++ ": " ++ show vr
                return (res, Missing)
    ExitFailure c -> do
      res <- liftIO $ BS.readFile mainpath
      Log.attention_ $ "GuardTime failed " ++ show c ++ " of document #" ++ show did
      return (res, Missing)
  when (status /= Missing) $ do
    Log.mixlog_ $ "Adding new sealed file to DB"
    sealedfileid <- dbUpdate $ NewFile (filename file) newfilepdf
    Log.mixlog_ $ "Finished adding sealed file to DB with fileid " ++ show sealedfileid ++ "; now adding to document"
    dbUpdate $ AppendSealedFile sealedfileid status $ systemActor now

-- | Extend a document: replace the digital signature with a keyless one.  Trigger callbacks.
extendDigitalSignature :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, Log.MonadLog m, MonadReader SchedulerData m, CryptoRNG m, DocumentMonad m, AmazonMonad m) => m ()
extendDigitalSignature = do
  Just file <- documentsealedfileM =<< theDocument
  did <- theDocumentID
  withSystemTempDirectory' ("ExtendSignature-" ++ show did ++ "-") $ \tmppath -> do
    content <- getFileContents file
    let sealedpath = tmppath </> "sealed.pdf"
    liftIO $ BS.writeFile sealedpath content
    now <- currentTime
    gtconf <- asks (guardTimeConf . sdAppConf)
    templates <- getGlobalTemplates
    res <- runTemplatesT (defaultValue, templates) $ digitallyExtendFile now gtconf sealedpath (filename file)
    when res $ triggerAPICallbackIfThereIsOne =<< theDocument -- Users that get API callback on document change, also get information about sealed file being extended.

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

digitallyExtendFile :: (TemplatesMonad m, MonadThrow m, CryptoRNG m, Log.MonadLog m, MonadIO m, DocumentMonad m)
                    => UTCTime -> GuardTimeConf -> FilePath -> String -> m Bool
digitallyExtendFile ctxtime ctxgtconf pdfpath pdfname = do
  documentid <- theDocumentID
  code <- GT.digitallyExtend ctxgtconf pdfpath
  mr <- case code of
    ExitSuccess -> do
      vr <- GT.verify ctxgtconf pdfpath
      case vr of
           GT.Valid gsig | GT.extended gsig -> do
                res <- liftIO $ BS.readFile pdfpath
                Log.mixlog_ $ "GuardTime verification result: " ++ show vr
                Log.mixlog_ $ "GuardTime extended successfully #" ++ show documentid
                return $ Just (res, Guardtime (GT.extended gsig) (GT.privateGateway gsig))
           _ -> do
                Log.mixlog_ $ "GuardTime verification after extension failed for document #" ++ show documentid ++ ": " ++ show vr
                return Nothing
    ExitFailure c -> do
      Log.attention_ $ "GuardTime failed " ++ show c ++ " for document #" ++ show documentid
      return Nothing
  case mr of
    Nothing -> return False
    Just (extendedfilepdf, status) -> do
      Log.mixlog_ $ "Adding new extended file to DB"
      sealedfileid <- dbUpdate $ NewFile pdfname (Binary extendedfilepdf)
      Log.mixlog_ $ "Finished adding extended file to DB with fileid " ++ show sealedfileid ++ "; now adding to document"
      dbUpdate $ AppendExtendedSealedFile sealedfileid status $ systemActor ctxtime
      return True
