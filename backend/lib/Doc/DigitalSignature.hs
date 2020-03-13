module Doc.DigitalSignature
  ( addDigitalSignature
  , extendDigitalSignature
  ) where

import Control.Arrow
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG (CryptoRNG)
import Data.Text (Text)
import Log
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Data.ByteString as BS

import DB (dbUpdate)
import Doc.API.Callback.Model (triggerAPICallbackIfThereIsOne)
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils (fileFromMainFile)
import Doc.Model (AppendExtendedSealedFile(..), AppendSealedFile(..))
import Doc.Types.Document
  ( documentid, documentsealedfile, documentsealingmethod
  )
import File.File (filename)
import File.Storage
import GuardTime (GuardTimeConfMonad, getGuardTimeConf)
import Log.Identifier
import Log.Utils
import PdfToolsLambda.Class
import SealingMethod
import Util.Actor (systemActor)
import Utils.Directory (withSystemTempDirectory')
import qualified Doc.SealStatus as SealStatus
import qualified GuardTime as GT

addDigitalSignature
  :: ( CryptoRNG m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , DocumentMonad m
     , MonadFileStorage m
     , TemplatesMonad m
     , GuardTimeConfMonad m
     , PdfToolsLambdaMonad m
     )
  => m Bool
addDigitalSignature = do
  mfile <- fileFromMainFile =<< (documentsealedfile <$> theDocument)
  file  <- case mfile of
    Nothing   -> unexpectedError "addDigitalSignature: File is not sealed"
    Just file -> return file
  content <- getFileContents file
  let fileName = filename file
  (docId, sealingMethod) <- (documentid &&& documentsealingmethod) <$> theDocument
  logInfo "Sealing document"
    $ object ["document_id" .= show docId, "sealing_method" .= show sealingMethod]
  case sealingMethod of
    Guardtime -> addGuardtimeSignature fileName content
    Pades     -> addPadesSignature fileName content

addPadesSignature
  :: ( CryptoRNG m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , DocumentMonad m
     , MonadFileStorage m
     , TemplatesMonad m
     , PdfToolsLambdaMonad m
     )
  => Text
  -> BS.ByteString
  -> m Bool
addPadesSignature fileName inputFileContent = do
  documentNumberText <- showt <$> theDocumentID
  answer             <- callPdfToolsPadesSign PadesSignSpec { .. }
  case answer of
    Just result -> do
      logInfo_ "Document successfully signed using PAdES"
      logInfo_ "Adding new sealed file to DB"
      sealedfileid <- saveNewFile fileName result
      logInfo "Finished adding sealed file to DB, adding to document"
        $ object [identifier sealedfileid]
      now <- currentTime
      dbUpdate $ AppendSealedFile sealedfileid SealStatus.Pades (systemActor now)
      return True
    Nothing -> do
      logInfo_ "PAdES signing failed"
      return False

addGuardtimeSignature
  :: ( CryptoRNG m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , DocumentMonad m
     , MonadFileStorage m
     , TemplatesMonad m
     , GuardTimeConfMonad m
     )
  => Text
  -> BS.ByteString
  -> m Bool
addGuardtimeSignature fileName content = theDocumentID >>= \did ->
  withSystemTempDirectory' ("DigitalSignature-" ++ show did ++ "-") $ \tmppath -> do
    let mainpath = tmppath </> "main.pdf"
    logInfo "Temp file write" $ object
      [ "bytes_written" .= (BS.length content)
      , "originator" .= ("addDigitalSignature" :: Text)
      ]
    liftIO $ BS.writeFile mainpath content
    gtconf                 <- getGuardTimeConf
    -- Guardtime signs in place
    (code, stdout, stderr) <- GT.digitallySign gtconf mainpath
    case code of
      ExitSuccess -> do
        vr <- GT.verify gtconf mainpath
        case vr of
          GT.Valid gsig -> do
            res <- liftIO $ BS.readFile mainpath
            logInfo "GuardTime verification result" $ logObject_ vr
            logInfo_ "GuardTime signed successfully"
            logInfo_ "Adding new sealed file to DB"
            sealedfileid <- saveNewFile fileName res
            logInfo "Finished adding sealed file to DB, adding to document"
              $ object [identifier sealedfileid]
            now <- currentTime
            dbUpdate
              $ AppendSealedFile
                  sealedfileid
                  (SealStatus.Guardtime (GT.extended gsig) (GT.privateGateway gsig))
              $ systemActor now
            return True
          _ -> do
            logAttention "GuardTime verification after signing failed for document"
              $ object
                  [ logPair_ vr
                  , "signing_stdout" `equalsExternalBSL` stdout
                  , "signing_stderr" `equalsExternalBSL` stderr
                  ]
            return False
      ExitFailure c -> do
        logAttention "GuardTime failed" $ object ["code" .= c]
        return False

-- | Extend a document: replace the digital signature with a keyless
-- one.  Trigger callbacks.
extendDigitalSignature
  :: ( CryptoRNG m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , DocumentMonad m
     , MonadFileStorage m
     , TemplatesMonad m
     , GuardTimeConfMonad m
     )
  => m Bool
extendDigitalSignature = do
  mfile <- fileFromMainFile =<< (documentsealedfile <$> theDocument)
  file  <- case mfile of
    Nothing   -> unexpectedError "extendDigitalSignature: File is not sealed"
    Just file -> return file

  did <- theDocumentID
  withSystemTempDirectory' ("ExtendSignature-" ++ show did ++ "-") $ \tmppath -> do
    content <- getFileContents file
    let sealedpath = tmppath </> "sealed.pdf"
    logInfo "Temp file write" $ object
      [ "bytes_written" .= (BS.length content)
      , "originator" .= ("extendDigitalSignature" :: Text)
      ]
    liftIO $ BS.writeFile sealedpath content
    now <- currentTime
    res <- digitallyExtendFile now sealedpath (filename file)
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

digitallyExtendFile
  :: ( MonadFileStorage m
     , TemplatesMonad m
     , MonadBase IO m
     , MonadThrow m
     , CryptoRNG m
     , MonadLog m
     , MonadIO m
     , MonadMask m
     , DocumentMonad m
     , GuardTimeConfMonad m
     )
  => UTCTime
  -> FilePath
  -> Text
  -> m Bool
digitallyExtendFile time pdfpath pdfname = do
  gtconf                 <- getGuardTimeConf
  (code, stdout, stderr) <- GT.digitallyExtend gtconf pdfpath
  mr                     <- case code of
    ExitSuccess -> do
      vr <- GT.verify gtconf pdfpath
      case vr of
        GT.Valid gsig | GT.extended gsig -> do
          res <- liftIO $ BS.readFile pdfpath
          logInfo "GuardTime verification result" $ logObject_ vr
          logInfo_ "GuardTime extended successfully"
          return $ Just
            (res, SealStatus.Guardtime (GT.extended gsig) (GT.privateGateway gsig))
        _ -> do
          logAttention "GuardTime verification after extension failed" $ object
            [ logPair_ vr
            , "extending_stdout" `equalsExternalBSL` stdout
            , "extending_stderr" `equalsExternalBSL` stderr
            ]
          return Nothing
    ExitFailure c -> do
      logAttention "GuardTime failed for document" $ object ["code" .= c]
      return Nothing
  case mr of
    Nothing -> return False
    Just (extendedfilepdf, status) -> do
      logInfo_ "Adding new extended file to DB"
      sealedfileid <- saveNewFile pdfname extendedfilepdf
      logInfo "Finished adding extended file to DB, adding to document"
        $ object [identifier sealedfileid]
      dbUpdate $ AppendExtendedSealedFile sealedfileid status $ systemActor time
      return True
