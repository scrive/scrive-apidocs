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
import Log
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Data.ByteString as BS

import DB
import DigitalSignatureMethod
import Doc.API.Callback.Model (triggerAPICallbackIfThereIsOne)
import Doc.DigitalSignatureStatus (DigitalSignatureStatus(Missing))
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.Model
  ( AppendClosedFileWithDigitalSignatureEvidence(..)
  , AppendClosedVerimiQesFileWithDigitalSignatureEvidence(..)
  , AppendExtendedGuardTimeSignedClosedFileWithDigitalSignatureEvidence(..)
  , AppendExtendedGuardTimeSignedClosedVerimiQesFileWithDigitalSignatureEvidence(..)
  )
import Doc.Types.Document
  ( documentdigitalsignaturemethod, documentfile, documentid
  )
import Doc.Types.DocumentFile
  ( DigitallySignedFile(..), DocumentFile(..), digitallySignedFile
  )
import File.Storage
import File.Types (File(..))
import GuardTime (GuardTimeConfMonad, getGuardTimeConf)
import Log.Identifier
import Log.Utils
import PdfToolsLambda.Class
import PdfToolsLambda.Conf
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.Actor (systemActor)
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Utils.Directory (withSystemTempDirectory')
import qualified Doc.DigitalSignatureStatus as DigitalSignatureStatus
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
  (docId, digitalSignatureMethod) <-
    (documentid &&& documentdigitalsignaturemethod) <$> theDocument
  logInfo
      "addDigitalSignature: Starting adding of digital signature ('Sealing document')."
    $ object
        ["document_id" .= show docId, "sealing_method" .= show digitalSignatureMethod]

  documentfile <$> theDocument >>= \case
    Just ClosedFile {..}
      | DigitallySignedFile mainfile Missing <- mainfileWithEvidence -> do
        content <- getFileContents mainfile
        case digitalSignatureMethod of
          Guardtime -> do
            mGuardTimeResult <- addGuardtimeSignature (filename mainfile) content
            whenJust mGuardTimeResult $ \(digitallySignedFile, digitalSignatureStatus) ->
              do
                now <- currentTime
                dbUpdate
                  . AppendClosedFileWithDigitalSignatureEvidence
                      (DigitallySignedFile digitallySignedFile digitalSignatureStatus)
                  $ systemActor now
            return $ isJust mGuardTimeResult

          Pades -> addPadesSignature (filename mainfile) content
      | otherwise -> unexpectedError
        "addDigitalSignature failed: file is already digitally signed!"

    Just (ClosedVerimiQesFile mainfileWithQesSignatures evidenceFile)
      | DigitallySignedFile evidenceFile' Missing <- evidenceFile -> do
        content <- getFileContents evidenceFile'
        case digitalSignatureMethod of
          Guardtime -> do
            mGuardTimeResult <- addGuardtimeSignature (filename evidenceFile') content
            whenJust mGuardTimeResult
              $ \(digitallySignedEvidenceFile, digitalSignatureStatus) -> do
                  now <- currentTime
                  dbUpdate
                    . AppendClosedVerimiQesFileWithDigitalSignatureEvidence
                        mainfileWithQesSignatures
                        (DigitallySignedFile digitallySignedEvidenceFile
                                             digitalSignatureStatus
                        )
                    $ systemActor now
            return $ isJust mGuardTimeResult
          Pades -> unexpectedError
            "addDigitalSignature failed: Pades with Verimi QES not (yet) implemented!"
      | otherwise -> unexpectedError
        "addDigitalSignature failed: file is already digitally signed!"

    _ -> unexpectedError "addDigitalSignature failed: file not closed!"



addPadesSignature
  :: ( CryptoRNG m
     , MonadIO m
     , MonadDB m
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
  documentNumberText     <- showt <$> theDocumentID
  authorid               <- guardJustM $ getAuthorUserId <$> theDocument
  mauthor                <- dbQuery $ GetUserByID authorid
  le                     <- lambdaEnv
  overrideAPICredentials <- case (globalSign le, mauthor) of
    (Just gsc, Just author) -> do
      authorugwp <- dbQuery . UserGroupGetWithParentsByUserID $ author ^. #id
      case ugwpSettings authorugwp ^. #padesCredentialsLabel of
        Nothing       -> return Nothing
        Just apiLabel -> return $ lookup apiLabel (gsc ^. #apiCredentials)
    _ -> return Nothing
  answer <- callPdfToolsPadesSign PadesSignSpec { .. }
  case answer of
    Just result -> do
      logInfo_ "Document successfully signed using PAdES"
      logInfo_ "Adding new sealed file to DB"
      closedmainfile <- saveNewFile fileName result
      logInfo "Finished adding sealed file to DB, adding to document"
        $ object [identifier . fileid $ closedmainfile]
      now <- currentTime
      dbUpdate $ AppendClosedFileWithDigitalSignatureEvidence
        (DigitallySignedFile closedmainfile DigitalSignatureStatus.Pades)
        (systemActor now)
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
  -> m (Maybe (File, DigitalSignatureStatus))
addGuardtimeSignature fileName content = theDocumentID >>= \did ->
  withSystemTempDirectory' ("DigitalSignature-" ++ show did ++ "-") $ \tmppath -> do
    let mainpath = tmppath </> "main.pdf"
    logInfo "Temp file write" $ object
      [ "bytes_written" .= BS.length content
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
            sealedfile <- saveNewFile fileName res
            logInfo "Finished adding sealed file to DB, adding to document"
              $ object [identifier $ fileid sealedfile]
            return $ Just
              ( sealedfile
              , DigitalSignatureStatus.Guardtime (GT.extended gsig)
                                                 (GT.privateGateway gsig)
              )
          _ -> do
            logAttention "GuardTime verification after signing failed for document"
              $ object
                  [ logPair_ vr
                  , "signing_stdout" `equalsExternalBSL` stdout
                  , "signing_stderr" `equalsExternalBSL` stderr
                  ]
            return Nothing
      ExitFailure c -> do
        logAttention "GuardTime failed" $ object ["code" .= c]
        return Nothing

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
  did <- theDocumentID
  withSystemTempDirectory' ("ExtendSignature-" ++ show did ++ "-") $ \tmppath -> do
    let sealedpath = tmppath </> "sealed.pdf"

    documentfile <$> theDocument >>= \case
      Just ClosedFile {..} | DigitallySignedFile mainfile _ <- mainfileWithEvidence -> do
        content <- getFileContents mainfile

        logInfo "Temp file write" $ object
          [ "bytes_written" .= BS.length content
          , "originator" .= ("extendDigitalSignature" :: Text)
          ]
        liftIO $ BS.writeFile sealedpath content
        now <- currentTime
        res <- digitallyExtendFile sealedpath (filename mainfile)
        whenJust res $ \(digitallySignedFile, digitalSignatureStatus) ->
          dbUpdate
            . AppendExtendedGuardTimeSignedClosedFileWithDigitalSignatureEvidence
                DigitallySignedFile { .. }
            $ systemActor now

        when (isJust res) $ triggerAPICallbackIfThereIsOne =<< theDocument -- Users that get API callback on document change, also get information about sealed file being extended.
        return $ isJust res

      Just (ClosedVerimiQesFile mainfileWithQesSignatures evidenceFile)
        | DigitallySignedFile evidenceFile' _ <- evidenceFile -> do
          content <- getFileContents evidenceFile'

          logInfo "Temp file write" $ object
            [ "bytes_written" .= BS.length content
            , "originator" .= ("extendDigitalSignature" :: Text)
            ]
          liftIO $ BS.writeFile sealedpath content
          now <- currentTime
          res <- digitallyExtendFile sealedpath (filename evidenceFile')
          whenJust res $ \(digitallySignedFile, digitalSignatureStatus) ->
            dbUpdate
              . AppendExtendedGuardTimeSignedClosedVerimiQesFileWithDigitalSignatureEvidence
                  mainfileWithQesSignatures
                  DigitallySignedFile { .. }
              $ systemActor now

          when (isJust res) $ triggerAPICallbackIfThereIsOne =<< theDocument -- Users that get API callback on document change, also get information about sealed file being extended.
          return $ isJust res
      _ -> unexpectedError "extendDigitalSignature: File is not closed (sealed)"


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
  => FilePath
  -> Text
  -> m (Maybe (File, DigitalSignatureStatus))
digitallyExtendFile pdfpath pdfname = do
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
          return
            $ Just
                ( res
                , DigitalSignatureStatus.Guardtime (GT.extended gsig)
                                                   (GT.privateGateway gsig)
                )
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
    Nothing -> return Nothing
    Just (extendedfilepdf, status) -> do
      logInfo_ "Adding new extended file to DB"
      sealedfile <- saveNewFile pdfname extendedfilepdf
      logInfo "Finished adding extended file to DB, adding to document"
        $ object [identifier $ fileid sealedfile]
      return $ Just (sealedfile, status)
