{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocSeal
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  not portable
--
-- All that is needed to seal a document
-----------------------------------------------------------------------------
module Doc.DocSeal
  ( sealDocument
  , presealDocumentFile
  , digitallySealDocument
  , digitallyExtendDocument
  ) where

import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Function (on)
import Data.Maybe
import Data.List
import Doc.DocStateData
import Doc.DocumentID (DocumentID)
import Doc.Model
import Doc.Rendering
import Doc.SealStatus (SealStatus(..))
import File.Storage
import File.File
import Doc.DocView
import Doc.DocUtils
import GuardTime (GuardTimeConf)
import qualified HostClock.Model as HC
import MinutesTime (MinutesTime, daysBefore, toCalendarTime)
import Utils.Directory
import Utils.Read
import Utils.IO
import System.Directory
import System.Exit
import Kontra
import Text.StringTemplates.Templates
import Text.Printf
import Text.JSON.Gen
import Text.JSON.Pretty (pp_value)
import System.Time
import System.Locale
import Templates
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.Lazy as BSL (empty)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Base64 as B64
import qualified Doc.SealSpec as Seal
import qualified GuardTime as GT
import qualified Log
import System.IO.Temp
import System.IO hiding (stderr)
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import File.Model
import Crypto.RNG
import DB
import Control.Applicative
import Control.Arrow (first)
import EvidenceLog.Model
import EvidenceLog.View
import Util.Actor
import qualified Text.StringTemplates.Fields as F
import Control.Logic
import Utils.Prelude
import qualified Amazon as AWS
import Doc.API.Callback.Model

personFromSignatory :: (BS.ByteString,BS.ByteString) -> SignatoryLink -> Seal.Person
personFromSignatory boxImages signatory =
    Seal.Person { Seal.fullname = getFullName signatory
                , Seal.company = getCompanyName signatory
                , Seal.email = getEmail signatory
                , Seal.phone = getMobile signatory
                , Seal.personalnumber = getPersonalNumber signatory
                , Seal.companynumber = getCompanyNumber signatory
                , Seal.fullnameverified = False
                , Seal.companyverified = False
                , Seal.numberverified = False
                , Seal.emailverified = True
                , Seal.phoneverified = False
                , Seal.fields = fieldsFromSignatory False [] boxImages signatory
                }

personExFromSignatoryLink :: (BS.ByteString,BS.ByteString) -> SignatoryLink -> (Seal.Person, String)
personExFromSignatoryLink boxImages (sl@SignatoryLink { signatorysignatureinfo
                                                      , signatorylinkdeliverymethod
                                                      }) =
  ((personFromSignatory boxImages sl)
     { Seal.emailverified    = signatorylinkdeliverymethod `elem` [EmailDelivery, EmailAndMobileDelivery]
     , Seal.fullnameverified = fullnameverified
     , Seal.companyverified  = False
     , Seal.numberverified   = numberverified
     , Seal.phoneverified    = signatorylinkdeliverymethod `elem` [MobileDelivery, EmailAndMobileDelivery]
     }
    , map head $ words $ getFullName sl
    )
  where fullnameverified = maybe False (\s -> signaturefstnameverified s
                                              && signaturelstnameverified s)
                           signatorysignatureinfo
        numberverified = maybe False signaturepersnumverified signatorysignatureinfo

fieldsFromSignatory :: Bool -> [(FieldType,String)] -> (BS.ByteString,BS.ByteString) -> SignatoryLink -> [Seal.Field]
fieldsFromSignatory addEmpty emptyFieldsText (checkedBoxImage,uncheckedBoxImage) SignatoryLink{signatoryfields} =
  silenceJPEGFieldsFromFirstSignature $ concatMap makeSealField  signatoryfields
  where
    silenceJPEGFieldsToTheEnd [] = []
    silenceJPEGFieldsToTheEnd (field@(Seal.FieldJPG{}):xs) = (field { Seal.includeInSummary = False }) : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsToTheEnd (x:xs) = x : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsFromFirstSignature [] = []
    silenceJPEGFieldsFromFirstSignature (field@(Seal.FieldJPG{Seal.includeInSummary = True}):xs) =
      field : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsFromFirstSignature (x:xs) = x : silenceJPEGFieldsFromFirstSignature xs

    makeSealField :: SignatoryField -> [Seal.Field]
    makeSealField sf = case sfType sf of
       SignatureFT _ -> case (sfPlacements sf,drop 1 $ dropWhile (\e -> e /= ',') $ sfValue sf) of
                           (_,"") -> []  -- We skip signature that don't have a drawing
                           ([],v) -> maybeToList $ fieldJPEGFromSignatureField v
                           (plsms,v) -> concatMap (maybeToList . (fieldJPEGFromPlacement v)) plsms
       CheckboxFT _ -> map (uncheckedImageFromPlacement <| null (sfValue sf) |>  checkedImageFromPlacement) (sfPlacements sf)
       _ -> for (sfPlacements sf) $ \p -> case (addEmpty, sfValue sf, sfType sf) of
                                                (True,"",CustomFT n _) -> fieldFromPlacement True n p
                                                (True,"",ft) -> fieldFromPlacement True (fromMaybe "" (lookup ft emptyFieldsText)) p
                                                _ -> fieldFromPlacement False (sfValue sf) p
    fieldFromPlacement greyed sf placement =
      Seal.Field { Seal.value            = sf
                 , Seal.x                = placementxrel placement
                 , Seal.y                = placementyrel placement
                 , Seal.page             = placementpage placement
                 , Seal.fontSize         = placementfsrel placement
                 , Seal.greyed           = greyed
                 , Seal.includeInSummary = True
                 }

    checkedImageFromPlacement = iconWithPlacement checkedBoxImage
    uncheckedImageFromPlacement = iconWithPlacement uncheckedBoxImage
    iconWithPlacement image placement = Seal.FieldJPG
                 { valueBase64           = BS.toString $ B64.encode image
                 , Seal.x                = placementxrel placement
                 , Seal.y                = placementyrel placement
                 , Seal.page             = placementpage placement
                 , Seal.image_w          = placementwrel placement
                 , Seal.image_h          = placementhrel placement
                 , Seal.includeInSummary = False
                 , Seal.onlyForSummary   = False
                 , Seal.keyColor         = Nothing
                 }
    fieldJPEGFromPlacement v placement =
          Just $ Seal.FieldJPG
                 { valueBase64           = v
                 , Seal.x                = placementxrel placement
                 , Seal.y                = placementyrel placement
                 , Seal.page             = placementpage placement
                 , Seal.image_w          = placementwrel placement
                 , Seal.image_h          = placementhrel placement
                 , Seal.includeInSummary = True
                 , Seal.onlyForSummary   = False
                 , Seal.keyColor         = Just (255,255,255) -- white is transparent
                 }
    fieldJPEGFromSignatureField v =
          Just $ Seal.FieldJPG
                 { valueBase64           = v
                 , Seal.x                = 0
                 , Seal.y                = 0
                 , Seal.page             = 0
                 , Seal.image_w          = 0
                 , Seal.image_h          = 0
                 , Seal.includeInSummary = True
                 , Seal.onlyForSummary   = True
                 , Seal.keyColor         = Just (255,255,255) -- white is transparent
                 }

listAttachmentsFromDocument :: Document -> [(SignatoryAttachment,SignatoryLink)]
listAttachmentsFromDocument document =
  concatMap extract (documentsignatorylinks document)
  where extract sl = map (\at -> (at,sl)) (signatoryattachments sl)

findOutAttachmentDesc :: (KontraMonad m, MonadIO m, MonadDB m, TemplatesMonad m, AWS.AmazonMonad m) => Document -> m [Seal.FileDesc]
findOutAttachmentDesc document = do
  a <- mapM findAttachmentsForAuthorAttachment authorAttsNumbered
  b <- mapM findAttachmentsForSignatoryAttachment attAndSigsNumbered
  return (a ++ b)
  where
      attAndSigs = listAttachmentsFromDocument document
      authorAtts = documentauthorattachments document
      authorAttsNumbered = zip [1::Int ..] authorAtts
      attAndSigsNumbered = zipWith (\num (at,sl) -> (num,at,sl)) [(length authorAtts + 1) ..] attAndSigs

      authorName = fromMaybe "" $ fmap getSmartName $ getAuthorSigLink document

      findAttachmentsForAuthorAttachment (num, authorattach) = do
        let fileid' = authorattachmentfile authorattach
        (numberOfPages,name) <- do
          contents <- getFileIDContents fileid'
          file <- dbQuery $ GetFileByFileID fileid'
          let name' = filename file
          return (getNumberOfPDFPages contents, name')
        numberOfPagesText <-
          if numberOfPages==1
           then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
           else renderLocalTemplate document "_numberOfPages" $ do
             F.value "pages" numberOfPages

        attachedByText <- renderLocalTemplate document "_documentAttachedBy" $ do
                                     F.value "author" authorName

        attachmentNumText <- renderLocalTemplate document "_attachedDocument" $ do
                                     F.value "number" num

        return $ Seal.FileDesc
                 { fileTitle      = removeExtIfAny name
                 , fileRole       = attachmentNumText
                 , filePagesText  = numberOfPagesText
                 , fileAttachedBy = attachedByText
                 }

      removeExtIfAny fname = case dropWhile (/= '.') (reverse fname) of
                               "" -> fname
                               x -> reverse (drop 1 x)

      findAttachmentsForSignatoryAttachment (num, sigattach, sl) = do
        let personName = getSmartName sl
        numberOfPages <- case signatoryattachmentfile sigattach of
                       Nothing -> return 1
                       Just fileid' -> do
                                   contents <- getFileIDContents fileid'
                                   return $ getNumberOfPDFPages contents
        numberOfPagesText <-
          if numberOfPages==1
           then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
           else renderLocalTemplate document "_numberOfPages" $ do
             F.value "pages" numberOfPages

        attachedByText <- renderLocalTemplate document "_documentAttachedBy" $ do
                                     F.value "author" personName

        attachmentNumText <- renderLocalTemplate document "_attachedDocument" $ do
                                     F.value "number" num

        return $ Seal.FileDesc
                 { fileTitle      = signatoryattachmentname sigattach
                 , fileRole       = attachmentNumText
                 , filePagesText  = numberOfPagesText
                 , fileAttachedBy = attachedByText
                 }

evidenceOfIntentAttachment :: (TemplatesMonad m, MonadDB m, MonadIO m, AWS.AmazonMonad m) => String -> [SignatoryLink] -> m Seal.SealAttachment
evidenceOfIntentAttachment title sls = do
  ss <- dbQuery $ GetSignatoryScreenshots (map signatorylinkid sls)
  let sortBySignTime = sortBy (on compare (fmap signtime . maybesigninfo . fst))
  html <- evidenceOfIntentHTML title $ sortBySignTime [ (sl, s) | (i, s) <- ss, sl <- filter ((==i) . signatorylinkid) sls ]
  return $ Seal.SealAttachment { Seal.fileName = "EvidenceofIntent.html"
                               , Seal.mimeType = Nothing
                               , Seal.fileBase64Content = BS.toString $ B64.encode $ BS.fromString html
                               }

{-
 formatCalendarTime does not support %z as modifier. We have to implement it ourselves here.
-}
formatMinutesTimeForVerificationPage :: MinutesTime -> String
formatMinutesTimeForVerificationPage mt = formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" caltime ++ " (" ++ tzinfo ++ ")"
  where
    caltime = MinutesTime.toCalendarTime mt
    tzoffset = ctTZ caltime `div` 60 -- convert seconds into minutes
    tzinfo = printf "%+03d%02d"  (tzoffset `div` 60) (tzoffset `mod` 60)

sealSpecFromDocument :: (KontraMonad m, MonadIO m, TemplatesMonad m, MonadDB m, AWS.AmazonMonad m)
                     => (BS.ByteString,BS.ByteString)
                     -> String
                     -> Document
                     -> [DocumentEvidenceEvent]
                     -> HC.ClockErrorStatistics
                     -> BS.ByteString
                     -> String
                     -> String
                     -> m Seal.SealSpec
sealSpecFromDocument boxImages hostpart document elog ces content inputpath outputpath = do
  additionalAttachments <- findOutAttachmentDesc document
  sigVerFile <- liftIO $ BS.toString <$> B64.encode <$> BS.readFile "files/verification.html"
  evidenceDoc <- liftIO $ BS.toString <$> B64.encode <$> BS.readFile "files/evidenceDocumentation.html"
  sealSpecFromDocument2 boxImages hostpart document elog ces content inputpath outputpath additionalAttachments sigVerFile evidenceDoc

sealSpecFromDocument2 :: (TemplatesMonad m, MonadDB m, MonadIO m, AWS.AmazonMonad m)
                     => (BS.ByteString,BS.ByteString)
                     -> String
                     -> Document
                     -> [DocumentEvidenceEvent]
                     -> HC.ClockErrorStatistics
                     -> BS.ByteString
                     -> String
                     -> String
                     -> [Seal.FileDesc]
                     -> String
                     -> String
                     -> m Seal.SealSpec
sealSpecFromDocument2 boxImages hostpart document elog ces content inputpath outputpath additionalAttachments sigVerFile evidenceDoc =
  let docid = documentid document
      Just authorsiglink = getAuthorSigLink document

      signatories = [ personExFromSignatoryLink boxImages s
                        | s <- documentsignatorylinks document
                        , signatoryispartner $ s
                    ]

      secretaries = [ personFromSignatory boxImages $ s
                        | s <- documentsignatorylinks document
                        , not . signatoryispartner $ s
                    ]

      (persons, initialsx) = unzip signatories
      paddeddocid = pad0 20 (show docid)

      initials = intercalate ", " initialsx

      mkHistEntry ev = do
        actor <- approximateActor document ev
        comment <- simplyfiedEventText (Just actor) ev
        return $ Seal.HistEntry{ Seal.histdate = formatMinutesTimeForVerificationPage $ evTime ev
                               , Seal.histcomment = comment
                               , Seal.histaddress = maybe "" show $ evIP4 ev
                               }

  in do
      -- Log.debug "Creating seal spec from file."
      -- Remove resealing-induced events and non-signing parties
      let eventsForHistory = filter (not . (`elem` map Current [ResealedPDF, AttachGuardtimeSealedFileEvidence, AttachExtendedSealedFileEvidence]) . evType)
                           . filter (not . viewingParty)
          viewingParty e = viewer (evAffectedSigLink e) || viewer (evSigLink e)
               where viewer s = (signatoryispartner <$> s) == Just False
      history <- (mapM mkHistEntry . eventsForHistory) =<< getSignatoryLinks (eventsForLog elog)
      -- Log.debug ("about to render staticTexts")
      staticTexts <- renderLocalTemplate document "contractsealingtexts" $ do
                        documentInfoFields document
                        F.value "hostpart" hostpart
      -- Log.debug ("finished staticTexts: " ++ show staticTexts)
      readtexts <- case maybeRead staticTexts of
                     Just x -> return x
                     Nothing -> do
                       --Log.error $ "Cannot read SealingTexts: " ++ staticTexts
                       error $ "Cannot read SealingTexts: " ++ staticTexts
      -- Log.debug ("read texts: " ++ show readtexts)

      -- Creating HTML Evidence Log
      htmllogs <- htmlDocFromEvidenceLog (documenttitle document) (suppressRepeatedEvents elog) ces
      let evidenceattachment = Seal.SealAttachment { Seal.fileName = "EvidenceLog.html"
                                                   , Seal.mimeType = Nothing
                                                   , Seal.fileBase64Content = BS.toString $ B64.encode $ BS.fromString htmllogs }
      evidenceOfIntent <- evidenceOfIntentAttachment (documenttitle document) (documentsignatorylinks document)
      -- add signature verification documentation
      let signatureVerificationAttachment =
            Seal.SealAttachment { Seal.fileName = "DigitalSignatureDocumentation.html"
                                , Seal.mimeType = Nothing
                                , Seal.fileBase64Content = sigVerFile
                                }
      let evidenceDocumentationAttachment =
            Seal.SealAttachment { Seal.fileName = "EvidenceDocumentation.html"
                                , Seal.mimeType = Nothing
                                , Seal.fileBase64Content = evidenceDoc
                                }


      let numberOfPages = getNumberOfPDFPages content
      numberOfPagesText <-
        if numberOfPages==1
           then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
           else renderLocalTemplate document "_numberOfPages" $ do
             F.value "pages" numberOfPages

      attachedByText <- renderLocalTemplate document "_documentSentBy" $ do
        F.value "author" (getSmartName authorsiglink)

      mainDocumentText <- renderLocalTemplate document "_mainDocument"
                          $ (return ())

      return $ Seal.SealSpec
            { Seal.input          = inputpath
            , Seal.output         = outputpath
            , Seal.documentNumber = paddeddocid
            , Seal.persons        = persons
            , Seal.secretaries    = secretaries
            , Seal.history        = history
            , Seal.initials       = initials
            , Seal.hostpart       = hostpart
            , Seal.staticTexts    = readtexts
            , Seal.attachments    = [evidenceDocumentationAttachment, evidenceattachment, evidenceOfIntent, signatureVerificationAttachment]
            , Seal.filesList      =
              [ Seal.FileDesc { fileTitle = documenttitle document
                              , fileRole = mainDocumentText
                              , filePagesText = numberOfPagesText
                              , fileAttachedBy = attachedByText
                              } ] ++ additionalAttachments
            }

presealSpecFromDocument :: [(FieldType,String)] -> (BS.ByteString,BS.ByteString) -> Document -> String -> String -> Seal.PreSealSpec
presealSpecFromDocument emptyFieldsText boxImages document inputpath outputpath =
       Seal.PreSealSpec
            { Seal.pssInput          = inputpath
            , Seal.pssOutput         = outputpath
            , Seal.pssFields         = concatMap (fieldsFromSignatory True emptyFieldsText boxImages) (documentsignatorylinks document)
            }


sealDocument :: (CryptoRNG m, MonadBaseControl IO m, MonadDB m, KontraMonad m, TemplatesMonad m, MonadIO m, AWS.AmazonMonad m)
             => Document
             -> m (Either String Document)
sealDocument document = do
  mfile <- documentfileM document
  case mfile of
    Just file -> do
      Log.debug $ "Sealing document #" ++ show (documentid document)
      result <- sealDocumentFile document file
      Log.debug $ "Sealing of document #" ++ show (documentid document) ++ " should be done now"
      return result
    Nothing ->do
      let msg = "Sealing of document #" ++ show (documentid document) ++ " because it has no main file attached"
      Log.debug $ msg
      return $ Left msg

collectClockErrorStatistics :: MonadDB m => [DocumentEvidenceEvent] -> m HC.ClockErrorStatistics
collectClockErrorStatistics [] = return $ HC.ClockErrorStatistics Nothing Nothing Nothing 0 0
collectClockErrorStatistics elog = do
  let endtime   = maximum (map evTime elog)
      starttime = minimum (map evTime elog) `min` (1 `daysBefore` endtime)
  dbQuery $ HC.GetClockErrorStatistics (Just starttime) (Just endtime)

sealDocumentFile :: (CryptoRNG m, MonadBaseControl IO m, MonadDB m, KontraMonad m, TemplatesMonad m, MonadIO m, AWS.AmazonMonad m)
                 => Document
                 -> File
                 -> m (Either String Document)
sealDocumentFile document@Document{documentid} file@File{fileid, filename} =
  withSystemTempDirectory' ("seal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    Context{ctxhostpart, ctxtime, ctxgtconf} <- getContext
    elog <- dbQuery $ GetEvidenceLog documentid
    ces <- collectClockErrorStatistics elog
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "public/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "public/img/checkbox_unchecked.jpg"
    config <- sealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) ctxhostpart document elog ces content tmpin tmpout

    (code,_stdout,stderr) <- liftIO $ do
      let sealspecpath = tmppath ++ "/sealspec.json"
      liftIO $ BS.writeFile sealspecpath (BS.fromString $ show $ pp_value (toJSValue config))
      readProcessWithExitCode' "java" ["-jar", "pdfsealjava/pdfseal.jar", sealspecpath] (BSL.empty)

    Log.debug $ "Sealing completed with " ++ show code
    case code of
      ExitSuccess -> do
        _ <- update $ InsertEvidenceEvent
            AttachSealedFileEvidence
            (return ())
            (Just documentid)
            (systemActor ctxtime)
        Just _sealedfileid <- digitallySealDocument True ctxtime ctxgtconf documentid tmpout filename
        triggerAPICallbackIfThereIsOne document -- Callback does not depend on sealed file
        res <- dbQuery $ GetDocumentByDocumentID documentid
        return $ (Right res)
      ExitFailure _ -> do
        -- error handling
        msg <- liftIO $ do
          systmp <- getTemporaryDirectory
          (path, handle) <- openTempFile systmp ("seal-failed-" ++ show documentid ++ "-" ++ show fileid ++ "-.pdf")
          let msg = "Cannot seal document #" ++ show documentid ++ " because of file #" ++ show fileid
          Log.error $ msg ++ ": " ++ path
          Log.error $ BSL.toString stderr
          Log.error $ "Sealing configuration: " ++ show config
          BS.hPutStr handle content
          hClose handle
          return msg
        _ <- dbUpdate $ ErrorDocument documentid ("Could not seal document because of file #" ++ show fileid) (systemActor ctxtime)
        triggerAPICallbackIfThereIsOne document
        return $ Left msg

digitallySealDocument :: (TemplatesMonad m, Applicative m, CryptoRNG m, MonadIO m, MonadDB m)
                      => Bool -> MinutesTime -> GuardTimeConf -> DocumentID -> FilePath -> String -> m (Maybe FileID)
digitallySealDocument forceAttach ctxtime ctxgtconf documentid pdfpath pdfname = do
  -- GuardTime signs in place
  code <- liftIO $ GT.digitallySign ctxgtconf pdfpath
  (newfilepdf, status) <- first Binary <$> case code of
    ExitSuccess -> do
      vr <- liftIO $ GT.verify ctxgtconf pdfpath
      case vr of
           GT.Valid gsig -> do
                res <- liftIO $ BS.readFile pdfpath
                Log.debug $ "GuardTime verification result: " ++ show vr
                Log.debug $ "GuardTime signed successfully #" ++ show documentid
                return (res, Guardtime (GT.extended gsig) (GT.privateGateway gsig))
           _ -> do
                res <- liftIO $ BS.readFile pdfpath
                Log.debug $ "GuardTime verification after signing failed for document #" ++ show documentid ++ ": " ++ show vr
                Log.error $ "GuardTime verification after signing failed for document #" ++ show documentid ++ ": " ++ show vr
                return (res, Missing)
    ExitFailure c -> do
      res <- liftIO $ BS.readFile pdfpath
      Log.debug $ "GuardTime failed " ++ show c ++ " of document #" ++ show documentid
      Log.error $ "GuardTime failed for document #" ++ show documentid
      return (res, Missing)
  if status /= Missing || forceAttach then do
    Log.debug $ "Adding new sealed file to DB"
    sealedfileid <- dbUpdate $ NewFile pdfname newfilepdf
    Log.debug $ "Finished adding sealed file to DB with fileid " ++ show sealedfileid ++ "; now adding to document"
    dbUpdate $ AppendFirstSealedFile documentid sealedfileid status $ systemActor ctxtime
    return (Just sealedfileid)
   else do
    return Nothing

digitallyExtendDocument :: (TemplatesMonad m, Applicative m, CryptoRNG m, MonadIO m, MonadDB m)
                      => MinutesTime -> GuardTimeConf -> DocumentID -> FilePath -> String -> m Bool
digitallyExtendDocument ctxtime ctxgtconf documentid pdfpath pdfname = do
  code <- liftIO $ GT.digitallyExtend ctxgtconf pdfpath
  mr <- case code of
    ExitSuccess -> do
      vr1 <- liftIO $ GT.verify ctxgtconf pdfpath
      vr <- case vr1 of
        GT.Invalid "SYNTACTIC_CHECK_FAILURE" -> do
          Log.debug "Verification failed with SYNTACTIC_CHECK_FAILURE - trying temporary Guardtime extension tool"
          _ <- liftIO $ GT.digitallyExtendCore1 pdfpath
          liftIO $ GT.verify ctxgtconf pdfpath
        _ -> return vr1
      case vr of
           GT.Valid gsig | GT.extended gsig -> do
                res <- liftIO $ BS.readFile pdfpath
                Log.debug $ "GuardTime verification result: " ++ show vr
                Log.debug $ "GuardTime extended successfully #" ++ show documentid
                return $ Just (res, Guardtime (GT.extended gsig) (GT.privateGateway gsig))
           _ -> do
                Log.error $ "GuardTime verification after extension failed for document #" ++ show documentid ++ ": " ++ show vr
                return Nothing
    ExitFailure c -> do
      Log.error $ "GuardTime failed " ++ show c ++ " for document #" ++ show documentid
      return Nothing
  case mr of
    Nothing -> return False
    Just (extendedfilepdf, status) -> do
      Log.debug $ "Adding new extended file to DB"
      sealedfileid <- dbUpdate $ NewFile pdfname (Binary extendedfilepdf)
      Log.debug $ "Finished adding extended file to DB with fileid " ++ show sealedfileid ++ "; now adding to document"
      dbUpdate $ AppendExtendedSealedFile documentid sealedfileid status $ systemActor ctxtime
      return True

-- | Generate file that has all placements printed on it. It will look same as final version except for footers and verification page.
presealDocumentFile :: (MonadBaseControl IO m, MonadDB m, KontraMonad m, TemplatesMonad m, MonadIO m, AWS.AmazonMonad m)
                 => Document
                 -> File
                 -> m (Either String BS.ByteString)
presealDocumentFile document@Document{documentid} file@File{fileid} =
  withSystemTempDirectory' ("preseal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    Log.debug ("presealing: " ++ show fileid)
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "public/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "public/img/checkbox_unchecked.jpg"
    emptyFieldsText <- emptyFieldsTextT
    let config = presealSpecFromDocument emptyFieldsText (checkedBoxImage,uncheckedBoxImage) document tmpin tmpout

    (code,_stdout,stderr) <- liftIO $ do
      let sealspecpath = tmppath ++ "/sealspec.json"
      liftIO $ BS.writeFile sealspecpath (BS.fromString $ show $ pp_value (toJSValue config))
      readProcessWithExitCode' "java" ["-jar", "pdfsealjava/pdfseal.jar", sealspecpath] (BSL.empty)
    Log.debug $ "PreSealing completed with " ++ show code
    case code of
      ExitSuccess -> do
          res <- liftIO $ BS.readFile tmpout
          Log.debug $ "Returning presealed content"
          return $ Right res
      ExitFailure _ -> do
          Log.error $ BSL.toString stderr
          Log.error $ "Presealing failed for configuration: " ++ show config
          return $ Left "Error when preprinting fields on PDF"

emptyFieldsTextT :: (TemplatesMonad m) => m [(FieldType,String)]
emptyFieldsTextT = do
  fstname <- renderTemplate_ "_firstname"
  sndname <- renderTemplate_ "_lastname"
  email <- renderTemplate_ "_email"
  company <- renderTemplate_ "_company"
  companynumber <- renderTemplate_ "_orgnumber"
  personalnumber <- renderTemplate_  "_personnumber"
  return [(FirstNameFT, fstname),
          (LastNameFT, sndname),
          (CompanyFT, company),
          (PersonalNumberFT, personalnumber),
          (CompanyNumberFT,companynumber),
          (EmailFT, email)]
