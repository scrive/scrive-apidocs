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
  ) where

import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Function (on)
import Data.Maybe
import Data.List
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.Model
import Doc.Rendering
import Doc.SealStatus (SealStatus(..))
import File.Storage
import File.File
import Doc.DocView
import Doc.DocUtils
import qualified HostClock.Model as HC
import MailContext (MailContext(..), MailContextMonad, getMailContext)
import MinutesTime (MinutesTime, daysBefore, toCalendarTime)
import Utils.Directory
import Utils.Read
import Utils.IO
import System.Directory
import System.Exit
import Kontra
import Text.HTML.TagSoup (Tag(..), parseTags)
import Text.StringTemplates.Templates
import Text.Printf
import Text.JSON.Gen
import Text.JSON.Pretty (pp_value)
import System.FilePath (takeFileName)
import System.Time
import System.Locale
import Templates
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.Lazy as BSL (empty)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Base64 as B64
import qualified Doc.SealSpec as Seal
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
import EvidenceLog.Model
import EvidenceLog.View
import Util.Actor
import qualified Text.StringTemplates.Fields as F
import Control.Logic
import Utils.Prelude
import qualified Amazon as AWS
import Data.Char

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
                , Seal.fields = fieldsFromSignatory boxImages signatory
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

fieldsFromSignatory :: (BS.ByteString,BS.ByteString) -> SignatoryLink -> [Seal.Field]
fieldsFromSignatory (checkedBoxImage,uncheckedBoxImage) SignatoryLink{signatoryfields} =
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
       _ -> for (sfPlacements sf) $ fieldFromPlacement False (sfValue sf)
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

findOutAttachmentDesc :: (MonadIO m, MonadDB m, Log.MonadLog m, TemplatesMonad m, AWS.AmazonMonad m) => Document -> m [Seal.FileDesc]
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
          if (".png" `isSuffixOf` (map toLower name) || ".jpg" `isSuffixOf` (map toLower name) )
           then return ""
           else if numberOfPages==1
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
        (numberOfPages,name) <- case signatoryattachmentfile sigattach of
                       Nothing -> return (1,"")
                       Just fileid' -> do
                                   file <- dbQuery $ GetFileByFileID fileid'
                                   contents <- getFileIDContents fileid'
                                   return $ (getNumberOfPDFPages contents,filename file)
        numberOfPagesText <-
          if (".png" `isSuffixOf` (map toLower name) || ".jpg" `isSuffixOf` (map toLower name) )
           then return ""
           else if numberOfPages==1
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

evidenceOfIntentAttachment :: (TemplatesMonad m, MonadDB m, Log.MonadLog m, MonadIO m, AWS.AmazonMonad m) => String -> [SignatoryLink] -> m Seal.SealAttachment
evidenceOfIntentAttachment title sls = do
  ss <- dbQuery $ GetSignatoryScreenshots (map signatorylinkid sls)
  let sortBySignTime = sortBy (on compare (fmap signtime . maybesigninfo . fst))
  html <- evidenceOfIntentHTML title $ sortBySignTime [ (sl, s) | (i, s) <- ss, sl <- filter ((==i) . signatorylinkid) sls ]
  return $ Seal.SealAttachment { Seal.fileName = "Evidence_of_Intent.html"
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

sealSpecFromDocument :: (MonadIO m, TemplatesMonad m, MonadDB m, Log.MonadLog m, AWS.AmazonMonad m)
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
  docs <- mapM (\f -> ((takeFileName f,) . BS.toString . B64.encode) <$> liftIO (BS.readFile f))
            [ "files/Evidence_Documentation.html"
            , "files/Digital_Signature_Documentation_Part_I.html"
            , "files/Digital_Signature_Documentation_Part_II.html"
            ]

  sealSpecFromDocument2 boxImages hostpart document elog ces content inputpath outputpath additionalAttachments docs

sealSpecFromDocument2 :: (TemplatesMonad m, MonadDB m, Log.MonadLog m, MonadIO m, AWS.AmazonMonad m)
                     => (BS.ByteString,BS.ByteString)
                     -> String
                     -> Document
                     -> [DocumentEvidenceEvent]
                     -> HC.ClockErrorStatistics
                     -> BS.ByteString
                     -> String
                     -> String
                     -> [Seal.FileDesc]
                     -> [(String, String)]
                     -> m Seal.SealSpec
sealSpecFromDocument2 boxImages hostpart document elog ces content inputpath outputpath additionalAttachments docs =
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

      removeTags s = concat [t | TagText t <- parseTags s]

      mkHistEntry ev = do
        actor <- approximateActor document ev
        comment <- removeTags <$> simplyfiedEventText (Just actor) document ev
        return $ Seal.HistEntry{ Seal.histdate = formatMinutesTimeForVerificationPage $ evTime ev
                               , Seal.histcomment = comment
                               , Seal.histaddress = maybe "" show $ evIP4 ev
                               }

  in do
      -- Log.mixlog_ "Creating seal spec from file."
      -- Remove events induced by resealing and non-signing party activities
      let eventsForHistory = filter eventForVerificationPage . filter (not . viewingParty)
          viewingParty e = viewer (evAffectedSigLink e) || viewer (evSigLink e)
               where viewer s = (signatoryispartner <$> s) == Just False
      history <- (mapM mkHistEntry . eventsForHistory) =<< getSignatoryLinks (eventsForLog elog)
      -- Log.mixlog_ ("about to render staticTexts")
      staticTexts <- renderLocalTemplate document "contractsealingtexts" $ do
                        documentInfoFields document
                        F.value "hostpart" hostpart
      -- Log.mixlog_ ("finished staticTexts: " ++ show staticTexts)
      readtexts <- case maybeRead staticTexts of
                     Just x -> return x
                     Nothing -> do
                       --Log.mixlog_ $ "Cannot read SealingTexts: " ++ staticTexts
                       error $ "Cannot read SealingTexts: " ++ staticTexts
      -- Log.mixlog_ ("read texts: " ++ show readtexts)

      -- Creating HTML Evidence Log
      htmllogs <- htmlDocFromEvidenceLog (documenttitle document) (suppressRepeatedEvents elog) ces
      let evidenceattachment = Seal.SealAttachment { Seal.fileName = "Evidence_Log.html"
                                                   , Seal.mimeType = Nothing
                                                   , Seal.fileBase64Content = BS.toString $ B64.encode $ BS.fromString htmllogs }
      evidenceOfIntent <- evidenceOfIntentAttachment (documenttitle document) (documentsignatorylinks document)

      -- documentation files
      let docAttachments =
            [ Seal.SealAttachment { Seal.fileName = name
                                  , Seal.mimeType = Nothing
                                  , Seal.fileBase64Content = doc
                                  }
            | (name, doc) <- docs ]

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
            , Seal.attachments    = docAttachments ++ [evidenceattachment, evidenceOfIntent]
            , Seal.filesList      =
              [ Seal.FileDesc { fileTitle = documenttitle document
                              , fileRole = mainDocumentText
                              , filePagesText = numberOfPagesText
                              , fileAttachedBy = attachedByText
                              } ] ++ additionalAttachments
            }

presealSpecFromDocument :: (BS.ByteString,BS.ByteString) -> Document -> String -> String -> Seal.PreSealSpec
presealSpecFromDocument boxImages document inputpath outputpath =
       Seal.PreSealSpec
            { Seal.pssInput          = inputpath
            , Seal.pssOutput         = outputpath
            , Seal.pssFields         = concatMap (fieldsFromSignatory boxImages) (documentsignatorylinks document)
            }


sealDocument :: (CryptoRNG m, MonadBaseControl IO m, MailContextMonad m, DocumentMonad m, TemplatesMonad m, MonadIO m, Log.MonadLog m, AWS.AmazonMonad m) => m ()
sealDocument = theDocumentID >>= \did -> do
  mfile <- theDocument >>= documentfileM
  case mfile of
    Just file -> do
      Log.mixlog_ $ "Sealing document #" ++ show did
      sealDocumentFile file
      Log.mixlog_ $ "Sealing of document #" ++ show did ++ " should be done now"
    Nothing -> do
      Log.mixlog_ $ "Sealing of document #" ++ show did ++ " failed because it has no main file attached"
      internalError

collectClockErrorStatistics :: MonadDB m => [DocumentEvidenceEvent] -> m HC.ClockErrorStatistics
collectClockErrorStatistics [] = return $ HC.ClockErrorStatistics Nothing Nothing Nothing 0 0
collectClockErrorStatistics elog = do
  let endtime   = maximum (map evTime elog)
      starttime = minimum (map evTime elog) `min` (1 `daysBefore` endtime)
  dbQuery $ HC.GetClockErrorStatistics (Just starttime) (Just endtime)

sealDocumentFile :: (CryptoRNG m, MonadBaseControl IO m, MailContextMonad m, DocumentMonad m, TemplatesMonad m, MonadIO m, Log.MonadLog m, AWS.AmazonMonad m)
                 => File
                 -> m ()
sealDocumentFile file@File{fileid, filename} = theDocumentID >>= \documentid ->
  withSystemTempDirectory' ("seal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    MailContext{mctxhostpart, mctxtime} <- getMailContext
    -- We generate this event before we attempt the sealing process so
    -- that the event gets included in the evidence package
    _ <- update $ InsertEvidenceEvent
        AttachSealedFileEvidence
        (return ())
        (systemActor mctxtime)
        documentid
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "frontend/app/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "frontend/app/img/checkbox_unchecked.jpg"
    elog <- dbQuery $ GetEvidenceLog documentid
    ces <- collectClockErrorStatistics elog
    config <- theDocument >>= \d -> sealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) mctxhostpart d elog ces content tmpin tmpout

    (code,_stdout,stderr) <- liftIO $ do
      let sealspecpath = tmppath ++ "/sealspec.json"
      liftIO $ BS.writeFile sealspecpath (BS.fromString $ show $ pp_value (toJSValue config))
      readProcessWithExitCode' "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "add-verification-pages", sealspecpath] (BSL.empty)

    Log.mixlog_ $ "Sealing completed with " ++ show code
    case code of
      ExitSuccess -> do
        sealedfileid <- dbUpdate . NewFile filename . Binary =<< liftIO (BS.readFile tmpout)
        dbUpdate $ AppendSealedFile sealedfileid Missing (systemActor mctxtime)
      ExitFailure _ -> do
        -- error handling
        liftIO $ do
          systmp <- getTemporaryDirectory
          (path, handle) <- openTempFile systmp ("seal-failed-" ++ show documentid ++ "-" ++ show fileid ++ "-.pdf")
          let msg = "Cannot seal document #" ++ show documentid ++ " because of file #" ++ show fileid
          Log.attention_ $ msg ++ ": " ++ path
          Log.attention_ $ BSL.toString stderr
          Log.attention_ $ "Sealing configuration: " ++ show config
          BS.hPutStr handle content
          hClose handle
        void $ dbUpdate $ ErrorDocument ("Could not seal document because of file #" ++ show fileid)
                            ErrorSealingDocumentEvidence
                            (return ())
                            (systemActor mctxtime)

-- | Generate file that has all placements printed on it. It will look same as final version except for footers and verification page.
presealDocumentFile :: (MonadBaseControl IO m, MonadDB m, Log.MonadLog m, KontraMonad m, TemplatesMonad m, MonadIO m, AWS.AmazonMonad m)
                 => Document
                 -> File
                 -> m (Either String BS.ByteString)
presealDocumentFile document@Document{documentid} file@File{fileid} =
  withSystemTempDirectory' ("preseal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    Log.mixlog_ ("presealing: " ++ show fileid)
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "frontend/app/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "frontend/app/img/checkbox_unchecked.jpg"
    let config = presealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) document tmpin tmpout

    (code,_stdout,stderr) <- liftIO $ do
      let sealspecpath = tmppath ++ "/sealspec.json"
      liftIO $ BS.writeFile sealspecpath (BS.fromString $ show $ pp_value (toJSValue config))
      readProcessWithExitCode' "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "add-verification-pages", sealspecpath] (BSL.empty)
    Log.mixlog_ $ "PreSealing completed with " ++ show code
    case code of
      ExitSuccess -> do
          res <- liftIO $ BS.readFile tmpout
          Log.mixlog_ $ "Returning presealed content"
          return $ Right res
      ExitFailure _ -> do
          Log.attention_ $ BSL.toString stderr
          Log.attention_ $ "Presealing failed for configuration: " ++ show config
          return $ Left "Error when preprinting fields on PDF"
