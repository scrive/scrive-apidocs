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

import Control.Monad.Base
import Control.Monad.Catch hiding (handle)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Char
import Data.Function (on)
import Data.Time
import Log
import System.Directory
import System.Exit
import System.FilePath (takeFileName, takeExtension, (</>))
import System.IO hiding (stderr)
import Text.HTML.TagSoup (Tag(..), parseTags)
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL (empty, writeFile)
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Unjson as Unjson
import qualified Text.StringTemplates.Fields as F

import Control.Logic
import Crypto.RNG
import DB
import DB.TimeZoneName
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils
import Doc.DocView
import Doc.Model
import Doc.Rendering
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryIdentification (SignatoryIdentifierMap, siInitials, siLink, signatoryIdentifier)
import EvidenceLog.Model
import EvidenceLog.View
import File.File
import File.Model
import File.Storage
import Kontra
import KontraPrelude
import MinutesTime
import Templates
import Util.Actor
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Directory
import Utils.IO
import Utils.Prelude
import Utils.Read
import qualified Amazon as AWS
import qualified Doc.SealSpec as Seal
import qualified HostClock.Model as HC

personFromSignatory :: (MonadDB m, MonadMask m, TemplatesMonad m, AWS.AmazonMonad m, MonadLog m, MonadBase IO m)
                    => TimeZoneName -> SignatoryIdentifierMap -> (BS.ByteString,BS.ByteString) -> SignatoryLink -> m Seal.Person
personFromSignatory tz sim boxImages signatory = do
    emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
    stime <- case  maybesigninfo signatory of
                  Nothing -> return ""
                  Just si -> formatUTCTimeForVerificationPage tz $ signtime si
    signedAtText <- if (null stime)
                       then return ""
                    else renderTemplate "_contractsealingtextssignedAtText" $ F.value "time" stime
    let personalnumber = getPersonalNumber signatory
        companynumber = getCompanyNumber signatory
    companyNumberText <- if (not (null companynumber))
                            then renderTemplate "_contractsealingtextsorgNumberText" $ F.value "companynumber" companynumber
                         else return ""
    personalNumberText <- if (not (null personalnumber))
                            then renderTemplate "_contractsealingtextspersonalNumberText" $ F.value "idnumber" personalnumber
                         else return ""
    fields <- fieldsFromSignatory boxImages signatory
    return $ Seal.Person { Seal.fullname           = fromMaybe "" $ signatoryIdentifier sim (signatorylinkid signatory) emptyNamePlaceholder
                         , Seal.company            = getCompanyName signatory
                         , Seal.email              = getEmail signatory
                         , Seal.phone              = getMobile signatory
                         , Seal.personalnumber     = getPersonalNumber signatory
                         , Seal.companynumber      = getCompanyNumber signatory
                         , Seal.fullnameverified   = False
                         , Seal.companyverified    = False
                         , Seal.numberverified     = False
                         , Seal.emailverified      = True
                         , Seal.phoneverified      = False
                         , Seal.fields             = fields
                         , Seal.signtime           = stime
                         , Seal.signedAtText       = signedAtText
                         , Seal.personalNumberText = personalNumberText
                         , Seal.companyNumberText  = companyNumberText
                         }

personExFromSignatoryLink :: (MonadDB m, MonadMask m, TemplatesMonad m, AWS.AmazonMonad m, MonadLog m, MonadBase IO m)
                          =>  TimeZoneName -> SignatoryIdentifierMap -> (BS.ByteString,BS.ByteString) -> SignatoryLink -> m Seal.Person
personExFromSignatoryLink tz sim boxImages sl@SignatoryLink{..} = do
  person <- personFromSignatory tz sim boxImages sl
  return person {
       Seal.emailverified    = signatorylinkdeliverymethod == EmailDelivery
     , Seal.fullnameverified = False
     , Seal.companyverified  = False
     , Seal.numberverified   = True
     , Seal.phoneverified    = (signatorylinkdeliverymethod == MobileDelivery) || (signatorylinkauthenticationmethod == SMSPinAuthentication)
     }

fieldsFromSignatory :: (MonadDB m, MonadThrow m, MonadLog m, MonadBase IO m, AWS.AmazonMonad m) => (BS.ByteString,BS.ByteString) -> SignatoryLink -> m [Seal.Field]
fieldsFromSignatory (checkedBoxImage,uncheckedBoxImage) SignatoryLink{signatoryfields} =
  silenceJPEGFieldsFromFirstSignature <$> concat <$> mapM makeSealField signatoryfields
  where
    silenceJPEGFieldsToTheEnd [] = []
    silenceJPEGFieldsToTheEnd (field@(Seal.FieldJPG{}):xs) = (field { Seal.includeInSummary = False }) : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsToTheEnd (x:xs) = x : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsFromFirstSignature [] = []
    silenceJPEGFieldsFromFirstSignature (field@(Seal.FieldJPG{Seal.includeInSummary = True}):xs) =
      field : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsFromFirstSignature (x:xs) = x : silenceJPEGFieldsFromFirstSignature xs

    makeSealField :: (MonadDB m, MonadThrow m, MonadLog m, MonadBase IO m, AWS.AmazonMonad m) => SignatoryField -> m [Seal.Field]
    makeSealField sf = case sf of
       SignatorySignatureField ssf -> case (fieldPlacements  sf, ssfValue ssf) of
                           (_, Nothing) -> return []  -- We skip signature that don't have a drawing
                           ([],Just f) -> (\x -> [x]) <$> fieldJPEGFromSignatureField f
                           (_, Just f) -> mapM (fieldJPEGFromPlacement f) (fieldPlacements  sf)
       SignatoryCheckboxField schf -> return $ map (uncheckedImageFromPlacement <| not (schfValue schf) |>  checkedImageFromPlacement) (fieldPlacements  sf)
       _ -> return $ for (fieldPlacements sf) $ fieldFromPlacement False sf
    fieldFromPlacement greyed sf placement =
      Seal.Field { Seal.value            = fromMaybe "" $ fieldTextValue sf
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
                 { valueBinary           = image
                 , Seal.x                = placementxrel placement
                 , Seal.y                = placementyrel placement
                 , Seal.page             = placementpage placement
                 , Seal.image_w          = placementwrel placement
                 , Seal.image_h          = placementhrel placement
                 , Seal.includeInSummary = False
                 , Seal.onlyForSummary   = False
                 , Seal.keyColor         = Nothing
                 }
    fieldJPEGFromPlacement f placement = do
          content  <- getFileIDContents f
          return $ Seal.FieldJPG
                 { valueBinary           = content
                 , Seal.x                = placementxrel placement
                 , Seal.y                = placementyrel placement
                 , Seal.page             = placementpage placement
                 , Seal.image_w          = placementwrel placement
                 , Seal.image_h          = placementhrel placement
                 , Seal.includeInSummary = True
                 , Seal.onlyForSummary   = False
                 , Seal.keyColor         = Just (255,255,255) -- white is transparent
                 }
    fieldJPEGFromSignatureField f = do
          content  <- getFileIDContents f
          return $ Seal.FieldJPG
                 { valueBinary           = content
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


findOutAttachmentDesc :: (MonadIO m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m, AWS.AmazonMonad m, MonadBaseControl IO m)
                      => SignatoryIdentifierMap -> String -> Document -> m [Seal.FileDesc]

findOutAttachmentDesc sim tmppath document = do
  a <- mapM findAttachmentsForAuthorAttachment authorAttsNumbered
  b <- mapM findAttachmentsForSignatoryAttachment attAndSigsNumbered
  return (a ++ b)
  where
      attAndSigs = listAttachmentsFromDocument document
      authorAtts = documentauthorattachments document
      authorAttsNumbered = zip [1::Int ..] authorAtts
      attAndSigsNumbered = zipWith (\num (at,sl) -> (num,at,sl)) [(length authorAtts + 1) ..] attAndSigs

      Just asl = getAuthorSigLink document

      findAttachmentsForAuthorAttachment (num, authorattach) =
        findAttachments (Just (authorattachmentfile authorattach)) num asl removeExtIfAny

      findAttachmentsForSignatoryAttachment (num, sigattach, sl) =
        findAttachments (signatoryattachmentfile sigattach) num sl (const (signatoryattachmentname sigattach))

      findAttachments mfileid num sl titlef = do
        (contents,numberOfPages,name) <- case mfileid of
          Nothing -> return (BS.empty, 1, "")
          Just fileid' -> do
            contents <- getFileIDContents fileid'
            file <- dbQuery $ GetFileByFileID fileid'
            eNumberOfPages <- liftIO $ getNumberOfPDFPages contents
            numberOfPages <- case eNumberOfPages of
                              Left e -> do
                                logAttention_ $ "Calculating number of pages of document #" ++ show (documentid document) ++ " failed, falling back to 1. Reason: " ++ show e
                                return 1
                              Right x -> return x
            return (contents, numberOfPages, filename file)
        numberOfPagesText <-
          if (".png" `isSuffixOf` (map toLower name) || ".jpg" `isSuffixOf` (map toLower name) )
           then return ""
           else if numberOfPages==1
                  then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
                  else renderLocalTemplate document "_numberOfPages" $ do
                    F.value "pages" numberOfPages

        attachedByText <- do
          emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
          renderLocalTemplate document "_documentAttachedBy" $ do
            F.value "identifier" $ signatoryIdentifier sim (signatorylinkid sl) emptyNamePlaceholder

        attachmentNumText <- renderLocalTemplate document "_attachedDocument" $ do
                                     F.value "number" num
        let attachmentPath = tmppath </> attachmentNumText ++ takeExtension name
        liftIO $ BS.writeFile attachmentPath contents

        return $ Seal.FileDesc
                 { fileTitle      = titlef name
                 , fileRole       = attachmentNumText
                 , filePagesText  = numberOfPagesText
                 , fileAttachedBy = attachedByText
                 , fileInput      = Just attachmentPath
                 }

      removeExtIfAny fname = case dropWhile (/= '.') (reverse fname) of
                               "" -> fname
                               x -> reverse (drop 1 x)


evidenceOfIntentAttachment :: (TemplatesMonad m, MonadDB m, MonadThrow m, MonadLog m, MonadBase IO m, AWS.AmazonMonad m)
                           => SignatoryIdentifierMap -> Document -> m Seal.SealAttachment
evidenceOfIntentAttachment sim doc = do
  let title = documenttitle doc
  let sls = documentsignatorylinks doc
  ss <- dbQuery $ GetSignatoryScreenshots (map signatorylinkid sls)
  let sortBySignTime = sortBy (on compare (fmap signtime . maybesigninfo . fst))
  html <- evidenceOfIntentHTML sim title $ sortBySignTime [ (sl, s) | (i, s) <- ss, sl <- filter ((==i) . signatorylinkid) sls ]
  return $ Seal.SealAttachment { Seal.fileName = "Evidence_of_Intent.html"
                               , Seal.mimeType = Nothing
                               , Seal.fileContent = BS.fromString html
                               }

{-
 formatCalendarTime does not support %z as modifier. We have to implement it ourselves here.
-}
formatUTCTimeForVerificationPage :: (MonadDB m, MonadMask m) => TimeZoneName -> UTCTime -> m String
formatUTCTimeForVerificationPage tz mt = withTimeZone tz $ do
  runQuery_ $ rawSQL "SELECT $1, to_char($1, 'TZ')" (Identity mt)
  (t::ZonedTime, tmz) <- fetchOne id
  return $ formatTime' ("%Y-%m-%d %H:%M:%S" <+> tmz <+> "(%z)") t

createSealingTextsForDocument :: (TemplatesMonad m) => Document -> String -> m Seal.SealingTexts
createSealingTextsForDocument document hostpart = do

  let render templ = renderLocalTemplate document templ $ do
        documentInfoFields document
        F.value "hostpart" hostpart
        F.value "verifyurl" ("https://scrive.com/verify"::String)

  verificationTitle' <- render "_contractsealingtexts"
  partnerText' <- render "_contractsealingtextspartnerText"
  initiatorText' <- render "_contractsealingtextsinitiatorText"
  documentText' <- render "_documentText"
  eventsText' <- render "_contractsealingtextseventsText"
  dateText' <- render "_contractsealingtextsdateText"
  historyText' <- render "_contractsealingtextshistoryText"
  verificationFooter' <- render "_verificationFooter"
  hiddenAttachmentText' <- render "_contractsealingtextshiddenAttachment"
  onePageText' <- render "_numberOfPagesIs1"

  let sealingTexts = Seal.SealingTexts
        { verificationTitle    = verificationTitle'
        , partnerText          = partnerText'
        , initiatorText        = initiatorText'
        , documentText         = documentText'
        , eventsText           = eventsText'
        , dateText             = dateText'
        , historyText          = historyText'
        , verificationFooter   = verificationFooter'
        , hiddenAttachmentText = hiddenAttachmentText'
        , onePageText          = onePageText'
        }

  return sealingTexts

sealSpecFromDocument :: (MonadIO m, TemplatesMonad m, MonadDB m, MonadMask m, MonadLog m, AWS.AmazonMonad m, MonadBaseControl IO m)
                     => (BS.ByteString,BS.ByteString)
                     -> String
                     -> Document
                     -> [DocumentEvidenceEvent]
                     -> HC.ClockErrorStatistics
                     -> BS.ByteString
                     -> String
                     -> String
                     -> String
                     -> m Seal.SealSpec
sealSpecFromDocument boxImages hostpart document elog ces content tmppath inputpath outputpath = do
  -- Keep only simple events and remove events induced by resealing
  let velog = filter eventForVerificationPage $ eventsForLog elog
  -- Form initials from signing parties
  sim <- getSignatoryIdentifierMap False $ filter eventForVerificationPage velog

  -- Remove events generated by non-signing parties
  -- FIXME: Should we include events for deleted documents?
  let viewingParty e = viewer (evAffectedSigLink e) || viewer (evSigLink e)
           where viewer s = ((signatoryisauthor ||^ signatoryispartner) <$>
                             (s >>= flip Map.lookup sim >>= siLink))
                         == Just False
      spelog = filter (not . viewingParty) velog

  additionalAttachments <- findOutAttachmentDesc sim tmppath document
  docs <- mapM (\f -> (takeFileName f,) <$> liftIO (BS.readFile f))
            [ "files/Evidence_Documentation.html"
            , "files/Digital_Signature_Documentation_Part_I.html"
            , "files/Digital_Signature_Documentation_Part_II.html"
            ]

  let docid = documentid document
      paddeddocid = pad0 20 (show docid)
      Just authorsiglink = getAuthorSigLink document
      removeTags s = concat [t | TagText t <- parseTags s]
      mkHistEntry ev = do
        actor <- approximateActor EventForVerificationPages document sim ev
        comment <- removeTags <$> simplyfiedEventText EventForVerificationPages (Just actor) document sim ev
        etime <- formatUTCTimeForVerificationPage (documenttimezonename document) (evTime ev)
        return $ Seal.HistEntry{ Seal.histdate = etime
                               , Seal.histcomment = comment
                               , Seal.histaddress = maybe "" show $ evIP4 ev
                               }

  persons <- sequence $ [ personExFromSignatoryLink (documenttimezonename document) sim boxImages s
                    | s <- documentsignatorylinks document
                    , signatoryispartner $ s
                ]

  secretaries <- sequence $ [ personFromSignatory (documenttimezonename document) sim boxImages $ s
                    | s <- documentsignatorylinks document
                    , not . signatoryispartner $ s
                ]

  initiator <- if (signatoryispartner authorsiglink)
                then return Nothing
                else Just <$> (personFromSignatory (documenttimezonename document) sim boxImages authorsiglink)

  let initials = intercalate ", " $ catMaybes
                   [ siInitials <$> Map.lookup (signatorylinkid s) sim
                   | s <- documentsignatorylinks document
                   , signatoryispartner s
                   ]

  -- logInfo_ "Creating seal spec from file."

  history <- mapM mkHistEntry spelog

  staticTexts <- createSealingTextsForDocument document hostpart

  -- Creating HTML Evidence Log
  let htmlevents = suppressRepeatedEvents elog
  elogsim <- getSignatoryIdentifierMap True htmlevents
  htmllogs <- htmlDocFromEvidenceLog (documenttitle document) elogsim htmlevents ces
  let evidenceattachment = Seal.SealAttachment { Seal.fileName = "Evidence_Log.html"
                                               , Seal.mimeType = Nothing
                                               , Seal.fileContent = BS.fromString htmllogs }
  evidenceOfIntent <- evidenceOfIntentAttachment elogsim document

  -- documentation files
  let docAttachments =
        [ Seal.SealAttachment { Seal.fileName = name
                              , Seal.mimeType = Nothing
                              , Seal.fileContent = doc
                              }
        | (name, doc) <- docs ]

  eNumberOfPages <- liftIO $ getNumberOfPDFPages content
  numberOfPages <- case eNumberOfPages of
                        Left e -> do
                          logAttention_ $ "Calculating number of pages of document #" ++ show (documentid document) ++ " failed, falling back to 1. Reason: " ++ show e
                          return 1
                        Right x -> return x

  numberOfPagesText <-
    if numberOfPages==1
       then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
       else renderLocalTemplate document "_numberOfPages" $ do
         F.value "pages" numberOfPages

  attachedByText <- do
    emptyNamePlaceholder <- renderTemplate_ "_notNamedParty"
    renderLocalTemplate document "_documentSentBy" $ do
      F.value "identifier" $ signatoryIdentifier sim (signatorylinkid authorsiglink) emptyNamePlaceholder

  mainDocumentText <- renderLocalTemplate document "_mainDocument"
                      $ (return ())

  documentNumberText <- renderLocalTemplate document "_contractsealingtextsDocumentNumber" $ do
    F.value "documentnumber" $ paddeddocid

  initialsText <- renderLocalTemplate document "_contractsealingtextsInitialsText" $ do
    F.value "initials" $ initials

  return $ Seal.SealSpec
        { Seal.input          = inputpath
        , Seal.output         = outputpath
        , Seal.documentNumberText = documentNumberText
        , Seal.persons        = persons
        , Seal.secretaries    = secretaries
        , Seal.initiator      = initiator
        , Seal.history        = history
        , Seal.initialsText       = initialsText
        , Seal.hostpart       = hostpart
        , Seal.staticTexts    = staticTexts
        , Seal.attachments    = docAttachments ++ [evidenceattachment, evidenceOfIntent]
        , Seal.filesList      =
          [ Seal.FileDesc { fileTitle = documenttitle document
                          , fileRole = mainDocumentText
                          , filePagesText = numberOfPagesText
                          , fileAttachedBy = attachedByText
                          , fileInput = Nothing
                          } ] ++ additionalAttachments
        }

presealSpecFromDocument :: (MonadIO m, TemplatesMonad m, MonadDB m, MonadMask m, MonadLog m, AWS.AmazonMonad m, MonadBaseControl IO m)
                           => (BS.ByteString,BS.ByteString)
                           -> Document
                           -> String
                           -> String
                           -> m Seal.PreSealSpec
presealSpecFromDocument boxImages document inputpath outputpath = do
       fields <- concat <$> mapM (fieldsFromSignatory boxImages) (documentsignatorylinks document)
       return $ Seal.PreSealSpec
            { Seal.pssInput          = inputpath
            , Seal.pssOutput         = outputpath
            , Seal.pssFields         = fields
            }

sealDocument :: (CryptoRNG m, MonadBaseControl IO m, DocumentMonad m, TemplatesMonad m, MonadIO m, MonadMask m, MonadLog m, AWS.AmazonMonad m) => String -> m ()
sealDocument hostpart = theDocumentID >>= \did -> do
  mfile <- theDocument >>= documentfileM
  case mfile of
    Just file -> do
      logInfo_ $ "Sealing document #" ++ show did
      sealDocumentFile hostpart file
      logInfo_ $ "Sealing of document #" ++ show did ++ " should be done now"
    Nothing -> do
      logInfo_ $ "Sealing of document #" ++ show did ++ " failed because it has no main file attached"
      internalError

collectClockErrorStatistics :: (MonadDB m, MonadThrow m) => [DocumentEvidenceEvent] -> m HC.ClockErrorStatistics
collectClockErrorStatistics [] = return $ HC.ClockErrorStatistics Nothing Nothing Nothing 0 0
collectClockErrorStatistics elog = do
  let endtime   = $maximum (map evTime elog)
      starttime = $minimum (map evTime elog) `min` (1 `daysBefore` endtime)
  dbQuery $ HC.GetClockErrorStatistics (Just starttime) (Just endtime)

sealDocumentFile :: (CryptoRNG m, MonadMask m, MonadBaseControl IO m, DocumentMonad m, TemplatesMonad m, MonadIO m, MonadLog m, AWS.AmazonMonad m)
                 => String
                 -> File
                 -> m ()
sealDocumentFile hostpart file@File{fileid, filename} = theDocumentID >>= \documentid ->
  withSystemTempDirectory' ("seal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    now <- currentTime
    -- We generate this event before we attempt the sealing process so
    -- that the event gets included in the evidence package
    _ <- update $ InsertEvidenceEvent
        AttachSealedFileEvidence
        (return ())
        (systemActor now)
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "frontend/app/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "frontend/app/img/checkbox_unchecked.jpg"
    elog <- dbQuery $ GetEvidenceLog documentid
    ces <- collectClockErrorStatistics elog
    config <- theDocument >>= \d -> sealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) hostpart d elog ces content tmppath tmpin tmpout

    let json_config = Unjson.unjsonToByteStringLazy Seal.unjsonSealSpec config
    (code,_stdout,stderr) <- liftIO $ do
      let sealspecpath = tmppath ++ "/sealspec.json"
      liftIO $ BSL.writeFile sealspecpath json_config
      readProcessWithExitCode' "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "add-verification-pages", sealspecpath] (BSL.empty)

    logInfo_ $ "Sealing completed with " ++ show code
    case code of
      ExitSuccess -> do
        sealedfileid <- dbUpdate . NewFile filename . Binary =<< liftIO (BS.readFile tmpout)
        dbUpdate $ AppendSealedFile sealedfileid Missing (systemActor now)
      ExitFailure _ -> do
        systmp <- liftIO $ getTemporaryDirectory
        (path, handle) <- liftIO $ openTempFile systmp ("seal-failed-" ++ show documentid ++ "-" ++ show fileid ++ "-.pdf")
        let msg = "Cannot seal document #" ++ show documentid ++ " because of file #" ++ show fileid
        logAttention_ $ msg ++ ": " ++ path
        logAttention_ $ BSL.toString stderr
        -- show JSON'd config as that's what the java app is fed.
        liftIO $ BS.hPutStr handle content
        liftIO $ hClose handle
        void $ dbUpdate $ ErrorDocument ("Could not seal document because of file #" ++ show fileid)
                            ErrorSealingDocumentEvidence
                            (return ())
                            (systemActor now)

-- | Generate file that has all placements printed on it. It will look same as final version except for footers and verification page.
presealDocumentFile :: (MonadBaseControl IO m, MonadDB m, MonadLog m, KontraMonad m, TemplatesMonad m, MonadIO m, MonadMask m, AWS.AmazonMonad m)
                 => Document
                 -> File
                 -> m (Either String BS.ByteString)
presealDocumentFile document@Document{documentid} file@File{fileid} =
  withSystemTempDirectory' ("preseal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    logInfo_ ("presealing: " ++ show fileid)
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "frontend/app/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "frontend/app/img/checkbox_unchecked.jpg"
    config <- presealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) document tmpin tmpout

    let json_config = Unjson.unjsonToByteStringLazy Seal.unjsonPreSealSpec config
    (code,_stdout,stderr) <- liftIO $ do
      let sealspecpath = tmppath ++ "/sealspec.json"
      liftIO $ BSL.writeFile sealspecpath json_config
      readProcessWithExitCode' "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "add-verification-pages", sealspecpath] (BSL.empty)
    logInfo_ $ "PreSealing completed with " ++ show code
    case code of
      ExitSuccess -> do
          res <- liftIO $ BS.readFile tmpout
          logInfo_ $ "Returning presealed content"
          return $ Right res
      ExitFailure _ -> do
          logAttention_ $ BSL.toString stderr
          -- show JSON'd config as that's what the java app is fed.
          return $ Left "Error when preprinting fields on PDF"
