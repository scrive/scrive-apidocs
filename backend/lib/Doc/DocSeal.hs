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

import Control.Conditional ((<|), (|>))
import Control.Monad.Catch hiding (handle)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Char
import Data.Digest.SHA2
import Data.Function (on)
import Data.Time
import Log
import System.Directory
import System.Exit
import System.FilePath (takeFileName, takeExtension, (</>))
import System.IO hiding (stderr)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Text.HTML.TagSoup (Tag(..), parseTags)
import Text.StringTemplates.Templates
import qualified Data.ByteString as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL (empty, writeFile)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Unjson as Unjson
import qualified Text.StringTemplates.Fields as F

import Crypto.RNG
import DB
import DB.TimeZoneName
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils
import Doc.DocView
import Doc.Logging
import Doc.Model
import Doc.Rendering
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryIdentification (SignatoryIdentifierMap, siInitials, siLink, signatoryIdentifier)
import EvidenceLog.Model
import EvidenceLog.View
import EvidencePackage.EvidenceLog
import EvidencePackage.EvidenceOfIntent
import EvidencePackage.EvidenceOfTime
import File.File
import File.Model
import File.Storage
import Kontra
import KontraPrelude
import Log.Identifier
import Log.Utils
import Templates
import Util.Actor
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Directory
import Utils.Read
import qualified Amazon as AWS
import qualified Doc.SealSpec as Seal
import qualified HostClock.Model as HC

personFromSignatory :: (MonadDB m, MonadMask m, TemplatesMonad m, AWS.AmazonMonad m, MonadLog m, MonadBaseControl IO m)
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

personExFromSignatoryLink :: (MonadDB m, MonadMask m, TemplatesMonad m, AWS.AmazonMonad m, MonadLog m, MonadBaseControl IO m)
                          =>  TimeZoneName -> SignatoryIdentifierMap -> (BS.ByteString,BS.ByteString) -> SignatoryLink -> m Seal.Person
personExFromSignatoryLink tz sim boxImages sl@SignatoryLink{..} = do
  person <- personFromSignatory tz sim boxImages sl
  return person {
       Seal.emailverified    = signatorylinkdeliverymethod == EmailDelivery
     , Seal.fullnameverified = False
     , Seal.companyverified  = False
     , Seal.numberverified   = True
     , Seal.phoneverified    = (signatorylinkdeliverymethod == MobileDelivery) || (signatorylinkauthenticationtosignmethod == SMSPinAuthenticationToSign)
     }

fieldsFromSignatory :: forall m. (MonadDB m, MonadThrow m, MonadLog m, MonadBaseControl IO m, AWS.AmazonMonad m) => (BS.ByteString,BS.ByteString) -> SignatoryLink -> m [Seal.Field]
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

    makeSealField :: SignatoryField -> m [Seal.Field]
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

findOutAttachmentDesc sim tmppath document = logDocument (documentid document) $ do
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
        findAttachments (Just (authorattachmentfileid authorattach)) num asl (authorattachmentname authorattach) (authorattachmentaddtosealedfile authorattach)

      findAttachmentsForSignatoryAttachment (num, sigattach, sl) =
        findAttachments (signatoryattachmentfile sigattach) num sl (signatoryattachmentname sigattach) True

      findAttachments mfileid num sl title addContent = do
        (contents,numberOfPages,name) <- case mfileid of
          Nothing -> return (BS.empty, 1, "")
          Just fileid' -> do
            contents <- getFileIDContents fileid'
            file <- dbQuery $ GetFileByFileID fileid'
            eNumberOfPages <- liftIO $ getNumberOfPDFPages contents
            numberOfPages <- case eNumberOfPages of
              Left e -> do
                logAttention "Calculating number of pages of document failed, falling back to 1" $ object [
                    "reason" .= e
                  ]
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

        attachedToSealedFileText <- renderLocalTemplate document (if addContent then "_attachedToSealedFile"  else "_notAttachedToSealedFile") (return ())

        return $ Seal.FileDesc
                 { fileTitle      = title
                 , fileRole       = attachmentNumText
                 , filePagesText  = numberOfPagesText
                 , fileAttachedBy = attachedByText
                 , fileAttachedToSealedFileText = Just attachedToSealedFileText
                 , fileInput      = if addContent then (Just attachmentPath) else Nothing
                 }


evidenceOfIntentAttachment :: (TemplatesMonad m, MonadDB m, MonadThrow m, MonadLog m, MonadBaseControl IO m, AWS.AmazonMonad m)
                           => SignatoryIdentifierMap -> Document -> m Seal.SealAttachment
evidenceOfIntentAttachment sim doc = do
  let title = documenttitle doc
  let sls = documentsignatorylinks doc
  ss <- dbQuery $ GetSignatoryScreenshots (map signatorylinkid sls)
  let sortBySignTime = sortBy (on compare (fmap signtime . maybesigninfo . fst))
  html <- evidenceOfIntentHTML sim title $ sortBySignTime [ (sl, s) | (i, s) <- ss, sl <- filter ((==i) . signatorylinkid) sls ]
  return $ Seal.SealAttachment { Seal.fileName = "Appendix 5 Evidence of Intent.html"
                               , Seal.mimeType = Nothing
                               , Seal.fileContent = BS.fromString html
                               }

{-
 We need to handle all timezones that postgres handles, so date formatting must be done through the db.
-}
formatUTCTimeForVerificationPage :: (MonadDB m, MonadMask m) => TimeZoneName -> UTCTime -> m String
formatUTCTimeForVerificationPage tz mt = withTimeZone tz $ do
  -- We can't retrieve ZonedTime, because libpq always applies local machine's time zone (by design)
  -- so let's format everything on the db
  -- withTimeZone is only needed for 'TZ' format specifier to handle nice timezone names (e.g. Europe/Berlin -> CEST)
  -- all other calculations are not session timezone related

  -- version for postgres-9.4
  -- runQuery_ $ rawSQL "SELECT TO_CHAR(($1 AT TIME ZONE $2)::timestamptz, 'YYYY-MM-DD HH24:MI:SS TZ (OF00)')" (mt, toString tz)

  -- but postgres < 9.4 does not support timezone offset specifier, so we have to do it by hand
  -- ($1) is timestamp input, ($2) is timezone input
  let intervalFromUTC = "AGE(($1 AT TIME ZONE 'UTC')::TIMESTAMP, ($1 AT TIME ZONE $2)::TIMESTAMP)"
      offsetHours = "-EXTRACT(HOURS FROM " ++ intervalFromUTC ++ ")"
      niceOffsetHours' = "TO_CHAR(" ++ offsetHours ++ ", 'S00')" -- e.g. 2 -> +02
      niceOffsetHours = "CONCAT(" ++ niceOffsetHours' ++ ", '00')" -- append two trailing zeros

      datetimeWithoutOffset = "TO_CHAR(($1 AT TIME ZONE $2)::TIMESTAMPTZ, 'YYYY-MM-DD HH24:MI:SS TZ')"

      sqlConcat ss = "CONCAT(" ++ intercalate ", " ss ++ ")"
  runQuery_ $ rawSQL (BS.fromString $ "SELECT " ++ sqlConcat [datetimeWithoutOffset, "' ('", niceOffsetHours, "')'"]) (mt, toString tz)
  fetchOne runIdentity

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
                     -> [HC.ClockErrorEstimate]
                     -> BS.ByteString
                     -> BS.ByteString
                     -> String
                     -> String
                     -> String
                     -> m Seal.SealSpec
sealSpecFromDocument boxImages hostpart document elog offsets graphEvidenceOfTime content tmppath inputpath outputpath = do
  -- Keep only simple events and remove events induced by resealing
  let velog = filter eventForVerificationPage $ eventsForLog elog
  -- Form initials from signing parties
  sim <- getSignatoryIdentifierMap False $ filter eventForVerificationPage velog

  -- Remove events generated by non-signing parties
  -- FIXME: Should we include events for deleted documents?
  let viewingParty e = viewer (evAffectedSigLink e) || viewer (evSigLink e)
           where viewer s = ((signatoryisauthor || signatoryispartner) <$>
                             (s >>= flip Map.lookup sim >>= siLink))
                         == Just False
      spelog = filter (not . viewingParty) velog

  additionalAttachments <- findOutAttachmentDesc sim tmppath document
  docs <- mapM (\f -> (takeFileName f,) <$> liftIO (BS.readFile f))
            [ "files/Evidence Quality of Scrive E-signed Documents.html"
            , "files/Appendix 1 Evidence Quality Framework.html"
            , "files/Appendix 2 Service Description.html"
            , "files/Appendix 6 Digital Signature Documentation.html"
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
  htmllogs <- htmlDocFromEvidenceLog (documenttitle document) elogsim htmlevents
  let evidenceattachment = Seal.SealAttachment { Seal.fileName = "Appendix 3 Evidence Log.html"
                                               , Seal.mimeType = Nothing
                                               , Seal.fileContent = BS.fromString htmllogs }
  htmlEvidenceOfTime <- evidenceOfTimeHTML (documenttitle document) offsets graphEvidenceOfTime
  let evidenceOfTime = Seal.SealAttachment { Seal.fileName = "Appendix 4 Evidence of Time.html"
                                           , Seal.mimeType = Nothing
                                           , Seal.fileContent = BS.fromString htmlEvidenceOfTime }
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
      logAttention "Calculating number of pages of document failed, falling back to 1" $ object [
          "reason" .= e
        ]
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
        , Seal.attachments    = docAttachments ++ [evidenceattachment, evidenceOfTime, evidenceOfIntent]
        , Seal.filesList      =
          [ Seal.FileDesc { fileTitle = documenttitle document
                          , fileRole = mainDocumentText
                          , filePagesText = numberOfPagesText
                          , fileAttachedBy = attachedByText
                          , fileAttachedToSealedFileText = Nothing
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
sealDocument hostpart = do
  mfile <- fileFromMainFile =<< documentfile <$> theDocument
  case mfile of
    Just file -> do
      logInfo_ "Sealing document"
      sealDocumentFile hostpart file
      logInfo_ "Sealing of document should be done now"
    Nothing -> do
      logInfo_ "Sealing of document failed because it has no main file attached"
      internalError

sealDocumentFile :: (CryptoRNG m, MonadMask m, MonadBaseControl IO m, DocumentMonad m, TemplatesMonad m, MonadIO m, MonadLog m, AWS.AmazonMonad m)
                 => String
                 -> File
                 -> m ()
sealDocumentFile hostpart file@File{fileid, filename} = theDocumentID >>= \documentid ->
  withSystemTempDirectory' ("seal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    now <- currentTime
    -- We add events before we attempt the sealing process so
    -- that the event gets included in the evidence package
    addSealedEvidenceEvents (systemActor now)
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "frontend/app/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "frontend/app/img/checkbox_unchecked.jpg"
    elog <- dbQuery $ GetEvidenceLog documentid
    -- Evidence of Time documentation says we collect last 1000 samples
    offsets <- dbQuery $ HC.GetNClockErrorEstimates 1000
    when (not $ HC.enoughClockErrorOffsetSamples offsets) $ do
      logAttention_ "Cannot seal document because there are no valid host_clock samples"
      void $ dbUpdate $ ErrorDocument ErrorSealingDocumentEvidence
        (return ())
        (systemActor now)
    graphEvidenceOfTime <- generateEvidenceOfTimeGraph 100 (tmppath ++ "/eot_samples.txt") (tmppath ++ "/eot_graph.svg") (map HC.offset offsets)
    config <- theDocument >>= \d -> sealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) hostpart d elog offsets graphEvidenceOfTime content tmppath tmpin tmpout

    let json_config = Unjson.unjsonToByteStringLazy Seal.unjsonSealSpec config
    (code,_stdout,stderr) <- liftIO $ do
      let sealspecpath = tmppath ++ "/sealspec.json"
      liftIO $ BSL.writeFile sealspecpath json_config
      readProcessWithExitCode "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "add-verification-pages", sealspecpath] (BSL.empty)

    logInfo "Sealing completed" $ object [
        "code" .= show code
      ]
    case code of
      ExitSuccess -> do
        tmpoutContent <- liftIO (BS.readFile tmpout)
        case tmpoutContent of
          "" -> do
            logAttention_ $ "Sealing document resulted in an empty output"
            internalError
          _ -> do
            sealedfileid <- dbUpdate $ NewFile filename $ Binary tmpoutContent
            dbUpdate $ AppendSealedFile sealedfileid Missing (systemActor now)
      ExitFailure _ -> do
        systmp <- liftIO $ getTemporaryDirectory
        (path, handle) <- liftIO $ openTempFile systmp ("seal-failed-" ++ show documentid ++ "-" ++ show fileid ++ "-.pdf")
        logAttention "Cannot seal document because of file" $ object [
            identifier_ fileid
          , "path" .= path
          , "stderr" `equalsExternalBSL` stderr
          ]
        -- show JSON'd config as that's what the java app is fed.
        liftIO $ BS.hPutStr handle content
        liftIO $ hClose handle
        void $ dbUpdate $ ErrorDocument
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
    logInfo "Presealing file" $ object [
        identifier_ fileid
      ]
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
      readProcessWithExitCode "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "add-verification-pages", sealspecpath] (BSL.empty)
    logInfo "Presealing completed" $ object [
        "code" .= show code
      ]
    case code of
      ExitSuccess -> do
          res <- liftIO $ BS.readFile tmpout
          logInfo_ "Returning presealed content"
          return $ Right res
      ExitFailure _ -> do
          logAttention "Presealing failed" $ object [
              "stderr" `equalsExternalBSL` stderr
            ]
          -- show JSON'd config as that's what the java app is fed.
          return $ Left "Error when preprinting fields on PDF"


addSealedEvidenceEvents ::  (MonadBaseControl IO m, MonadDB m, MonadLog m, TemplatesMonad m, MonadIO m, DocumentMonad m, AWS.AmazonMonad m, MonadThrow m)
                 => Actor -> m ()
addSealedEvidenceEvents actor = do
  notAddedAttachments <- filter (not . authorattachmentaddtosealedfile) <$> documentauthorattachments <$> theDocument
  forM_ notAddedAttachments $ \a -> do
    contents <- getFileIDContents $ authorattachmentfileid a
    let hash = filter (not . isSpace) $ show $ sha256 $ BB.unpack contents
    _ <- update $ InsertEvidenceEvent
        AuthorAttachmentHashComputed
        (F.value "attachment_name" (authorattachmentname a) >> F.value "hash" hash)
        actor
    return ()
  _ <- update $ InsertEvidenceEvent
        AttachSealedFileEvidence
        (return ())
        actor
  return ()
