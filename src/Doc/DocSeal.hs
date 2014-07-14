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
import Data.Monoid.Space
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
import MinutesTime
import Utils.Directory
import Utils.Read
import Utils.IO
import System.Directory
import System.Exit
import Kontra
import Text.HTML.TagSoup (Tag(..), parseTags)
import Text.StringTemplates.Templates
import Text.JSON.Gen
import Text.JSON.Pretty (pp_value)
import System.FilePath (takeFileName, takeExtension, (</>))
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
import DB.TimeZoneName
import Control.Applicative
import EvidenceLog.Model
import EvidenceLog.View
import Util.Actor
import qualified Text.StringTemplates.Fields as F
import Control.Logic
import Utils.Prelude
import qualified Amazon as AWS
import Data.Char


personFromSignatory :: (MonadDB m,MonadBaseControl IO m, TemplatesMonad m)
                    => TimeZoneName -> (BS.ByteString,BS.ByteString) -> SignatoryLink -> m Seal.Person
personFromSignatory tz boxImages signatory = do
    stime <- case  maybesigninfo signatory of
                  Nothing -> return ""
                  Just si -> formatMinutesTimeForVerificationPage tz $ signtime si
    signedAtText <- renderTemplate "_contractsealingtextssignedAtText" $ F.value "time" stime
    let personalnumber = getPersonalNumber signatory
        companynumber = getCompanyNumber signatory
    companyNumberText <- if (not (null companynumber))
                            then renderTemplate "_contractsealingtextsorgNumberText2" $ F.value "companynumber" companynumber
                         else return ""
    personalNumberText <- if (not (null personalnumber))
                            then renderTemplate "_contractsealingtextspersonalNumberText2" $ F.value "personalnumber" personalnumber
                         else return ""

    return $ Seal.Person { Seal.fullname = getFullName signatory
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
                , Seal.signtime = stime
                , Seal.signedAtText = signedAtText
                , Seal.personalNumberText = personalNumberText
                , Seal.companyNumberText = companyNumberText
                }

personExFromSignatoryLink :: (MonadDB m,MonadBaseControl IO m, TemplatesMonad m)
                          =>  TimeZoneName -> (BS.ByteString,BS.ByteString) -> SignatoryLink -> m (Seal.Person, String)
personExFromSignatoryLink tz boxImages (sl@SignatoryLink { signatorysignatureinfo
                                                      , signatorylinkdeliverymethod
                                                      , signatorylinkauthenticationmethod
                                                      }) = do
  person <- personFromSignatory tz boxImages sl
  return ((person {
       Seal.emailverified    = signatorylinkdeliverymethod == EmailDelivery
     , Seal.fullnameverified = fullnameverified
     , Seal.companyverified  = False
     , Seal.numberverified   = numberverified
     , Seal.phoneverified    = (signatorylinkdeliverymethod == MobileDelivery) || (signatorylinkauthenticationmethod == SMSPinAuthentication)
     })
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

findOutAttachmentDesc :: (MonadIO m, MonadDB m, Log.MonadLog m, TemplatesMonad m, AWS.AmazonMonad m) => String -> Document -> m [Seal.FileDesc]
findOutAttachmentDesc tmppath document = do
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
            return (contents, getNumberOfPDFPages contents, filename file)
        numberOfPagesText <-
          if (".png" `isSuffixOf` (map toLower name) || ".jpg" `isSuffixOf` (map toLower name) )
           then return ""
           else if numberOfPages==1
                  then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
                  else renderLocalTemplate document "_numberOfPages" $ do
                    F.value "pages" numberOfPages

        attachedByText <- renderLocalTemplate document "_documentAttachedBy" $ do
                                     F.value "identifier" $ getIdentifier sl

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
formatMinutesTimeForVerificationPage :: (MonadDB m,MonadBaseControl IO m) => TimeZoneName -> MinutesTime -> m String
formatMinutesTimeForVerificationPage tz mt = do
  withTimeZone tz $ do
    runQuery_ $ "select TO_CHAR(" <?> mt <+> ",'YYYY-MM-DD HH24:MI:SS TZ')"
    (ftime::String) <- fetchOne unSingle
    return $ ftime ++ " (" ++ (zoneDiff (fromJust $ parseMinutesTimeUTC $ take 19 ftime) mt) ++ ")"
  where
    zoneDiff mt1 mt2 = let
                         secDiff = (toMinutes mt1) - (toMinutes mt2)
                         mDiff = (secDiff `mod` 60)
                         hDiff = (secDiff `div` 60) `mod` 24
                         hDiffText = show (hDiff `div` 10) ++ show (hDiff `mod` 10)  ++ show (mDiff `div` 10) ++ show (mDiff `mod` 10)
                       in if (secDiff >= 0)
                            then "+" ++ hDiffText
                            else "-" ++ hDiffText

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

sealSpecFromDocument :: (MonadIO m, TemplatesMonad m, MonadDB m, MonadBaseControl IO m, Log.MonadLog m, AWS.AmazonMonad m)
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
  additionalAttachments <- findOutAttachmentDesc tmppath document
  docs <- mapM (\f -> ((takeFileName f,) . BS.toString . B64.encode) <$> liftIO (BS.readFile f))
            [ "files/Evidence_Documentation.html"
            , "files/Digital_Signature_Documentation_Part_I.html"
            , "files/Digital_Signature_Documentation_Part_II.html"
            ]

  sealSpecFromDocument2 boxImages hostpart document elog ces content inputpath outputpath additionalAttachments docs

sealSpecFromDocument2 :: (TemplatesMonad m, MonadDB m, Log.MonadLog m, MonadIO m, AWS.AmazonMonad m, MonadBaseControl IO m)
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
      paddeddocid = pad0 20 (show docid)
      Just authorsiglink = getAuthorSigLink document
      removeTags s = concat [t | TagText t <- parseTags s]
      mkHistEntry ev = do
        actor <- approximateActor True document ev
        comment <- removeTags <$> simplyfiedEventText (Just actor) document ev
        etime <- formatMinutesTimeForVerificationPage (documenttimezonename document) (evTime ev)
        return $ Seal.HistEntry{ Seal.histdate = etime
                               , Seal.histcomment = comment
                               , Seal.histaddress = maybe "" show $ evIP4 ev
                               }

  in do
      signatories <- sequence $ [ personExFromSignatoryLink (documenttimezonename document) boxImages s
                        | s <- documentsignatorylinks document
                        , signatoryispartner $ s
                    ]

      secretaries <- sequence $ [ personFromSignatory (documenttimezonename document) boxImages $ s
                        | s <- documentsignatorylinks document
                        , not . signatoryispartner $ s
                    ]

      initiator <- if (signatoryispartner authorsiglink)
                    then return Nothing
                    else Just <$> (personFromSignatory (documenttimezonename document) boxImages authorsiglink)

      let (persons, initialsx) = unzip signatories
      let initials = intercalate ", " initialsx

      -- Log.mixlog_ "Creating seal spec from file."
      -- Remove events induced by resealing and non-signing party activities
      let eventsForHistory = filter eventForVerificationPage . filter (not . viewingParty)
          viewingParty e = viewer (evAffectedSigLink e) || viewer (evSigLink e)
               where viewer s = (signatoryispartner <$> s) == Just False
      history <- (mapM mkHistEntry . eventsForHistory) =<< getSignatoryLinks (eventsForLog elog)

      staticTexts <- createSealingTextsForDocument document hostpart

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
        F.value "identifier" $ getIdentifier authorsiglink

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
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "frontend/app/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "frontend/app/img/checkbox_unchecked.jpg"
    elog <- dbQuery $ GetEvidenceLog documentid
    ces <- collectClockErrorStatistics elog
    config <- theDocument >>= \d -> sealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) mctxhostpart d elog ces content tmppath tmpin tmpout

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
        systmp <- liftIO $ getTemporaryDirectory
        (path, handle) <- liftIO $ openTempFile systmp ("seal-failed-" ++ show documentid ++ "-" ++ show fileid ++ "-.pdf")
        let msg = "Cannot seal document #" ++ show documentid ++ " because of file #" ++ show fileid
        Log.attention_ $ msg ++ ": " ++ path
        Log.attention_ $ BSL.toString stderr
        Log.attention_ $ "Sealing configuration: " ++ show config
        liftIO $ BS.hPutStr handle content
        liftIO $ hClose handle
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
