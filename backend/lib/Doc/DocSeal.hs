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
import Crypto.RNG
import Data.Function (on)
import Data.Time
import Log
import System.FilePath ((</>), takeExtension, takeFileName)
import Text.StringTemplates.Templates
import qualified Crypto.Hash as H
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.ICU.Normalize as ICU
import qualified Text.StringTemplates.Fields as F

import DB
import DB.TimeZoneName
import Doc.CheckboxPlacementsUtils
  ( CheckboxImagesMapping, getCheckboxImage, readCheckboxImagesMapping
  )
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils
import Doc.DocView
import Doc.Logging
import Doc.Model
import Doc.RadiobuttonPlacementsUtils
  ( RadiobuttonImagesMapping, getRadiobuttonImage, readRadiobuttonImagesMapping
  )
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryIdentification
  ( SignatoryIdentifierMap, siInitials, signatoryIdentifier
  )
import EID.Authentication.Model
import EID.EIDService.Types
import EID.Nets.Types
import EID.Signature.Model
import EvidenceLog.Model
import EvidenceLog.View
import EvidencePackage.EvidenceLog
import EvidencePackage.EvidenceOfIntent
import EvidencePackage.EvidenceOfTime
import File.File
import File.Model
import File.Storage
import Kontra
import Log.Identifier
import PdfToolsLambda.Conf
import PdfToolsLambda.Control
import Templates
import Util.Actor
import Util.HasSomeUserInfo
import Util.PDFUtil
import Util.SignatoryLinkUtils
import Utils.Directory
import Utils.Read
import qualified Doc.SealSpec as Seal
import qualified HostClock.Model as HC

personFromSignatory
  :: forall m
   . ( MonadDB m
     , MonadIO m
     , MonadMask m
     , TemplatesMonad m
     , MonadFileStorage m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => Text
  -> TimeZoneName
  -> SignatoryIdentifierMap
  -> CheckboxImagesMapping
  -> RadiobuttonImagesMapping
  -> SignatoryLink
  -> m Seal.Person
personFromSignatory inputpath tz sim checkboxMapping radiobuttonMapping signatory = do
  emptyNamePlaceholder <- renderTextTemplate_ "_notNamedParty"
  stime                <- case maybesigninfo signatory of
    Nothing -> return ""
    Just si -> formatUTCTimeForVerificationPage tz $ signtime si
  signedAtText <- if (T.null stime)
    then return ""
    else renderTextTemplate "_contractsealingtextssignedAtText" $ F.value "time" stime
  let personalnumber =
        if signatorylinkhidepn signatory then "" else getPersonalNumber signatory
      companynumber = getCompanyNumber signatory
  companyNumberText <- if T.null companynumber
    then return ""
    else renderTextTemplate "_contractsealingtextsorgNumberText"
      $ F.value "companynumber" companynumber
  personalNumberText <- if T.null personalnumber
    then return ""
    else renderTextTemplate "_contractsealingtextspersonalNumberText"
      $ F.value "idnumber" personalnumber

  eauthentication <-
    dbQuery $ GetEAuthenticationWithoutSession AuthenticationToView $ signatorylinkid
      signatory
  identifiedNameText <- case eauthentication of
    Just (CGISEBankIDAuthentication_ authentication) ->
      renderTextTemplate "_identifiedBySwedishBankIDText"
        . F.value "name"
        $ cgisebidaSignatoryName authentication
    Just (NetsNOBankIDAuthentication_ authentication) ->
      renderTextTemplate "_identifiedByNOBankIDText"
        . F.value "name"
        $ netsNOBankIDSignatoryName authentication
    Just (NetsDKNemIDAuthentication_ authentication) ->
      renderTextTemplate "_identifiedByDKNemIDText"
        . F.value "name"
        $ netsDKNemIDSignatoryName authentication
    Just (NetsFITupasAuthentication_ authentication) ->
      renderTextTemplate "_identifiedByFITupasText"
        . F.value "name"
        $ netsFITupasSignatoryName authentication
    Just (EIDServiceVerimiAuthentication_ authentication) ->
      renderTextTemplate "_identifiedByVerimiText"
        . F.value "email"
        $ eidServiceVerimiVerifiedEmail authentication
    Just (EIDServiceIDINAuthentication_ authentication) ->
      renderTextTemplate "_identifiedByIDINText"
        . F.value "email"
        $ eidServiceIDINVerifiedEmail authentication
    Just (EIDServiceNemIDAuthentication_ authentication) ->
      renderTextTemplate "_identifiedByDKNemIDText"
        . F.value "name"
        $ eidServiceNemIDSignatoryName authentication
    Just (EIDServiceNOBankIDAuthentication_ authentication) ->
      renderTextTemplate "_identifiedByNOBankIDText"
        . F.value "name"
        $ eidServiceNOBankIDSignatoryName authentication
    Just (SMSPinAuthentication_ _) -> return ""
    Nothing -> return ""

  esignature   <- dbQuery $ GetESignature $ signatorylinkid signatory
  nameFromText <- case esignature of
    Just (CGISEBankIDSignature_ sig) ->
      renderTextTemplate "_nameFromSwedishBankIDText"
        . F.value "name"
        $ cgisebidsSignatoryName sig
    Just (NetsNOBankIDSignature_ sig) ->
      renderTextTemplate "_nameFromNOBankIDText" . F.value "name" $ netsnoSignatoryName
        sig
    Just (NetsDKNemIDSignature_ sig) ->
      renderTextTemplate "_nameFromDKNemIDText" . F.value "name" $ netsdkSignatoryName sig
    _ -> return ""

  fields <-
    maybeAddBankIDLogo
      =<< fieldsFromSignatory checkboxMapping radiobuttonMapping signatory
  highlightedImages <- mapM (highlightedImageFromHighlightedPage inputpath)
                            (signatoryhighlightedpages signatory)
  return $ Seal.Person
    { Seal.fullname = fromMaybe "" $ signatoryIdentifier sim
                                                         (signatorylinkid signatory)
                                                         emptyNamePlaceholder
    , Seal.company            = getCompanyName signatory
    , Seal.email              = getEmail signatory
    , Seal.phone              = getMobile signatory
    , Seal.personalnumber     = personalnumber
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
    , Seal.highlightedImages  = highlightedImages
    , Seal.identifiedNameText = identifiedNameText
    , Seal.nameFromText       = nameFromText
    }
  where
    maybeAddBankIDLogo :: [Seal.Field] -> m [Seal.Field]
    maybeAddBankIDLogo = case signatorylinkauthenticationtosignmethod signatory of
      SEBankIDAuthenticationToSign -> addBankIDLogo "bankid_logo_se.png"
      NOBankIDAuthenticationToSign -> addBankIDLogo "bankid_logo_no.png"
      DKNemIDAuthenticationToSign -> addBankIDLogo "nemid_logo_dk.png"
      _ -> return

    addBankIDLogo :: FilePath -> [Seal.Field] -> m [Seal.Field]
    addBankIDLogo fname fields = do
      imgData <- liftIO . BS.readFile $ "files" </> "images" </> fname
      return (bankIDLogoJPEG imgData : fields)

    bankIDLogoJPEG :: BS.ByteString -> Seal.Field
    bankIDLogoJPEG imgData = Seal.FieldJPG { valueBinary           = imgData
                                           , Seal.x                = 0
                                           , Seal.y                = 0
                                           , Seal.page             = 0
                                           , Seal.image_w          = 1.0
                                           , Seal.image_h = 0.47222222222222222222
                                           , Seal.includeInSummary = True
                                           , Seal.onlyForSummary   = True
                                           , Seal.keyColor         = Just (255, 255, 255)
                                           }


personExFromSignatoryLink
  :: ( MonadDB m
     , MonadIO m
     , MonadMask m
     , TemplatesMonad m
     , MonadFileStorage m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => Text
  -> TimeZoneName
  -> SignatoryIdentifierMap
  -> CheckboxImagesMapping
  -> RadiobuttonImagesMapping
  -> SignatoryLink
  -> m Seal.Person
personExFromSignatoryLink inputpath tz sim checkboxMapping radiobuttonMapping sl@SignatoryLink {..}
  = do
    person <- personFromSignatory inputpath tz sim checkboxMapping radiobuttonMapping sl
    return person
      { Seal.emailverified    = signatorylinkdeliverymethod == EmailDelivery
      , Seal.fullnameverified = False
      , Seal.companyverified  = False
      , Seal.numberverified   = True
      , Seal.phoneverified    = (signatorylinkdeliverymethod == MobileDelivery)
                                  || (  signatorylinkauthenticationtosignmethod
                                     == SMSPinAuthenticationToSign
                                     )
      }

fieldsFromSignatory
  :: forall m
   . ( MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , MonadFileStorage m
     )
  => CheckboxImagesMapping
  -> RadiobuttonImagesMapping
  -> SignatoryLink
  -> m [Seal.Field]
fieldsFromSignatory checkboxMapping radiobuttonMapping SignatoryLink { signatoryfields }
  = silenceJPEGFieldsFromFirstSignature <$> concat <$> mapM makeSealField signatoryfields
  where
    silenceJPEGFieldsToTheEnd [] = []
    silenceJPEGFieldsToTheEnd (field@(Seal.FieldJPG{}) : xs) =
      (field { Seal.includeInSummary = False }) : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsToTheEnd (x : xs) = x : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsFromFirstSignature [] = []
    silenceJPEGFieldsFromFirstSignature (field@(Seal.FieldJPG { Seal.includeInSummary = True }) : xs)
      = field : silenceJPEGFieldsToTheEnd xs
    silenceJPEGFieldsFromFirstSignature (x : xs) =
      x : silenceJPEGFieldsFromFirstSignature xs

    makeSealField :: SignatoryField -> m [Seal.Field]
    makeSealField sf = case sf of
      SignatorySignatureField ssf -> case (fieldPlacements sf, ssfValue ssf) of
        (_ , Nothing) -> return []  -- We skip signature that don't have a drawing
        ([], Just f ) -> (\x -> [x]) <$> fieldJPEGFromSignatureField f
        (_ , Just f ) -> mapM (fieldJPEGFromPlacement f) (fieldPlacements sf)
      SignatoryCheckboxField schf ->
        return $ map (checkboxJPEG $ schfValue schf) (fieldPlacements sf)
      SignatoryRadioGroupField srgf -> return $ zipWith
        (radiobuttonJPEG $ srgfSelectedValue srgf)
        (srgfValues srgf)
        (fieldPlacements sf)

      _ -> return $ for (fieldPlacements sf) $ fieldFromPlacement False sf
    fieldFromPlacement greyed sf placement = Seal.Field
      { Seal.value            = fromMaybe "" $ fieldTextValue sf
      , Seal.x                = placementxrel placement
      , Seal.y                = placementyrel placement
      , Seal.page             = placementpage placement
      , Seal.fontSize         = placementfsrel placement
      , Seal.greyed           = greyed
      , Seal.includeInSummary = True
      }

    checkboxJPEG checked placement = Seal.FieldJPG
      { valueBinary = getCheckboxImage checkboxMapping (placementwrel placement) checked
      , Seal.x                = placementxrel placement
      , Seal.y                = placementyrel placement
      , Seal.page             = placementpage placement
      , Seal.image_w          = placementwrel placement
      , Seal.image_h          = 0 -- hrel for checkboxes should be set to 0 anyway
      , Seal.includeInSummary = False
      , Seal.onlyForSummary   = False
      , Seal.keyColor         = Nothing
      }
    radiobuttonJPEG selected_value value placement = Seal.FieldJPG
      { valueBinary           = getRadiobuttonImage radiobuttonMapping
                                                    (placementwrel placement)
                                                    (selected_value == Just value)
      , Seal.x                = placementxrel placement
      , Seal.y                = placementyrel placement
      , Seal.page             = placementpage placement
      , Seal.image_w          = placementwrel placement
      , Seal.image_h          = 0 -- hrel for radiobuttons should be set to 0 anyway
      , Seal.includeInSummary = False
      , Seal.onlyForSummary   = False
      , Seal.keyColor         = Nothing
      }
    fieldJPEGFromPlacement f placement = do
      content <- getFileIDContents f
      return $ Seal.FieldJPG { valueBinary           = content
                             , Seal.x                = placementxrel placement
                             , Seal.y                = placementyrel placement
                             , Seal.page             = placementpage placement
                             , Seal.image_w          = placementwrel placement
                             , Seal.image_h          = placementhrel placement
                             , Seal.includeInSummary = True
                             , Seal.onlyForSummary   = False
                             , Seal.keyColor         = Just (255, 255, 255) -- white is transparent
                             }
    fieldJPEGFromSignatureField f = do
      content <- getFileIDContents f
      return $ Seal.FieldJPG { valueBinary           = content
                             , Seal.x                = 0
                             , Seal.y                = 0
                             , Seal.page             = 0
                             , Seal.image_w          = 0
                             , Seal.image_h          = 0
                             , Seal.includeInSummary = True
                             , Seal.onlyForSummary   = True
                             , Seal.keyColor         = Just (255, 255, 255) -- white is transparent
                             }

highlightedImageFromHighlightedPage
  :: forall m
   . ( MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , MonadFileStorage m
     )
  => Text
  -> HighlightedPage
  -> m Seal.HighlightedImage
highlightedImageFromHighlightedPage inputpath HighlightedPage {..} = do
  pdfContent     <- liftBase $ BS.readFile $ T.unpack inputpath
  content        <- getFileIDContents highlightedPageFileID
  mMaskedContent <- clipHighlightImageFromPage pdfContent
                                               content
                                               (fromIntegral highlightedPagePage)
  case mMaskedContent of
    Nothing            -> internalError
    Just maskedContent -> return Seal.HighlightedImage
      { Seal.hiPage  = highlightedPagePage
      , Seal.hiImage = maskedContent
      }

listAttachmentsFromDocument :: Document -> [(SignatoryAttachment, SignatoryLink)]
listAttachmentsFromDocument document = concatMap extract
                                                 (documentsignatorylinks document)
  where extract sl = map (\at -> (at, sl)) (signatoryattachments sl)


findOutAttachmentDesc
  :: ( MonadIO m
     , MonadDB m
     , MonadMask m
     , MonadLog m
     , TemplatesMonad m
     , MonadFileStorage m
     , MonadBaseControl IO m
     )
  => SignatoryIdentifierMap
  -> Text
  -> Document
  -> m [Seal.FileDesc]

findOutAttachmentDesc sim tmppath document = logDocument (documentid document) $ do
  a <- mapM findAttachmentsForAuthorAttachment authorAttsNumbered
  b <- mapM findAttachmentsForSignatoryAttachment attAndSigsNumbered
  return (a <> catMaybes b)
  where
    attAndSigs         = listAttachmentsFromDocument document
    authorAtts         = documentauthorattachments document
    authorAttsNumbered = zip [1 :: Int ..] authorAtts
    attAndSigsNumbered =
      zipWith (\num (at, sl) -> (num, at, sl)) [(length authorAtts + 1) ..] attAndSigs

    Just asl = getAuthorSigLink document

    findAttachmentsForAuthorAttachment (num, authorattach) = findAttachments
      (Just (authorattachmentfileid authorattach))
      num
      asl
      (authorattachmentname authorattach)
      (authorattachmentaddtosealedfile authorattach)

    findAttachmentsForSignatoryAttachment (num, sigattach, sl) = do
      case (signatoryattachmentrequired sigattach, signatoryattachmentfile sigattach) of
        (False, Nothing) -> return Nothing
        _ ->
          Just
            <$> findAttachments (signatoryattachmentfile sigattach)
                                num
                                sl
                                (signatoryattachmentname sigattach)
                                True

    findAttachments mfileid num sl title addContent = do
      (contents, numberOfPages, name) <- case mfileid of
        Nothing      -> return (BS.empty, 1, "")
        Just fileid' -> do
          contents       <- getFileIDContents fileid'
          file           <- dbQuery $ GetFileByFileID fileid'
          eNumberOfPages <- liftIO $ getNumberOfPDFPages contents
          numberOfPages  <- case eNumberOfPages of
            Left e -> do
              logInfo "Calculating number of pages of document failed, falling back to 1"
                $ object ["reason" .= e]
              return 1
            Right x -> return x
          return (contents, numberOfPages, filename file)
      numberOfPagesText <-
        if (              ".png"
           `T.isSuffixOf` (T.toLower name)
           ||             ".jpg"
           `T.isSuffixOf` (T.toLower name)
           )
        then
          return ""
        else
          if numberOfPages == 1
            then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
            else renderLocalTemplate document "_numberOfPages" $ do
              F.value "pages" numberOfPages

      attachedByText <- do
        emptyNamePlaceholder <- renderTextTemplate_ "_notNamedParty"
        renderLocalTemplate document "_documentAttachedBy" $ do
          F.value "identifier"
            $ signatoryIdentifier sim (signatorylinkid sl) emptyNamePlaceholder

      attachmentNumText <- renderLocalTemplate document "_attachedDocument" $ do
        F.value "number" num
      let attachmentPath =
            (T.unpack tmppath)
              </> ((T.unpack attachmentNumText) <> takeExtension (T.unpack name))
      logInfo "Temp file write" $ object
        [ "bytes_written" .= (BS.length contents)
        , "originator" .= ("findOutAttachmentDesc" :: Text)
        ]
      liftIO $ BS.writeFile attachmentPath contents

      attachedToSealedFileText <- if addContent
        then renderLocalTemplate document "_attachedToSealedFile" (return ())
        else renderLocalTemplate document "_notAttachedToSealedFile" (return ())

      return $ Seal.FileDesc
        { fileTitle                    = ICU.normalize ICU.NFC title
        , fileRole                     = attachmentNumText
        , filePagesText                = numberOfPagesText
        , fileAttachedBy               = attachedByText
        , fileSealedOn                 = Nothing
        , fileAttachedToSealedFileText = Just attachedToSealedFileText
        , fileInput = if addContent then (Just $ T.pack attachmentPath) else Nothing
        }


evidenceOfIntentAttachment
  :: ( TemplatesMonad m
     , MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , MonadFileStorage m
     )
  => SignatoryIdentifierMap
  -> Document
  -> m Seal.SealAttachment
evidenceOfIntentAttachment sim doc = do
  let title = documenttitle doc
  let sls   = documentsignatorylinks doc
  ss <- dbQuery $ GetSignatoryScreenshots (map signatorylinkid sls)
  let sortBySignTime = sortBy (on compare (fmap signtime . maybesigninfo . fst))
  html <- evidenceOfIntentHTML sim title $ sortBySignTime
    [ (sl, s) | (i, s) <- ss, sl <- filter ((== i) . signatorylinkid) sls ]
  return $ Seal.SealAttachment { Seal.fileName    = "Appendix 5 Evidence of Intent.html"
                               , Seal.mimeType    = Nothing
                               , Seal.fileContent = TE.encodeUtf8 html
                               }

{-
 We need to handle all timezones that postgres handles, so date formatting must be done through the db.
-}
formatUTCTimeForVerificationPage
  :: (MonadDB m, MonadMask m) => TimeZoneName -> UTCTime -> m Text
formatUTCTimeForVerificationPage tz mt = withTimeZone tz $ do
  -- We can't retrieve ZonedTime, because libpq always applies local machine's time zone (by design)
  -- so let's format everything on the db
  -- withTimeZone is only needed for 'TZ' format specifier to handle nice timezone names (e.g. Europe/Berlin -> CEST)
  -- all other calculations are not session timezone related

  -- version for postgres-9.4
  -- runQuery_ $ rawSQL "SELECT TO_CHAR(($1 AT TIME ZONE $2)::timestamptz, 'YYYY-MM-DD HH24:MI:SS TZ (OF00)')" (mt, toString tz)

  -- but postgres < 9.4 does not support timezone offset specifier, so we have to do it by hand
  -- ($1) is timestamp input, ($2) is timezone input
  let intervalFromUTC =
        "AGE(($1 AT TIME ZONE 'UTC')::TIMESTAMP, ($1 AT TIME ZONE $2)::TIMESTAMP)"
      offsetHours      = "-EXTRACT(HOURS FROM " <> intervalFromUTC <> ")"
      niceOffsetHours' = "TO_CHAR(" <> offsetHours <> ", 'S00')" -- e.g. 2 -> +02
      niceOffsetHours  = "CONCAT(" <> niceOffsetHours' <> ", '00')" -- append two trailing zeros

      datetimeWithoutOffset =
        "TO_CHAR(($1 AT TIME ZONE $2)::TIMESTAMPTZ, 'YYYY-MM-DD HH24:MI:SS TZ')"

      sqlConcat ss = "CONCAT(" <> intercalate ", " ss <> ")"
  runQuery_ $ rawSQL
    (T.pack $ "SELECT " <> sqlConcat
      [datetimeWithoutOffset, "' ('", niceOffsetHours, "')'"]
    )
    (mt, toString tz)
  fetchOne runIdentity

createSealingTextsForDocument
  :: (TemplatesMonad m) => Document -> Text -> m Seal.SealingTexts
createSealingTextsForDocument document hostpart = do

  let render templ = renderLocalTemplate document templ $ do
        documentInfoFields document
        F.value "hostpart" hostpart
        F.value "verifyurl" ("https://scrive.com/verify" :: String)

  verificationTitle'           <- render "_contractsealingtexts"
  partnerText'                 <- render "_contractsealingtextspartnerText"
  initiatorText'               <- render "_contractsealingtextsinitiatorText"
  documentText'                <- render "_documentText"
  verificationPageDescription' <- render "_verificationPageDescription"
  hiddenAttachmentText'        <- render "_contractsealingtextshiddenAttachment"
  onePageText'                 <- render "_numberOfPagesIs1"

  let sealingTexts = Seal.SealingTexts
        { verificationTitle           = verificationTitle'
        , partnerText                 = partnerText'
        , initiatorText               = initiatorText'
        , documentText                = documentText'
        , verificationPageDescription = verificationPageDescription'
        , hiddenAttachmentText        = hiddenAttachmentText'
        , onePageText                 = onePageText'
        }

  return sealingTexts

sealSpecFromDocument
  :: ( MonadIO m
     , TemplatesMonad m
     , MonadDB m
     , MonadMask m
     , MonadLog m
     , MonadFileStorage m
     , MonadBaseControl IO m
     )
  => CheckboxImagesMapping
  -> RadiobuttonImagesMapping
  -> Text
  -> Document
  -> [DocumentEvidenceEvent]
  -> [HC.ClockErrorEstimate]
  -> EvidenceOfTime
  -> BS.ByteString
  -> Text
  -> Text
  -> Text
  -> m Seal.SealSpec
sealSpecFromDocument checkboxMapping radiobuttonMapping hostpart document elog offsets eotData content tmppath inputpath outputpath
  = do
  -- Form initials from signing parties
    sim                   <- getSignatoryIdentifierMap False elog

    additionalAttachments <- findOutAttachmentDesc sim tmppath document
    docs                  <- mapM
      (\f -> (takeFileName f, ) <$> liftIO (BS.readFile f))
      [ "files/Evidence Quality of Scrive E-signed Documents.html"
      , "files/Appendix 1 Evidence Quality Framework.html"
      , "files/Appendix 2 Service Description.html"
      , "files/Appendix 6 Digital Signature Documentation.html"
      ]

    let docid              = documentid document
        paddeddocid        = pad0 20 (show docid)
        Just authorsiglink = getAuthorSigLink document

    persons <-
      sequence
        $ [ personExFromSignatoryLink inputpath
                                      (documenttimezonename document)
                                      sim
                                      checkboxMapping
                                      radiobuttonMapping
                                      s
          | s <- documentsignatorylinks document
          , isSignatory $ s
          ]

    secretaries <-
      sequence
        $ [ personFromSignatory inputpath
                                (documenttimezonename document)
                                sim
                                checkboxMapping
                                radiobuttonMapping
              $ s
          | s <- documentsignatorylinks document
          , not . isSignatory $ s
          ]

    initiator <- if (isSignatory authorsiglink)
      then return Nothing
      else
        Just
          <$> (personFromSignatory inputpath
                                   (documenttimezonename document)
                                   sim
                                   checkboxMapping
                                   radiobuttonMapping
                                   authorsiglink
              )

    let initials = T.intercalate ", " $ catMaybes
          [ siInitials <$> Map.lookup (signatorylinkid s) sim
          | s <- documentsignatorylinks document
          , isSignatory s
          ]

    staticTexts <- createSealingTextsForDocument document hostpart

    -- Creating HTML Evidence Log
    let htmlevents = suppressRepeatedEvents elog
    elogsim  <- getSignatoryIdentifierMap True htmlevents
    htmllogs <- htmlDocFromEvidenceLog (documenttitle document) elogsim htmlevents
    let evidenceattachment = Seal.SealAttachment
          { Seal.fileName    = "Appendix 3 Evidence Log.html"
          , Seal.mimeType    = Nothing
          , Seal.fileContent = BS.fromString htmllogs
          }
    htmlEvidenceOfTime <- evidenceOfTimeHTML (T.unpack $ documenttitle document)
                                             offsets
                                             eotData
    let evidenceOfTime = Seal.SealAttachment
          { Seal.fileName    = "Appendix 4 Evidence of Time.html"
          , Seal.mimeType    = Nothing
          , Seal.fileContent = BS.fromString htmlEvidenceOfTime
          }
    evidenceOfIntent <- evidenceOfIntentAttachment elogsim document

    -- documentation files
    let docAttachments =
          [ Seal.SealAttachment { Seal.fileName    = T.pack name
                                , Seal.mimeType    = Nothing
                                , Seal.fileContent = doc
                                }
          | (name, doc) <- docs
          ]

    eNumberOfPages <- liftIO $ getNumberOfPDFPages content
    numberOfPages  <- case eNumberOfPages of
      Left e -> do
        logInfo "Calculating number of pages of document failed, falling back to 1"
          $ object ["reason" .= e]
        return 1
      Right x -> return x

    numberOfPagesText <- if numberOfPages == 1
      then renderLocalTemplate document "_numberOfPagesIs1" $ return ()
      else renderLocalTemplate document "_numberOfPages" $ do
        F.value "pages" numberOfPages

    attachedByText <- do
      emptyNamePlaceholder <- renderTextTemplate_ "_notNamedParty"
      renderLocalTemplate document "_documentSentOnBy" $ do
        F.value "identifier"
          $ signatoryIdentifier sim (signatorylinkid authorsiglink) emptyNamePlaceholder
        F.valueM "time" $ mapM
          (formatUTCTimeForVerificationPage (documenttimezonename document))
          (signtime <$> documentinvitetime document)

    sealedOn <- do
      renderLocalTemplate document "_documentSealedOn" $ do
        F.valueM "time" $ formatUTCTimeForVerificationPage
          (documenttimezonename document)
          (getLastSignedOrApprovedTime document)

    mainDocumentText   <- renderLocalTemplate document "_mainDocument" $ (return ())

    documentNumberText <-
      renderLocalTemplate document "_contractsealingtextsDocumentNumber" $ do
        F.value "documentnumber" $ paddeddocid

    initialsText <- renderLocalTemplate document "_contractsealingtextsInitialsText" $ do
      F.value "initials" $ initials

    let title = ICU.normalize ICU.NFC $ documenttitle document

    return $ Seal.SealSpec
      { Seal.input              = inputpath
      , Seal.output             = outputpath
      , Seal.documentNumberText = documentNumberText
      , Seal.persons            = persons
      , Seal.secretaries        = secretaries
      , Seal.initiator          = initiator
      , Seal.initialsText       = initialsText
      , Seal.hostpart           = hostpart
      , Seal.staticTexts        = staticTexts
      , Seal.attachments        = docAttachments
                                    <> [evidenceattachment, evidenceOfTime, evidenceOfIntent]
      , Seal.disableFooter      = documentisreceipt document
      , Seal.filesList          = [ Seal.FileDesc { fileTitle = title
                                                  , fileRole = mainDocumentText
                                                  , filePagesText = numberOfPagesText
                                                  , fileAttachedBy = attachedByText
                                                  , fileSealedOn = Just sealedOn
                                                  , fileAttachedToSealedFileText = Nothing
                                                  , fileInput = Nothing
                                                  }
                                  ]
                                    <> additionalAttachments
      }

presealSpecFromDocument
  :: ( MonadIO m
     , TemplatesMonad m
     , MonadDB m
     , MonadMask m
     , MonadLog m
     , MonadFileStorage m
     , MonadBaseControl IO m
     )
  => CheckboxImagesMapping
  -> RadiobuttonImagesMapping
  -> Document
  -> Text
  -> Text
  -> m Seal.PreSealSpec
presealSpecFromDocument checkboxMapping radiobuttonMapping document inputpath outputpath
  = do
    fields <- concat <$> mapM (fieldsFromSignatory checkboxMapping radiobuttonMapping)
                              (documentsignatorylinks document)
    return $ Seal.PreSealSpec { Seal.pssInput  = inputpath
                              , Seal.pssOutput = outputpath
                              , Seal.pssFields = fields
                              }

sealDocument
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , DocumentMonad m
     , TemplatesMonad m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadFileStorage m
     , PdfToolsLambdaMonad m
     )
  => Text
  -> m ()
sealDocument hostpart = do
  mfile <- fileFromMainFile =<< documentfile <$> theDocument
  case mfile of
    Just file -> do
      logInfo_ "Sealing document"
      startTime <- currentTime
      sealDocumentFile hostpart file
      finishTime <- currentTime
      logInfo "Sealing of document should be done now" $ object
        [ "elapsed_time" .= (realToFrac (diffUTCTime finishTime startTime) :: Double)
        , logPair_ file
        ]
    Nothing -> do
      logInfo_ "Sealing of document failed because it has no main file attached"
      internalError

sealDocumentFile
  :: ( CryptoRNG m
     , MonadMask m
     , MonadBaseControl IO m
     , DocumentMonad m
     , TemplatesMonad m
     , MonadIO m
     , MonadLog m
     , MonadFileStorage m
     , PdfToolsLambdaMonad m
     )
  => Text
  -> File
  -> m ()
sealDocumentFile hostpart file@File { fileid, filename } =
  theDocumentID >>= \documentid ->
    withSystemTempDirectory' ("seal-" <> show documentid <> "-" <> show fileid <> "-")
      $ \tmppath -> do
          now <- currentTime
          -- We add events before we attempt the sealing process so
          -- that the event gets included in the evidence package
          addSealedEvidenceEvents (systemActor now)
          let tmpin  = tmppath <> "/input.pdf"
          let tmpout = tmppath <> "/output.pdf"
          content <- getFileContents file
          liftIO $ BS.writeFile tmpin content
          logInfo "Temp file write" $ object
            [ "bytes_written" .= (BS.length content)
            , "originator" .= ("sealDocumentFile" :: Text)
            ]

          checkboxMapping    <- liftIO $ readCheckboxImagesMapping
          radiobuttonMapping <- liftIO $ readRadiobuttonImagesMapping
          elog               <- dbQuery $ GetEvidenceLog documentid
          -- Evidence of Time documentation says we collect last 1000 samples
          offsets            <- dbQuery $ HC.GetNClockErrorEstimates 1000
          unless (HC.enoughClockErrorOffsetSamples offsets) $ do
            logAttention_
              "Cannot seal document because there are no valid host_clock samples"
            void $ dbUpdate $ ErrorDocument ErrorSealingDocumentEvidence
                                            (return ())
                                            (systemActor now)
          eotData <- liftBase $ generateEvidenceOfTimeData
            100
            (tmppath <> "/eot_samples.txt")
            (tmppath <> "/eot_graph.svg")
            (map HC.offset offsets)
          spec <- theDocument >>= \d -> do
            sealSpecFromDocument checkboxMapping
                                 radiobuttonMapping
                                 hostpart
                                 d
                                 elog
                                 offsets
                                 eotData
                                 content
                                 (T.pack tmppath)
                                 (T.pack tmpin)
                                 (T.pack tmpout)
          logInfo_ "Seal specification generated"
          runLambdaSealing tmppath filename spec

-- | Generate file that has all placements printed on it. It will look same as final version except for footers and verification page.
presealDocumentFile
  :: ( MonadBaseControl IO m
     , MonadDB m
     , MonadLog m
     , KontraMonad m
     , TemplatesMonad m
     , MonadIO m
     , MonadMask m
     , MonadFileStorage m
     , PdfToolsLambdaMonad m
     , CryptoRNG m
     )
  => Document
  -> File
  -> m (Either Text BS.ByteString)
presealDocumentFile document@Document { documentid } file@File { fileid } =
  withSystemTempDirectory' ("preseal-" <> show documentid <> "-" <> show fileid <> "-")
    $ \tmppath -> do
        logInfo "Presealing file" $ logObject_ file
        let tmpin  = tmppath <> "/input.pdf"
        let tmpout = tmppath <> "/output.pdf"
        content <- getFileContents file
        liftIO $ BS.writeFile tmpin content
        logInfo "Temp file write" $ object
          [ "bytes_written" .= (BS.length content)
          , "originator" .= ("presealDocumentFile" :: Text)
          ]
        checkboxMapping    <- liftIO $ readCheckboxImagesMapping
        radiobuttonMapping <- liftIO $ readRadiobuttonImagesMapping
        spec               <- presealSpecFromDocument checkboxMapping
                                                      radiobuttonMapping
                                                      document
                                                      (T.pack tmpin)
                                                      (T.pack tmpout)
        runLambdaPresealing tmppath spec

addSealedEvidenceEvents
  :: ( MonadBaseControl IO m
     , MonadDB m
     , MonadLog m
     , TemplatesMonad m
     , MonadIO m
     , DocumentMonad m
     , MonadFileStorage m
     , MonadMask m
     )
  => Actor
  -> m ()
addSealedEvidenceEvents actor = do
  notAddedAttachments <-
    filter (not . authorattachmentaddtosealedfile)
    <$> documentauthorattachments
    <$> theDocument
  forM_ notAddedAttachments $ \a -> do
    contents <- getFileIDContents $ authorattachmentfileid a
    let hash = show $ H.hashWith H.SHA256 contents
    void $ update $ InsertEvidenceEvent
      AuthorAttachmentHashComputed
      (F.value "attachment_name" (authorattachmentname a) >> F.value "hash" hash)
      actor
    return ()
  void $ update $ InsertEvidenceEvent AttachSealedFileEvidence (return ()) actor
  return ()

runLambdaSealing
  :: ( CryptoRNG m
     , DocumentMonad m
     , MonadBaseControl IO m
     , MonadDB m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , PdfToolsLambdaMonad m
     , TemplatesMonad m
     )
  => FilePath
  -> Text
  -> Seal.SealSpec
  -> m ()
runLambdaSealing _tmppath fn spec = do
  now        <- currentTime
  lambdaconf <- getPdfToolsLambdaEnv
  logInfo_ "Sealing document with lambda started"
  (msealedcontent :: Maybe BS.ByteString) <- callPdfToolsSealing lambdaconf spec
  case msealedcontent of
    Just sealedcontent -> do
      logInfo_ "Sealing document with lambda finished"
      sealedfileid <- saveNewFile fn sealedcontent
      dbUpdate $ AppendSealedFile sealedfileid Missing (systemActor now)
    _ -> do
      logAttention_ "Sealing document with lambda failed"
      void $ dbUpdate $ ErrorDocument ErrorSealingDocumentEvidence
                                      (return ())
                                      (systemActor now)


runLambdaPresealing
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , MonadDB m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , PdfToolsLambdaMonad m
     , TemplatesMonad m
     )
  => FilePath
  -> Seal.PreSealSpec
  -> m (Either Text BS.ByteString)
runLambdaPresealing _tmppath spec = do
  lambdaconf     <- getPdfToolsLambdaEnv
  msealedcontent <- callPdfToolsPresealing lambdaconf spec
  case msealedcontent of
    Just sealedcontent -> do
      return $ Right sealedcontent
    _ -> do
      logAttention_ "Presealing in lambda failed"
      -- show JSON'd config as that's what the java app is fed.
      return $ Left "Error when preprinting fields on PDF"
