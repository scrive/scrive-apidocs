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
module Doc.DocSeal(sealDocument, presealDocumentFile) where

import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Ord
import Doc.DocProcess
import Doc.DocStateData
import Doc.Model
import Doc.Rendering
import File.Storage
import Doc.DocView
import Doc.DocUtils
import IPAddress
import Utils.Directory
import Utils.Read
import Utils.String
import Utils.IO
import System.Directory
import System.Exit
import Kontra
import Templates.Templates
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Base64 as B64
import qualified SealSpec as Seal
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
import EvidenceLog.Model
import EvidenceLog.View
import Util.Actor
import Control.Concurrent
import Control.Monad.Trans.Maybe
import Data.String.Utils
import qualified Templates.Fields as F
import Control.Logic
import Utils.Prelude
import qualified Codec.Picture.Png as PNG
import qualified Codec.Picture.Saving as JPG
import Utils.Either


personFromSignatoryDetails :: (BS.ByteString,BS.ByteString) -> SignatoryDetails -> Seal.Person
personFromSignatoryDetails boxImages details =
    Seal.Person { Seal.fullname = getFullName details
                , Seal.company = getCompanyName details
                , Seal.email = getEmail details
                , Seal.personalnumber = getPersonalNumber details
                , Seal.companynumber = getCompanyNumber details
                , Seal.fullnameverified = False
                , Seal.companyverified = False
                , Seal.numberverified = False
                , Seal.emailverified = True
                , Seal.fields = fieldsFromSignatory False [] boxImages details
                }

personFields :: Monad m => Document -> (Seal.Person, SignInfo, SignInfo, Bool, Maybe SignatureProvider, String) -> Fields m ()
personFields doc (person, signinfo,_seeninfo, _ , mprovider, _initials) = do
   F.value "personname" $ Seal.fullname person
   F.value "signip" $  formatIP (signipnumber signinfo)
   F.value "seenip" $  formatIP (signipnumber signinfo)
   F.value "provider" $ isJust mprovider
   F.value "bankid" $ mprovider == Just BankIDProvider
   F.value "nordea" $ mprovider == Just NordeaProvider
   F.value "telia"  $ mprovider == Just TeliaProvider
   F.value "email"  $ documentdeliverymethod doc == EmailDelivery
   F.value "pad"    $ documentdeliverymethod doc == PadDelivery
   F.value "api"    $ documentdeliverymethod doc == APIDelivery

personExFromSignatoryLink :: (BS.ByteString,BS.ByteString)
                          -> SignatoryLink
                          -> (Seal.Person, SignInfo, SignInfo, Bool, Maybe SignatureProvider, String)
personExFromSignatoryLink boxImages (sl@SignatoryLink { signatorydetails
                                                      , maybesigninfo = Just signinfo
                                                      , maybeseeninfo
                                                      , signatorysignatureinfo
                                                      }) =
  ((personFromSignatoryDetails boxImages signatorydetails)
     { Seal.emailverified    = True
     , Seal.fullnameverified = fullnameverified
     , Seal.companyverified  = False
     , Seal.numberverified   = numberverified
     }
    , maybe signinfo id maybeseeninfo -- some old broken documents do not have seeninfo before signinfo
    , signinfo
    , isAuthor sl
    , maybe Nothing (Just . signatureinfoprovider) signatorysignatureinfo
    , map head $ words $ getFullName signatorydetails
    )
  where fullnameverified = maybe False (\s -> signaturefstnameverified s
                                              && signaturelstnameverified s)
                           signatorysignatureinfo
        numberverified = maybe False signaturepersnumverified signatorysignatureinfo

personExFromSignatoryLink _ (SignatoryLink { signatorydetails
                                           , maybesigninfo = Nothing
                                           }) =
 error $ "Person '" ++ getFullName signatorydetails ++ "' hasn't signed yet. Cannot personExFromSignatoryLink for him/her."


fieldsFromSignatory :: Bool -> [(FieldType,String)] -> (BS.ByteString,BS.ByteString) -> SignatoryDetails -> [Seal.Field]
fieldsFromSignatory addEmpty emptyFieldsText (checkedBoxImage,uncheckedBoxImage) SignatoryDetails{signatoryfields} =
  concatMap makeSealField  signatoryfields
  where
    makeSealField :: SignatoryField -> [Seal.Field]
    makeSealField sf = case sfType sf of
       SignatureFT _ -> case (sfPlacements sf) of
                           [] -> maybeToList $ fieldJPEGFromSignatureField (sfValue sf)
                           plsms -> concatMap (maybeToList . (fieldJPEGFromPlacement (sfValue sf))) plsms
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
                 , Seal.image_w          = if placementwrel placement /= 0
                                           then placementwrel placement
                                           else 12 / 943
                 , Seal.image_h          = if placementhrel placement /= 0
                                           then placementhrel placement
                                           else 12 / 1335
                 , Seal.internal_image_w = 24
                 , Seal.internal_image_h = 24
                 , Seal.includeInSummary = False
                 , Seal.onlyForSummary   = False
                 , Seal.keyColor         = Nothing
                 }
    fieldJPEGFromPlacement v placement =
      case split "|" v of
        [_,_,""] -> Nothing
        [w,h,c] -> do
          wi <- maybeRead w -- NOTE: Maybe monad usage
          hi <- maybeRead h
          let content = drop 1 $ dropWhile (\e -> e /= ',') c
          let content' = if ("image/png" `isInfixOf` take 20 c)
                         then BS.toString $ B64.encode $ JPG.imageToJpg 100 $ fromRight $ PNG.decodePng $ fromRight $ B64.decode $ BS.fromString content
                         else content
          Just $ Seal.FieldJPG
                 { valueBase64           = content'
                 , Seal.x                = placementxrel placement
                 , Seal.y                = placementyrel placement
                 , Seal.page             = placementpage placement
                 , Seal.image_w          = if placementwrel placement /= 0
                                           then placementwrel placement
                                           else fromIntegral wi / 943
                 , Seal.image_h          = if placementhrel placement /= 0
                                           then placementhrel placement
                                           else fromIntegral hi / 1335
                 , Seal.internal_image_w = 4 * wi
                 , Seal.internal_image_h = 4 * hi
                 , Seal.includeInSummary = True
                 , Seal.onlyForSummary   = False
                 , Seal.keyColor         = Just (255,255,255) -- white is transparent
                 }
        _ -> Nothing
    fieldJPEGFromSignatureField v =
      case split "|" v of
        [_,_,""] -> Nothing
        [w,h,c] -> do
          wi <- maybeRead w -- NOTE: Maybe monad usage
          hi <- maybeRead h
          let content = drop 1 $ dropWhile (\e -> e /= ',') c
          let content' = if ("image/png" `isInfixOf` take 20 c)
                         then BS.toString $ B64.encode $ JPG.imageToJpg 100 $ fromRight $ PNG.decodePng $ fromRight $ B64.decode $ BS.fromString content
                         else content
          Just $ Seal.FieldJPG
                 { valueBase64           = content'
                 , Seal.x                = 0
                 , Seal.y                = 0
                 , Seal.page             = 0
                 , Seal.image_w          = fromIntegral wi / 943
                 , Seal.image_h          = fromIntegral hi / 1335
                 , Seal.internal_image_w = 4 * wi
                 , Seal.internal_image_h = 4 * hi
                 , Seal.includeInSummary = True
                 , Seal.onlyForSummary   = True
                 , Seal.keyColor         = Just (255,255,255) -- white is transparent
                 }
        _ -> Nothing


listAttachmentsFromDocument :: Document -> [(SignatoryAttachment,SignatoryLink)]
listAttachmentsFromDocument document =
  concatMap extract (documentsignatorylinks document)
  where extract sl = map (\at -> (at,sl)) (signatoryattachments sl)

findOutAttachmentDesc :: (KontraMonad m, MonadIO m, MonadDB m, TemplatesMonad m) => Document -> m [Seal.FileDesc]
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
          mfile <- dbQuery $ GetFileByFileID fileid'
          let name' = maybe "" filename mfile
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

evidenceOfIntentAttachment :: (TemplatesMonad m, MonadDB m) => String -> [SignatoryLink] -> m Seal.SealAttachment
evidenceOfIntentAttachment title sls = do
  ss <- dbQuery $ GetSignatoryScreenshots (map signatorylinkid sls)
  let sortBySignTime = sortBy (on compare (fmap signtime . maybesigninfo . fst))
  html <- evidenceOfIntentHTML title $ sortBySignTime [ (sl, s) | (i, s) <- ss, sl <- filter ((==i) . signatorylinkid) sls ]
  return $ Seal.SealAttachment { Seal.fileName = "EvidenceofIntent.html"
                               , Seal.mimeType = Nothing
                               , Seal.fileBase64Content = BS.toString $ B64.encode $ BS.fromString html
                               }

sealSpecFromDocument :: (KontraMonad m, MonadIO m, TemplatesMonad m, MonadDB m)
                     => (BS.ByteString,BS.ByteString)
                     -> String
                     -> Document
                     -> [DocumentEvidenceEvent]
                     -> BS.ByteString
                     -> String
                     -> String
                     -> m Seal.SealSpec
sealSpecFromDocument boxImages hostpart document elog content inputpath outputpath = do
  additionalAttachments <- findOutAttachmentDesc document
  sigVerFile <- liftIO $ BS.toString <$> B64.encode <$> BS.readFile "files/verification.html"
  evidenceDoc <- liftIO $ BS.toString <$> B64.encode <$> BS.readFile "files/evidenceDocumentation.html"
  sealSpecFromDocument2 boxImages hostpart document elog content inputpath outputpath additionalAttachments sigVerFile evidenceDoc

sealSpecFromDocument2 :: (TemplatesMonad m, MonadDB m)
                     => (BS.ByteString,BS.ByteString)
                     -> String
                     -> Document
                     -> [DocumentEvidenceEvent]
                     -> BS.ByteString
                     -> String
                     -> String
                     -> [Seal.FileDesc]
                     -> String
                     -> String
                     -> m Seal.SealSpec
sealSpecFromDocument2 boxImages hostpart document elog content inputpath outputpath additionalAttachments sigVerFile evidenceDoc =
  let docid = documentid document
      Just authorsiglink = getAuthorSigLink document

      authordetails = signatorydetails authorsiglink
      signatories = [ personExFromSignatoryLink boxImages s
                        | s <- documentsignatorylinks document
                        , signatoryispartner $ signatorydetails s
                    ]

      secretaries = [ personFromSignatoryDetails boxImages $ signatorydetails s
                        | s <- documentsignatorylinks document
                        , not . signatoryispartner $ signatorydetails s
                    ]

      persons = map (\(a,_,_,_,_,_) -> a) signatories
      initialsx = map (\(_,_,_,_,_,a) -> a) signatories
      paddeddocid = pad0 20 (show docid)

      initials = intercalate ", " initialsx
      makeHistoryEntryFromSignatory personInfo@(_ ,seen, signed, isauthor, _, _)  = do
          seenDesc <- renderLocalTemplate document "_seenHistEntry" $ do
                        personFields document personInfo
                        documentInfoFields document
          let seenEvent = Seal.HistEntry
                            { Seal.histdate = show (signtime seen)
                            , Seal.histcomment = pureString seenDesc
                            , Seal.histaddress = "IP: " ++ show (signipnumber seen)
                            }
          signDesc <- renderLocalTemplate document "_signHistEntry" $ do
                        personFields document personInfo
                        documentInfoFields document
          let signEvent = Seal.HistEntry
                            { Seal.histdate = show (signtime signed)
                            , Seal.histcomment = pureString signDesc
                            , Seal.histaddress = "IP: " ++ show (signipnumber signed)
                            }
          return $ if (isauthor)
                    then [signEvent]
                    else [seenEvent,signEvent]
      invitationSentEntry =
        if (sendMailsDuringSigning &&^ hasOtherSignatoriesThenAuthor) document
           then case documentinvitetime document of
                  Just (SignInfo time ipnumber) -> do
                    -- Here we need to sort signing signatories according to sign order,
                    -- then group them by sign order. Invitation to next group is sent
                    -- when the previous group is done signing, so maxSignTime of prev.

                    let groupOn f = groupBy (\a b -> f a == f b)
                    let sortOn f = sortBy (\a b -> compare (f a) (f b))
                    let sortedPeople =
                          groupOn (signatorysignorder . signatorydetails) .
                          sortOn (signatorysignorder . signatorydetails) .
                          filter (not . signatoryisauthor . signatorydetails ||^ (/= SignOrder 1) . signatorysignorder . signatorydetails) .
                          filter (signatoryispartner . signatorydetails) .
                          documentsignatorylinks $
                          document

                    descs <- flip mapM sortedPeople $ \people ->
                      renderLocalTemplate document "_invitationSentEntry" $ do
                        partylist <- lift $ renderListTemplateNormal . map getSmartName $ people
                        F.value "partyList" partylist
                        documentInfoFields document
                        documentAuthorInfo document
                        case people of
                          [person] -> do
                            F.value "oneSignatory" True
                            F.value "personname" $ getFullName person
                          _ -> return ()
                    -- times is offset by one in position wrt
                    -- sortedPeople last element of times is actually
                    -- ignored here
                    let times = time : map (maximum . map (signtime . fromJust . maybesigninfo)) sortedPeople
                    let mkEntry time' desc = Seal.HistEntry
                                            { Seal.histdate = show time'
                                            , Seal.histcomment = pureString desc
                                            , Seal.histaddress = "IP: " ++ show ipnumber
                                            }
                    return $ zipWith mkEntry times descs
                  _ -> do
                    -- document does not have documentinvitetime, what
                    -- does it mean really?
                    return []
          else do
            -- either emails were not sent at all during document
            -- signing or there was nobody to sent invitation to
            return []
      maxsigntime = maximum (map (signtime . (\(_,_,c,_,_,_) -> c)) signatories)
      -- document fields
      lastHistEntry = do
                       desc <- renderLocalTemplate document "_lastHistEntry" (documentInfoFields document)
                       return $ [Seal.HistEntry
                                { Seal.histdate = show maxsigntime
                                , Seal.histcomment = pureString desc
                                , Seal.histaddress = ""
                                }]

  in do
      -- Log.debug "Creating seal spec from file."
      events <- fmap concat $ sequence $
                    (map makeHistoryEntryFromSignatory signatories) ++
                    [invitationSentEntry] ++
                    [lastHistEntry]
      -- Log.debug ("events created: " ++ show events)
      -- here we use Data.List.sort that is *stable*, so it puts
      -- signatories actions before what happened with a document
      let history = sortBy (comparing Seal.histdate) events
      -- Log.debug ("about to render staticTexts")
      staticTexts <- renderLocalTemplateForProcess document processsealingtext $ do
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
      htmllogs <- htmlDocFromEvidenceLog (documenttitle document) elog
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
        F.value "author" (getSmartName authordetails)

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
            , Seal.pssFields         = concatMap (fieldsFromSignatory True emptyFieldsText boxImages . signatorydetails) (documentsignatorylinks document)
            }


sealDocument :: (CryptoRNG m, MonadBaseControl IO m, MonadDB m, KontraMonad m, TemplatesMonad m)
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


sealDocumentFile :: (CryptoRNG m, MonadBaseControl IO m, MonadDB m, KontraMonad m, TemplatesMonad m)
                 => Document
                 -> File
                 -> m (Either String Document)
sealDocumentFile document@Document{documentid} file@File{fileid, filename} =
  withSystemTempDirectory' ("seal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
    Context{ctxhostpart, ctxtime, ctxgtconf} <- getContext
    elog <- dbQuery $ GetEvidenceLog documentid
    Log.debug ("sealing: " ++ show fileid)
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- getFileContents file
    liftIO $ BS.writeFile tmpin content
    checkedBoxImage <- liftIO $ BS.readFile "public/img/checkbox_checked.jpg"
    uncheckedBoxImage <- liftIO $  BS.readFile "public/img/checkbox_unchecked.jpg"
    config <- sealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) ctxhostpart document elog content tmpin tmpout
    Log.debug $ "Config " ++ show config
    (code,_stdout,stderr) <- liftIO $ readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))
    liftIO $ threadDelay 500000

    Log.debug $ "Sealing completed with " ++ show code
    case code of
      ExitSuccess -> do
        -- GuardTime signs in place
        code2 <- liftIO $ GT.digitallySign ctxgtconf tmpout
        newfilepdf <- Binary <$> case code2 of
          ExitSuccess -> do
            vr <- liftIO $ GT.verify tmpout
            case vr of
                 GT.Valid _ _ -> do
                      res <- liftIO $ BS.readFile tmpout
                      Log.debug $ "GuardTime signed successfully #" ++ show documentid
                      return res
                 _ -> do
                      res <- liftIO $ BS.readFile tmpout
                      Log.debug $ "GuardTime verification after signing failed for document #" ++ show documentid
                      Log.error $ "GuardTime verification after signing failed for document #" ++ show documentid
                      return res
          ExitFailure c -> do
            res <- liftIO $ BS.readFile tmpout
            Log.debug $ "GuardTime failed " ++ show c ++ " of document #" ++ show documentid
            Log.error $ "GuardTime failed for document #" ++ show documentid
            return res
        Log.debug $ "Adding new sealed file to DB"
        File{fileid = sealedfileid} <- dbUpdate $ NewFile filename newfilepdf
        Log.debug $ "Finished adding sealed file to DB with fileid " ++ show sealedfileid ++ "; now adding to document"
        res <- runMaybeT $ do
          dbUpdate $ AttachSealedFile documentid sealedfileid $ systemActor ctxtime
          Just doc <- dbQuery $ GetDocumentByDocumentID documentid
          return doc
        Log.debug $ "Should be attached to document; is it? " ++ show (((Just sealedfileid==) . documentsealedfile) <$> res)
        return $ maybe (Left "AttachSealedFile failed") Right res
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
        return $ Left msg


-- | Generate file that has all placements printed on it. It will look same as final version except for footers and verification page.
presealDocumentFile :: (MonadBaseControl IO m, MonadDB m, KontraMonad m, TemplatesMonad m)
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
    Log.debug $ "Config " ++ show config
    (code,_stdout,stderr) <- liftIO $ readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))
    liftIO $ threadDelay 500000
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
  fstname <- renderTemplate_ "fstnameEmptyFieldsText"
  sndname <- renderTemplate_ "sndnameEmptyFieldsText"
  email <- renderTemplate_ "emailEmptyFieldsText"
  company <- renderTemplate_ "companyEmptyFieldsText"
  companynumber <- renderTemplate_ "companynumberEmptyFieldsText"
  personalnumber <- renderTemplate_  "personalnumberEmptyFieldsText"
  return [(FirstNameFT, fstname),
          (LastNameFT, sndname),
          (CompanyFT, company),
          (PersonalNumberFT, personalnumber),
          (CompanyNumberFT,companynumber),
          (EmailFT, email)]
