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
import Data.Maybe
import Data.List
import Data.Ord
import Debug.Trace
import Doc.DocProcess
import Doc.DocStateData
import Doc.Model
import Doc.DocStorage
import Doc.DocView
import Doc.DocUtils
import IPAddress
import Misc
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
import Util.Actor
import Control.Concurrent
import Data.String.Utils
import qualified Templates.Fields as F
import Control.Logic

personFromSignatoryDetails :: (BS.ByteString,BS.ByteString) -> SignatoryDetails -> Seal.Person
personFromSignatoryDetails (checkedBoxImage,uncheckedBoxImage) details =
    Seal.Person { Seal.fullname = getFullName details
                , Seal.company = getCompanyName details
                , Seal.email = getEmail details
                , Seal.personalnumber = getPersonalNumber details
                , Seal.companynumber = getCompanyNumber details
                , Seal.fullnameverified = False
                , Seal.companyverified = False
                , Seal.numberverified = False
                , Seal.emailverified = True
                , Seal.fields = fieldsFromSignatory (checkedBoxImage,uncheckedBoxImage) details
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
   F.value "email"  $ EmailIdentification `elem` (documentallowedidtypes doc)
   F.value "pad"    $ PadIdentification `elem` (documentallowedidtypes doc)

personsFromDocument :: (BS.ByteString,BS.ByteString) -> Document -> [(Seal.Person, SignInfo, SignInfo, Bool, Maybe SignatureProvider, String)]
personsFromDocument (checkedBoxImage,uncheckedBoxImage) document =
    let
        links = filter isSignatory $ documentsignatorylinks document
        x (sl@SignatoryLink { signatorydetails
                            , maybesigninfo = Just signinfo
                            , maybeseeninfo
                            , signatorysignatureinfo
                            })
             -- FIXME: this one should really have seentime always...
             = ((personFromSignatoryDetails (checkedBoxImage,uncheckedBoxImage) signatorydetails)
                { Seal.emailverified = True
                , Seal.fullnameverified = fullnameverified
                , Seal.companyverified = False
                , Seal.numberverified = numberverified}
              , maybe signinfo id maybeseeninfo
              , signinfo
              , isAuthor sl
              , maybe Nothing (Just . signatureinfoprovider) signatorysignatureinfo
              , map head $ words $ getFullName signatorydetails
              )
                  where fullnameverified = maybe False (\s -> signaturefstnameverified s
                                                        && signaturelstnameverified s)
                                                signatorysignatureinfo
                        numberverified = maybe False signaturepersnumverified signatorysignatureinfo

        x link = trace (show link) $ error "SignatoryLink does not have all the necessary data"
    in map x links

fieldsFromSignatory :: (BS.ByteString,BS.ByteString) -> SignatoryDetails -> [Seal.Field]
fieldsFromSignatory (checkedBoxImage,uncheckedBoxImage) SignatoryDetails{signatoryfields} =
  concatMap makeSealField  signatoryfields
  where
    makeSealField :: SignatoryField -> [Seal.Field]
    makeSealField sf = case  sfType sf of
                         SignatureFT -> concatMap (maybeToList . (fieldJPEGFromPlacement (sfValue sf))) (sfPlacements sf)
                         CheckboxOptionalFT _ -> map (uncheckedImageFromPlacement <| null (sfValue sf) |>  checkedImageFromPlacement) (sfPlacements sf)
                         CheckboxObligatoryFT _ -> map (uncheckedImageFromPlacement <| null (sfValue sf) |>  checkedImageFromPlacement) (sfPlacements sf) 
                         _ -> map (fieldFromPlacement (sfValue sf)) (sfPlacements sf)
    fieldFromPlacement sf placement = 
      Seal.Field { Seal.value            = sf
                 , Seal.x                = placementx placement
                 , Seal.y                = placementy placement
                 , Seal.page             = placementpage placement
                 , Seal.w                = placementpagewidth placement
                 , Seal.h                = placementpageheight placement
                 , Seal.includeInSummary = True
                 }
    checkedImageFromPlacement = iconWithPlacement checkedBoxImage
    uncheckedImageFromPlacement = iconWithPlacement uncheckedBoxImage
    iconWithPlacement image placement = Seal.FieldJPG
                 { valueBase64           = BS.toString $ B64.encode image
                 , Seal.x                = placementx placement
                 , Seal.y                = placementy placement
                 , Seal.page             = placementpage placement
                 , Seal.w                = placementpagewidth placement
                 , Seal.h                = placementpageheight placement
                 , Seal.image_w          = 16
                 , Seal.image_h          = 16
                 , Seal.internal_image_w = 16
                 , Seal.internal_image_h = 16
                 , Seal.includeInSummary = False
                 }    
    fieldJPEGFromPlacement v placement =
      case split "|" v of
        [w,h,c] -> do
          wi <- maybeRead w -- NOTE: Maybe monad usage
          hi <- maybeRead h
          Just $ Seal.FieldJPG
                 { valueBase64           = drop 1 $ dropWhile (\e -> e /= ',') c
                 , Seal.x                = placementx placement
                 , Seal.y                = placementy placement
                 , Seal.page             = placementpage placement
                 , Seal.w                = placementpagewidth placement
                 , Seal.h                = placementpageheight placement
                 , Seal.image_w          = wi
                 , Seal.image_h          = hi
                 , Seal.internal_image_w = 4 * wi
                 , Seal.internal_image_h = 4 * hi
                 , Seal.includeInSummary = True
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

sealSpecFromDocument :: (KontraMonad m, MonadIO m, TemplatesMonad m, MonadDB m)
                     => (BS.ByteString,BS.ByteString)
                     -> String 
                     -> Document
                     -> [DocumentEvidenceEvent]
                     -> BS.ByteString
                     -> String
                     -> String
                     -> m Seal.SealSpec
sealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) hostpart document elog content inputpath outputpath =
  let docid = documentid document
      Just authorsiglink = getAuthorSigLink document
      authorHasSigned = isSignatory authorsiglink && isJust (maybesigninfo authorsiglink)
      signatoriesdetails = [signatorydetails sl | sl <- documentsignatorylinks document
                                                , SignatoryPartner `elem` signatoryroles sl]
      authordetails = signatorydetails authorsiglink
      signatories = personsFromDocument (checkedBoxImage,uncheckedBoxImage) document
      secretaries = if authorHasSigned then [] else [personFromSignatoryDetails (checkedBoxImage,uncheckedBoxImage) authordetails]

      persons = map (\(a,_,_,_,_,_) -> a) signatories
      initialsx = map (\(_,_,_,_,_,a) -> a) signatories
      paddeddocid = pad0 20 (show docid)

      initials = concatComma initialsx
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
      invitationSentEntry = case (documentinvitetime document,(sendMailsDurringSigning &&^ hasOtherSignatoriesThenAuthor) document ) of
                                (Just (SignInfo time ipnumber),True) -> do
                                   desc <-  renderLocalTemplate document "_invitationSentEntry" $ do
                                       let partyListButAuthor' = partyListButAuthor document
                                       partylist <- lift $ renderListTemplateNormal . map getSmartName $ partyListButAuthor'
                                       F.value "partyList" partylist
                                       documentInfoFields document
                                       documentAuthorInfo document
                                       F.value "oneSignatory"  (length signatories>1)
                                       F.value "personname" $ listToMaybe $ map getFullName  signatoriesdetails
                                   return  [ Seal.HistEntry
                                      { Seal.histdate = show time
                                      , Seal.histcomment = pureString desc
                                      , Seal.histaddress = "IP: " ++ show ipnumber
                                      }]
                                _ -> return []
      maxsigntime = maximum (map (signtime . (\(_,_,c,_,_,_) -> c)) signatories)
      concatComma = concat . intersperse ", "
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
                       Log.error $ "Cannot read SealingTexts: " ++ staticTexts
                       error $ "Cannot read SealingTexts: " ++ staticTexts
      -- Log.debug ("read texts: " ++ show readtexts)

      -- Creating HTML Evidence Log
      htmllogs <- htmlDocFromEvidenceLog (documenttitle document) elog
      let evidenceattachment = Seal.SealAttachment { Seal.fileName = "evidencelog.html"
                                                   , Seal.fileBase64Content = BS.toString $ B64.encode $ BS.fromString htmllogs }

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
      additionalAttachments <- findOutAttachmentDesc document

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
            , Seal.attachments    = [evidenceattachment]
            , Seal.filesList      =
              [ Seal.FileDesc { fileTitle = documenttitle document
                              , fileRole = mainDocumentText
                              , filePagesText = numberOfPagesText
                              , fileAttachedBy = attachedByText
                              } ] ++ additionalAttachments
            }

presealSpecFromDocument :: (BS.ByteString,BS.ByteString) -> Document -> String -> String -> Seal.PreSealSpec
presealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) document inputpath outputpath =
       Seal.PreSealSpec
            { Seal.pssInput          = inputpath
            , Seal.pssOutput         = outputpath
            , Seal.pssFields         = concatMap (fieldsFromSignatory (checkedBoxImage,uncheckedBoxImage). signatorydetails) (documentsignatorylinks document)
            }
            

sealDocument :: (CryptoRNG m, MonadBaseControl IO m, MonadDB m, KontraMonad m, TemplatesMonad m)
             => Document
             -> m (Either String Document)
sealDocument document = do
  files <- documentfilesM document
  Log.debug $ "Sealing document"
  mapM_ (sealDocumentFile document) files
  Log.debug $ "Sealing should be done now"
  Just newdocument <- dbQuery $ GetDocumentByDocumentID (documentid document)
  return $ Right newdocument


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
            res <- liftIO $ BS.readFile tmpout
            Log.debug $ "GuardTime signed successfully"
            return res
          ExitFailure c -> do
            res <- liftIO $ BS.readFile tmpout
            Log.debug $ "GuardTime failed: " ++ show c 
            return res
        Log.debug $ "Adding new sealed file to DB"
        File{fileid = sealedfileid} <- dbUpdate $ NewFile filename newfilepdf
        Log.debug $ "Finished adding sealed file to DB with fileid " ++ show sealedfileid ++ "; now adding to document"
        res <- dbUpdate $ AttachSealedFile documentid sealedfileid (systemActor ctxtime)
        Log.debug $ "Should be attached to document; is it? " ++ show ((elem sealedfileid . documentsealedfiles) <$> res)
        return res
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
    let config = presealSpecFromDocument (checkedBoxImage,uncheckedBoxImage) document tmpin tmpout
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
        