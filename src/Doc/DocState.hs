{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.DocState
    ( module Doc.DocStateData
    , isTemplate -- fromUtils
    , isShared -- fromUtils
    , isDeletableDocument -- fromUtils
    , anyInvitationUndelivered
    , undeliveredSignatoryLinks
    , ArchiveDocuments(..)
    , ArchiveDocumentForAll(..)
    , RestoreArchivedDocuments(..)
    , ReallyDeleteDocuments(..)
    , DeleteDocumentRecordIfRequired(..)
    , AttachFile(..)
    , AttachSealedFile(..)
    , AuthorSignDocument(..)
    , AuthorSendDocument(..)
    , RejectDocument(..)
    , FileModTime(..)
    , FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , GetDocumentByDocumentID(..)
    , GetDocumentStats(..)
    , GetDocumentStatsByUser(..)
    , GetDocuments(..)
    , GetDocumentsByAuthor(..)
    , GetDocumentsBySignatory(..)
    , GetDocumentsByCompany(..)
    , GetDocumentsSharedInCompany(..)
    , GetDocumentsByUser(..)
    , GetDeletedDocumentsByCompany(..)
    , GetDeletedDocumentsByUser(..)
    , GetFilesThatShouldBeMovedToAmazon(..)
    , GetNumberOfDocumentsOfUser(..)
    , GetTimeoutedButPendingDocuments(..)
    , MarkDocumentSeen(..)
    , MarkInvitationRead(..)
    , SetInvitationDeliveryStatus(..)
    , NewDocument(..)
    , SaveDocumentForUser(..)
    , SetDocumentTimeoutTime(..)
    , SetDocumentTags(..)
    , SetDocumentUI(..)
    , GetDocumentsByCompanyAndTags(..)
    , SetDocumentTrustWeaverReference(..)
    , ShareDocument(..)
    , SetDocumentTitle(..)
    , SignDocument(..)
    , TimeoutDocument(..)
    , UpdateDocument(..)
    , UpdateDocumentSimple(..)
    , AttachCSVUpload(..)
    , GetDocumentsByDocumentID(..)
    , UpdateDocumentAttachments(..)
    , CloseDocument(..)
    , CancelDocument(..)
    , RestartDocument(..)
    , ChangeSignatoryEmailWhenUndelivered(..)
    , signatoryDetailsFromUser
    , GetUniqueSignatoryLinkID(..)
    , GetMagicHash(..)
    , GetDocumentByFileID(..)
    , ErrorDocument(..)
    , TemplateFromDocument(..)
    , SignableFromDocument(..)
    , SignableFromDocumentIDWithUpdatedAuthor(..)
    , DocumentFromSignatoryData(..)
    , UpdateSigAttachments(..)
    , SaveSigAttachment(..)
    --, MigrateDocumentSigAccounts(..)
    , MigrateDocumentSigLinkCompanies(..)
    , FixBug510ForDocument(..)
    , StoreDocumentForTesting(..)
    , SignLinkFromDetailsForTest(..)
    , DeleteSigAttachment(..)
    , GetSignatoryLinkIDs(..)
    , AdminOnlySaveForUser(..)
    )
where

import API.Service.Model
import Company.Model
import Control.Monad
import Control.Monad.Reader (ask)
import Data.List (find)
import Data.Maybe
import Data.Word
import DB.Types
import Doc.DocProcess
import Doc.DocStateData
import Doc.DocStateUtils
import Doc.DocUtils
import Happstack.Data.IxSet as IxSet hiding (null)
import Happstack.State
import Mails.MailsUtil
import MinutesTime
import Misc
import User.Model
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import Util.SignatoryLinkUtils
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

{- |
    All documents for the given service.  This does not include
    documents where documentdeleted = True.
-}
getDocuments:: (Maybe ServiceID) -> Query Documents [Document]
getDocuments mservice = queryDocs $ \documents ->
    toList $ documents @= mservice

{- |
    Document for the given ID.  This would return a document even if
    documentdeleted = True.
-}
getDocumentByDocumentID :: DocumentID -> Query Documents (Maybe Document)
getDocumentByDocumentID documentid = queryDocs $ \documents ->
    getOne $ documents @= documentid

{- |
    Filter documents according to whether the given indicated sig link has even been deleted.
    The first param should be True to get the docs which have been deleted,
    and so would appear in the recycle bin//trash can.  It should be False to get docs which
    have never been deleted.
-}
filterDocsWhereDeleted :: (SignatoryLinkIdentity a) => Bool -> a -> [Document] -> [Document]
filterDocsWhereDeleted deleted siglinkidentifier docs =
  filter isIncludedDoc docs
  where
    isIncludedDoc Document{documentsignatorylinks} = 
      not . Prelude.null $ filter isIncludedSigLink documentsignatorylinks
    isIncludedSigLink sl@SignatoryLink{signatorylinkdeleted} =
      isSigLinkFor siglinkidentifier sl && signatorylinkdeleted==deleted

{- |
    All documents authored by the user that have never been deleted.
-}
getDocumentsByAuthor :: UserID -> Query Documents [Document]
getDocumentsByAuthor userid = queryDocs $ \documents ->
    filterDocsWhereDeleted False userid $ IxSet.toList (documents @= Author (userid))

{- |
    All documents which are saved for the user which have never been deleted.
    This doesn't respect sign order, so should be used carefully.
    This also makes sure that the documents match the user's service.
-}
getDocumentsByUser :: User -> Query Documents [Document]
getDocumentsByUser User{userid, userservice} = queryDocs $ \documents ->
  filterDocsWhereDeleted False userid $ IxSet.toList $ (documents @= userid) @= userservice

{- |
    Filter documents according to whether the indicated signatory link has been activated
    according to the sign order.  Author links are included either way.
-}
filterDocsWhereActivated :: SignatoryLinkIdentity a => a -> [Document] -> [Document]
filterDocsWhereActivated siglinkidentifier docs =
  filter isIncludedDoc docs
  where
    isIncludedDoc doc@Document{documentsignatorylinks} = 
      not . Prelude.null $ filter (isIncludedSigLink doc) documentsignatorylinks
    isIncludedSigLink doc 
                      sl@SignatoryLink{signatorylinkdeleted, 
                                       signatoryroles, 
                                       signatorydetails} =
      let isRelevant = isSigLinkFor siglinkidentifier sl && not signatorylinkdeleted
          isAuthorLink = SignatoryAuthor `elem` signatoryroles
          isActivatedForSig = (documentcurrentsignorder doc) >= (signatorysignorder signatorydetails)
      in  isRelevant && (isAuthorLink || isActivatedForSig)

{- |
    All documents where the user is a signatory that are not deleted.  An author is a type
    of signatory, so authored documents are included too.
    This also filters so that documents where a user is a signatory, but that signatory
    has not yet been activated according to the document's sign order, are excluded.
-}
getDocumentsBySignatory :: User -> Query Documents [Document]
getDocumentsBySignatory User{userid, userservice} = queryDocs $ \documents ->
  filterDocsWhereActivated userid . filterDocsWhereDeleted False userid $ 
    IxSet.toList $ (documents @= (Signatory userid)) @= userservice

{- |
    All documents for a company that are not deleted.  This only returns docs that the user
    has admin rights to.
    This filters in a similar way as getDocumentsBySignatory so that documents returned would
    have been activated if there was a sign order on the document.
-}
getDocumentsByCompany :: User -> Query Documents [Document]
getDocumentsByCompany User{useriscompanyadmin, usercompany, userservice} = 
  case (useriscompanyadmin, usercompany) of
    (True, Just companyid) -> do
      queryDocs $ \documents ->
        filterDocsWhereActivated companyid . filterDocsWhereDeleted False companyid $ 
          IxSet.toList $ (documents @= companyid) @= userservice
    _ -> return []

{- |
    All documents for a company that are deleted.  This also takes care to match the documents
    with the user's service.  This only returns docs that the user has
    admin rights to.
-}
getDeletedDocumentsByCompany :: User -> Query Documents [Document]
getDeletedDocumentsByCompany User{useriscompanyadmin, usercompany, userservice} = 
  case (useriscompanyadmin, usercompany) of
    (True, Just companyid) -> do
      queryDocs $ \documents ->
        filterDocsWhereDeleted True companyid $ 
          IxSet.toList $ (documents @= companyid) @= userservice
    _ -> return []

{- |
    All documents which are saved for the user which have been deleted.  This also takes care to match the documents
    with the user's service.
    This also makes sure that the documents match the user's service.
-}
getDeletedDocumentsByUser :: User -> Query Documents [Document]
getDeletedDocumentsByUser User{userid, userservice} = queryDocs $ \documents ->
  filterDocsWhereDeleted True userid $ IxSet.toList $ (documents @= userid) @= userservice

{- |
    All documents that have been authored within the company and which are shared.
-}
getDocumentsSharedInCompany :: User -> Query Documents [Document]
getDocumentsSharedInCompany User{usercompany, userservice} =
  case usercompany of
    (Just companyid) -> do
      queryDocs $ \documents ->
        filter ((== Shared) . documentsharing) . filterDocsWhereActivated companyid . filterDocsWhereDeleted False companyid $ 
          IxSet.toList $ (documents @= companyid) @= userservice
    _ -> return []

{- |
    All documents which are in a pending state and have a timeout time that comes
    before the given time.
-}
getTimeoutedButPendingDocuments :: MinutesTime -> Query Documents [Document]
getTimeoutedButPendingDocuments now = queryDocs $ \documents ->
  IxSet.toList $ (documents @= Pending) @< TimeoutTime now

{- |
    Determines whether a new document should have either Advanced or Basic
    functionality according to the document's type and the user's preferences.
-}
newDocumentFunctionality :: DocumentType -> User -> DocumentFunctionality
newDocumentFunctionality documenttype user =
  case (getValueForProcess documenttype processbasicavailable, 
        preferreddesignmode $ usersettings user) of
    (Just True, Nothing) -> BasicFunctionality
    (Just True, Just BasicMode) -> BasicFunctionality
    _ -> AdvancedFunctionality

{- |
    Creates and saves a new document that's authored by the given user,
    and doesn't belong to a company.
    If the given user and company don't match then a Left is returned.
-}
newDocument :: User
            -> Maybe Company
            -> BS.ByteString
            -> DocumentType
            -> MinutesTime
            -> Update Documents (Either String Document)
newDocument user mcompany title documenttype ctime =
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do
      let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                        then [SignatoryAuthor]
                        else [SignatoryPartner, SignatoryAuthor]
      authorlink0 <- (signLinkFromDetails
                      (signatoryDetailsFromUser user mcompany)
                      authorRoles)

      let authorlink = authorlink0 {
                         maybesignatory = Just $ userid user, 
                         maybecompany = usercompany user }

      let doc = blankDocument {
                  documenttitle                = title
                , documentsignatorylinks       = [authorlink]
                , documenttype                 = documenttype
                , documentfunctionality        = newDocumentFunctionality documenttype user
                , documentctime                = ctime
                , documentmtime                = ctime
                , documentservice              = userservice user
                , documentauthorattachments    = []
                , documentsignatoryattachments = []
                } `appendHistory` [DocumentHistoryCreated ctime]

      inserteddoc <- insertNewDocument doc
      return $ Right inserteddoc 

{- |
    A blank document containing default values that need to be set before
    saving.
-}
blankDocument :: Document
blankDocument =
          Document
          { documentid                   = DocumentID 0
          , documenttitle                = BS.empty
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentfunctionality        = BasicFunctionality
          , documentctime                = fromSeconds 0 
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = Nothing
          , documenttimeouttime          = Nothing
          , documentlog                  = []
          , documentinvitetext           = BS.empty
          , documentsealedfiles          = []
          , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = []
          , documentcsvupload            = Nothing
          , documentcancelationreason    = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Private
          , documentrejectioninfo        = Nothing
          , documenttags                 = []
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          , documentsignatoryattachments = []
          , documentattachments          = []
          }

fileMovedToAWS :: FileID
               -> BS.ByteString
               -> BS.ByteString
               -> Update Documents ()
fileMovedToAWS fileid bucket url = fileMovedTo fileid $ FileStorageAWS bucket url

fileMovedToDisk :: FileID -> FilePath -> Update Documents ()
fileMovedToDisk fileid filepath = fileMovedTo fileid $ FileStorageDisk filepath

fileMovedTo :: FileID -> FileStorage -> Update Documents ()
fileMovedTo fid fstorage = do
    documents <- ask
    let docs = toList (documents @= fid)
    mapM_ (\doc -> modifySignableOrTemplate (documentid doc) moved) docs
    where
    moved doc@Document{documentfiles, documentsealedfiles, documentsignatoryattachments, documentauthorattachments} =
        Right $ doc { documentfiles = map moved1 documentfiles
                    , documentsealedfiles = map moved1 documentsealedfiles
                    , documentsignatoryattachments = map movedsig documentsignatoryattachments
                    , documentauthorattachments = map movedaut documentauthorattachments
                    }
    moved1 file@File{ fileid
                    , filestorage = FileStorageMemory _
                    } | fileid == fid =
                                 file { filestorage = fstorage }
                      | otherwise = file
    moved1 file = file
    movedsig sa@SignatoryAttachment{signatoryattachmentfile=Just file} = sa {signatoryattachmentfile = Just (moved1 file)}
    movedsig sa = sa
    movedaut aa@AuthorAttachment{authorattachmentfile = file} = aa {authorattachmentfile = moved1 file}

{- |
    Gets the document for the given FileID.  This includes documents where
    documentdeleted = True.  If a document cannot be found then this is counted
    as an error and a Left is returned.
 -}
getDocumentByFileID :: FileID -> Query Documents (Either String Document)
getDocumentByFileID fileid' = queryDocs $ \documents ->
  case toList (documents @= fileid') of
    [] -> Left $ "cannot find document for file #" ++ show fileid'
    (document:_) -> Right document

{- |
    Attaches a file to the indicated document.
    If there is a problem, such as the document not existing,
    then a Left is returned.
-}
attachFile :: DocumentID
           -> BS.ByteString
           -> BS.ByteString
           -> Update Documents (Either String Document)
attachFile documentid filename1 content = do
  documents <- ask
  fileid2 <- getUnique documents FileID
  modifySignableOrTemplate documentid $ \document ->
      let nfile = File { fileid = fileid2
                       , filename = filename1
                       , filestorage = FileStorageMemory content
                       }
      in Right $ document { documentfiles = documentfiles document ++ [nfile] }

{- |
    Attaches a sealed file to the indicated document.
    If there is a problem, such as the document not existing,
    or the document not being a signable then a Left is returned.
-}

attachSealedFile :: DocumentID
                 -> BS.ByteString
                 -> BS.ByteString
                 -> Update Documents (Either String Document)
attachSealedFile documentid filename1 content = do
  documents <- ask
  fileid2 <- getUnique documents FileID
  modifySignable documentid $ \document ->
      let nfile = File { fileid = fileid2
                       , filename = filename1
                       , filestorage = FileStorageMemory content
                       }
      in Right $ document { documentsealedfiles = documentsealedfiles document ++ [nfile] }

{- |
    Updates an existing document, typically this stores information collected
    from the doc design view.  If there is a problem, such as the document not existing,
    or the document not being in preparation mode then a Left is returned.
-}
updateDocument :: MinutesTime
               -> DocumentID
               -> BS.ByteString
               -> [(SignatoryDetails,[SignatoryRole])]
               -> Maybe Int
               -> BS.ByteString
               -> (SignatoryDetails, [SignatoryRole], UserID, Maybe CompanyID)
               -> [IdentificationType]
               -> Maybe Int
               -> DocumentFunctionality
               -> Update Documents (Either String Document)
updateDocument time documentid docname signatories daystosign invitetext (authordetails, authorroles, authorid, mcompanyid) idtypes mcsvsigindex docfunctionality =
    modifySignableOrTemplateWithAction documentid $ \document ->
        if documentstatus document == Preparation
         then do
             authorlink0 <- signLinkFromDetails authordetails authorroles
             let authorlink = authorlink0 { --do we need to be doing this?  surely the author id stays the same throughout
                                maybesignatory = Just authorid,
                                maybecompany = mcompanyid
                              }
             signatorylinks <- sequence $ map (uncurry $ signLinkFromDetails) signatories
             let alllinks = authorlink : signatorylinks
                 csvupload = case (documentcsvupload document,
                                   fmap (checkCSVSigIndex alllinks) mcsvsigindex) of
                               (Just cu, Just (Right newsigindex))
                                 -> Just cu{csvsignatoryindex=newsigindex}
                               _ -> Nothing
                 updatedFstFileName  = case (documentfiles document) of
                                         (f:fs) -> (f {filename= docname} :fs)
                                         fs -> fs
                 isbasic = BasicFunctionality == docfunctionality
             return $ Right $ document
                    { documentsignatorylinks         = alllinks
                    , documentdaystosign             = daystosign
                    , documentmtime                  = time
                    , documenttitle                  = docname
                    , documentinvitetext             = invitetext
                    , documentallowedidtypes         = idtypes
                    , documentcsvupload              = if isbasic then Nothing else csvupload
                    , documentfunctionality          = docfunctionality
                    , documentfiles                  = updatedFstFileName
                    , documentauthorattachments      = if isbasic then [] else documentauthorattachments document
                    , documentsignatoryattachments   = if isbasic then [] else documentsignatoryattachments document
                    }
         else return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be in Preparation to use updateDocument"

{- |
    A cut down version of updateDocument that requires fewer parameters.  This is used by the integration api.
    If there is a problem, such as the document not existing,
    or the document not being in preparation mode then a Left is returned.
-}
updateDocumentSimple::DocumentID -> (SignatoryDetails, User) -> [SignatoryDetails] -> Update Documents (Either String Document)
updateDocumentSimple did (authordetails,author) signatories = do
   now <- getMinuteTimeDB
   modifySignableOrTemplateWithAction did $ \document ->
        if documentstatus document == Preparation
         then do
             let authorroles = 
                   case getValueForProcess document processauthorsend of
                     Just True -> [SignatoryAuthor]
                     _ -> [SignatoryPartner,SignatoryAuthor]
             authorlink0 <- signLinkFromDetails authordetails authorroles
             let authorlink = authorlink0 {
                                maybesignatory = Just $ userid author,
                                maybecompany = usercompany author
                              }
             signatorylinks <- sequence $ map (flip signLinkFromDetails [SignatoryPartner]) signatories
             let alllinks = authorlink : signatorylinks
             return $ Right $ document
                    { documentsignatorylinks         = alllinks
                    , documentmtime                  = now
                    , documentallowedidtypes         = [EmailIdentification]
                    }
         else return $ Left "Document not in preparation"

{- |
    This attaches a csv upload to the document.
    If there is a problem, such as the document not existing,
    or the document not being in preparation mode then a Left is returned.
-}
attachCSVUpload :: DocumentID
                   -> CSVUpload
                   -> Update Documents (Either String Document)
attachCSVUpload documentid csvupload =
    modifySignableOrTemplateWithAction documentid $ \document ->
        let msigindex = checkCSVSigIndex
                                         (documentsignatorylinks document)
                                         (csvsignatoryindex csvupload) in
        case (msigindex, documentstatus document) of
          (Left s, _) -> return $ Left s
          (Right _, Preparation) -> return . Right $ document { documentcsvupload = Just csvupload }
          _ -> return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be in Preparation to use attachCSVUpload"

{- |
    A bulk version of getDocumentsByDocumentID, this will get a list of
    documents corresponding to the given list of document ids.  This would return documents
    where documentdeleted = True.  If one of the documents doesn't exist then this returns
    a Left.
-}
getDocumentsByDocumentID :: [DocumentID] -> Query Documents (Either String [Document])
getDocumentsByDocumentID docids = queryDocs $ \documents ->
  let relevantdocs = documents @+ docids in
  if (length docids == size relevantdocs)
    then Right . toList $ relevantdocs
    else Left "documents don't exist for all the given ids"

{- |
   Add and remove attachments to a document in Preparation.
   If there is a problem, such as the document not existing, or
   the document not being in preparation mode then a Left is returned.
-}
updateDocumentAttachments :: DocumentID
                          -> [DocumentID]
                          -> [FileID]
                          -> Update Documents (Either String Document)
updateDocumentAttachments docid idstoadd idstoremove = do
  documents <- ask
  let addattachments   = toList $ documents @+ idstoadd
      foundattachments = length idstoadd == length addattachments
      newattachment Document{ documenttitle, documentfiles = (file:_) }
        = Just AuthorAttachment{ authorattachmentfile = file { filename = documenttitle } }
      newattachment _ = Nothing
      addfiles = catMaybes $ map newattachment addattachments
  case foundattachments of
    False -> return $ Left "documents don't exist for all the given ids"
    True -> modifySignableOrTemplate docid $ \doc ->
      case documentstatus doc of
        Preparation -> Right doc { documentauthorattachments =
                                      [da | da <- documentauthorattachments doc,
                                       not ((fileid $ authorattachmentfile da) `elem` idstoremove)]
                                      ++
                                      addfiles
                                 }
        _ -> Left "Can only attach to document in Preparation"

{- |
    Creates a new document by copying an existing document pumping some values into a particular signatory.
    This is used as part of the csv functionality.
    If there is a problem, such as the document not existing, then a Left is returned.
-}
documentFromSignatoryData :: DocumentID
                              -> Int
                              -> BS.ByteString
                              -> BS.ByteString
                              -> BS.ByteString
                              -> BS.ByteString
                              -> BS.ByteString
                              -> BS.ByteString
                              -> [BS.ByteString]
                              -> Update Documents (Either String Document)
documentFromSignatoryData docid sigindex fstname sndname email company personalnumber companynumber fieldvalues = newFromDocument toNewDoc docid
  where
    toNewDoc :: Document -> Document
    toNewDoc d = d { documentsignatorylinks = map snd . map toNewSigLink . zip [0..] $ (documentsignatorylinks d)
                    , documentcsvupload = Nothing
                    , documentsharing = Private
                    , documenttype = newDocType $ documenttype d
                    , documentsignatoryattachments = map replaceCSV (documentsignatoryattachments d)
                    }
    replaceCSV :: SignatoryAttachment -> SignatoryAttachment
    replaceCSV sa =
      if signatoryattachmentemail sa == BS.fromString "csv"
      then sa { signatoryattachmentemail = email }
      else sa
    newDocType :: DocumentType -> DocumentType
    newDocType (Signable p) = Signable p
    newDocType (Template p) = Signable p
    newDocType dt = dt
    toNewSigLink :: (Int, SignatoryLink) -> (Int, SignatoryLink)
    toNewSigLink (i, sl)
      | i==sigindex = (i, pumpData sl)
      | otherwise = (i, sl)
    pumpData :: SignatoryLink -> SignatoryLink
    pumpData siglink = replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues

{- |
    Timeouts the indicated document.  If there is a problem, such as the document not existing,
    or the document not being in a pending status, then a Left will be returned.
-}
timeoutDocument :: DocumentID
                -> MinutesTime
                -> Update Documents (Either String Document)
timeoutDocument documentid time = do
  modifySignable documentid $ \document ->
      let
          newdocument = document { documentstatus = Timedout } `appendHistory` [DocumentHistoryTimedOut time]
      in case documentstatus document of
           Pending -> Right newdocument
           _ -> Left "Illegal document status change"

{- |
    Signs a particular signatory link.  If there is a problem, such as the document not existing,
    or the document not being pending, or having timedout then a Left is returned.
-}
signDocument :: DocumentID
             -> SignatoryLinkID
             -> MinutesTime
             -> Word32
             -> Maybe SignatureInfo
             -> [(BS.ByteString, BS.ByteString)]
             -> Update Documents (Either String Document)
signDocument documentid signatorylinkid1 time ipnumber msiginfo fields = do
  modifySignable documentid $ \document ->
    let signeddocument = document { documentsignatorylinks = newsignatorylinks
                                  } `appendHistory` [DocumentHistorySigned time ipnumber (signatorydetails signatoryLink)]
        Just signatoryLink = getSigLinkFor document signatorylinkid1
        newsignatorylinks = map maybesign (documentsignatorylinks document)
        maybesign link@SignatoryLink{signatorylinkid, signatorydetails = details@SignatoryDetails{signatoryfields}}
          | signatorylinkid == signatorylinkid1 =
            link { maybesigninfo = Just (SignInfo time ipnumber)
                 , signatorydetails = details {
                     signatoryfields = map (updateSigField fields) signatoryfields
                   }
                 , signatorysignatureinfo = msiginfo
                 }
        maybesign link = link
        allbutauthor = [sl | sl <- newsignatorylinks
                           , not $ isAuthor sl]
        signatoryHasSigned x = not (SignatoryPartner `elem` signatoryroles x) || isJust (maybesigninfo x)
        allsignedbutauthor = all signatoryHasSigned allbutauthor
        isallsigned = all signatoryHasSigned newsignatorylinks

        -- Check if there are custom fields in any signatory (that is, not author)
        -- ??: We don't use this anymore? -EN
        -- hasfields = any ((any (not . fieldfilledbyauthor)) . (signatoryotherfields . signatorydetails)) (documentsignatorylinks document)

        updateSigField sfields sf =
          case sfType sf of
            CompanyFT -> updateF $ BS.pack "sigco"
            PersonalNumberFT -> updateF $ BS.pack "sigpersnr"
            CompanyNumberFT -> updateF $ BS.pack "sigcompnr"
            CustomFT label _ -> updateF label
            _ -> sf
          where
            updateF = maybe sf (\v -> sf { sfValue = v }) . flip lookup sfields

        signeddocument2 =
              if isallsigned
              then signeddocument { documentstatus = Closed } `appendHistory` [DocumentHistoryClosed time ipnumber]
              else if allsignedbutauthor
                   then signeddocument { documentstatus = AwaitingAuthor }
                   else signeddocument

    in case documentstatus document of
      Pending ->  Right signeddocument2
      Timedout -> Left "FÃ¶rfallodatum har passerat"
      _ ->        Left ("Bad document status: " ++ show (documentstatus document))

{- |
    A helper function that signs the given signatory link.
-}
signWithUserID :: [SignatoryLink]
                  -> UserID
                  -> Maybe SignInfo
                  -> Maybe SignatureInfo
                  -> [SignatoryLink]
signWithUserID [] _ _ _ = []
signWithUserID (s:ss) uid sinfo msiginfo
    | maybe False (((==) uid)) (maybesignatory s) = s {maybesigninfo = sinfo, maybeseeninfo = maybe sinfo Just (maybeseeninfo s) , signatorysignatureinfo = msiginfo} : ss
    | otherwise = s : signWithUserID ss uid sinfo msiginfo

{- |
    Called when an author sends a document.  This will return Left when there are problems
    such as the document not existing, or it not being in Preparation mode, or it being timedout.
-}
authorSendDocument :: DocumentID
                   -> MinutesTime
                   -> Word32
                   -> Maybe SignatureInfo
                   -> Update Documents (Either String Document)
authorSendDocument documentid time ipnumber _msiginfo =
    modifySignable documentid $ \document ->
        case documentstatus document of
          Preparation ->
              let timeout = do
                             days <- documentdaystosign document
                             return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                  sinfo = Just (SignInfo time ipnumber)
                  siglinks = documentsignatorylinks document
                  sigdetails = map signatorydetails siglinks
              in Right $ document { documenttimeouttime = timeout
                                  , documentmtime = time
                                  , documentstatus = Pending
                                  , documentinvitetime = sinfo
                                  } `appendHistory` [DocumentHistoryInvitationSent time ipnumber sigdetails]

          Timedout -> Left "FÃ¶rfallodatum har passerat" -- possibly quite strange here...
          _ ->        Left ("Bad document status: " ++ show (documentstatus document))

{- |
    Signs the document as an author.  This expects the document to be in preparation mode,
    so is the signing that happens just before sending the doc to be signed by others.
    This returns Left if there is a problem such as the document
    not existing, or the document not being in preparation mode.
    
    As author is no longer special we should consider refactoring to delete this function.
-}
authorSignDocument :: DocumentID
                   -> MinutesTime
                   -> Word32
                   -> Maybe SignatureInfo
                   -> Update Documents (Either String Document)
authorSignDocument documentid time ipnumber msiginfo =
    modifySignable documentid $ \document ->
        case documentstatus document of
          Preparation ->
              let timeout = do
                             days <- documentdaystosign document
                             return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                  Just authorsiglink = getAuthorSigLink document
                  Just authorid = maybesignatory authorsiglink
                  sinfo = Just (SignInfo time ipnumber)
                  sigdetails = map signatorydetails (documentsignatorylinks document)
                  -- are there no signatories besides the author
                  authorOnly = Prelude.null [sl | sl <- documentsignatorylinks document
                                                , not $ isAuthor sl
                                                , isSignatory sl]
                  newsiglinks = signWithUserID (documentsignatorylinks document) authorid sinfo msiginfo
                  signeddocument = document { documenttimeouttime = timeout
                                  , documentmtime = time
                                  , documentsignatorylinks = newsiglinks
                                  , documentstatus = if authorOnly then Closed else Pending
                                  , documentinvitetime = sinfo
                                  } `appendHistory` ([DocumentHistoryInvitationSent time ipnumber sigdetails] ++ if authorOnly then [DocumentHistoryClosed time ipnumber] else [])
              in Right $ signeddocument

          Timedout -> Left "FÃ¶rfallodatum har passerat" -- possibly quite strange here...
          _ ->        Left ("Bad document status: " ++ show (documentstatus document))

getMagicHash :: Update Documents MagicHash
getMagicHash = getRandom

{- |
    Rejects a doc for a particular signatory.
    Returns Left if there is a problem, such as the document not existing,
    or having timedout, or not being in pending mode.
-}
rejectDocument :: DocumentID
               -> SignatoryLinkID
               -> MinutesTime
               -> Word32
               -> Maybe BS.ByteString
               -> Update Documents (Either String Document)
rejectDocument documentid signatorylinkid1 time ipnumber customtext = do
  modifySignable documentid $ \document ->
      let
          signlinks = documentsignatorylinks document
          Just sl = find ((== signatorylinkid1) . signatorylinkid) signlinks
          newdocument = document { documentstatus = Rejected
                                 , documentrejectioninfo = Just (time, signatorylinkid1, maybe (BS.fromString "") id customtext)
                                 } `appendHistory`
                        [DocumentHistoryRejected time ipnumber (signatorydetails sl)]
      in case documentstatus document of
           Pending ->  Right newdocument
           Timedout -> Left "FÃ¶rfallodatum har passerat"
           _ ->        Left "Bad document status"

{- |
    Marks an signatory invitation to sign as being read.
    This would happen when the sendgrid api tells us that the user has read their
    initial invitation email.
    It shouldn't change the read time later on.
    
    This goes against a trend and doesn't return an Either (String Document).
    It probably should.
-}
markInvitationRead :: DocumentID
                   -> SignatoryLinkID
                   -> MinutesTime
                   -> Update Documents ()
markInvitationRead documentid linkid time = do
    _ <- modifySignable documentid $ \document ->
        if (any shouldMark (documentsignatorylinks document))
            then Right $ document { documentsignatorylinks = mapIf shouldMark mark (documentsignatorylinks document)}
            else Left ""
    return ()
       where
        shouldMark l = (signatorylinkid l) == linkid && (isNothing $ maybeseeninfo l)
        mark l =  l { maybereadinvite = Just time }

{- |
    Marks an document to sign as being seen by a signatory.
    This would happen when the document was first seen by the user.
    It shouldn't change the first seen time later on.
    
    If it doesn't mark the time it returns a Left.  This could be because
    the document doesn't exist, or because the signatory has already been marked
    as seen.
-}
markDocumentSeen :: DocumentID
                 -> SignatoryLinkID
                 -> MagicHash
                 -> MinutesTime
                 -> Word32
                 -> Update Documents (Either String Document)
markDocumentSeen documentid signatorylinkid1 mh time ipnumber = do
    modifySignable documentid $ \document -> case documentstatus document of
      Closed -> Left "Cannot modify a Closed document"
      Preparation -> Left "Cannot view a document in Preparation"
      _ -> 
        if (any shouldMark (documentsignatorylinks document))
          then Right $ document { documentsignatorylinks = mapIf shouldMark mark (documentsignatorylinks document) }
          else Left ""
        where
          shouldMark l = (signatorylinkid l) == signatorylinkid1 && (signatorymagichash l == mh) && (isNothing $ maybeseeninfo l)
          mark l = l { maybeseeninfo = Just (SignInfo time ipnumber) }

{- |
    Sets information about a signatory's invitation delivery.
    Elsewhere in the code we as autocancel the document if an inviation is undelivered.
    This will return a Left if there is a problem such as the document not existing.
-}
setInvitationDeliveryStatus::DocumentID -> SignatoryLinkID -> MailsDeliveryStatus -> Update Documents (Either String Document)
setInvitationDeliveryStatus docid siglnkid status = do
    modifySignable docid $ \doc -> do
            Right $ doc {documentsignatorylinks = map setStatus $ documentsignatorylinks doc}
    where
        setStatus sl =
            if (signatorylinkid sl == siglnkid)
                then sl {invitationdeliverystatus = status}
                else sl

{- |
    Gets summary stats about the documents in the system.
    This excludes documents where documentdeleted = True.
    When calculating signature counts this includes ones which are deleted.
    It looks like this function needs work.
-}
getDocumentStats :: Query Documents DocStats
getDocumentStats = queryDocs $ \documents ->
  let undeleteddocs = filter (not . documentdeleted) $ IxSet.toList documents
      signatureCountForDoc :: Document -> Int
      signatureCountForDoc doc = length $ filter (isJust . maybesigninfo) (documentsignatorylinks doc) in
  DocStats
      { doccount = (length undeleteddocs)
      , signaturecount = sum $ map signatureCountForDoc undeleteddocs
      , signaturecount1m = 0
      , signaturecount2m = 0
      , signaturecount3m = 0
      , signaturecount6m = 0
      , signaturecount12m = 0
      }

{- |
    Queries for the last modified time of the document for the indicated file.
    If there isn't a document for the file this will return the 0 time.
    
    Maybe we should have some sort of error case instead of returning 0.
-}
fileModTime :: FileID -> Query Documents MinutesTime
fileModTime fileid = queryDocs $ \documents ->
  maximum $ (fromSeconds 0) : (map documentmtime $ toList (documents @= fileid))

{- |
    Links up a signatory link to a user account.  This should happen when 
      \1. a document moves from preparation to pending more
      \2. a signer creates an account after signing to save their document
      \3. the email of a signatory is corrected to that of an existing user
-}
saveDocumentForUser :: DocumentID -> User -> SignatoryLinkID
                          -> Update Documents (Either String Document)
saveDocumentForUser documentid User{userid, usercompany} signatorylinkid1 = do
  modifySignable documentid $ \document ->
      let saveddocument = document { documentsignatorylinks = newsignatorylinks }
          newsignatorylinks = map maybesave (documentsignatorylinks document)
          maybesave x@(SignatoryLink {signatorylinkid} )
            | signatorylinkid == signatorylinkid1 =
               x { maybesignatory = Just userid,
                   maybecompany = usercompany }
          maybesave x = x
      in Right saveddocument

{- |
    The number of documents that a user has authored.  This excludes documents that they have deleted.

    This function needs checking - especially to see if deletion is done correctly.
-}
getNumberOfDocumentsOfUser :: User -> Query Documents Int
getNumberOfDocumentsOfUser User{userid} = queryDocs $ \documents ->
  length . filterDocsWhereDeleted False userid $ IxSet.toList (documents @= Author (userid))

{- |
    Gets documents stats for a particular user.  This excludes documents that they have deleted.

    This function needs checking - especially to see if deletion is done correctly.
-}
getDocumentStatsByUser :: User -> MinutesTime -> Query Documents DocStats
getDocumentStatsByUser user time = do
  doccount' <- getNumberOfDocumentsOfUser user
  sigdocs <- getDocumentsBySignatory user
  let signaturecount' = length $ allsigns
      signaturecount1m' = length $ filter (isSignedNotLaterThanMonthsAgo 1) $ allsigns
      signaturecount2m' = length $ filter (isSignedNotLaterThanMonthsAgo 2) $ allsigns
      signaturecount3m' = length $ filter (isSignedNotLaterThanMonthsAgo 3) $ allsigns
      signaturecount6m' = length $ filter (isSignedNotLaterThanMonthsAgo 6) $ allsigns
      signaturecount12m' = length $ filter (isSignedNotLaterThanMonthsAgo 12) $ allsigns
      timeMonthsAgo m = (-m * 30 * 24 * 60) `minutesAfter` time
      isSignedNotLaterThanMonthsAgo m = (timeMonthsAgo m <) . documentmtime
      allsigns = filter (isSigned . relevantSigLink) sigdocs
      relevantSigLink :: Document -> Maybe SignatoryLink
      relevantSigLink doc = listToMaybe $ filter (isSigLinkFor $ userid user) (documentsignatorylinks doc)
      isSigned :: Maybe SignatoryLink -> Bool
      isSigned = maybe False (isJust . maybesigninfo)
  return DocStats { doccount          = doccount'
                  , signaturecount    = signaturecount'
                  , signaturecount1m  = signaturecount1m'
                  , signaturecount2m  = signaturecount2m'
                  , signaturecount3m  = signaturecount3m'
                  , signaturecount6m  = signaturecount6m'
                  , signaturecount12m = signaturecount12m'
                  }

{- |
    Sets the document's timeout time.  This will return a Left if the document isn't a signable,
    or if the document doesn't exist.
-}
setDocumentTimeoutTime :: DocumentID -> TimeoutTime -> Update Documents (Either String Document)
setDocumentTimeoutTime documentid timeouttime = do
  -- check if document status change is a legal transition
  modifySignable documentid $ \doc ->
      Right $ doc{ documenttimeouttime = Just timeouttime }

{- |
    Sets tags on a document.  This will return a Left if the document doesn't exist.
    This is used by the API.
-}
setDocumentTags :: DocumentID -> [DocumentTag] -> Update Documents (Either String Document)
setDocumentTags docid doctags =
  modifySignableOrTemplate docid $ \doc -> Right $
    doc {
      documenttags = doctags
    }

{- |
    Sets the UI settings on a document.  This will return a Left if the document doesn't exist.
    This is used by the API for white labelling.
-}
setDocumentUI :: DocumentID -> DocumentUI  -> Update Documents (Either String Document)
setDocumentUI  docid docui =
  modifySignableOrTemplate docid $ \doc -> Right $
    doc {
      documentui = docui
    }

{- |
    Fetches documents by company and tags, this won't return documents that have been deleted (so ones
    that would appear in the recycle bin//trash can.)  It also makes sure to respect the sign order in
    cases where the company is linked via a signatory that hasn't yet been activated.
-}
getDocumentsByCompanyAndTags :: (Maybe ServiceID) -> CompanyID ->  [DocumentTag] -> Query Documents ([Document])
getDocumentsByCompanyAndTags  mservice companyid doctags = queryDocs $ \documents ->
  filterDocsWhereActivated companyid . filterDocsWhereDeleted False companyid $ 
    IxSet.toList $ ((documents @= companyid)  @= mservice) @* doctags

{- |
    Deletes documents for the given user.  This is just a soft delete, the document
    will appear in their recycle bin//trash can.
    A Left is returned when there are problems, such as a document not existing or a document
    being in pending mode.
-}
archiveDocuments :: User -> [DocumentID] -> Update Documents (Either String [Document])
archiveDocuments user docids =
  archiveDocumentsFor docids (isSigLinkSavedFor user)

{- |
    Deletes a document for all of it's signatory links.  This is just a soft delete,
    the document will appear in their recycle bin//trash cans.
    A Left is returned when there are problems, such as the document not existing or a document
    being in pending mode.
-}
archiveDocumentForAll :: DocumentID -> Update Documents (Either String Document)
archiveDocumentForAll docid = archiveDocumentFor (const True) docid

{- |
    Helper function that makes it easier to run update functions that return Either over
    a list of documents.
-}
forEachDocument :: (a -> Update Documents (Either String Document))
                   -> [a]
                   -> Update Documents (Either String [Document])
forEachDocument updateFunc items = foldM updateFoldFunc (Right []) items
  where
    updateFoldFunc (Left msg) _ = return $ Left msg
    updateFoldFunc (Right docs) item = do
      mdoc <- updateFunc item
      case mdoc of
        Left msg -> return $ Left msg
        Right doc -> return . Right $ docs ++ [doc]

{- |
    A helper function that archives signatory links that pass the given filter function.
    This will set signatorylinkisdeleted to True, which won't really delete the document,
    but put it in the recycle bin//trash can.  This won't allow documents to be deleted when they
    are pending.
-}
archiveDocumentsFor :: [DocumentID] -> (SignatoryLink -> Bool) -> Update Documents (Either String [Document])
archiveDocumentsFor docids sigfilter = forEachDocument (archiveDocumentFor sigfilter) docids

archiveDocumentFor :: (SignatoryLink -> Bool) -> DocumentID -> Update Documents (Either String Document)
archiveDocumentFor sigfilter docid = do
  modifySignableOrTemplate docid $ \doc -> do
    if isDeletableDocument doc
      then Right $ doc { documentsignatorylinks = map maybeDeleteSigLink $ documentsignatorylinks doc }
      else Left "Document is pending so it can't be removed"
  where
    maybeDeleteSigLink sl =
      if sigfilter sl
        then sl { signatorylinkdeleted = True }
        else sl

{- |
    Restores documents for the given user.  This will undo an archive for those documents which have
    been soft deleted and so are in the recycle bin//trash can.
    A Left is returned when there are problems, such as a document not existing.
-}
restoreArchivedDocuments :: User -> [DocumentID] -> Update Documents (Either String [Document])
restoreArchivedDocuments user docids = forEachDocument restoreDocument docids
  where
    restoreDocument :: DocumentID -> Update Documents (Either String Document)
    restoreDocument docid = do
      modifySignableOrTemplate docid $ \doc ->
        return $ doc { documentsignatorylinks = map maybeRestoreSigLink $ documentsignatorylinks doc }
    maybeRestoreSigLink sl =
      if isSigLinkSavedFor user sl
        then sl { signatorylinkdeleted = False }
        else sl

{- |
    Deletes documents for the given user.  This is a hard delete that is only posssible for those documents
    that have been soft deleted first and so are in the recycle bin//trash can.
    You must pass through the documentid alongside a list of relevant users, the reason being that it
    will check to see if those users still exist.  If they don't exist, then the doc link is counted
    as being deleted.
    This will only delete where a user is a single user who isn't in a company, or the user is a company admin.
    Standard company users without permissions won't be able to really delete anything.
    A Left is returned when there are problems, such as a document not existing.
-}
reallyDeleteDocuments :: User -> [(DocumentID, [User])] -> Update Documents (Either String [Document])
reallyDeleteDocuments deletinguser docidsAndUsers = forEachDocument deleteDocument docidsAndUsers
  where
    deleteDocument :: (DocumentID, [User]) -> Update Documents (Either String Document)
    deleteDocument (docid, users) = do
      mdoc <- deleteDocumentSigLinks docid
      case mdoc of
        Left msg -> return $ Left msg
        Right doc -> deleteDocumentRecordIfRequired (documentid doc) users
    deleteDocumentSigLinks :: DocumentID -> Update Documents (Either String Document)
    deleteDocumentSigLinks docid =
      modifySignableOrTemplate docid $ \doc ->
        return $ doc { documentsignatorylinks = map maybeDeleteSigLink $ documentsignatorylinks doc }
    maybeDeleteSigLink :: SignatoryLink -> SignatoryLink
    maybeDeleteSigLink sl@SignatoryLink{signatorylinkdeleted} =
      let isSavedForSingleUser = isNothing (usercompany deletinguser) && isSigLinkFor (userid deletinguser) sl
          isSavedForCompanyAdmin = useriscompanyadmin deletinguser && isSigLinkFor (usercompany deletinguser) sl
      in if (isSavedForSingleUser || isSavedForCompanyAdmin) && signatorylinkdeleted
        then sl { signatorylinkreallydeleted = True }
        else sl

{- |
    This checks whether any of the given users is linked to a document,
    and if they're not if really deletes the record by setting documentdeleted = True.
    The document won't contain any data, just the ids remain to stop repetitions in the future.
-}
deleteDocumentRecordIfRequired :: DocumentID -> [User] -> Update Documents (Either String Document)
deleteDocumentRecordIfRequired docid users =
  modifySignableOrTemplate docid $ \doc ->
    if shouldBeReallyDeleted doc
      then return $ setupForDeletion doc
      else return doc
  where
    shouldBeReallyDeleted :: Document -> Bool
    shouldBeReallyDeleted doc = all isCutSigLink $ documentsignatorylinks doc
      where
        isCutSigLink sl = isReallyDeletedSigLink sl || isOrphanedSigLink sl
        isReallyDeletedSigLink SignatoryLink{signatorylinkreallydeleted} = signatorylinkreallydeleted
        isOrphanedSigLink SignatoryLink{maybesignatory,maybecompany} = isNoUser maybesignatory && isNothing maybecompany
        isNoUser Nothing = True
        isNoUser (Just uid) = not $ uid `elem` (map userid users)

{- |
    This is a very boring function that creates a new zombie document record
    to replace the existing one.  For legal reasons we have to take all the data off,
    but on the other hand I've left the File and SignatoryLinkIDs in tact so that
    we don't reuse in the future.
-}
setupForDeletion :: Document -> Document
setupForDeletion doc = blankDocument {
                         documentid = documentid doc,
                         documentdeleted = True,
                         documentfiles = map (blankFile . fileid) (documentfiles doc),
                         documentsealedfiles = map (blankFile . fileid) (documentsealedfiles doc),
                         documentsignatorylinks = map (blankSigLink . signatorylinkid) (documentsignatorylinks doc) }
  where
  blankFile :: FileID -> File
  blankFile fid = File { fileid = fid,
                         filename = BS.empty,
                         filestorage = FileStorageMemory BS.empty }
  blankSigLink :: SignatoryLinkID -> SignatoryLink
  blankSigLink siglinkid = SignatoryLink {
                             signatorylinkid = siglinkid
                           , signatorydetails = blankSigDetails
                           , signatorymagichash = MagicHash 0
                           , maybesignatory = Nothing
                           , maybesupervisor = Nothing -- this field is now deprecated, should use maybecompany instead
                           , maybecompany = Nothing
                           , maybesigninfo  = Nothing
                           , maybeseeninfo  = Nothing
                           , maybereadinvite = Nothing
                           , invitationdeliverystatus = Unknown
                           , signatorysignatureinfo = Nothing
                           , signatoryroles = []
                           , signatorylinkdeleted = False
                           , signatorylinkreallydeleted = False
                           }
  blankSigDetails :: SignatoryDetails
  blankSigDetails = SignatoryDetails {
      signatorysignorder = SignOrder 0
    , signatoryfields = []
    }

{- |
    Shares the indicated documents.  This will return a Left if there is a problem,
    such as the user trying to share isn't the document author, or if the document
    doesn't exist.
-}
shareDocument :: DocumentID -> Update Documents (Either String Document)
shareDocument docid = 
  modifySignableOrTemplate docid $ \doc ->
  Right $ doc { documentsharing = Shared }

{- |
    Sets the document title for the indicated document.
    Will return Left if there is a problem, such as the document not existing,
    or not being in preparation mode.
-}
setDocumentTitle :: DocumentID -> BS.ByteString -> Update Documents (Either String Document)
setDocumentTitle docid doctitle =
  modifySignableOrTemplate docid $ \doc@Document{documentstatus} ->
    if documentstatus == Preparation
    then Right $ doc { documenttitle = doctitle }
    else Left $ "Can't update title unless the status is in preparation"

--This is only for awaiting author
--We should add current state checkers here (not co cancel closed documents etc.)
closeDocument :: DocumentID
              -> MinutesTime
              -> Word32
              -> Maybe SignatureInfo
              -> Update Documents (Maybe Document)
closeDocument docid time ipnumber msiginfo = do
  doc <- modifySignable docid $ \document -> 
    case documentstatus document of
      AwaitingAuthor -> let timeout = do
                              days <- documentdaystosign document
                              return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                            Just authorsiglink = getAuthorSigLink document
                            Just authorid = maybesignatory authorsiglink
                            sinfo = Just (SignInfo time ipnumber)
                            newdocument = document { documenttimeouttime = timeout
                                                   , documentmtime = time
                                                   , documentsignatorylinks = signWithUserID (documentsignatorylinks document) authorid sinfo msiginfo
                                                   , documentstatus = Closed
                                                   } `appendHistory` [DocumentHistoryClosed time ipnumber]
                        in Right $ newdocument
      _ -> Left "Only documents in AwaitingAuthor can be closed like this"
  case doc of
    Left _ -> return Nothing
    Right d -> return $ Just d

{- |
    Cancels a document that is either in pending or awaiting autor state.
    If it's in the wrong state, doesn't exist, or isn't a signable then a Left will be returned.
-}
cancelDocument :: DocumentID -> CancelationReason -> MinutesTime -> Word32 -> Update Documents (Either String Document)
cancelDocument docid cr time ipnumber = modifySignable docid $ \document -> do
    let canceledDocument =  document {
                              documentstatus = Canceled
                            , documentcancelationreason = Just cr}
                            `appendHistory` [DocumentHistoryCanceled time ipnumber]
    case documentstatus document of
        Pending -> Right canceledDocument
        AwaitingAuthor -> Right canceledDocument
        _ -> Left $ "Invalid document status " ++ show (documentstatus document) ++ " in cancelDocument"

{- |
    Gathers together all the files in the system attached to documents.
    This excludes all documents where documentdeleted is True.
-}
getFilesThatShouldBeMovedToAmazon :: Query Documents [File]
getFilesThatShouldBeMovedToAmazon = queryDocs $ \documents ->
  let doclist = filter (not . documentdeleted) $ IxSet.toList documents
      getFiles d@Document{documentfiles,documentsealedfiles} =
        documentfiles
        ++ documentsealedfiles
        ++ map authorattachmentfile (documentauthorattachments d)
        ++ [f | SignatoryAttachment{signatoryattachmentfile = Just f} <- (documentsignatoryattachments d)]
      allFiles = concatMap getFiles doclist
      getID file@File{ filestorage = FileStorageMemory _ } = [file]
      getID _ = [] in
  concatMap getID allFiles


{- |
    This restarts a document.
    It will check that the document is in the correct state, as it can't be Canceled, Timedout, or Rejected,
    and also that the user attempting to restart is an author.
    To restart it will clear all the sign info from the document, and then finally return a copy of that document
    so that it has a different document id.
    In the event of problems it returns a Left.
-}
restartDocument :: Document -> User -> MinutesTime -> Word32 -> Update Documents (Either String Document)
restartDocument doc user time ipnumber = do
  mndoc <- tryToGetRestarted
  case mndoc of
    Right newdoc -> newFromDocument (const newdoc) (documentid doc)
    other -> return other
  where
    tryToGetRestarted :: Update Documents (Either String Document)
    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
      else if (not $ isAuthor (doc, user))
           then return $ Left $ "Can't restart document if you are not it's author"
           else do
             doc' <- clearSignInfofromDoc
             let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
             return $ Right doc''
    clearSignInfofromDoc :: Update Documents Document
    clearSignInfofromDoc = do
      let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x)) $ documentsignatorylinks doc
          Just asl = getAuthorSigLink doc
      newSignLinks <- sequence $ map (uncurry $ signLinkFromDetails) signatoriesDetails
      let Just authorsiglink0 = find isAuthor newSignLinks
          authorsiglink = authorsiglink0 {
                            maybesignatory = maybesignatory asl,
                            maybecompany = maybecompany asl
                          }
          othersiglinks = filter (not . isAuthor) newSignLinks
          newsiglinks = authorsiglink : othersiglinks
      return doc {documentstatus = Preparation,
                  documenttimeouttime = Nothing,
                  documentsignatorylinks = newsiglinks
                 }

{- |
    This updates a signatory email, which is required in situations where an invitation can't be delivered to an email.
    When the signatory email is changed we need to update the maybesignatory and maybecompany fields
    accordingly to keep up with who this signatory link is for.
    If there is a problem then a Left is returned, for example if the document doesn't exist, or is not in a pending state.
-}
changeSignatoryEmailWhenUndelivered::DocumentID -> SignatoryLinkID -> Maybe User -> BS.ByteString ->  Update Documents (Either String Document)
changeSignatoryEmailWhenUndelivered did slid mnewuser email = modifySignable did $ changeEmail
  where changeEmail doc = let signlinks = documentsignatorylinks doc
                              mnsignlink = do
                                           sl <- find ((== slid) . signatorylinkid) signlinks
                                           when (invitationdeliverystatus sl /= Undelivered && invitationdeliverystatus sl /= Deferred) Nothing
                                           return sl { invitationdeliverystatus = Unknown
                                                     , signatorydetails = setEmail $ signatorydetails sl
                                                     , maybesignatory = fmap userid mnewuser
                                                     , maybecompany = mnewuser >>= usercompany }
                              setEmail sd@SignatoryDetails{signatoryfields} = sd { signatoryfields =
                                map (\sf -> case sfType sf of
                                        EmailFT -> sf { sfValue = email }
                                        _       -> sf) signatoryfields
                                }
                          in case mnsignlink  of
                           Just nsl -> let sll = for signlinks $ \sl -> if ( slid == signatorylinkid sl) then nsl else sl
                                       in  if (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor)
                                            then Right $ doc {documentsignatorylinks = sll}
                                            else Left "We cant change status of not pending documents"

                           Nothing -> Left "We could not find signatory"

{- |
    Creates a new signatory link with a unique id, that contains the given details.
-}
signLinkFromDetails :: SignatoryDetails -> [SignatoryRole] -> Update Documents SignatoryLink
signLinkFromDetails details roles = do
          sg <- ask
          linkid <- getUnique sg SignatoryLinkID
          magichash <- getRandom
          return $ SignatoryLink
                     { signatorylinkid = linkid
                     , signatorydetails = details
                     , signatorymagichash = magichash
                     , maybesignatory = Nothing
                     , maybesupervisor = Nothing  -- This field is now deprecated should use maybecompany instead
                     , maybecompany = Nothing
                     , maybesigninfo  = Nothing
                     , maybeseeninfo  = Nothing
                     , maybereadinvite = Nothing
                     , invitationdeliverystatus = Unknown
                     , signatorysignatureinfo = Nothing
                     , signatoryroles = roles
                     , signatorylinkdeleted = False
                     , signatorylinkreallydeleted = False
                     }

signLinkFromDetailsForTest :: SignatoryDetails -> [SignatoryRole] -> Update Documents SignatoryLink
signLinkFromDetailsForTest = signLinkFromDetails

{- |
    Gets a unique signatory link id.
-}
getUniqueSignatoryLinkID :: Update Documents SignatoryLinkID
getUniqueSignatoryLinkID = do
  sg <- ask
  linkid <- getUnique sg SignatoryLinkID
  return linkid

{- |
    Sets the reference used for the document within TrustWeaver.
    If the document doesn't exist, or isn't a signable, then this will return a Left.
-}
setDocumentTrustWeaverReference :: DocumentID -> String -> Update Documents (Either String Document)
setDocumentTrustWeaverReference documentid reference = do
  modifySignable documentid $ \document ->
      let
          newdocument = document { documenttrustweaverreference = Just (BS.fromString reference) }
      in Right newdocument

{- |
    Turns the document status into DocumentError with the error given.
    If the document doesn't exist then this will return a Left.
-}
errorDocument :: DocumentID -> String -> Update Documents (Either String Document)
errorDocument documentid errormsg =
  modifySignableOrTemplate documentid $ \document ->
      return $ document { documentstatus = DocumentError errormsg }

{- |
    Creates a new signable document from a template document.
-}
signableFromDocument :: Document -> Update Documents Document
signableFromDocument doc = insertNewDocument $ templateToDocument doc

{- |
    This is creates a new document that is a signable copy of a template.  This signable copy
    has the author fields replaced with fields from the given user.
    
    
   You basicly always want to update author data. Not only if you want to change author
   This is due to the fact that author personal data could get changed.  Also the company
   data may have changed.
-}
signableFromDocumentIDWithUpdatedAuthor :: User -> Maybe Company -> DocumentID -> Update Documents (Either String Document)
signableFromDocumentIDWithUpdatedAuthor user mcompany docid = 
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do
      (flip newFromDocument) docid $ \doc -> 
        (templateToDocument doc) {
              documentsignatorylinks = map replaceAuthorSigLink (documentsignatorylinks doc)
                                       -- FIXME: Need to remove authorfields?
        }
    where replaceAuthorSigLink :: SignatoryLink -> SignatoryLink
          replaceAuthorSigLink sl
            | isAuthor sl = replaceSignatoryUser sl user mcompany
            | otherwise = sl

{- |
    Creates a signable document from a template document.
    The new signable will have the same process as the template,
    and it will be in preparation mode.  It won't be shared.
-}
templateToDocument :: Document -> Document
templateToDocument doc =
    let Template process = documenttype doc in
    doc {
          documentstatus = Preparation
        , documenttype =  Signable process
        , documentsharing = Private
    }

{- |
    Replaces signatory data with given user's data.
-}
replaceSignatoryUser :: SignatoryLink
                        -> User
                        -> Maybe Company
                        -> SignatoryLink
replaceSignatoryUser siglink user mcompany =
  let newsl = replaceSignatoryData
                       siglink
                       (getFirstName      user)
                       (getLastName       user)
                       (getEmail          user)
                       (getCompanyName    mcompany)
                       (getPersonalNumber user)
                       (getCompanyNumber  mcompany)
                       (map sfValue $ filter isFieldCustom $ signatoryfields $ signatorydetails siglink) in
  newsl { maybesignatory = Just $ userid user,
          maybecompany = usercompany user }

{- |
    Pumps data into a signatory link
-}
replaceSignatoryData :: SignatoryLink
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> [BS.ByteString]
                        -> SignatoryLink
replaceSignatoryData siglink@SignatoryLink{signatorydetails} fstname sndname email company personalnumber companynumber fieldvalues =
  siglink { signatorydetails = signatorydetails { signatoryfields = pumpData (signatoryfields signatorydetails) fieldvalues } }
  where
    pumpData [] _ = []
    pumpData (sf:rest) vs = (case sfType sf of
      FirstNameFT      -> sf { sfValue = fstname }
      LastNameFT       -> sf { sfValue = sndname }
      CompanyFT        -> sf { sfValue = company }
      PersonalNumberFT -> sf { sfValue = personalnumber }
      CompanyNumberFT  -> sf { sfValue = companynumber }
      EmailFT          -> sf { sfValue = email }
      CustomFT label _ -> sf { sfType = CustomFT label (not $ BS.null v), sfValue = v })
        : pumpData rest vs'
      where
        (v, vs') = case sfType sf of
          CustomFT{} -> if null vs
                           then (BS.empty, [])
                           else (head vs, tail vs)
          _          -> (error "you can't use it", vs)

{- |
    Creates a new template document out of a signable document.  This template will have the same process
    as the signable document.  The template will have preparation status.
    If there is a problem then Left will be returned, for example if the document doesn't
    exist, or if the document isn't a signable.
-}
templateFromDocument :: DocumentID -> Update Documents (Either String Document)
templateFromDocument docid = modifySignable docid $ \doc ->
    let Signable process = documenttype doc in
    Right $ doc {
          documentstatus = Preparation
        , documenttype =  Template process
        }

{- |
    The existance of this function is wrong.  What it means is that storing
    maybesignatory and maybecompany on the signatory links is the wrong way of doing it,
    and there should be something else for hooking accounts to sig links that doesn't
    involve editing all the docs as a user moves between private and company accounts.
-}
adminOnlySaveForUser :: DocumentID -> User -> Update Documents (Either String Document)
adminOnlySaveForUser docid user =
  modifyDocumentWithActionTime False (const True) docid $ \doc ->
    return . Right $ doc {
      documentsignatorylinks = map maybeSaveSigLink $ documentsignatorylinks doc
    }
  where
    maybeSaveSigLink :: SignatoryLink -> SignatoryLink
    maybeSaveSigLink siglink@SignatoryLink{maybesignatory}
      | maybesignatory == Just (userid user) = siglink { maybecompany = usercompany user }
      | otherwise = siglink
        

fixBug510ForDocument :: DocumentID -> Update Documents (Either String Document)
fixBug510ForDocument docid =
  modifyDocumentWithActionTime False (const True) docid $ \doc ->
    return $ Right doc {
        documentsignatorylinks = map maybeRemoveAuthorAsPartner $ documentsignatorylinks doc
      , documentstatus =
          if shouldBecomeClosed doc
            then Closed
            else documentstatus doc
      }
  where
    maybeRemoveAuthorAsPartner :: SignatoryLink -> SignatoryLink
    maybeRemoveAuthorAsPartner sl =
      if SignatoryAuthor `elem` (signatoryroles sl)
        then sl { signatoryroles = [SignatoryAuthor] }
        else sl
    shouldBecomeClosed :: Document -> Bool
    shouldBecomeClosed Document{documentstatus,documentsignatorylinks} =
      let inPendingState = documentstatus == Pending || documentstatus == AwaitingAuthor
          otherSigHasSigned = all hasSigned . filter (not . isAuthor) $ documentsignatorylinks
      in inPendingState && otherSigHasSigned

{- |
    This will migrate the indicated document's signatory links by populating maybecompany on them.
    The list of users passed to it is the list of live users that are signatories or supervisors 
    of this document. The function will lookup the user for each sig link out of the given list, and then if it finds
    a match it will copy the user's company as the sig link company.
-} 
migrateDocumentSigLinkCompanies :: DocumentID -> [User] -> Update Documents (Either String Document)
migrateDocumentSigLinkCompanies docid sigusers =
  modifyDocumentWithActionTime False (const True) docid $ \doc ->
    return . Right $ doc {
      documentsignatorylinks = map migrateSigLink $ documentsignatorylinks doc
    }
  where
    migrateSigLink :: SignatoryLink -> SignatoryLink
    migrateSigLink siglink@SignatoryLink{maybesignatory,maybesupervisor} =
      case maybecompany siglink of
        Nothing ->
          let muser = find isMatchingUser sigusers in
          case muser of
            Nothing -> siglink --there isn't a relevant user
            Just user -> siglink {
                           -- set the sig link company to match the user's company
                           maybecompany = usercompany user
                         }
        Just _ -> siglink -- the sig link already has a company, so leave it
      where
        isMatchingUser :: User -> Bool
        isMatchingUser User{userid} = maybesignatory == Just userid 
                                        || maybesupervisor == Just userid

{- |
    This migration should be deleted soon.  It makes sure that maybesignatory and maybesupervisor
    are properly populated on the signatory links.
-}
{-
migrateDocumentSigAccounts :: DocumentID -> [User] -> Update Documents (Either String Document)
migrateDocumentSigAccounts docid sigusers =
  modifyDocumentWithActionTime False (const True) docid $ \doc ->
    return . Right $ doc {
      documentsignatorylinks = map (migrateSigLink doc) $ documentsignatorylinks doc
    }
  where 
    migrateSigLink :: Document -> SignatoryLink -> SignatoryLink
    migrateSigLink doc' siglink
      | isSuitableForSave doc' siglink =
          let sigemail = signatoryemail $ signatorydetails siglink
              muser = find ((==) sigemail . unEmail . useremail . userinfo) sigusers in
          case muser of
            Nothing -> siglink --okay we can't find the user, but they may have been deleted or not signed up.  we can leave be.
            (Just user) -> siglink {
                             maybesignatory = Just $ userid user,
                             maybesupervisor = fmap (UserID . unSupervisorID) $ usersupervisor user
                           }
      | otherwise = clearSignatoryAccount siglink
    isSuitableForSave :: Document -> SignatoryLink -> Bool
    isSuitableForSave Document{documenttype,documentstatus} siglink =
         isAuthor siglink 
      || (documenttype==Attachment) 
      || ((isSignable documenttype) && (documentstatus /= Preparation))
    clearSignatoryAccount :: SignatoryLink -> SignatoryLink
    clearSignatoryAccount siglink = siglink { maybesignatory = Nothing, maybecompany = Nothing }
-}
{- |
    Updates the list of required signatory attachments on the given document.
    This ensures that the document is in a preperation state.  If there's a problem,
    for example if the document doesn't exist, or the document is in the wrong state, then
    a Left is returned.
-}
updateSigAttachments :: DocumentID -> [SignatoryAttachment] -> Update Documents (Either String Document)
updateSigAttachments docid sigatts =
  modifySignableOrTemplate docid $ \doc ->
  case documentstatus doc of
    Preparation -> Right doc { documentsignatoryattachments = sigatts }
    _ -> Left "Can only attach to document in Preparation"

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
saveSigAttachment :: DocumentID -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Update Documents (Either String Document)
saveSigAttachment docid name email content = do
  documents <- ask
  fileid <- getUnique documents FileID
  modifySignable docid $ \doc ->
    case documentstatus doc `elem` [Pending, AwaitingAuthor] of
      False -> Left "Only attach when the document is signable."
      True -> Right doc { documentsignatoryattachments = newsigatts }
        where
          newsigatts = map addfile $ documentsignatoryattachments doc
          addfile a | email == signatoryattachmentemail a && name == signatoryattachmentname a =
            a { signatoryattachmentfile =
                   Just $ File { fileid = fileid
                               , filename = name
                               , filestorage = FileStorageMemory content
                               }
              }
          addfile a = a

{- |
    Deletes a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
deleteSigAttachment :: DocumentID -> BS.ByteString -> FileID -> Update Documents (Either String Document)
deleteSigAttachment docid email fid =
  modifySignable docid $ \doc -> do
    let newsigatts = map removefile $ documentsignatoryattachments doc
        removefile a | email == signatoryattachmentemail a && 
                       isJust (signatoryattachmentfile a) &&
                       (fileid $ fromJust $ signatoryattachmentfile a) == fid = a { signatoryattachmentfile = Nothing }
        removefile a = a


    case documentstatus doc `elem` [Pending, AwaitingAuthor] of
      False -> Left "Only attach when the document is signable."
      True -> Right doc { documentsignatoryattachments = newsigatts }

storeDocumentForTesting :: Document -> Update Documents DocumentID
storeDocumentForTesting doc = do
  d2 <- insertNewDocument doc
  return $ documentid d2

getSignatoryLinkIDs :: Query Documents [SignatoryLinkID]
getSignatoryLinkIDs =
  (concatMap (map signatorylinkid . documentsignatorylinks) . toList) `fmap` ask

-- create types for event serialization
$(mkMethods ''Documents [ 'getDocuments
                        , 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'getDocumentsByCompany
                        , 'getDocumentsSharedInCompany
                        , 'getDeletedDocumentsByCompany
                        , 'getDeletedDocumentsByUser
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'getTimeoutedButPendingDocuments
                        , 'updateDocument
                        , 'updateDocumentSimple
                        , 'attachCSVUpload
                        , 'getDocumentsByDocumentID
                        , 'updateDocumentAttachments
                        , 'updateSigAttachments
                        , 'signDocument
                        , 'authorSignDocument
                        , 'authorSendDocument
                        , 'rejectDocument
                        , 'attachFile
                        , 'attachSealedFile
                        , 'markDocumentSeen
                        , 'markInvitationRead
                        , 'setInvitationDeliveryStatus
                        , 'getDocumentStats
                        , 'getDocumentStatsByUser
                        , 'fileModTime
                        , 'saveDocumentForUser
                        , 'adminOnlySaveForUser
                        , 'getDocumentsByUser
                        , 'getNumberOfDocumentsOfUser
                        , 'setDocumentTimeoutTime
                        , 'setDocumentTags
                        , 'setDocumentUI
                        , 'getDocumentsByCompanyAndTags
                        , 'setDocumentTrustWeaverReference
                        , 'archiveDocuments
                        , 'archiveDocumentForAll
                        , 'restoreArchivedDocuments
                        , 'reallyDeleteDocuments
                        , 'deleteDocumentRecordIfRequired
                        , 'shareDocument
                        , 'setDocumentTitle
                        , 'timeoutDocument
                        , 'closeDocument
                        , 'cancelDocument
                        , 'fileMovedToAWS
                        , 'fileMovedToDisk
                        , 'deleteSigAttachment
                          -- admin only area follows
                        , 'getFilesThatShouldBeMovedToAmazon
                        , 'restartDocument
                        , 'changeSignatoryEmailWhenUndelivered
                        , 'getUniqueSignatoryLinkID
                        , 'getMagicHash
                        , 'saveSigAttachment
                        , 'storeDocumentForTesting
                        , 'signLinkFromDetailsForTest

                        , 'getDocumentByFileID
                        , 'errorDocument
                        , 'signableFromDocument
                        , 'signableFromDocumentIDWithUpdatedAuthor
                        , 'documentFromSignatoryData
                        , 'templateFromDocument
                        --, 'migrateDocumentSigAccounts
                        , 'migrateDocumentSigLinkCompanies
                        , 'fixBug510ForDocument
                        , 'getSignatoryLinkIDs
                        ])
