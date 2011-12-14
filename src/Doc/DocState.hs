{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Doc.DocState
#ifdef DOCUMENTS_IN_POSTGRES
    ( Documents
    , GetAllDocuments(..)
    )
where

import Happstack.Data.IxSet as IxSet hiding (null)
import Happstack.State
import Doc.DocStateData
import Control.Monad.Reader (ask)

getAllDocuments :: Query Documents [Document]
getAllDocuments = return . toList =<< ask

-- create types for event serialization
$(mkMethods ''Documents [ 'getAllDocuments
                        ])

#else
    ( isTemplate -- fromUtils
    , isShared -- fromUtils
    , isDeletableDocument -- fromUtils
    , anyInvitationUndelivered
    , undeliveredSignatoryLinks
    , ArchiveDocument(..)
    , RestoreArchivedDocument(..)
    , ReallyDeleteDocument(..)
    , AttachFile(..)
    , AttachSealedFile(..)
    , ChangeMainfile(..)
    , RejectDocument(..)
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
    , ShareDocument(..)
    , SetDocumentTitle(..)
    , SignDocument(..)
    , TimeoutDocument(..)
    , UpdateDocumentSimple(..)
    , AttachCSVUpload(..)
    , AddDocumentAttachment(..)
    , RemoveDocumentAttachment(..)
    , CloseDocument(..)
    , CancelDocument(..)
    , RestartDocument(..)
    , ChangeSignatoryEmailWhenUndelivered(..)
    , signatoryDetailsFromUser
    , GetUniqueSignatoryLinkID(..)
    , GetMagicHash(..)
    , ErrorDocument(..)
    , TemplateFromDocument(..)
    , SignableFromDocument(..)
    , SignableFromDocumentIDWithUpdatedAuthor(..)
    , DocumentFromSignatoryData(..)
    , UpdateSigAttachments(..)
    , SaveSigAttachment(..)
    , PreparationToPending(..)
    , AddInvitationEvidence(..)
    , UpdateFields(..)
    , PendingToAwaitingAuthor(..)
    , SetSignatoryCompany(..)
    , RemoveSignatoryCompany(..)
    , SetSignatoryUser(..)
    , RemoveSignatoryUser(..)
    , SetInviteText(..)
    , SetDaysToSign(..)
    , RemoveDaysToSign(..)
    , SetDocumentAdvancedFunctionality(..)
    , SetCSVSigIndex(..)
    , SetEmailIdentification(..)
    , SetElegitimationIdentification(..)
    , ResetSignatoryDetails(..)
    , SetDocumentLocale(..)
    --, MigrateDocumentSigAccounts(..)
    , MigrateDocumentSigLinkCompanies(..)
    , FixBug510ForDocument(..)
    , StoreDocumentForTesting(..)
    , SignLinkFromDetailsForTest(..)
    , DeleteSigAttachment(..)
    , GetSignatoryLinkIDs(..)
    , AdminOnlySaveForUser(..)
    -- , populateDBWithDocumentsIfEmpty
    )
where

import API.Service.Model
import Company.Model
import Control.Monad
import Control.Monad.Reader (ask)
--import Database.HDBC
import Data.Maybe
import Data.Word
--import DB.Classes
import DB.Types
--import DB.Utils
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
--import Util.HasSomeCompanyInfo
--import Util.HasSomeUserInfo
import Control.Applicative
import Data.List
import File.FileID
import Doc.DocStateCommon

--import qualified AppLogger as Log
--import qualified Doc.Model as D
--import qualified Doc.Tables as D

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
                , documentregion               = getRegion user
                , documentfunctionality        = newDocumentFunctionality documenttype user
                , documentctime                = ctime
                , documentmtime                = ctime
                , documentservice              = userservice user
                , documentauthorattachments    = []
                , documentsignatoryattachments = []
                , documentallowedidtypes       = [EmailIdentification]
                , documentui           = (documentui blankDocument) {documentmailfooter = BS.fromString <$> (customfooter $ usersettings user)}
                } `appendHistory` [DocumentHistoryCreated ctime]

      inserteddoc <- insertNewDocument doc
      return $ Right inserteddoc

setDocumentLocale :: DocumentID -> Locale -> MinutesTime -> Update Documents (Either String Document)
setDocumentLocale did locale time =
  modifySignableOrTemplate did $ guardStatus "set locale for" Preparation $ \document ->
    Right $ document { documentregion = getRegion locale
                     , documentmtime = time}


{- |
    Attaches a file to the indicated document.
    If there is a problem, such as the document not existing,
    then a Left is returned.
-}
attachFile :: DocumentID
           -> FileID
           -> MinutesTime
           -> Update Documents (Either String Document)
attachFile documentid fid time = do
  modifySignableOrTemplate documentid $ guardStatus "attach a file to" Preparation $ \document ->
    Right $ document { documentfiles = documentfiles document ++ [fid]
                     , documentmtime = time}

{- |
    Attaches a sealed file to the indicated document.
    If there is a problem, such as the document not existing,
    or the document not being a signable then a Left is returned.
-}
attachSealedFile :: DocumentID
                 -> FileID
                 -> MinutesTime
                 -> Update Documents (Either String Document)
attachSealedFile documentid fid time = do
  modifySignable documentid $ \document -> 
    Right $ document { documentsealedfiles = [fid] 
                     , documentmtime = time
                     }

changeMainfile :: DocumentID -> FileID -> Update Documents (Either String Document)
changeMainfile did fid = do
    modifySignable did $ \doc ->
        if (documentstatus doc == Closed || allHadSigned doc)
         then Right $ doc { documentsealedfiles = [fid] , documentstatus = Closed}
         else Right $ doc { documentfiles = [fid] }
    where
        allHadSigned doc = all (hasSigned ||^ (not . isSignatory)) $ documentsignatorylinks doc

setSignatoryCompany :: DocumentID -> SignatoryLinkID -> CompanyID -> Update Documents (Either String Document)
setSignatoryCompany documentid slid cid = do
  modifySignable documentid $ \doc ->
    modifysiglink doc slid (\sl -> sl { maybecompany = Just cid })

modifysiglink :: Document -> SignatoryLinkID -> (SignatoryLink -> SignatoryLink) -> Either String Document
modifysiglink doc slid f = case getSigLinkFor doc slid of
    Nothing -> Left $ "No signatory for document " ++ show (documentid doc) ++ " and signatorylinkid " ++ show slid
    Just _ -> Right $ doc { documentsignatorylinks = mapIf (isSigLinkFor slid) f $ documentsignatorylinks doc }


{- | Remove the maybecompany for a signatorylink
 -}
removeSignatoryCompany :: DocumentID -> SignatoryLinkID -> Update Documents (Either String Document)
removeSignatoryCompany documentid slid = do
  modifySignable documentid $ \doc ->
    modifysiglink doc slid (\sl -> sl { maybecompany = Nothing })


{- | Set the user of a signatorylink
 -}
setSignatoryUser :: DocumentID -> SignatoryLinkID -> UserID -> Update Documents (Either String Document)
setSignatoryUser documentid slid uid = do
  modifySignable documentid $ \doc ->
    modifysiglink doc slid (\sl -> sl { maybesignatory = Just uid })

{- | Remove the user of a signatorylink
 -}
removeSignatoryUser :: DocumentID -> SignatoryLinkID -> Update Documents (Either String Document)
removeSignatoryUser documentid slid = do
  modifySignable documentid $ \doc ->
    modifysiglink doc slid (\sl -> sl { maybesignatory = Nothing })

setInviteText :: DocumentID -> BS.ByteString -> MinutesTime -> Update Documents (Either String Document)
setInviteText docid text time = do
  modifySignableOrTemplate docid $ guardStatus "set invite text" Preparation $ \doc ->
    Right $ doc { documentinvitetext = text
                , documentmtime      = time }

guardStatus :: String -> DocumentStatus -> (Document -> Either String Document) -> (Document -> Either String Document)
guardStatus name status f = \doc ->
  if status == documentstatus doc
  then f doc
  else Left $ "Cannot " ++ name ++ " on document " ++ show (documentid doc) ++ " because not in " ++ show status ++ "; in " ++ show (documentstatus doc)

setDaysToSign :: DocumentID -> Int -> MinutesTime -> Update Documents (Either String Document)
setDaysToSign docid daystosign time = do
  modifySignableOrTemplate docid $ guardStatus "set days to sign" Preparation $ \doc ->
    Right $ doc { documentdaystosign = Just daystosign
                , documentmtime = time}

removeDaysToSign :: DocumentID -> MinutesTime -> Update Documents (Either String Document)
removeDaysToSign docid time = do
  modifySignableOrTemplate docid $ guardStatus "remove days to sign" Preparation $ \doc ->
    Right $ doc { documentdaystosign = Nothing
                , documentmtime = time}

setDocumentAdvancedFunctionality :: DocumentID -> MinutesTime -> Update Documents (Either String Document)
setDocumentAdvancedFunctionality did time =
  modifySignableOrTemplate did $ guardStatus "set document functionality on" Preparation $ \doc ->
  if documentfunctionality doc == AdvancedFunctionality
  then Left "Document is already in AdvancedFunctionality."
  else Right $ doc { documentfunctionality = AdvancedFunctionality
                   , documentmtime         = time
                   }

setCSVSigIndex :: DocumentID -> Int -> MinutesTime -> Update Documents (Either String Document)
setCSVSigIndex did csvsigindex time =
  modifySignableOrTemplate did $ guardStatus "set csv sig index on" Preparation $ \doc ->
  case documentfunctionality doc of
    BasicFunctionality -> Left $ "Cannot set csvindex on basic functionality document " ++ show did
    AdvancedFunctionality -> case documentcsvupload doc of
      Nothing -> Left $ "There is no csv upload for document " ++ show did
      Just cu -> case checkCSVSigIndex (documentsignatorylinks doc) csvsigindex of
        Left s -> Left s
        Right i -> Right $ doc { documentmtime = time
                               , documentcsvupload = Just $ cu { csvsignatoryindex = i }
                               }

setEmailIdentification :: DocumentID -> MinutesTime -> Update Documents (Either String Document)
setEmailIdentification did time =
  modifySignableOrTemplate did $ guardStatus "set email identification on" Preparation $ \doc ->
  Right $ doc { documentmtime = time
              , documentallowedidtypes = [EmailIdentification]
              }

setElegitimationIdentification :: DocumentID -> MinutesTime -> Update Documents (Either String Document)
setElegitimationIdentification did time =
  modifySignableOrTemplate did $ guardStatus "set elegitimation identification on" Preparation $ \doc ->
  Right $ doc { documentmtime = time
              , documentallowedidtypes = [ELegitimationIdentification]
              }

_signatoriesToBasic :: [SignatoryLink] -> [SignatoryLink]
_signatoriesToBasic sls =
  let news = [sl { signatorydetails = removeFieldsAndPlacements (signatorydetails sl)} | sl <- sls]
      asl  = head   [ sl { signatoryroles = [SignatoryAuthor] }  | sl <- news, isAuthor sl]
      nsls = take 1 [ sl { signatoryroles = [SignatoryPartner] } | sl <- news, not $ isAuthor sl]
  in asl : nsls


resetSignatoryDetails :: DocumentID
                       -> [(SignatoryDetails, [SignatoryRole])]
                       -> MinutesTime
                       -> Update Documents (Either String Document)
resetSignatoryDetails documentid signatories time =
  modifySignableOrTemplateWithAction documentid $ \document ->
  case checkResetSignatoryData document signatories of
    [] -> do
      -- this reassigns signlinkid and magic hash each time we call it
      signatorylinks <- sequence $ map (uncurry $ signLinkFromDetails) signatories
      -- copy the author's user and company
      let signatorylinks2 = mapIf isAuthor (\sl -> sl { maybesignatory = maybe Nothing maybesignatory mauthorsiglink
                                                      , maybecompany   = maybe Nothing maybecompany   mauthorsiglink
                                                      }) signatorylinks
          mauthorsiglink = getAuthorSigLink document
      return $ Right $ document { documentsignatorylinks = signatorylinks2
                                , documentmtime = time
                                }
    s -> return $ Left $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s

{- |
    A cut down version of updateDocument that requires fewer parameters.  This is used by the integration api.
    If there is a problem, such as the document not existing,
    or the document not being in preparation mode then a Left is returned.
    DEPRECATED; Don't use this.
    Gracjan, please don't migrate this function to Postgres
     -- Eric
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
                 docfunctionality = if length alllinks > 2
                                    then AdvancedFunctionality
                                    else documentfunctionality document
             return $ Right $ document
                    { documentsignatorylinks         = alllinks
                    , documentmtime                  = now
                    , documentallowedidtypes         = [EmailIdentification]
                    , documentfunctionality          = docfunctionality
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

addDocumentAttachment :: DocumentID -> FileID -> Update Documents (Either String Document)
addDocumentAttachment docid fileid =
  modifySignableOrTemplate docid $ guardStatus "attach to" Preparation $ \doc ->
  let aa = AuthorAttachment fileid
      as = documentauthorattachments doc
  in Right doc { documentauthorattachments = as ++ ([aa] <| (aa `notElem` as) |> []) }

removeDocumentAttachment :: DocumentID -> FileID -> Update Documents (Either String Document)
removeDocumentAttachment docid fileid =
  modifySignableOrTemplate docid $ guardStatus "remove attachment from" Preparation $ \doc ->
  let aa = AuthorAttachment fileid
      as = documentauthorattachments doc
  in Right doc { documentauthorattachments = filter (/= aa) as }

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
  modifySignable documentid $ guardStatus "timeout" Pending $ \document ->
    Right $ document { documentstatus = Timedout } `appendHistory` [DocumentHistoryTimedOut time]


{- |
    Signs a particular signatory link.  If there is a problem, such as the document not existing,
    or the document not being pending, or having timedout then a Left is returned.
-}
signDocument :: DocumentID
             -> SignatoryLinkID
             -> MagicHash
             -> MinutesTime
             -> Word32
             -> Maybe SignatureInfo
             -> Update Documents (Either String Document)
signDocument documentid slid mh time ipnumber msiginfo = do
  modifySignable documentid $ \document ->
    case checkSignDocument document slid mh of
      [] -> let signeddocument = document { documentsignatorylinks = newsignatorylinks
                                          } `appendHistory` [DocumentHistorySigned time ipnumber (signatorydetails signatoryLink)]
                -- this is a mess and could be cleaned up by good use of SQL (PLEASE!)
                Just signatoryLink = getSigLinkFor document slid
                newsignatorylinks = mapIf (\sl -> signatorylinkid sl == slid)  sign (documentsignatorylinks document)
                sign link = link { maybesigninfo = Just (SignInfo time ipnumber)
                                 , signatorysignatureinfo = msiginfo
                                 }

            in Right signeddocument
      s -> Left $ "Cannot sign for signatory " ++ show slid ++ " because " ++ concat s

updateFields :: DocumentID ->
                SignatoryLinkID ->
                [(BS.ByteString, BS.ByteString)] ->
                Update Documents (Either String Document)
updateFields docid slid fields =
  modifySignable docid $ \document ->
  case checkUpdateFields document slid of
    [] -> let updateSigFields sl@SignatoryLink{signatorydetails = sd@SignatoryDetails{signatoryfields}} =
                sl { signatorydetails = sd { signatoryfields = map updateSigField signatoryfields } }
              updateSigField sf =
                let updateF n = case lookup n fields of
                      Just v  -> sf { sfValue = v }
                      Nothing -> sf
                in case sfType sf of
                  CompanyFT        -> updateF $ BS.pack "sigco"
                  PersonalNumberFT -> updateF $ BS.pack "sigpersnr"
                  CompanyNumberFT  -> updateF $ BS.pack "sigcompnr"
                  CustomFT label _ -> updateF label
                  _                -> sf

          in Right $ document { documentsignatorylinks =
                                   mapIf (\sl -> signatorylinkid sl == slid)
                                   updateSigFields $ documentsignatorylinks document
                              }
    s -> Left $ "Cannot updateFields on document " ++ show docid ++ " because " ++ concat s

{- | Move a document from Preparation to Pending
     Modifieds the documentstatus, documenttimeouttime, and the documentmtime
 -}
preparationToPending :: DocumentID -> MinutesTime -> Update Documents (Either String Document)
preparationToPending documentid time =
  modifySignable documentid $ \document ->
  case checkPreparationToPending document of
    [] -> let timeout = do
                days <- documentdaystosign document
                return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
          in Right $ document { documentstatus = Pending
                              , documenttimeouttime = timeout
                              , documentmtime = time
                              }
    s -> Left $ "Document " ++ show documentid ++ " cannot go from Preparation to Pending. " ++ concat s


{- | Add a bit of evidence about when a signatory was invited.
     Store the invite time + a history log message;
     NOTE: This currently does not store a very good evidence message.
 -}
addInvitationEvidence :: DocumentID -> SignatoryLinkID -> MinutesTime -> Word32 -> Update Documents (Either String Document)
addInvitationEvidence docid slid time ipnumber =
  modifySignable docid $ \document ->
  case checkAddEvidence document slid of
    [] -> let Just sds = signatorydetails <$> getSigLinkFor document slid
          in Right $ document { documentinvitetime = Just (SignInfo time ipnumber) }
             `appendHistory` [DocumentHistoryInvitationSent time ipnumber [sds]]
    s -> Left $ "Document " ++ show documentid ++ " cannot have evidence attached for signatory " ++ show slid ++ " because " ++ concat s



{- | Close a document
 -}
closeDocument :: DocumentID -> MinutesTime -> Word32 -> Update Documents (Either String Document)
closeDocument docid time ipnumber =
  modifySignable docid $ \document ->
  case checkCloseDocument document of
    [] -> Right $ document { documentstatus = Closed
                           , documentmtime  = time
                           } `appendHistory` [DocumentHistoryClosed time ipnumber]
    s -> Left $ "Cannot Close document " ++ show docid ++ " because " ++ concat s


{- | Pending to AwaitingAuthor
-}
pendingToAwaitingAuthor :: DocumentID -> MinutesTime -> Update Documents (Either String Document)
pendingToAwaitingAuthor docid time =
  modifySignable docid $ \document ->
  case checkPendingToAwaitingAuthor document of
    [] -> Right $ document { documentstatus = AwaitingAuthor
                           , documentmtime  = time
                           }
    s -> Left $ "Cannot move document to awaiting author" ++ show docid ++ " because " ++ concat s

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
        shouldMark l = (signatorylinkid l) == linkid && (isNothing $ maybereadinvite l)
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
setDocumentTimeoutTime :: DocumentID -> MinutesTime -> Update Documents (Either String Document)
setDocumentTimeoutTime documentid timeouttime = do
  -- check if document status change is a legal transition
  modifySignable documentid $ \doc ->
      Right $ doc{ documenttimeouttime = Just (TimeoutTime timeouttime) }

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
archiveDocument :: User -> DocumentID -> Update Documents (Either String Document)
archiveDocument user docid = do
  modifySignableOrTemplate docid $ \doc -> do
    if isDeletableDocument doc
      then Right $ doc { documentsignatorylinks = map maybeDeleteSigLink $ documentsignatorylinks doc }
      else Left "Document is pending so it can't be removed"
  where
    maybeDeleteSigLink sl =
      if isSigLinkSavedFor user sl
        then sl { signatorylinkdeleted = True }
        else sl

{- |
    Restores documents for the given user.  This will undo an archive for those documents which have
    been soft deleted and so are in the recycle bin//trash can.
    A Left is returned when there are problems, such as a document not existing.
-}
restoreArchivedDocument :: User -> DocumentID -> Update Documents (Either String Document)
restoreArchivedDocument user docid =
  modifySignableOrTemplate docid $ \doc ->
    return $ doc { documentsignatorylinks = map maybeRestoreSigLink $ documentsignatorylinks doc }
  where
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
reallyDeleteDocument :: User -> DocumentID -> Update Documents (Either String Document)
reallyDeleteDocument user docid =
  modifySignableOrTemplate docid $ \doc ->
    return $ doc { documentsignatorylinks = map maybeDeleteSigLink $ documentsignatorylinks doc }
  where
    maybeDeleteSigLink :: SignatoryLink -> SignatoryLink
    maybeDeleteSigLink sl@SignatoryLink{signatorylinkdeleted} =
      let isSavedForSingleUser = isNothing (usercompany user) && isSigLinkFor (userid user) sl
          isSavedForCompanyAdmin = useriscompanyadmin user && isSigLinkFor (usercompany user) sl
      in if (isSavedForSingleUser || isSavedForCompanyAdmin) && signatorylinkdeleted
        then sl { signatorylinkreallydeleted = True }
        else sl

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
setDocumentTitle :: DocumentID -> BS.ByteString -> MinutesTime -> Update Documents (Either String Document)
setDocumentTitle docid doctitle time =
  modifySignableOrTemplate docid $ \doc@Document{documentstatus} ->
    if documentstatus == Preparation
    then Right $ doc { documenttitle = doctitle,
                       documentmtime = time}
    else Left $ "Can't update title unless the status is in preparation"

{- |
    Cancels a document that is either in pending or awaiting autor state.
    If it's in the wrong state, doesn't exist, or isn't a signable then a Left will be returned.
-}
cancelDocument :: DocumentID -> CancelationReason -> MinutesTime -> Word32 -> Update Documents (Either String Document)
cancelDocument docid cr time ipnumber = modifySignable docid $ \document -> do
    let canceledDocument =  document { documentstatus = Canceled
                                     , documentcancelationreason = Just cr}
                            `appendHistory` [DocumentHistoryCanceled time ipnumber]
    case documentstatus document of
        Pending        -> Right canceledDocument
        AwaitingAuthor -> Right canceledDocument
        _              -> Left $ "Invalid document status " ++ show (documentstatus document) ++ " in cancelDocument"

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
          linkid <- getUnique64 sg SignatoryLinkID
          magichash <- getRandom
          return $ signLinkFromDetails' details roles linkid magichash


signLinkFromDetailsForTest :: SignatoryDetails -> [SignatoryRole] -> Update Documents SignatoryLink
signLinkFromDetailsForTest = signLinkFromDetails

{- |
    Gets a unique signatory link id.
-}
getUniqueSignatoryLinkID :: Update Documents SignatoryLinkID
getUniqueSignatoryLinkID = do
  sg <- ask
  linkid <- getUnique64 sg SignatoryLinkID
  return linkid

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
signableFromDocumentIDWithUpdatedAuthor :: User -> Maybe Company -> DocumentID -> MinutesTime -> Update Documents (Either String Document)
signableFromDocumentIDWithUpdatedAuthor user mcompany docid time =
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do
      (flip newFromDocument) docid $ \doc ->
        (templateToDocument doc) {
              documentsignatorylinks = map replaceAuthorSigLink (documentsignatorylinks doc)
                                       -- FIXME: Need to remove authorfields?
              , documentctime = time
        }
    where replaceAuthorSigLink :: SignatoryLink -> SignatoryLink
          replaceAuthorSigLink sl
            | isAuthor sl = replaceSignatoryUser sl user mcompany
            | otherwise = sl



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
-- CHANGE TO ADD/REMOVE operations
{- |
    Updates the list of required signatory attachments on the given document.
    This ensures that the document is in a preperation state.  If there's a problem,
    for example if the document doesn't exist, or the document is in the wrong state, then
    a Left is returned.
-}
updateSigAttachments :: DocumentID -> [SignatoryAttachment] -> MinutesTime -> Update Documents (Either String Document)
updateSigAttachments docid sigatts time =
  modifySignableOrTemplate docid $ \doc ->
  case documentstatus doc of
    Preparation -> Right doc { documentsignatoryattachments = sigatts
                             , documentmtime = time}
    _ -> Left "Can only attach to document in Preparation"

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
saveSigAttachment :: DocumentID -> BS.ByteString -> BS.ByteString -> FileID -> Update Documents (Either String Document)
saveSigAttachment docid name email fid = do
  modifySignable docid $ \doc ->
    case documentstatus doc `elem` [Pending, AwaitingAuthor] of
      False -> Left $ "saveSigAttachment can be used only in Pending or AwaitingAuthor status, document is in " ++ show (documentstatus doc) ++ " docid #" ++ show docid
      True -> Right doc { documentsignatoryattachments = newsigatts }
        where
          newsigatts = map addfile $ documentsignatoryattachments doc
          addfile a | email == signatoryattachmentemail a && name == signatoryattachmentname a =
            a { signatoryattachmentfile = Just $ fid }
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
                       signatoryattachmentfile a == Just fid = a { signatoryattachmentfile = Nothing }
        removefile a = a


    case documentstatus doc `elem` [Pending, AwaitingAuthor] of
      False -> Left $ "deleteSigAttachment can be used only in Pending or AwaitingAuthor status, document is in " ++ show (documentstatus doc) ++ " docid #" ++ show docid
      True -> Right doc { documentsignatoryattachments = newsigatts }

storeDocumentForTesting :: Document -> Update Documents DocumentID
storeDocumentForTesting doc = do
  d2 <- insertNewDocument doc
  return $ documentid d2

getSignatoryLinkIDs :: Query Documents [SignatoryLinkID]
getSignatoryLinkIDs =
  (concatMap (map signatorylinkid . documentsignatorylinks) . toList) `fmap` ask

getAllDocuments :: Query Documents [Document]
getAllDocuments = return . toList =<< ask

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
                        , 'updateDocumentSimple
                        , 'attachCSVUpload
                        , 'addDocumentAttachment
                        , 'removeDocumentAttachment
                        , 'updateSigAttachments
                        , 'signDocument
                        , 'setDocumentLocale
--                        , 'authorSendDocument
                        , 'setSignatoryCompany
                        , 'removeSignatoryCompany
                        , 'setSignatoryUser
                        , 'removeSignatoryUser
                        , 'setInviteText
                        , 'setDaysToSign
                        , 'removeDaysToSign
                        , 'setDocumentAdvancedFunctionality
                        , 'setCSVSigIndex
                        , 'setEmailIdentification
                        , 'setElegitimationIdentification
                        , 'resetSignatoryDetails
                        , 'rejectDocument
                        , 'attachFile
                        , 'attachSealedFile
                        , 'changeMainfile
                        , 'markDocumentSeen
                        , 'markInvitationRead
                        , 'setInvitationDeliveryStatus
                        , 'getDocumentStats
                        , 'getDocumentStatsByUser
                        , 'saveDocumentForUser
                        , 'adminOnlySaveForUser
                        , 'getDocumentsByUser
                        , 'getNumberOfDocumentsOfUser
                        , 'setDocumentTimeoutTime
                        , 'setDocumentTags
                        , 'setDocumentUI
                        , 'getDocumentsByCompanyAndTags
                        , 'archiveDocument
                        , 'restoreArchivedDocument
                        , 'reallyDeleteDocument
                        , 'shareDocument
                        , 'setDocumentTitle
                        , 'timeoutDocument
                        , 'closeDocument
                        , 'cancelDocument
                        , 'deleteSigAttachment
                        , 'preparationToPending
                        , 'addInvitationEvidence
                        , 'updateFields
                        , 'pendingToAwaitingAuthor
                          -- admin only area follows
                        , 'restartDocument
                        , 'changeSignatoryEmailWhenUndelivered
                        , 'getUniqueSignatoryLinkID
                        , 'getMagicHash
                        , 'saveSigAttachment
                        , 'storeDocumentForTesting
                        , 'signLinkFromDetailsForTest
                        , 'errorDocument
                        , 'signableFromDocument
                        , 'signableFromDocumentIDWithUpdatedAuthor
                        , 'documentFromSignatoryData
                        , 'templateFromDocument
                        --, 'migrateDocumentSigAccounts
                        , 'migrateDocumentSigLinkCompanies
                        , 'fixBug510ForDocument
                        , 'getSignatoryLinkIDs
                        , 'getAllDocuments
                        ])

-- stuff for converting to pgsql
#if 0
populateDBWithDocumentsIfEmpty :: DB ()
populateDBWithDocumentsIfEmpty = do
  [docsnumber] :: [Int] <- wrapDB $ \conn -> quickQuery' conn "SELECT COUNT(*) FROM documents" [] >>= return . map fromSql . join
  when (docsnumber == 0) $ do
    Log.debug "No documents in database, populating with values from happstack-state..."
    docs <- query GetAllDocuments
    _ <- wrapDB $ \conn -> runRaw conn "SET CONSTRAINTS ALL DEFERRED"
    forM_ docs $ \doc -> do
      Log.debug $ show doc
      let (doctype, docprocess) = case documenttype doc of
            Signable p -> (D.Signable, Just p)
            Template p -> (D.Template, Just p)
            Attachment -> (D.Attachment, Nothing)
            AttachmentTemplate -> (D.AttachmentTemplate, Nothing)
          mdocfile = listToMaybe $ documentfiles doc
          mdocsealedfile = listToMaybe $ documentsealedfiles doc
      insertFile mdocfile
      insertFile mdocsealedfile
      _ <- wrapDB $ \conn -> run conn ("INSERT INTO documents ("
        ++ "  id"
        ++ ", service_id"
        ++ ", file_id"
        ++ ", sealed_file_id"
        ++ ", title"
        ++ ", status"
        ++ ", type"
        ++ ", process"
        ++ ", functionality"
        ++ ", ctime"
        ++ ", mtime"
        ++ ", days_to_sign"
        ++ ", timeout_time"
        ++ ", invite_time"
        ++ ", invite_ip"
        ++ ", log"
        ++ ", invite_text"
        ++ ", trust_weaver_reference"
        ++ ", allowed_id_types"
        ++ ", csv_title"
        ++ ", csv_contents"
        ++ ", csv_signatory_index"
        ++ ", cancelation_reason"
        ++ ", sharing"
        ++ ", rejection_time"
        ++ ", rejection_signatory_link_id"
        ++ ", rejection_reason"
        ++ ", tags"
        ++ ", mail_footer"
        ++ ", region"
        ++ ", deleted) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)") [
            toSql $ documentid doc
          , toSql $ documentservice doc
          , toSql $ fileid `fmap` mdocfile
          , toSql $ fileid `fmap` mdocsealedfile
          , toSql $ documenttitle doc
          , toSql $ documentstatus doc
          , toSql doctype
          , toSql docprocess
          , toSql $ documentfunctionality doc
          , toSql $ documentctime doc
          , toSql $ documentmtime doc
          , toSql $ documentdaystosign doc
          , toSql $ unTimeoutTime `fmap` documenttimeouttime doc
          , toSql $ signtime `fmap` documentinvitetime doc
          , toSql $ signipnumber `fmap` documentinvitetime doc
          , toSql $ documentlog doc
          , toSql $ documentinvitetext doc
          , toSql $ "" -- documenttrustweaverreference doc
          , toSql $ documentallowedidtypes doc
          , toSql $ csvtitle `fmap` documentcsvupload doc
          , toSql $ csvcontents `fmap` documentcsvupload doc
          , toSql $ csvsignatoryindex `fmap` documentcsvupload doc
          , toSql $ documentcancelationreason doc
          , toSql $ documentsharing doc
          , toSql $ fst3 `fmap` documentrejectioninfo doc
          , toSql $ snd3 `fmap` documentrejectioninfo doc
          , toSql $ thd3 `fmap` documentrejectioninfo doc
          , toSql $ documenttags doc
          , toSql $ documentmailfooter $ documentui doc
          , toSql $ documentregion doc
          , toSql $ documentdeleted doc
          ]
      forM_ (documentsignatorylinks doc) $ \sl -> do
        _ <- wrapDB $ \conn -> run conn ("INSERT INTO signatory_links ("
          ++ "  id"
          ++ ", document_id"
          ++ ", user_id"
          ++ ", company_id"
          ++ ", fields"
          ++ ", sign_order"
          ++ ", token"
          ++ ", sign_time"
          ++ ", sign_ip"
          ++ ", seen_time"
          ++ ", seen_ip"
          ++ ", read_invitation"
          ++ ", invitation_delivery_status"
          ++ ", signinfo_text"
          ++ ", signinfo_signature"
          ++ ", signinfo_certificate"
          ++ ", signinfo_provider"
          ++ ", signinfo_first_name_verified"
          ++ ", signinfo_last_name_verified"
          ++ ", signinfo_personal_number_verified"
          ++ ", roles"
          ++ ", deleted"
          ++ ", really_deleted) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)") [
              toSql $ signatorylinkid sl
            , toSql $ documentid doc
            , toSql $ maybesignatory sl
            , toSql $ maybecompany sl
            , toSql $ signatoryfields $ signatorydetails sl
            , toSql $ signatorysignorder $ signatorydetails sl
            , toSql $ signatorymagichash sl
            , toSql $ signtime `fmap` maybesigninfo sl
            , toSql $ signipnumber `fmap` maybesigninfo sl
            , toSql $ signtime `fmap` maybeseeninfo sl
            , toSql $ signipnumber `fmap` maybeseeninfo sl
            , toSql $ maybereadinvite sl
            , toSql $ invitationdeliverystatus sl
            , toSql $ signatureinfotext `fmap` signatorysignatureinfo sl
            , toSql $ signatureinfosignature `fmap` signatorysignatureinfo sl
            , toSql $ signatureinfocertificate `fmap` signatorysignatureinfo sl
            , toSql $ signatureinfoprovider `fmap` signatorysignatureinfo sl
            , toSql $ signaturelstnameverified `fmap` signatorysignatureinfo sl
            , toSql $ signaturelstnameverified `fmap` signatorysignatureinfo sl
            , toSql $ signaturepersnumverified `fmap` signatorysignatureinfo sl
            , toSql $ signatoryroles sl
            , toSql $ signatorylinkdeleted sl
            , toSql $ signatorylinkreallydeleted sl
            ]
        return ()
      forM_ (documentauthorattachments doc) $ \(AuthorAttachment file) -> do
        _ <- wrapDB $ \conn -> run conn ("INSERT INTO author_attachments ("
          ++ "  file_id"
          ++ ", document_id) VALUES (?, ?)") [
              toSql $ fileid file
            , toSql $ documentid doc
            ]
        return ()
      forM_ (documentsignatoryattachments doc) $ \file -> do
        let msigfile = signatoryattachmentfile file
        insertFile msigfile
        fid <- FileID `fmap` getUniqueID D.tableFiles
        when (isNothing msigfile) $ do
          _ <- wrapDB $ \conn -> run conn ("INSERT INTO files ("
            ++ "  id"
            ++ ", name) VALUES (?, ?)") [
                toSql fid
              , toSql $ signatoryattachmentname file
              ]
          return ()
        _ <- wrapDB $ \conn -> run conn ("INSERT INTO signatory_attachments ("
          ++ "  file_id"
          ++ ", document_id"
          ++ ", email"
          ++ ", description) VALUES (?, ?, ?, ?)") [
              toSql $ maybe fid (FileID . fromIntegral . unFileID . fileid) msigfile
            , toSql $ documentid doc
            , toSql $ signatoryattachmentemail file
            , toSql $ signatoryattachmentdescription file
            ]
        return ()
      return ()
  where
    insertFile Nothing = return ()
    insertFile (Just file) = do
      let (storage, content) = case filestorage file of
            FileStorageMemory s -> (Nothing, Just $ Binary s)
            fs -> (Just fs, Nothing)
      _ <- wrapDB $ \conn -> run conn ("INSERT INTO files ("
        ++ "  id"
        ++ ", name"
        ++ ", content"
        ++ ", storage) VALUES (?, ?, ?, ?)") [
            toSql $ fileid file
          , toSql $ filename file
          , toSql content
          , toSql storage
          ]
      return ()
#endif
#endif
