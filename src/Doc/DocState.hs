{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror -fno-warn-orphans #-}
module Doc.DocState 
    ( module Doc.DocStateData
    , isTemplate -- fromUtils
    , isContract -- fromUtils
    , isOffer -- fromUtils
    , matchingType -- fromUtils
    , signatoryname -- fromUtils
    , SignatoryAccount -- fromUtils
    , getSignatoryAccount -- fromUtils
    , isDeletableDocument -- fromUtils
    , isMatchingSignatoryLink
    , anyInvitationUndelivered
    , undeliveredSignatoryLinks
    , ArchiveDocuments(..)
    , ArchiveDocumentForAll(..)
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
    , GetQuarantinedDocuments(..)
    , GetDocumentsByAuthor(..)
    , GetDocumentsBySignatory(..)
    , GetDocumentsBySupervisor(..)
    , GetDocumentsByUser(..)
    , GetFilesThatShouldBeMovedToAmazon(..)
    , GetNumberOfDocumentsOfUser(..)
    , GetTimeoutedButPendingDocuments(..)
    , MarkDocumentSeen(..)
    , MarkInvitationRead(..)
    , SetInvitationDeliveryStatus(..)
    , NewDocument(..)
    , NewDocumentWithMCompany(..)
    , SaveDocumentForSignedUser(..)
    , SetDocumentTimeoutTime(..)
    , SetDocumentTags(..)
    , GetDocumentsByCompanyAndTags(..)
    , SetDocumentTrustWeaverReference(..)
    , ShareDocuments(..)
    , SetDocumentTitle(..)
    , SignDocument(..)
    , TimeoutDocument(..)
    , UpdateDocument(..)
    , UpdateDocumentSimple(..)
    , AttachCSVUpload(..)
    , GetDocumentsByDocumentID(..)
    , UpdateDocumentAttachments(..)
    , FinaliseAttachments(..)
    , CloseDocument(..)
    , CancelDocument(..)
    , RestartDocument(..)
    , ChangeSignatoryEmailWhenUndelivered(..)
    , signatoryDetailsFromUser
    , SetSignatoryLinks(..)
    , GetUniqueSignatoryLinkID(..)
    , GetMagicHash(..)
    , GetDocumentByFileID(..)
    , ErrorDocument(..)
    , GetUserTemplates(..)
    , GetSharedTemplates(..)
    , TemplateFromDocument(..)
    , SignableFromDocument(..)
    , SignableFromDocumentID(..)
    , SignableFromSharedDocumentID(..)
    , ContractFromSignatoryData(..)
--    , MigrateToSigLinks(..)
    , ExtendDocumentQuarantine(..)
    , ReviveQuarantinedDocument(..)
    , GetExpiredQuarantinedDocuments(..)
    , EndQuarantineForDocument(..)
    , MigrateForDeletion(..)
    , UpdateDocumentRecordStatus(..)
    )
where

import API.Service.ServiceState
import Company.CompanyState
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.List (find, nub)
import Data.Maybe
import Data.Word
import Doc.DocStateData
import Doc.DocStateUtils
import Doc.DocUtils
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.State
import Mails.MailsUtil
import MinutesTime
import Misc
import User.UserState
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS


getDocuments:: (Maybe ServiceID) -> Query Documents [Document]
getDocuments mservice = queryDocs $ \documents ->
    toList $ documents @= mservice

getQuarantinedDocuments :: (Maybe ServiceID) -> Query Documents [Document]
getQuarantinedDocuments mservice = queryQuarantinedDocs $ \documents ->
    toList $ documents @= mservice

getDocumentByDocumentID :: DocumentID -> Query Documents (Maybe Document)
getDocumentByDocumentID documentid = queryDocs $ \documents ->
    getOne $ documents @= documentid

getDocumentsByAuthor :: UserID -> Query Documents [Document]
getDocumentsByAuthor userid = queryDocs $ \documents ->
    [doc | doc <- toList documents
         , isUserIDAuthor doc userid
         , not $ isDeletedForUserID doc userid
         ]

getDocumentsByUser :: User -> Query Documents [Document]
getDocumentsByUser user = do
  authorDocs <- getDocumentsByAuthor $ userid user
  signatoryDocs <- getDocumentsBySignatory $ user
  return $ nub (authorDocs ++ signatoryDocs)
    
filterSignatoryLinksByUser :: Document -> User -> [SignatoryLink]
filterSignatoryLinksByUser doc user = 
    [sl | sl <- documentsignatorylinks doc
        , isMatchingSignatoryLink user sl     -- user must match
        , not $ signatorylinkdeleted sl    ]  -- filter out deleted links
    
signatoryCanView :: User -> Document -> Bool
signatoryCanView user doc = 
    let usersiglinks = filterSignatoryLinksByUser doc user
    in signatoryCanView' usersiglinks (documentstatus doc) (documentcurrentsignorder doc)

{- |
    Purpose of this is to share the filtering of documents for signatories
    between getDocumentsBySignatory and getDocumentsBySupervisor.
-}
signatoryCanView' :: [SignatoryLink] -> DocumentStatus -> SignOrder -> Bool
signatoryCanView' siglinks docstatus docsignorder = 
    let isnotpreparation = Preparation /= docstatus
        hasLink = not $ Prelude.null siglinks
        signatoryActivated = all ((>=) docsignorder . signatorysignorder . signatorydetails) siglinks
    in isnotpreparation && hasLink && signatoryActivated


getDocumentsBySignatory :: User -> Query Documents [Document]
getDocumentsBySignatory user = queryDocs $ \documents ->
    filter (signatoryCanView user) (toList $ documents @= userservice user) 

filterSignatoryLinksBySupervisor :: Document -> User -> [SignatoryLink]
filterSignatoryLinksBySupervisor doc user =
    [sl | sl <- documentsignatorylinks doc
        , isSigLinkForSupervisor (userid user) sl
        , not $ signatorylinkdeleted sl]

supervisorCanView :: User -> Document -> Bool
supervisorCanView user doc =
    let supersiglinks = filterSignatoryLinksBySupervisor doc user
        isSupervisedSignatory = signatoryCanView' supersiglinks (documentstatus doc) (documentcurrentsignorder doc)
        isSupervisedAuthor = any siglinkIsAuthor supersiglinks
    in isSupervisedAuthor || isSupervisedSignatory

getDocumentsBySupervisor :: User -> Query Documents [Document]
getDocumentsBySupervisor user = queryDocs $ \documents ->
   filter (supervisorCanView user) (toList $ documents @= userservice user)

getTimeoutedButPendingDocuments :: MinutesTime -> Query Documents [Document]
getTimeoutedButPendingDocuments now = queryDocs $ \docs ->
  (flip filter) (toList docs) $ \doc -> case (documenttimeouttime doc) of
                                            Just timeout -> (documentstatus doc) == Pending &&(unTimeoutTime timeout) < now
                                            _ -> False
    
newDocumentFunctionality :: DocumentType -> User -> DocumentFunctionality
newDocumentFunctionality documenttype user = 
  case (documenttype, preferreddesignmode $ usersettings user) of
    (Contract, Nothing)           -> AdvancedFunctionality
    (Contract, Just AdvancedMode) -> AdvancedFunctionality
    (Contract, Just BasicMode)    -> BasicFunctionality
    (_, _)                        -> AdvancedFunctionality

newDocument :: User
            -> BS.ByteString
            -> DocumentType
            -> MinutesTime 
            -> Update Documents Document
newDocument = newDocumentWithMCompany Nothing

newDocumentWithMCompany :: (Maybe CompanyID) 
            -> User
            -> BS.ByteString
            -> DocumentType
            -> MinutesTime 
            -> Update Documents Document
newDocumentWithMCompany mcompany user title documenttype ctime = do
  let authorRoles = if (isContract documenttype)
                    then [SignatoryPartner, SignatoryAuthor]
                    else [SignatoryAuthor]
  authorlink0 <- (signLinkFromDetails
                  (signatoryDetailsFromUser user) 
                  authorRoles)
  
  let authorlink = copySignatoryAccount user authorlink0

  let doc = blankDocument {
              documenttitle                = title
            , documentsignatorylinks       = [authorlink]
            , documenttype                 = documenttype
            , documentfunctionality        = newDocumentFunctionality documenttype user
            , documentctime                = ctime
            , documentmtime                = ctime
            , documentservice              = userservice user
            , documentattachments          = []
            , documentoriginalcompany      = mcompany `mplus` usercompany user
            } `appendHistory` [DocumentHistoryCreated ctime]

  insertNewDocument doc
  
blankDocument :: Document 
blankDocument =
          Document
          { documentid                   = DocumentID 0
          , documenttitle                = BS.empty
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Contract
          , documentfunctionality        = BasicFunctionality
          , documentctime                = MinutesTime 0 0
          , documentmtime                = MinutesTime 0 0
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
          , documentservice              = Nothing
          , documentattachments          = []
          , documentoriginalcompany      = Nothing
          , documentrecordstatus         = LiveDocument
          , documentquarantineexpiry     = Nothing
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
    moved doc@Document{documentfiles, documentsealedfiles} =
        Right $ doc { documentfiles = map moved1 documentfiles
                    , documentsealedfiles = map moved1 documentsealedfiles
                    }
    moved1 file@File{ fileid
                    , filestorage = FileStorageMemory _
                    } | fileid == fid =
                                 file { filestorage = fstorage }
                      | otherwise = file
    moved1 file = file

getDocumentByFileID :: FileID 
                       -> Query Documents (Either String Document)
getDocumentByFileID fileid' = queryDocs $ \documents ->
  case getOne (documents @= fileid') of
    Nothing -> Left "no such file id"
    Just document -> Right document

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

updateDocument :: MinutesTime
               -> DocumentID
               -> BS.ByteString
               -> [(SignatoryDetails,[SignatoryRole])]
               -> Maybe Int
               -> BS.ByteString
               -> (SignatoryDetails, [SignatoryRole], SignatoryAccount)
               -> [IdentificationType]
               -> Maybe Int
               -> DocumentFunctionality
               -> Update Documents (Either String Document)
updateDocument time documentid docname signatories daystosign invitetext (authordetails, authorroles, authoraccount) idtypes mcsvsigindex docfunctionality =
    modifySignableOrTemplateWithAction documentid $ \document ->  
        if documentstatus document == Preparation
         then do
             authorlink0 <- signLinkFromDetails authordetails authorroles 
             let authorlink = copySignatoryAccount authoraccount authorlink0
             signatorylinks <- sequence $ map (uncurry $ signLinkFromDetails) signatories
             let alllinks = authorlink : signatorylinks
                 csvupload = case (documentcsvupload document, 
                                   fmap (checkCSVSigIndex alllinks) mcsvsigindex) of
                               (Just cu, Just (Right newsigindex)) 
                                 -> Just cu{csvsignatoryindex=newsigindex}
                               _ -> Nothing
             return $ Right $ document 
                    { documentsignatorylinks         = alllinks
                    , documentdaystosign             = daystosign 
                    , documentmtime                  = time
                    , documenttitle                  = docname
                    , documentinvitetext             = invitetext
                    , documentallowedidtypes         = idtypes
                    , documentcsvupload              = csvupload
                    , documentfunctionality          = docfunctionality
                    }
         else return $ Left "Document not in preparation"


updateDocumentSimple::DocumentID -> (SignatoryDetails, SignatoryAccount) -> [SignatoryDetails] -> Update Documents (Either String Document)
updateDocumentSimple did (authordetails,authoraccount) signatories = do
   now <- getMinuteTimeDB
   modifySignableOrTemplateWithAction did $ \document ->  
        if documentstatus document == Preparation
         then do
             authorlink0 <- signLinkFromDetails authordetails [SignatoryPartner,SignatoryAuthor]
             let authorlink = copySignatoryAccount authoraccount authorlink0
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
          _ -> return $ Left "Document not in preparation"


getDocumentsByDocumentID :: [DocumentID] -> Query Documents (Either String [Document])
getDocumentsByDocumentID docids = queryDocs $ \documents ->
  let relevantdocs = documents @+ docids in
  if (length docids == size relevantdocs)
    then Right . toList $ relevantdocs
    else Left "documents don't exist for all the given ids"

updateDocumentAttachments :: DocumentID
                          -> [DocumentID]
                          -> [DocumentID]
                          -> Update Documents (Either String Document)
updateDocumentAttachments docid idstoadd idstoremove = do
  documents <- ask
  let allattachments = documents @+ attachmentids
      foundattachments = length attachmentids == size allattachments
  case (foundattachments, sequence . map checkDoc $ toList allattachments) of
    (False, _) -> return $ Left "documents don't exist for all the given ids"
    (_, Left msg) -> return $ Left msg
    (True, Right _) -> do
      mremoved <- mapM archiveDocumentForAll idstoremove
      case sequence mremoved of
        Left msg -> return $ Left msg
        Right _ ->
          modifySignableOrTemplate docid $ \doc ->
            case checkDoc doc of
              Left msg -> Left msg
              Right ddoc -> 
                return $ ddoc { documentattachments = updateAttachments $ documentattachments ddoc }
  where
    attachmentids :: [DocumentID]
    attachmentids = idstoadd ++ idstoremove
    updateAttachments :: [DocumentID] -> [DocumentID]
    updateAttachments atts = filter (\aid -> not $ aid `elem` attachmentids) atts ++ idstoadd
    checkDoc :: Document -> Either String Document
    checkDoc doc@Document{documentstatus} =
      if documentstatus == Preparation
      then Right doc
      else Left $ "Can't make attachments unless the status is in preparation"

{- |
    When a doc comes out of preparation mode the attachments should be finalised, meaning
    the signatory links setup and status changed to closed.
-}
finaliseAttachments :: [DocumentID] -> [SignatoryLink] -> Update Documents (Either String [Document])
finaliseAttachments attachmentids links = do
  mdocs <- forM attachmentids $ \attid ->
    modifySignableOrTemplate attid $ \doc@Document{documenttype} ->
      if documenttype == Attachment
      then Right $ doc { documentstatus = Pending, documentsignatorylinks = links }
      else Left $ "Needs to be an attachment"
  return $ sequence mdocs

{- |
    Creates a new contract by pumping some values into a particular signatory.
-}
contractFromSignatoryData :: DocumentID
                              -> Int 
                              -> BS.ByteString 
                              -> BS.ByteString 
                              -> BS.ByteString 
                              -> BS.ByteString 
                              -> BS.ByteString
                              -> BS.ByteString
                              -> [BS.ByteString]
                              -> Update Documents (Either String Document)
contractFromSignatoryData docid sigindex fstname sndname email company personalnumber companynumber fieldvalues = newFromDocument toNewDoc docid
  where
    toNewDoc :: Document -> Document
    toNewDoc d = d { documentsignatorylinks = map snd . map toNewSigLink . zip [0..] $ (documentsignatorylinks d)
                    , documentcsvupload = Nothing
                    , documenttype = Contract 
                    , documentsharing = Private }
    toNewSigLink :: (Int, SignatoryLink) -> (Int, SignatoryLink)
    toNewSigLink (i, sl)
      | i==sigindex = (i, pumpData sl)
      | otherwise = (i, sl)
    pumpData :: SignatoryLink -> SignatoryLink
    pumpData siglink = replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues

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
        Just signatoryLink = find (\x -> signatorylinkid x == signatorylinkid1) (documentsignatorylinks document)
        newsignatorylinks = map maybesign (documentsignatorylinks document)
        maybesign link@(SignatoryLink {signatorylinkid, signatorydetails} ) 
          | signatorylinkid == signatorylinkid1 = 
            link { maybesigninfo = Just (SignInfo time ipnumber)
                 , signatorydetails = updateWithFields fields signatorydetails
                 , signatorysignatureinfo = msiginfo
                 }
        maybesign link = link
        allbutauthor = [sl | sl <- newsignatorylinks
                           , not $ siglinkIsAuthor sl]
        signatoryHasSigned x = not (SignatoryPartner `elem` signatoryroles x) || isJust (maybesigninfo x)
        allsignedbutauthor = all signatoryHasSigned allbutauthor
        isallsigned = all signatoryHasSigned newsignatorylinks
          
        -- Check if there are custom fields in any signatory (that is, not author)
        -- ??: We don't use this anymore? -EN
        -- hasfields = any ((any (not . fieldfilledbyauthor)) . (signatoryotherfields . signatorydetails)) (documentsignatorylinks document)

        updateWithFields [] sd = sd
        updateWithFields ((name, value):fs) sd 
          | name == BS.fromString "sigco"     = updateWithFields fs sd { signatorycompany        = value }
          | name == BS.fromString "sigpersnr" = updateWithFields fs sd { signatorypersonalnumber = value }
          | name == BS.fromString "sigcompnr" = updateWithFields fs sd { signatorycompanynumber  = value }
          | otherwise = updateWithFields fs sd { signatoryotherfields = updateOtherFields name value (signatoryotherfields sd) }
        
        updateOtherFields _    _     []      = []
        updateOtherFields name value (f@FieldDefinition { fieldlabel }:fs)  
          | name == fieldlabel = f { fieldvalue = value} : fs
          | otherwise          = f : updateOtherFields name value fs

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

signWithUserID :: [SignatoryLink]
                  -> UserID
                  -> Maybe SignInfo
                  -> Maybe SignatureInfo
                  -> [SignatoryLink]
signWithUserID [] _ _ _ = []
signWithUserID (s:ss) uid sinfo msiginfo
    | maybe False (((==) uid)) (maybesignatory s) = s {maybesigninfo = sinfo, maybeseeninfo = maybe sinfo Just (maybeseeninfo s) , signatorysignatureinfo = msiginfo} : ss
    | otherwise = s : signWithUserID ss uid sinfo msiginfo

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


-- maybe this goes away
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
                                                , not $ siglinkIsAuthor sl
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

setSignatoryLinks :: DocumentID -> [SignatoryLink] -> Update Documents (Either String Document)
setSignatoryLinks docid links =
    modifySignableOrTemplate docid (\doc -> Right doc { documentsignatorylinks = links })

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

-- | 'markDocumentSeen' should set the time when the document was seen
-- first time by the user. It should change the first seen time later
-- on.
markDocumentSeen :: DocumentID 
                 -> SignatoryLinkID 
                 -> MagicHash
                 -> MinutesTime 
                 -> Word32
                 -> Update Documents (Either String Document)
markDocumentSeen documentid signatorylinkid1 mh time ipnumber = do
    modifySignable documentid $ \document -> 
        if (any shouldMark (documentsignatorylinks document))
          then Right $ document { documentsignatorylinks = mapIf shouldMark mark (documentsignatorylinks document) }
          else Left "" 
        where
          shouldMark l = (signatorylinkid l) == signatorylinkid1 && (signatorymagichash l == mh) && (isNothing $ maybeseeninfo l)
          mark l = l { maybeseeninfo = Just (SignInfo time ipnumber) }
                 
             
        
      


-- | We set info about delivering invitation. On undeliver we autocancel document
setInvitationDeliveryStatus::DocumentID -> SignatoryLinkID -> MailsDeliveryStatus -> Update Documents (Either String Document)
setInvitationDeliveryStatus docid siglnkid status = do
    modifySignable docid $ \doc -> do
            Right $ doc {documentsignatorylinks = map setStatus $ documentsignatorylinks doc}                    
    where 
        setStatus sl = 
            if (signatorylinkid sl == siglnkid)
                then sl {invitationdeliverystatus = status}
                else sl 
            


getDocumentStats :: Query Documents DocStats
getDocumentStats = queryDocs $ \documents ->
  let signatureCountForDoc :: Document -> Int
      signatureCountForDoc doc = length $ filter (isJust . maybesigninfo) (documentsignatorylinks doc) in
  DocStats 
      { doccount = (size documents)
      , signaturecount = sum $ map signatureCountForDoc (toList documents)
      , signaturecount1m = 0
      , signaturecount2m = 0
      , signaturecount3m = 0
      , signaturecount6m = 0
      , signaturecount12m = 0
      }

fileModTime :: FileID -> Query Documents MinutesTime
fileModTime fileid = queryDocs $ \documents ->
  maximum $ (MinutesTime 0 0) : (map documentmtime $ toList (documents @= fileid))
  

saveDocumentForSignedUser :: DocumentID -> SignatoryAccount -> SignatoryLinkID 
                          -> Update Documents (Either String Document)
saveDocumentForSignedUser documentid useraccount signatorylinkid1 = do
  modifySignable documentid $ \document ->
      let signeddocument = document { documentsignatorylinks = newsignatorylinks }
          newsignatorylinks = map maybesign (documentsignatorylinks document)
          maybesign x@(SignatoryLink {signatorylinkid} ) 
            | signatorylinkid == signatorylinkid1 = 
              copySignatoryAccount useraccount x
          maybesign x = x
      in Right signeddocument
     

getNumberOfDocumentsOfUser :: User -> Query Documents Int
getNumberOfDocumentsOfUser user = queryDocs $ \documents ->
  size $ documents @= Author (userid user)

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
      relevantSigLink doc = listToMaybe $ filter (isMatchingSignatoryLink user) (documentsignatorylinks doc)
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

setDocumentTimeoutTime :: DocumentID -> TimeoutTime -> Update Documents (Either String Document)
setDocumentTimeoutTime documentid timeouttime = do
  -- check if document status change is a legal transition
  modifySignable documentid $ \doc ->
      Right $ doc{ documenttimeouttime = Just timeouttime }

setDocumentTags :: DocumentID -> [DocumentTag] -> Update Documents (Either String Document)
setDocumentTags docid doctags =
  modifySignableOrTemplate docid $ \doc -> Right $
    doc {
      documenttags = doctags
    }

getDocumentsByCompanyAndTags :: (Maybe ServiceID) -> CompanyID ->  [DocumentTag] -> Query Documents ([Document])
getDocumentsByCompanyAndTags  mservice company doctags = queryDocs $ \documents ->
  toList $ (documents @= (Just company)  @= mservice @* doctags)
  
mapWhen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhen p f ls = map (\i -> if p i then f i else i) ls

archiveDocuments :: UserID -> BS.ByteString -> [(DocumentID, [User])] -> Update Documents (Either String [Document])
archiveDocuments userid useremail docs = do
  -- FIXME: can use a fold here
  mdocs <- mapM (\d -> deleteDocumentSignatoryLinks (fst d) (snd d) isSignatoryOrSupervisor) docs
  return $ sequence mdocs
  where isSignatoryOrSupervisor :: SignatoryLink -> Bool
        isSignatoryOrSupervisor sl = isSigLinkForUserInfo userid useremail sl || isSigLinkForSupervisor userid sl

archiveDocumentForAll :: DocumentID -> Update Documents (Either String Document)
archiveDocumentForAll docid = deleteDocumentSignatoryLinks docid [] (const True)

{- |
    This function deletes any signatory link that passes the predicate you gave it.
    This will then assess whether the document needs to be deleted, and delete as required.

    This is pretty icky but I couldn't get it nicer :(

    You need to pass this function a list of live users (probably just the live
    users relevant to the document in question, but you could pass all the users
    if you really wanted).  It'll use this list to lookup whether a user exists or not.
-}  
deleteDocumentSignatoryLinks :: DocumentID -> [User] -> (SignatoryLink -> Bool) -> Update Documents (Either String Document)
deleteDocumentSignatoryLinks docid users p = do
  now <- getMinuteTimeDB
  modifySignableOrTemplate docid $ \doc ->
    if isDeletableDocument doc
      then Right . deleteDocumentIfRequired now users $ deleteSigLinks doc
      else Left "Unable to delete siglinks for this doc"
  where
    deleteSigLinks doc@Document{documentsignatorylinks} =
      let deleteSigLink sl = sl { signatorylinkdeleted = True }
          newsiglinks = mapWhen p deleteSigLink $ documentsignatorylinks in
      doc { documentsignatorylinks = newsiglinks }

{- |
    This is a very boring function that creates a new zombie document record
    to replace the existing one.  For legal reasons we have to take all the data off,
    but on the other hand I've left the File and SignatoryLinkIDs in tact so that
    we don't reuse in the future.
-}
setupForDeletion :: Document -> Document
setupForDeletion doc = blankDocument { 
                         documentid = documentid doc,
                         documentrecordstatus = DeletedDocument,
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
                           , signatorymagichash = MagicHash { unMagicHash = 0 }
                           , maybesignatory = Nothing
                           , maybesupervisor = Nothing
                           , maybesigninfo  = Nothing
                           , maybeseeninfo  = Nothing
                           , maybereadinvite = Nothing
                           , invitationdeliverystatus = Unknown
                           , signatorysignatureinfo = Nothing
                           , signatoryroles = []
                           , signatorylinkdeleted = False
                           }
  blankSigDetails :: SignatoryDetails
  blankSigDetails = SignatoryDetails { 
      signatoryfstname = BS.empty 
    , signatorysndname = BS.empty 
    , signatorycompany = BS.empty
    , signatorypersonalnumber = BS.empty
    , signatorycompanynumber = BS.empty
    , signatoryemail = BS.empty
    , signatorysignorder = SignOrder 0
    , signatoryfstnameplacements = []
    , signatorysndnameplacements = []
    , signatorycompanyplacements = []
    , signatoryemailplacements = []
    , signatorypersonalnumberplacements = []
    , signatorycompanynumberplacements = []
    , signatoryotherfields = []
    }

{- |
    Call this to go through a record and update it's status by deleting or quarantining if needed.
-}
updateDocumentRecordStatus :: DocumentID -> [User] -> Update Documents (Either String Document)
updateDocumentRecordStatus docid users = do
  now <- getMinuteTimeDB
  modifySignableOrTemplate docid $ \doc ->
    if isDeletableDocument doc
      then Right doc
      else Right $ deleteDocumentIfRequired now users doc

deleteDocumentIfRequired :: MinutesTime -> [User] -> Document -> Document    
deleteDocumentIfRequired now users doc@Document{documentstatus, documentsignatorylinks, documentrecordstatus} =
  case (isNotDeleted, isLive, isInvisible, isInPreparation) of
    (True, _, True, True) -> setupForDeletion doc
    (_, True, True, False) -> 
      doc { documentrecordstatus = QuarantinedDocument, documentquarantineexpiry = Just quarantineExpiry }
    _ -> doc
  where
    quarantineExpiry = addMonths 3 now
    isInPreparation = documentstatus==Preparation
    isLive = documentrecordstatus == LiveDocument
    isQuarantined = documentrecordstatus == QuarantinedDocument
    isNotDeleted = isLive || isQuarantined
    isInvisible = all isInvisibleSigLink documentsignatorylinks
    isInvisibleSigLink sl = isDeletedSigLink sl || isOrphanedSigLink sl
    isDeletedSigLink SignatoryLink{signatorylinkdeleted} = signatorylinkdeleted
    isOrphanedSigLink SignatoryLink{maybesignatory,maybesupervisor} = isNoUser maybesignatory && isNoUser maybesupervisor
    isNoUser Nothing = True
    isNoUser (Just uid) = not $ uid `elem` (map userid users)

getExpiredQuarantinedDocuments :: MinutesTime -> Query Documents [Document]
getExpiredQuarantinedDocuments now = queryQuarantinedDocs $ \docs ->
  (flip filter) (toList docs) $ \doc -> case (documentquarantineexpiry doc) of
                                          Just expiry -> expiry < now
                                          _ -> False

endQuarantineForDocument :: DocumentID -> Update Documents (Either String Document)
endQuarantineForDocument documentid = do
  modifySignableOrTemplate documentid $ \document ->
    case documentrecordstatus document of
      QuarantinedDocument -> return $ setupForDeletion document
      _ -> Left "Document isn't in quarantine"

extendDocumentQuarantine :: DocumentID -> Update Documents (Either String Document)
extendDocumentQuarantine docid = do
  modifySignableOrTemplate docid $ \doc ->
    return $ doc { documentquarantineexpiry = fmap (addMonths 1) (documentquarantineexpiry doc) }

reviveQuarantinedDocument :: DocumentID -> SignatoryLinkID -> Update Documents (Either String Document)
reviveQuarantinedDocument docid siglinkid = do
  modifySignableOrTemplate docid $ \doc ->
    let newsiglinks = mapWhen (\sl -> siglinkid == signatorylinkid sl)
                              (\sl -> sl { signatorylinkdeleted = False })
                              (documentsignatorylinks doc) in
    return $ doc { documentsignatorylinks = newsiglinks,
                   documentrecordstatus = LiveDocument,
                   documentquarantineexpiry = Nothing }

shareDocuments :: User -> [DocumentID] -> Update Documents (Either String [Document])
shareDocuments user docidlist = do
  mdocs <- forM docidlist $ \docid ->
    modifySignableOrTemplate docid $ \doc ->
        if isUserAuthor doc user
          then Right $ doc { documentsharing = Shared }
          else Left $ "Can't share document unless you are the author"
  return $ sequence mdocs

setDocumentTitle :: DocumentID -> BS.ByteString -> Update Documents (Either String Document)
setDocumentTitle docid doctitle =
  modifySignableOrTemplate docid $ \doc@Document{documentstatus} ->
    if documentstatus == Preparation
    then Right $ doc { documenttitle = doctitle }
    else Left $ "Can't update title unless the status is in preparation"

--This is only for Eric functionality with awayting author
--We should add current state checkers here (not co cancel closed documents etc.)
closeDocument :: DocumentID 
              -> MinutesTime
              -> Word32
              -> Maybe SignatureInfo
              -> Update Documents (Maybe Document)
closeDocument docid time ipnumber msiginfo = do
  doc <- modifySignable docid $
          \document -> let timeout = do
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
  case doc of
    Left _ -> return Nothing
    Right d -> return $ Just d

cancelDocument :: DocumentID -> CancelationReason -> MinutesTime -> Word32 -> Update Documents (Either String Document)
cancelDocument docid cr time ipnumber = modifySignable docid $ \document -> do
    let canceledDocument =  document { 
                              documentstatus = Canceled 
                            , documentcancelationreason = Just cr} 
                            `appendHistory` [DocumentHistoryCanceled time ipnumber] 
    case documentstatus document of
        Pending -> Right canceledDocument
        AwaitingAuthor -> Right canceledDocument
        _ -> Left $ "Incalid document status " ++ show (documentstatus document) ++ " in cancelDocument"
  

getFilesThatShouldBeMovedToAmazon :: Query Documents [File]
getFilesThatShouldBeMovedToAmazon = queryDocs $ \documents ->
  let doclist = IxSet.toList documents
      getFiles Document{documentfiles,documentsealedfiles} = documentfiles ++ documentsealedfiles
      allFiles = concatMap getFiles doclist
      getID file@File{ filestorage = FileStorageMemory _ } = [file]
      getID _ = [] in
  concatMap getID allFiles


{- |
   Restarts document,    
   Checks the author and status
   Clears sign links and stuff
   Sets status to Pending
    
   It is passed a document 
-} 
restartDocument :: DocumentID -> User -> MinutesTime -> Word32 -> Update Documents (Either String Document)
restartDocument docid user time ipnumber =
   modifySignableWithAction docid (\d -> tryToGetRestarted d user time ipnumber)    


{- | 
    Returns restarted version of document
    Checks the autor and status
    Clears sign links and stuff
 -}
tryToGetRestarted :: Document -> User -> MinutesTime -> Word32 -> Update Documents (Either String Document)
tryToGetRestarted doc user time ipnumber = 
  if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
  then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
  else if (not $ isUserAuthor doc user)
       then return $ Left $ "Can't restart document if you are not it's author"
       else do 
         doc' <- clearSignInfofromDoc doc
         let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
         return $ Right doc''

clearSignInfofromDoc :: Document -> Update Documents Document
clearSignInfofromDoc doc = do
  let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x)) $ documentsignatorylinks doc
      Just asl = getAuthorSigLink doc
  newSignLinks <- sequence $ map (uncurry $ signLinkFromDetails) signatoriesDetails
  let Just authorsiglink = find siglinkIsAuthor newSignLinks
      othersiglinks = filter (not . siglinkIsAuthor) newSignLinks
      newsiglinks = copySignatoryAccount asl authorsiglink : othersiglinks
  return doc {documentstatus = Preparation,
              documenttimeouttime = Nothing,
              documentsignatorylinks = newsiglinks
             }

changeSignatoryEmailWhenUndelivered::DocumentID -> SignatoryLinkID -> BS.ByteString ->  Update Documents (Either String Document)
changeSignatoryEmailWhenUndelivered did slid email = modifySignable did $ changeEmail
  where changeEmail doc = let signlinks = documentsignatorylinks doc
                              mnsignlink = do
                                           sl <- find ((== slid) . signatorylinkid) signlinks
                                           when (invitationdeliverystatus sl /= Undelivered && invitationdeliverystatus sl /= Deferred) Nothing
                                           return $ sl {invitationdeliverystatus = Unknown, signatorydetails = (signatorydetails sl) {signatoryemail = email}}
                                           
                          in case mnsignlink  of
                           Just nsl -> let sll = for signlinks $ \sl -> if ( slid == signatorylinkid sl) then nsl else sl      
                                       in  if (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor)
                                            then Right $ doc {documentsignatorylinks = sll}
                                            else Left "We cant change status of not pending documents"
                                            
                           Nothing -> Left "We could not find signatory"            
                     
--UTILS - have to be put before creating action constructors
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
                     , maybesupervisor = Nothing
                     , maybesigninfo  = Nothing
                     , maybeseeninfo  = Nothing
                     , maybereadinvite = Nothing
                     , invitationdeliverystatus = Unknown
                     , signatorysignatureinfo = Nothing
                     , signatoryroles = roles
                     , signatorylinkdeleted = False
                     }
          
getUniqueSignatoryLinkID :: Update Documents SignatoryLinkID
getUniqueSignatoryLinkID = do
  sg <- ask
  linkid <- getUnique sg SignatoryLinkID
  return linkid

setDocumentTrustWeaverReference :: DocumentID -> String -> Update Documents (Either String Document)
setDocumentTrustWeaverReference documentid reference = do
  modifySignable documentid $ \document ->
      let
          newdocument = document { documenttrustweaverreference = Just (BS.fromString reference) }
      in Right newdocument
  
errorDocument :: DocumentID -> String -> Update Documents (Either String Document)
errorDocument documentid errormsg = 
  modifySignableOrTemplate documentid $ \document ->
      let
          newdocument = document { documentstatus = DocumentError errormsg }
      in Right newdocument


getUserTemplates:: UserID -> Query Documents [Document]
getUserTemplates userid = queryDocs $ \documents ->
    let mydocuments = (documents @= Author userid ) in
    toList $ justTemplates mydocuments

{- |
    The shared templates for the given users.
-}
getSharedTemplates :: [UserID] -> Query Documents [Document]
getSharedTemplates userids = queryDocs $ \documents ->
    let userdocs = documents @+ (map Author userids) in
    filter ((== Shared) . documentsharing) . toList $ justTemplates userdocs

justTemplates :: (Indexable a, Typeable a, Ord a) => IxSet a -> IxSet a
justTemplates docs = (docs @= OfferTemplate) ||| (docs @= ContractTemplate)

signableFromDocument :: Document -> Update Documents Document
signableFromDocument doc = insertNewDocument $ templateToDocument doc

signableFromDocumentID :: DocumentID -> Update Documents (Either String Document)
signableFromDocumentID = newFromDocument $ templateToDocument

signableFromSharedDocumentID :: User -> DocumentID -> Update Documents (Either String Document)
signableFromSharedDocumentID user = newFromDocument $ \doc -> 
    (templateToDocument doc) {
          documentsignatorylinks = map (replaceAuthorSigLink user doc) (documentsignatorylinks doc)
                                   -- FIXME: Need to remove authorfields?
    }
    where replaceAuthorSigLink :: User -> Document -> SignatoryLink -> SignatoryLink
          replaceAuthorSigLink usr _ sl 
            | siglinkIsAuthor sl = replaceSignatoryUser sl usr
            | otherwise = sl



templateToDocument :: Document -> Document
templateToDocument doc =   doc {
          documentstatus = Preparation
        , documenttype =  if (documenttype doc == OfferTemplate) 
                             then Offer
                             else if (documenttype doc == ContractTemplate)
                                    then Contract
                                    else documenttype doc
        , documentsharing = Private
    }
{- |
    Replaces signatory data with given user's data.
-}
replaceSignatoryUser :: SignatoryLink
                        -> User
                        -> SignatoryLink
replaceSignatoryUser siglink user =
  let newsl = replaceSignatoryData 
                       siglink
                       (userfstname         $ userinfo user)
                       (usersndname         $ userinfo user)
                       (unEmail $ useremail $ userinfo user)
                       (usercompanyname     $ userinfo user)
                       (userpersonalnumber  $ userinfo user)
                       (usercompanynumber   $ userinfo user)
                       [] in
  copySignatoryAccount user newsl

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
replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues =
  siglink { signatorydetails = pumpData (signatorydetails siglink) }
  where
    pumpData :: SignatoryDetails -> SignatoryDetails
    pumpData sd = sd 
                  { signatoryfstname = fstname
                  , signatorysndname = sndname
                  , signatorycompany = company
                  , signatorypersonalnumber = personalnumber
                  , signatorycompanynumber = companynumber
                  , signatoryemail = email
                  , signatoryotherfields = zipWith pumpField (fieldvalues ++ repeat BS.empty) $ signatoryotherfields sd}
    pumpField :: BS.ByteString -> FieldDefinition -> FieldDefinition
    pumpField val fd = fd
                       { fieldvalue = val
                       , fieldfilledbyauthor = (not $ BS.null val)
                       } 

templateFromDocument :: DocumentID -> Update Documents (Either String Document)
templateFromDocument docid = modifySignable docid $ \doc -> 
    Right $ doc {
          documentstatus = Preparation
        , documenttype =  if (documenttype doc == Offer) 
                             then OfferTemplate 
                             else if (documenttype doc == Contract)
                                    then ContractTemplate
                                    else documenttype doc
        }

migrateForDeletion :: [User] -> Update Documents ()
migrateForDeletion users = do
  docs <- fmap toList ask  
  now <- getMinuteTimeDB
  mapM_ (\doc -> modify (updateIx (documentid doc) (propagetUsers doc))) $ docs
  docs2 <- fmap toList ask
  mapM_ (\doc -> modify (updateIx (documentid doc) (deleteDocumentIfRequired now users doc))) $ docs2
  where
    {- |
        This populates the maybesignatory & maybesupervisor on each of the signatory links for
        the given user.
    -}
    propagetUsers :: Document -> Document
    propagetUsers doc =
     doc { documentsignatorylinks = for (documentsignatorylinks doc) $ \sl ->  
                                        case find (isSignatoryForUser sl) users of
                                            Just user -> copySignatoryAccount user sl
                                            Nothing -> sl } 
    isSignatoryForUser sl u = isSavedforUser sl u || isSignedByUser sl u
    isSavedforUser sl u = maybesignatory sl == Just (userid u)
    isSignedByUser sl u = signatoryemail (signatorydetails sl) == (unEmail $ useremail $ userinfo u ) && isJust (maybesigninfo sl)
      
      

{-
-- | Migrate author to the documentsignlinks so that he is not special anymore
migrateToSigLinks :: DocumentID -> User -> Update Documents ()
migrateToSigLinks docid author = do 
  sg <- ask
  linkid <- getUnique sg SignatoryLinkID
  magichash <- getRandom
  modifySignable docid $ 
      \doc ->
          case getAuthorSigLink doc of
            Just authorsiglink -> Right doc
            Nothing ->
                Right doc { documentsignatorylinks = newAuthorSigLink : (filter (\sl -> Just (userid author) /= maybesignatory sl) $ documentsignatorylinks doc) }
                    where newAuthorSigLink = SignatoryLink 
                                             { signatorylinkid = linkid
                                             , signatorydetails = authordetails
                                             , signatorymagichash = magichash
                                             , maybesignatory = Just $ userid author
                                             , maybesigninfo = Nothing
                                             , maybeseeninfo = documentinvitetime doc
                                             , invitationdeliverystatus = Unknown
                                             , signatorysignatureinfo = Nothing
                                             , signatoryroles = [SignatoryAuthor]
                                             , signatorylinkdeleted = documentdeleted doc
                                             }
                          authordetails = (signatoryDetailsFromUser author)
                                          { signatoryfstnameplacements = 
                                                authorfstnameplacements doc
                                          , signatorysndnameplacements =
                                                authorsndnameplacements doc
                                          , signatorycompanyplacements =
                                                authorcompanyplacements doc
                                          , signatoryemailplacements = 
                                                authoremailplacements doc
                                          , signatorypersonalnumberplacements =
                                                authorpersonalnumberplacements doc
                                          , signatorycompanynumberplacements = 
                                                authorcompanynumberplacements doc
                                          , signatoryotherfields =
                                                authorotherfields doc
                                          }
  return ()
-}                                 
-- create types for event serialization
$(mkMethods ''Documents [ 'getDocuments
                        , 'getQuarantinedDocuments
                        , 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'getDocumentsBySupervisor
                        , 'newDocument
                        , 'newDocumentWithMCompany 
                        , 'getDocumentByDocumentID
                        , 'getTimeoutedButPendingDocuments
                        , 'updateDocument
                        , 'updateDocumentSimple
                        , 'attachCSVUpload
                        , 'getDocumentsByDocumentID
                        , 'updateDocumentAttachments
                        , 'finaliseAttachments
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
                        , 'saveDocumentForSignedUser
                        , 'getDocumentsByUser
                        , 'getNumberOfDocumentsOfUser
                        , 'setDocumentTimeoutTime
                        , 'setDocumentTags
                        , 'getDocumentsByCompanyAndTags 
                        , 'setDocumentTrustWeaverReference
                        , 'archiveDocuments
                        , 'archiveDocumentForAll
                        , 'updateDocumentRecordStatus
                        , 'shareDocuments
                        , 'setDocumentTitle
                        , 'timeoutDocument
                        , 'closeDocument
                        , 'cancelDocument
                        , 'fileMovedToAWS
                        , 'fileMovedToDisk
                          -- admin only area follows
                        , 'extendDocumentQuarantine
                        , 'reviveQuarantinedDocument
                        , 'getExpiredQuarantinedDocuments
                        , 'endQuarantineForDocument
                        , 'getFilesThatShouldBeMovedToAmazon
                        , 'restartDocument
                        , 'changeSignatoryEmailWhenUndelivered
                        , 'setSignatoryLinks
                        , 'getUniqueSignatoryLinkID
                        , 'getMagicHash
                          
                        , 'getDocumentByFileID
                        , 'errorDocument
                        , 'getUserTemplates
                        , 'getSharedTemplates
                        , 'signableFromDocument
                        , 'signableFromDocumentID
                        , 'signableFromSharedDocumentID
                        , 'contractFromSignatoryData
                        , 'templateFromDocument
--                        , 'migrateToSigLinks
                        , 'migrateForDeletion
                        ])
