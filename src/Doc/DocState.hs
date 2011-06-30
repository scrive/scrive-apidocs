{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.DocState
    ( module Doc.DocStateData
    , isTemplate -- fromUtils
    , SignatoryAccount -- fromUtils
    , getSignatoryAccount -- fromUtils
    , isDeletableDocument -- fromUtils
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
    , GetDocumentByDocumentIDAllEvenQuarantinedDocuments(..)
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
    , SignableFromDocumentIDWithUpdatedAuthor(..)
    , DocumentFromSignatoryData(..)
--    , MigrateToSigLinks(..)
    , ExtendDocumentQuarantine(..)
    , ReviveQuarantinedDocument(..)
    , GetExpiredQuarantinedDocuments(..)
    , EndQuarantineForDocument(..)
    , MigrateForDeletion(..)
    , UpdateDocumentRecordStatus(..)
    , UpdateSigAttachments(..)
    , SaveSigAttachment(..)
    , MigrateDocumentAuthorAttachments(..)
    , UnquarantineAll(..)
    , MakeFirstSignatoryAuthor(..)
    )
where

import API.Service.ServiceState
import Company.CompanyState
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.List (find)
import Data.Maybe
import Data.Word
import Doc.DocProcess
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
import Util.SignatoryLinkUtils

getDocuments:: (Maybe ServiceID) -> Query Documents [Document]
getDocuments mservice = queryDocs $ \documents ->
    toList $ documents @= mservice

getQuarantinedDocuments :: (Maybe ServiceID) -> Query Documents [Document]
getQuarantinedDocuments mservice = queryQuarantinedDocs $ \documents ->
    toList $ documents @= mservice

getDocumentByDocumentID :: DocumentID -> Query Documents (Maybe Document)
getDocumentByDocumentID documentid = queryDocs $ \documents ->
    getOne $ documents @= documentid

getDocumentByDocumentIDAllEvenQuarantinedDocuments :: DocumentID -> Query Documents (Maybe Document)
getDocumentByDocumentIDAllEvenQuarantinedDocuments documentid = do
  documents <- ask
  return $ getOne $ documents @= documentid

getDocumentsByAuthor :: UserID -> Query Documents [Document]
getDocumentsByAuthor userid = queryDocs $ \documents ->
    IxSet.toList (documents @= Author (userid))

getDocumentsByUser :: User -> Query Documents [Document]
getDocumentsByUser user = do
  documents <- ask
  -- this should be looking up by userid, but it would miss docs that aren't yet saved for the user
  return $  filter (\d -> documentservice d == userservice user) $ IxSet.toList (documents @= (useremail $ userinfo user))

filterSignatoryLinksByUser :: Document -> User -> [SignatoryLink]
filterSignatoryLinksByUser doc user =
    [sl | sl <- documentsignatorylinks doc
        , isSigLinkFor user sl     -- user must match
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
    -- this should be looking up by userid but it would miss docs that aren't yet saved for the user
    filter (signatoryCanView user) (toList $ documents @= (useremail $ userinfo user) @= userservice user)

filterSignatoryLinksBySupervisor :: Document -> User -> [SignatoryLink]
filterSignatoryLinksBySupervisor doc user =
    [sl | sl <- documentsignatorylinks doc
        , isSigLinkFor (Supervisor (userid user)) sl
        , not $ signatorylinkdeleted sl]

supervisorCanView :: User -> Document -> Bool
supervisorCanView user doc =
    let supersiglinks = filterSignatoryLinksBySupervisor doc user
        isSupervisedSignatory = signatoryCanView' supersiglinks (documentstatus doc) (documentcurrentsignorder doc)
        isSupervisedAuthor = any isAuthor supersiglinks
    in isSupervisedAuthor || isSupervisedSignatory

getDocumentsBySupervisor :: User -> Query Documents [Document]
getDocumentsBySupervisor user = queryDocs $ \documents ->
   filter (supervisorCanView user) (toList $ documents @= Supervisor (userid user))

getTimeoutedButPendingDocuments :: MinutesTime -> Query Documents [Document]
getTimeoutedButPendingDocuments now = queryDocs $ \docs ->
  (flip filter) (toList docs) $ \doc -> case (documenttimeouttime doc) of
                                            Just timeout -> (documentstatus doc) == Pending &&(unTimeoutTime timeout) < now
                                            _ -> False

newDocumentFunctionality :: DocumentType -> User -> DocumentFunctionality
newDocumentFunctionality documenttype user =
  if documenttype == Signable Order 
   then AdvancedFunctionality
   else case (getValueForProcess documenttype processadvancedview, preferreddesignmode $ usersettings user) of
    (Just True, Nothing) -> BasicFunctionality
    (Just True, Just BasicMode) -> BasicFunctionality
    _ -> AdvancedFunctionality

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
  let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                    then [SignatoryAuthor]
                    else [SignatoryPartner, SignatoryAuthor]
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
            , documentauthorattachments    = []
            , documentsignatoryattachments = []
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
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentoriginalcompany      = Nothing
          , documentrecordstatus         = LiveDocument
          , documentquarantineexpiry     = Nothing
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

getDocumentByFileID :: FileID -> Query Documents (Either String Document)
getDocumentByFileID fileid' = queryDocs $ \documents ->
  case getOne (documents @= fileid') of
    Nothing -> Left $ "cannot find document for file #" ++ show fileid'
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
                 updatedFstFileName  = case (documentfiles document) of
                                         (f:fs) -> (f {filename= docname} :fs)
                                         fs -> fs
             return $ Right $ document
                    { documentsignatorylinks         = alllinks
                    , documentdaystosign             = daystosign
                    , documentmtime                  = time
                    , documenttitle                  = docname
                    , documentinvitetext             = invitetext
                    , documentallowedidtypes         = idtypes
                    , documentcsvupload              = csvupload
                    , documentfunctionality          = docfunctionality
                    , documentfiles                  = updatedFstFileName
                    }
         else return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be in Preparation to use updateDocument"

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
          _ -> return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be in Preparation to use attachCSVUpload"


getDocumentsByDocumentID :: [DocumentID] -> Query Documents (Either String [Document])
getDocumentsByDocumentID docids = queryDocs $ \documents ->
  let relevantdocs = documents @+ docids in
  if (length docids == size relevantdocs)
    then Right . toList $ relevantdocs
    else Left "documents don't exist for all the given ids"

{- |
   Add and remove attachments to a document in Preparation
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
    Creates a new contract by pumping some values into a particular signatory.
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
                           , not $ isAuthor sl]
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
  maximum $ (fromSeconds 0) : (map documentmtime $ toList (documents @= fileid))


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
      relevantSigLink doc = listToMaybe $ filter (isSigLinkFor user) (documentsignatorylinks doc)
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
        isSignatoryOrSupervisor sl = isSigLinkFor (userid, useremail) sl || isSigLinkFor (Supervisor userid) sl

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
      then Right $ deleteDocumentIfRequired now users $ deleteSigLinks doc
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
    quarantineExpiry = 3 `monthsAfter` now
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
    return $ doc { documentquarantineexpiry = fmap (monthsAfter 1) (documentquarantineexpiry doc) }

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
        if isAuthor (doc, user)
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
   Restarts document,
   Checks the author and status
   Clears sign links and stuff
   Sets status to Pending

   It is passed a document
-}
restartDocument :: Document -> User -> MinutesTime -> Word32 -> Update Documents (Either String Document)
restartDocument doc user time ipnumber = do
   mndoc <- tryToGetRestarted doc user time ipnumber
   case mndoc of
        Right newdoc -> newFromDocument (const newdoc) (documentid doc)
        other -> return other



{- |
    Returns restarted version of document
    Checks the autor and status
    Clears sign links and stuff
 -}
tryToGetRestarted :: Document -> User -> MinutesTime -> Word32 -> Update Documents (Either String Document)
tryToGetRestarted doc user time ipnumber =
  if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
  then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
  else if (not $ isAuthor (doc, user))
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
  let Just authorsiglink = find isAuthor newSignLinks
      othersiglinks = filter (not . isAuthor) newSignLinks
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
justTemplates docs = (docs @= Template Offer) ||| (docs @= Template Contract) ||| (docs @= Template Order)

signableFromDocument :: Document -> Update Documents Document
signableFromDocument doc = insertNewDocument $ templateToDocument doc


-- You basicly always want to update author data. Not only if you want to change author
-- This is due to the fact that author personal data could get changed
signableFromDocumentIDWithUpdatedAuthor :: User -> DocumentID -> Update Documents (Either String Document)
signableFromDocumentIDWithUpdatedAuthor user = newFromDocument $ \doc -> 
    (templateToDocument doc) {
          documentsignatorylinks = map (replaceAuthorSigLink user doc) (documentsignatorylinks doc)
                                   -- FIXME: Need to remove authorfields?
    }
    where replaceAuthorSigLink :: User -> Document -> SignatoryLink -> SignatoryLink
          replaceAuthorSigLink usr _ sl
            | isAuthor sl = replaceSignatoryUser sl usr
            | otherwise = sl



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
    let Signable process = documenttype doc in
    Right $ doc {
          documentstatus = Preparation
        , documenttype =  Template process
        }

migrateForDeletion :: [User] -> Update Documents ()
migrateForDeletion users = do
  docs <- fmap toList ask
  now <- getMinuteTimeDB
  let docs2 = map propagetUsers docs
      docs3 = map (deleteDocumentIfRequired now users) docs2
  mapM_ (\doc -> modify (updateIx (documentid doc) doc)) docs3
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

unquarantineAll :: Update Documents ([Either String Document])
unquarantineAll = do
  docs <- fmap toList ask
  let qdocs = [docid | Document{ documentrecordstatus = QuarantinedDocument
                               , documentid = docid} <- docs ]
  mapM unq qdocs
  where
    unq docid =
      modifySignableOrTemplate docid $ \doc ->
      return $ doc { documentrecordstatus = LiveDocument,
                     documentquarantineexpiry = Nothing }


updateSigAttachments :: DocumentID -> [SignatoryAttachment] -> Update Documents (Either String Document)
updateSigAttachments docid sigatts =
  modifySignableOrTemplate docid $ \doc ->
  case documentstatus doc of
    Preparation -> Right doc { documentsignatoryattachments = sigatts }
    _ -> Left "Can only attach to document in Preparation"

saveSigAttachment :: DocumentID -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Update Documents (Either String Document)
saveSigAttachment docid name email content = do
  documents <- ask
  fileid <- getUnique documents FileID
  modifySignableOrTemplate docid $ \doc ->
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


migrateDocumentAuthorAttachments :: DocumentID -> [File] -> Update Documents (Either String Document)
migrateDocumentAuthorAttachments docid files =
  modifyDocumentWithActionTime False (const True) docid $
  \doc -> return $ if length files > 0
          then Right doc { documentauthorattachments = for files (\f ->
                                                                   AuthorAttachment { authorattachmentfile = f })
                         , documentattachments = []
                         }
          else Left "No documentattachments."

makeFirstSignatoryAuthor :: DocumentID -> Update Documents (Either String Document)
makeFirstSignatoryAuthor docid =
  modifySignableOrTemplate docid $
  \doc -> case getAuthorSigLink doc of
    Just _ -> Left "Already has an author."
    Nothing ->
      let fsig = head (documentsignatorylinks doc)
          rsig = tail (documentsignatorylinks doc)
      in Right doc { documentsignatorylinks =
                        fsig { signatoryroles = SignatoryAuthor : (signatoryroles fsig) }

                        :

                        rsig
                   }

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
                        , 'saveSigAttachment

                        , 'getDocumentByFileID
                        , 'errorDocument
                        , 'getUserTemplates
                        , 'getSharedTemplates
                        , 'signableFromDocument
                        , 'signableFromDocumentIDWithUpdatedAuthor
                        , 'documentFromSignatoryData
                        , 'templateFromDocument
--                        , 'migrateToSigLinks
                        , 'migrateForDeletion
                        , 'migrateDocumentAuthorAttachments
                        , 'unquarantineAll
                        , 'getDocumentByDocumentIDAllEvenQuarantinedDocuments
                        , 'makeFirstSignatoryAuthor
                        ])
