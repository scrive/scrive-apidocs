module Doc.DocState 
    ( module Doc.DocStateData
    , isAuthor -- from Utils
    , signatoryname -- fromUtils
    , isMatchingSignatoryLink
    , anyInvitationUndelivered
    , undeliveredSignatoryLinks
    , ArchiveDocuments(..)
    , AttachFile(..)
    , AttachSealedFile(..)
    , AuthorSignDocument(..)
    , AuthorSendDocument(..)
    , RejectDocument(..)
    , FileModTime(..)
    , FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , FragileTakeOverDocuments(..)
    , GetDocumentByDocumentID(..)
    , GetDocumentStats(..)
    , GetDocumentStatsByUser(..)
    , GetDocuments(..)
    , GetDocumentsByAuthor(..)
    , GetDocumentsBySignatory(..)
    , GetDocumentsByUser(..)
    , GetFilesThatShouldBeMovedToAmazon(..)
    , GetNumberOfDocumentsOfUser(..)
    , GetTimeoutedButPendingDocuments(..)
    , MarkDocumentSeen(..)
    , SetInvitationDeliveryStatus(..)
    , NewDocument(..)
    , SaveDocumentForSignedUser(..)
    , SetDocumentTimeoutTime(..)
    , SetDocumentTrustWeaverReference(..)
    , SignDocument(..)
    , TimeoutDocument(..)
    , UpdateDocument(..)
    , AttachCSVUpload(..)
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
    , TemplateFromDocument(..)
    , ContractFromDocument(..)
    , ContractFromSignatoryData(..)
    , IdentificationType(..)
    , SignatureInfo(..)
    , SignatureProvider(..)
    , CancelationReason(..)
    )
where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans
import User.UserState
import Happstack.Data.IxSet as IxSet
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Debug.Trace
import Misc
import Control.Monad
import Data.List (find)
import MinutesTime
import Data.List (zipWith4,partition, find)
import System.Random
import Data.Word
import Data.Int
import System.Log.Logger (errorM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Mails.MailsUtil
import Data.Data (Data)
import qualified Data.Generics.SYB.WithClass.Derive as SYB
import Doc.DocStateData
import Doc.DocStateUtils

getDocuments:: Query Documents [Document]
getDocuments = do
    documents <- ask
    return $ toList documents  

getDocumentByDocumentID :: DocumentID -> Query Documents (Maybe Document)
getDocumentByDocumentID documentid = do
  documents <- ask
  return $ getOne (documents @= documentid)

getDocumentsByAuthor :: UserID -> Query Documents [Document]
getDocumentsByAuthor userid = do
    documents <- ask
    return $ toList (documents @= Author userid)

getDocumentsByUser :: User -> Query Documents [Document]
getDocumentsByUser user = do
    authoredDocs <- getDocumentsByAuthor $ userid user
    signatoryDocs <- getDocumentsBySignatory $ user
    return $ authoredDocs ++ signatoryDocs

getDocumentsBySignatory :: User -> Query Documents [Document]
getDocumentsBySignatory user = do
    documents <- ask
    let involvedAsSignatory doc = (any (isMatchingSignatoryLink user) $ documentsignatorylinks doc)
                                   && (Preparation /= documentstatus doc)
    return $ filter involvedAsSignatory (toList documents)
       

getTimeoutedButPendingDocuments :: MinutesTime -> Query Documents [Document]
getTimeoutedButPendingDocuments now = do
  docs <-  ask
  return $ (flip filter) (toList docs) $ \doc -> case (documenttimeouttime doc) of
                                                  Just timeout -> (documentstatus doc) == Pending &&(unTimeoutTime timeout) < now
                                                  _ -> False           
    

newDocument :: User
            -> BS.ByteString
            -> DocumentType
            -> MinutesTime 
            -> Update Documents Document
newDocument user title documenttype ctime = do
  authorlink <- signLinkFromDetails [(unEmail $ useremail $ userinfo user, user)] $ signatoryDetailsFromUser user
  let doc = Document
          { documentid = DocumentID 0
          , documenttitle = title
          , documentauthor = Author $ userid user
          , documentsignatorylinks = [authorlink]
          , documentfiles = []
          , documentstatus = Preparation
          , documenttype = documenttype
          , documentctime = ctime
          , documentmtime = ctime
          , documentdaystosign = Nothing
          , documenttimeouttime = Nothing
          , documentdeleted = False
          , documentlog = []
          , documentinvitetext = BS.empty
          , documentsealedfiles = []
          , documenttrustweaverreference = Nothing
          , documentallowedidtypes = [EmailIdentification]
          , documentcsvupload = Nothing
          , authorfstnameplacements = []
          , authorsndnameplacements = []
          , authoremailplacements = []
          , authorcompanyplacements = []
          , authornumberplacements = []
          , authorotherfields = []
          , documentcancelationreason = Nothing
          , documentinvitetime = Nothing
          } `appendHistory` [DocumentHistoryCreated ctime]

  insertNewDocument doc
  

fileMovedToAWS :: FileID 
               -> BS.ByteString
               -> BS.ByteString
               -> Update Documents (Either String Document)
fileMovedToAWS fileid bucket url = fileMoveTo fileid $  FileStorageAWS bucket url

fileMovedToDisk :: FileID -> FilePath -> Update Documents (Either String Document)
fileMovedToDisk fileid filepath = fileMoveTo fileid $ FileStorageDisk filepath

fileMoveTo :: FileID -> FileStorage -> Update Documents (Either String Document)
fileMoveTo fid fstorage = do
    documents <- ask
    case getOne (documents @= fid) of
         Nothing -> return $ Left "no such file id"
         Just document -> modifyContract (documentid document) $ moved
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
getDocumentByFileID fileid' = do
  documents <- ask
  case getOne (documents @= fileid') of
    Nothing -> return $ Left "no such file id"
    Just document -> return $ Right document

attachFile :: DocumentID 
           -> BS.ByteString 
           -> BS.ByteString 
           -> Update Documents (Either String Document)
attachFile documentid filename1 content = do
  documents <- ask
  fileid2 <- getUnique documents FileID
  modifyContractOrTemplate documentid $ \document ->
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
  modifyContract documentid $ \document ->
      let nfile = File { fileid = fileid2
                       , filename = filename1
                       , filestorage = FileStorageMemory content
                       }
      in Right $ document { documentsealedfiles = documentsealedfiles document ++ [nfile] }

updateDocument :: MinutesTime
               -> DocumentID
               -> [SignatoryDetails]
               -> Maybe Int
               -> BS.ByteString
               -> User
               -> SignatoryDetails
               -> [IdentificationType]
               -> Maybe Int
               -> Update Documents (Either String Document)
updateDocument time documentid signatories daystosign invitetext author authordetails idtypes mcsvsigindex =
    modifyContractOrTemplateWithAction documentid $ \document ->  
        if documentstatus document == Preparation
         then do
             let authoremail = unEmail $ useremail $ userinfo author
             signatorylinks <- sequence $ map (signLinkFromDetails [(authoremail, author)]) signatories
             let csvupload = case (documentcsvupload document, fmap (checkCSVSigIndex signatorylinks) mcsvsigindex) of
                               (Just cu@CSVUpload{csvsignatoryindex}, Just (Right newsigindex)) 
                                    | csvsignatoryindex==newsigindex -> Just cu
                               _ -> Nothing
             return $ Right $ document 
                    { documentsignatorylinks = signatorylinks
                      , documentdaystosign = daystosign 
                      , documentmtime = time
                      , documentinvitetext = invitetext
                      , documentallowedidtypes = idtypes
                      , documentcsvupload = csvupload
                      , authorfstnameplacements = signatoryfstnameplacements authordetails
                      , authorsndnameplacements = signatorysndnameplacements authordetails
                      , authoremailplacements = signatoryemailplacements authordetails
                      , authorcompanyplacements = signatorycompanyplacements authordetails
                      , authornumberplacements = signatorynumberplacements authordetails
                      , authorotherfields = signatoryotherfields authordetails
                      }
         else return $ Left "Document not in preparation"

{- |
    This attaches a csv upload to the document.
-}
attachCSVUpload :: DocumentID
                   -> CSVUpload
                   -> Update Documents (Either String Document)
attachCSVUpload documentid csvupload =
    modifyContractOrTemplateWithAction documentid $ \document ->
        let msigindex = checkCSVSigIndex (documentsignatorylinks document) $ csvsignatoryindex csvupload in
        case (msigindex, documentstatus document) of
          (Left s, _) -> return $ Left s
          (Right n, Preparation) -> return . Right $ document { documentcsvupload = Just csvupload }
          _ -> return $ Left "Document not in preparation"

checkCSVSigIndex :: [SignatoryLink] -> Int -> Either String Int
checkCSVSigIndex sls n
  | n==0 = Left "author can't be set from csv"
  | n>=(length sls) = Left $ "signatory with index " ++ (show n) ++ " doesn't exist."
  | otherwise = Right n

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
                              -> [BS.ByteString]
                              -> Update Documents (Either String Document)
contractFromSignatoryData docid sigindex fstname sndname email company number fieldvalues = newFromDocument toNewDoc docid
  where
    toNewDoc :: Document -> Document
    toNewDoc d = d { documentsignatorylinks = map snd . map toNewSigLink . zip [0..] $ (documentsignatorylinks d)
                    , documentcsvupload = Nothing
                    , documenttype = Contract }
    toNewSigLink :: (Int, SignatoryLink) -> (Int, SignatoryLink)
    toNewSigLink (i, sl)
      | i==sigindex = (i, sl { signatorydetails = pumpData (signatorydetails sl) })
      | otherwise = (i, sl)
    pumpData :: SignatoryDetails -> SignatoryDetails
    pumpData sd = sd 
                  { signatoryfstname = fstname
                  , signatorysndname = sndname
                  , signatorycompany = company
                  , signatorynumber = number
                  , signatoryemail = email
                  , signatoryotherfields = zipWith pumpField (fieldvalues ++ repeat BS.empty) $ signatoryotherfields sd}
    pumpField :: BS.ByteString -> FieldDefinition -> FieldDefinition
    pumpField val fd = fd
                       { fieldvalue = val
                       , fieldfilledbyauthor = (not $ BS.null val)
                       } 

timeoutDocument :: DocumentID
                -> MinutesTime
                -> Update Documents (Either String Document)
timeoutDocument documentid time = do
  modifyContract documentid $ \document ->
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
  modifyContract documentid $ \document ->
      let
          signeddocument = document { documentsignatorylinks = newsignatorylinks }
                           `appendHistory` [DocumentHistorySigned time ipnumber (signatorydetails signatoryLink)]
          Just signatoryLink = find (\x -> signatorylinkid x == signatorylinkid1) (documentsignatorylinks document)
          newsignatorylinks = map maybesign (documentsignatorylinks document)
          maybesign link@(SignatoryLink {signatorylinkid, signatorydetails} ) 
              | signatorylinkid == signatorylinkid1 = 
                  link { maybesigninfo = Just (SignInfo time ipnumber)
                       , signatorydetails = updateWithFields fields signatorydetails
                       , signatorysignatureinfo = msiginfo
                    }
          maybesign link = link
          authorid = unAuthor $ documentauthor signeddocument
          allbutauthor = filter ((maybe True (/= authorid)) . maybesignatory) newsignatorylinks
          allsignedbutauthor = all (isJust . maybesigninfo) allbutauthor
          isallsigned = all (isJust . maybesigninfo) newsignatorylinks
          
          -- Check if there are custom fields in any signatory (that is, not author)
          hasfields = any ((any (not . fieldfilledbyauthor)) . (signatoryotherfields . signatorydetails)) (documentsignatorylinks document)

          updateWithFields [] sd = sd
          updateWithFields ((name, value):fs) sd 
              | name == BS.fromString "sigco" = updateWithFields fs sd { signatorycompany = value }
              | name == BS.fromString "signr" = updateWithFields fs sd { signatorynumber = value }
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

signWithUserID [] _ _ _ = []
signWithUserID (s:ss) id sinfo msiginfo
    | maybe False (((==) id)) (maybesignatory s) = s {maybesigninfo = sinfo, maybeseeninfo = maybe sinfo Just (maybeseeninfo s) , signatorysignatureinfo = msiginfo} : ss
    | otherwise = s : signWithUserID ss id sinfo msiginfo

authorSendDocument :: DocumentID
                   -> MinutesTime
                   -> Word32
                   -> User
                   -> Maybe SignatureInfo
                   -> Update Documents (Either String Document)
authorSendDocument documentid time ipnumber author msiginfo =
    modifyContract documentid $ \document ->
        case documentstatus document of
          Preparation -> 
              let timeout = do
                             days <- documentdaystosign document 
                             return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                  authorid = userid author
                  sinfo = Just (SignInfo time ipnumber)
                  sigdetails = map signatorydetails (documentsignatorylinks document)
              in Right $ document { documenttimeouttime = timeout
                                  , documentmtime = time
                                  , documentstatus = Pending
                                  } `appendHistory` [DocumentHistoryInvitationSent time ipnumber sigdetails]
              
          Timedout -> Left "FÃ¶rfallodatum har passerat" -- possibly quite strange here...
          _ ->        Left ("Bad document status: " ++ show (documentstatus document))


-- maybe this goes away
authorSignDocument :: DocumentID
                   -> MinutesTime
                   -> Word32
                   -> User
                   -> Maybe SignatureInfo
                   -> Update Documents (Either String Document)
authorSignDocument documentid time ipnumber author msiginfo =
    modifyContract documentid $ \document ->
        case documentstatus document of
          Preparation -> 
              let timeout = do
                             days <- documentdaystosign document 
                             return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                  authorid = userid author
                  sinfo = Just (SignInfo time ipnumber)
                  sigdetails = map signatorydetails (documentsignatorylinks document)
                  signeddocument = document { documenttimeouttime = timeout
                                  , documentmtime = time
                                  , documentsignatorylinks = signWithUserID (documentsignatorylinks document) authorid sinfo msiginfo
                                  , documentstatus = Pending
                                  } `appendHistory` [DocumentHistoryInvitationSent time ipnumber sigdetails]
              in Right $ signeddocument
              
          Timedout -> Left "FÃ¶rfallodatum har passerat" -- possibly quite strange here...
          _ ->        Left ("Bad document status: " ++ show (documentstatus document))
  
getMagicHash :: Update Documents MagicHash
getMagicHash = getRandom

setSignatoryLinks :: DocumentID -> [SignatoryLink] -> Update Documents (Either String Document)
setSignatoryLinks docid links =
    modifyContractOrTemplate docid (\doc -> Right doc { documentsignatorylinks = links })

rejectDocument :: DocumentID
               -> SignatoryLinkID 
               -> MinutesTime 
               -> Word32 
               -> Update Documents (Either String Document)
rejectDocument documentid signatorylinkid1 time ipnumber = do
  modifyContract documentid $ \document ->
      let
          signlinks = documentsignatorylinks document
          Just sl = find ((== signatorylinkid1) . signatorylinkid) signlinks
          newdocument = document { documentstatus = Rejected } `appendHistory` 
                        [DocumentHistoryRejected time ipnumber (signatorydetails sl)]
      in case documentstatus document of
           Pending ->  Right newdocument
           Timedout -> Left "FÃ¶rfallodatum har passerat"
           _ ->        Left "Bad document status"
  

-- | 'markDocumentSeen' should set the time when the document was seen
-- first time by the user. It should change the first seen time later
-- on.
markDocumentSeen :: DocumentID 
                 -> SignatoryLinkID 
                 -> MinutesTime 
                 -> Word32
                 -> Update Documents ()
markDocumentSeen documentid signatorylinkid1 time ipnumber = do
    modifyContract documentid $ \document -> 
        if (any shouldMark (documentsignatorylinks document))
            then Right $ document { documentsignatorylinks = mapIf shouldMark mark (documentsignatorylinks document)}
            else Left "" 
    return ()        
       where
        shouldMark l = (signatorylinkid l) == signatorylinkid1 && (isNothing $ maybeseeninfo l)
        mark l =  l { maybeseeninfo = Just (SignInfo time ipnumber) }
                 
             
        
      


-- | We set info about delivering invitation. On undeliver we autocancel document
setInvitationDeliveryStatus::DocumentID -> SignatoryLinkID -> MailsDeliveryStatus -> Update Documents (Either String Document)
setInvitationDeliveryStatus docid siglnkid status = do
    modifyContract docid $ \doc -> do
            Right $ doc {documentsignatorylinks = map setStatus $ documentsignatorylinks doc}                    
    where 
        setStatus sl = 
            if (signatorylinkid sl == siglnkid)
                then sl {invitationdeliverystatus = status}
                else sl 
            


getDocumentStats :: Query Documents DocStats
getDocumentStats = do
  documents <- ask
  let signatureCountForDoc :: Document -> Int
      signatureCountForDoc doc = length $ filter (isJust . maybesigninfo) (documentsignatorylinks doc)
  return DocStats {
                      doccount = (size documents)
                    , signaturecount = sum $ map signatureCountForDoc (toList documents)
                   }

fileModTime :: FileID -> Query Documents MinutesTime
fileModTime fileid = do
  documents <- ask
  return $ maximum $ (MinutesTime 0 0) : (map documentmtime $ toList (documents @= fileid))
  

saveDocumentForSignedUser :: DocumentID -> UserID -> SignatoryLinkID 
                          -> Update Documents (Either String Document)
saveDocumentForSignedUser documentid userid signatorylinkid1 = do
  modifyContract documentid $ \document ->
      let signeddocument = document { documentsignatorylinks = newsignatorylinks }
          newsignatorylinks = map maybesign (documentsignatorylinks document)
          maybesign x@(SignatoryLink {signatorylinkid} ) 
            | signatorylinkid == signatorylinkid1 = 
              x { maybesignatory = Just (userid)
                }
          maybesign x = x
      in Right signeddocument
     

getNumberOfDocumentsOfUser :: User -> Query Documents Int
getNumberOfDocumentsOfUser user = do
  documents <- ask
  let numdoc = size (documents @= Author (userid user))
  return numdoc

getDocumentStatsByUser :: User -> Query Documents DocStats
getDocumentStatsByUser user = do
  doccount' <- getNumberOfDocumentsOfUser user
  sigdocs <- getDocumentsBySignatory user
  let signaturecount' = length $ filter (isSigned . relevantSigLink) sigdocs
      relevantSigLink :: Document -> SignatoryLink
      relevantSigLink doc = head $ filter (isMatchingSignatoryLink user) (documentsignatorylinks doc)
      isSigned :: SignatoryLink -> Bool
      isSigned = isJust . maybesigninfo
  return DocStats { doccount = doccount', 
                    signaturecount = signaturecount' }

setDocumentTimeoutTime :: DocumentID -> TimeoutTime -> Update Documents (Either String Document)
setDocumentTimeoutTime documentid timeouttime = do
  -- check if document status change is a legal transition
  modifyContract documentid $ \doc ->
      Right $ doc{ documenttimeouttime = Just timeouttime }


archiveDocuments :: User -> [DocumentID] -> Update Documents ()
archiveDocuments user docidlist = do
  -- FIXME: can use a fold here
  forM_ docidlist $ \docid -> 
     modifyContractOrTemplate docid $ \doc -> 
          if (isAuthor doc user)
            then Right $ doc { documentdeleted = True }
            else Left "Not author can not delete document"
          
      

fragileTakeOverDocuments :: User -> User -> Update Documents ()
fragileTakeOverDocuments destuser srcuser = do
  documents <- ask
  let hisdocuments = documents @= Author (userid srcuser)
      sigdocuments = filter ((any (isMatchingSignatoryLink srcuser)) . documentsignatorylinks) (toList documents)
  mapM_ (updateDoc takeoverAsAuthor) (IxSet.toList hisdocuments)
  mapM_ (updateDoc takeoverAsSignatory) sigdocuments
  return ()
    where 
        updateDoc takeover document = modifyContractOrTemplate (documentid document) (\doc -> Right $ takeover doc)
        takeoverAsAuthor document = document { documentauthor = Author (userid destuser) }
        takeoverAsSignatory document = document { documentsignatorylinks = takeoverSigLinks (documentsignatorylinks document) }
        takeoverSigLinks siglinks = (map takeoverSigLink matching) ++ others
                                    where (matching, others) = partition (isMatchingSignatoryLink srcuser) siglinks 
        takeoverSigLink siglink = siglink {maybesignatory = Just (userid destuser),
                                           signatorydetails = takeoverSigDetails (signatorydetails siglink) }
        takeoverSigDetails sigdetails = sigdetails {signatoryfstname = userfstname info,
                                                    signatorysndname = usersndname info,
                                                    signatorycompany = usercompanyname info,
                                                    signatoryemail = unEmail $ useremail info }
                                        where info = userinfo destuser

--This is only for Eric functionality with awayting author
--We should add current state checkers here (not co cancel closed documents etc.)
closeDocument :: DocumentID 
              -> MinutesTime
              -> Word32
              -> User
              -> Maybe SignatureInfo
              -> Update Documents (Maybe Document)
closeDocument docid time ipnumber author msiginfo = do
  doc <- modifyContract docid $
          \document -> let timeout = do
                                      days <- documentdaystosign document 
                                      return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                           authorid = userid author
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
cancelDocument docid cr time ipnumber = modifyContract docid $ \document -> do
    let canceledDocument =  document { 
                              documentstatus = Canceled 
                            , documentcancelationreason = Just cr} 
                            `appendHistory` [DocumentHistoryCanceled time ipnumber] 
    case documentstatus document of
        Pending -> Right canceledDocument
        AwaitingAuthor -> Right canceledDocument
        _ -> Left $ "Incalid document status " ++ show (documentstatus document) ++ " in cancelDocument"
  

getFilesThatShouldBeMovedToAmazon :: Query Documents [File]
getFilesThatShouldBeMovedToAmazon = do
  documents <- ask
  let doclist = IxSet.toList documents
  let getFiles Document{documentfiles,documentsealedfiles} = documentfiles ++ documentsealedfiles
  let allFiles = concatMap getFiles doclist
  let getID file@File{ filestorage = FileStorageMemory _ } = [file]
      getID _ = []
  return (concatMap getID allFiles)


{- |
   Restarts document,    
   Checks the author and status
   Clears sign links and stuff
   Sets status to Pending
    
   It is passed a document 
-} 
restartDocument :: DocumentID -> User -> MinutesTime -> Word32 -> Update Documents (Either String Document)
restartDocument docid user time ipnumber =
   modifyContractWithAction docid (\d -> tryToGetRestarted d user time ipnumber)    


{- | 
    Returns restarted version of document
    Checks the autor and status
    Clears sign links and stuff
 -}
tryToGetRestarted :: Document -> User -> MinutesTime -> Word32 -> Update Documents (Either String Document)
tryToGetRestarted doc user time ipnumber = 
    if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
    then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
    else if (not $ isAuthor doc user)
         then return $ Left $ "Can't restart document if you are not it's author"
         else do 
           doc' <- clearSignInfofromDoc doc user
           let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
           return $ Right doc''


clearSignInfofromDoc doc author = do
  let signatoriesDetails = map signatorydetails $ documentsignatorylinks doc
      authoremail = unEmail $ useremail $ userinfo author
  newSignLinks <- sequence $ map (signLinkFromDetails [(authoremail, author)]) signatoriesDetails
  return doc {documentstatus = Preparation,
              documenttimeouttime = Nothing,
              documentsignatorylinks = newSignLinks
             }

changeSignatoryEmailWhenUndelivered::DocumentID -> SignatoryLinkID -> BS.ByteString ->  Update Documents (Either String Document)
changeSignatoryEmailWhenUndelivered did slid email = modifyContract did $ changeEmail
  where changeEmail doc = let signlinks = documentsignatorylinks doc
                              mnsignlink = do
                                           sl <- find ((== slid) . signatorylinkid) signlinks
                                           when (invitationdeliverystatus sl /= Undelivered ) Nothing
                                           return $ sl {invitationdeliverystatus = Unknown, signatorydetails = (signatorydetails sl) {signatoryemail = email}}
                                           
                          in case mnsignlink  of
                           Just nsl -> let sll = for signlinks $ \sl -> if ( slid == signatorylinkid sl) then nsl else sl      
                                       in  if (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor)
                                            then Right $ doc {documentsignatorylinks = sll}
                                            else Left "We cant change status of not pending documents"
                                            
                           Nothing -> Left "We could not find signatory"            
                     
--UTILS - have to be put before creating action constructors
signLinkFromDetails emails details = do
          sg <- ask
          linkid <- getUnique sg SignatoryLinkID
          magichash <- getRandom
          let muser = findMaybeUserByEmail emails (signatoryemail details)
              msig = maybe Nothing (Just . userid) muser
          return $ SignatoryLink 
                     { signatorylinkid = linkid
                     , signatorydetails = details
                     , signatorymagichash = magichash
                     , maybesignatory = msig
                     , maybesigninfo  = Nothing
                     , maybeseeninfo  = Nothing
                     , invitationdeliverystatus = Unknown
                     , signatorysignatureinfo = Nothing
                     }
                     
          where
          findMaybeUserByEmail [] _ = Nothing
          findMaybeUserByEmail ((email, user):eus) em 
            | email == em = Just user
            | otherwise   = findMaybeUserByEmail eus em
            
getUniqueSignatoryLinkID :: Update Documents SignatoryLinkID
getUniqueSignatoryLinkID = do
  sg <- ask
  linkid <- getUnique sg SignatoryLinkID
  return linkid

setDocumentTrustWeaverReference :: DocumentID -> String -> Update Documents (Either String Document)
setDocumentTrustWeaverReference documentid reference = do
  modifyContract documentid $ \document ->
      let
          newdocument = document { documenttrustweaverreference = Just (BS.fromString reference) }
      in Right newdocument
  
errorDocument :: DocumentID -> String -> Update Documents (Either String Document)
errorDocument documentid errormsg = 
  modifyContractOrTemplate documentid $ \document ->
      let
          newdocument = document { documentstatus = DocumentError errormsg }
      in Right newdocument


getUserTemplates:: UserID -> Query Documents [Document]
getUserTemplates userid = do 
    documents <- ask
    return $ toList (documents @= Author userid @= Template)
    
contractFromDocument :: DocumentID -> Update Documents (Either String Document)
contractFromDocument = newFromDocument $ \doc -> 
    doc {
        documenttype = Contract
      , documentstatus = Preparation
    }
  

templateFromDocument :: DocumentID -> Update Documents (Either String Document)
templateFromDocument docid = modifyContract docid $ \doc -> 
    Right $ doc {
        documenttype = Template
      , documentstatus = Preparation
    }
  
-- create types for event serialization
$(mkMethods ''Documents [ 'getDocuments
                        , 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'getTimeoutedButPendingDocuments
                        , 'updateDocument
                        , 'attachCSVUpload
                        , 'signDocument
                        , 'authorSignDocument
                        , 'authorSendDocument
                        , 'rejectDocument
                        , 'attachFile
                        , 'attachSealedFile
                        , 'markDocumentSeen
                        , 'setInvitationDeliveryStatus
                        , 'getDocumentStats
                        , 'getDocumentStatsByUser
                        , 'fileModTime
                        , 'saveDocumentForSignedUser
                        , 'getDocumentsByUser
                        , 'getNumberOfDocumentsOfUser
                        , 'setDocumentTimeoutTime
                        , 'setDocumentTrustWeaverReference
                        , 'archiveDocuments
                        , 'timeoutDocument
                        , 'closeDocument
                        , 'cancelDocument
                        , 'fileMovedToAWS
                        , 'fileMovedToDisk

                          -- admin only area follows
                        , 'fragileTakeOverDocuments
                        , 'getFilesThatShouldBeMovedToAmazon
                        , 'restartDocument
                        , 'changeSignatoryEmailWhenUndelivered
                        , 'setSignatoryLinks
                        , 'getUniqueSignatoryLinkID
                        , 'getMagicHash
                          
                        , 'getDocumentByFileID
                        , 'errorDocument
                        , 'getUserTemplates
                        , 'contractFromDocument
                        , 'contractFromSignatoryData
                        , 'templateFromDocument
                        ])
