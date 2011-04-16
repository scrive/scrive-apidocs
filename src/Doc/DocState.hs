module Doc.DocState 
    ( module Doc.DocStateData
    , isAuthor -- from Utils
    , isTemplate -- fromUtils
    , isContract -- fromUtils
    , isOffer -- fromUtils
    , matchingType -- fromUtils
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
    , SignableFromDocument(..)
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
    
filterSignatoryLinksByUser doc user = 
    [sl | sl <- documentsignatorylinks doc
        , isMatchingSignatoryLink user sl     -- user must match
        , not $ signatorylinkdeleted sl    ]  -- filter out deleted links
    
signatoryCanView user doc = 
    let usersiglinks = filterSignatoryLinksByUser doc user
        isnotpreparation = Preparation /= documentstatus doc
        hasLink = not $ Prelude.null usersiglinks
    in isnotpreparation && hasLink

getDocumentsBySignatory :: User -> Query Documents [Document]
getDocumentsBySignatory user = do
    documents <- ask
    return $ filter (signatoryCanView user) (toList documents)
       

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
  authorlink <- signLinkFromDetails [(unEmail $ useremail $ userinfo user, user)]
                (signatoryDetailsFromUser user) 
                [SignatoryPartner]

  let doc = Document
          { documentid = DocumentID 0
          , documenttitle = title
          , documentauthor = Author $ userid user
          , documentsignatorylinks = if (isContract documenttype) then [authorlink] else []
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
          , authorpersonalnumberplacements = []
          , authorcompanynumberplacements = []
          , authorotherfields = []
          , documentcancelationreason = Nothing
          , documentinvitetime = Nothing
          } `appendHistory` [DocumentHistoryCreated ctime]

  insertNewDocument doc
  

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
               -> [(SignatoryDetails,[SignatoryRole])]
               -> Maybe Int
               -> BS.ByteString
               -> User
               -> SignatoryDetails
               -> [IdentificationType]
               -> Maybe Int
               -> Update Documents (Either String Document)
updateDocument time documentid signatories daystosign invitetext author authordetails idtypes mcsvsigindex =
    modifySignableOrTemplateWithAction documentid $ \document ->  
        if documentstatus document == Preparation
         then do
             let authoremail = unEmail $ useremail $ userinfo author
             signatorylinks <- sequence $ map (uncurry $ signLinkFromDetails [(authoremail, author)]) signatories
             let csvupload = case (documentcsvupload document, 
                                   fmap (checkCSVSigIndex (userid author) signatorylinks) mcsvsigindex) of
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
                      , authorpersonalnumberplacements = signatorypersonalnumberplacements authordetails
                      , authorcompanynumberplacements = signatorycompanynumberplacements authordetails
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
    modifySignableOrTemplateWithAction documentid $ \document ->
        let msigindex = checkCSVSigIndex (unAuthor $ documentauthor document) 
                                         (documentsignatorylinks document)
                                         (csvsignatoryindex csvupload) in
        case (msigindex, documentstatus document) of
          (Left s, _) -> return $ Left s
          (Right n, Preparation) -> return . Right $ document { documentcsvupload = Just csvupload }
          _ -> return $ Left "Document not in preparation"

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
                  , signatorypersonalnumber = personalnumber
                  , signatorycompanynumber = companynumber
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
          signatoryHasSigned x = not (SignatoryPartner `elem` signatoryroles x) || isJust (maybesigninfo x)
          allsignedbutauthor = all signatoryHasSigned allbutauthor
          isallsigned = all signatoryHasSigned newsignatorylinks
          
          -- Check if there are custom fields in any signatory (that is, not author)
          hasfields = any ((any (not . fieldfilledbyauthor)) . (signatoryotherfields . signatorydetails)) (documentsignatorylinks document)

          updateWithFields [] sd = sd
          updateWithFields ((name, value):fs) sd 
              | name == BS.fromString "sigco" = updateWithFields fs sd { signatorycompany = value }
              | name == BS.fromString "sigpersnr" = updateWithFields fs sd { signatorypersonalnumber = value }
              | name == BS.fromString "sigcompnr" = updateWithFields fs sd { signatorycompanynumber = value }
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
    modifySignable documentid $ \document ->
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
                                  , documentinvitetime = sinfo
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
    modifySignable documentid $ \document ->
        case documentstatus document of
          Preparation -> 
              let timeout = do
                             days <- documentdaystosign document 
                             return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                  authorid = userid author
                  sinfo = Just (SignInfo time ipnumber)
                  sigdetails = map signatorydetails (documentsignatorylinks document)
                  authorOnly = Prelude.null $ tail $ documentsignatorylinks document
                  signeddocument = document { documenttimeouttime = timeout
                                  , documentmtime = time
                                  , documentsignatorylinks = signWithUserID (documentsignatorylinks document) authorid sinfo msiginfo
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
               -> Update Documents (Either String Document)
rejectDocument documentid signatorylinkid1 time ipnumber = do
  modifySignable documentid $ \document ->
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
    modifySignable documentid $ \document -> 
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
    modifySignable docid $ \doc -> do
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
  modifySignable documentid $ \document ->
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
  modifySignable documentid $ \doc ->
      Right $ doc{ documenttimeouttime = Just timeouttime }


archiveDocuments :: User -> [DocumentID] -> Update Documents ()
archiveDocuments user docidlist = do
  -- FIXME: can use a fold here
  forM_ docidlist $ \docid -> 
     modifySignableOrTemplate docid $ \doc -> 
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
        updateDoc takeover document = modifySignableOrTemplate (documentid document) (\doc -> Right $ takeover doc)
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
  doc <- modifySignable docid $
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
    else if (not $ isAuthor doc user)
         then return $ Left $ "Can't restart document if you are not it's author"
         else do 
           doc' <- clearSignInfofromDoc doc user
           let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
           return $ Right doc''


clearSignInfofromDoc doc author = do
  let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x)) $ documentsignatorylinks doc
      authoremail = unEmail $ useremail $ userinfo author
  newSignLinks <- sequence $ map (uncurry $ signLinkFromDetails [(authoremail, author)]) signatoriesDetails
  return doc {documentstatus = Preparation,
              documenttimeouttime = Nothing,
              documentsignatorylinks = newSignLinks
             }

changeSignatoryEmailWhenUndelivered::DocumentID -> SignatoryLinkID -> BS.ByteString ->  Update Documents (Either String Document)
changeSignatoryEmailWhenUndelivered did slid email = modifySignable did $ changeEmail
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

signLinkFromDetails emails details roles = do
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
                     , signatoryroles = roles
                     , signatorylinkdeleted = False
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
getUserTemplates userid = do 
    documents <- ask
    let mydocuments = (documents @= Author userid )
    return $ toList $ (mydocuments @= OfferTemplate) ||| (mydocuments @= ContractTemplate)
    
signableFromDocument :: DocumentID -> Update Documents (Either String Document)
signableFromDocument = newFromDocument $ \doc -> 
    doc {
          documentstatus = Preparation
        , documenttype =  if (documenttype doc == OfferTemplate) 
                             then Offer
                             else if (documenttype doc == ContractTemplate)
                                    then Contract
                                    else documenttype doc
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
                        , 'signableFromDocument
                        , 'contractFromSignatoryData
                        , 'templateFromDocument
                        ])
