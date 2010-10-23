{-# LANGUAGE CPP, ScopedTypeVariables #-}

module DocControl where
import "base" Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans
import AppView
import Control.Concurrent
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Debug.Trace
import DocState hiding (updateDocument)
import DocView
import HSP
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import Happstack.Util.Common
import KontraLink
import MinutesTime
import Misc
import SendMail(Mail,sendMail,fullnameemails,attachments)
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Process
import User
import UserControl
import UserState
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Seal as Seal

{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

doctransPreparation2Pending :: Context -> Document -> IO Document
doctransPreparation2Pending ctx@Context{ctxtime, ctxipnumber } doc = do
  let MinutesTime m = ctxtime 
  logErrorWithDefault (update $ UpdateDocumentStatus (documentid doc) Pending ctxtime ctxipnumber) doc $ 
   \newdoc -> do
               let timeout = TimeoutTime (MinutesTime (m + documentdaystosign doc * 24 * 60))
               newdoc2 <- update $ SetDocumentTimeoutTime (documentid newdoc) timeout
               forkIO $ do
                 -- this is here to postpone email send a second
                 -- so our service has a chance to give answer first
                 -- is GHC using blocking calls or what?
                 threadDelay 5000 
                 sendInvitationEmails ctx newdoc2
               return newdoc2
 

doctransPending2Closed :: Context -> Document -> IO Document
doctransPending2Closed ctx@Context{ctxtime,ctxipnumber,ctxhostpart} doc = do
  logErrorWithDefault (update $ UpdateDocumentStatus (documentid doc) Closed ctxtime ctxipnumber) doc $ 
   \closedDoc  -> do
                   clearDoc <- update $ RemoveFileFromDoc (documentid doc)
                   Just user <- query $ GetUserByUserID (unAuthor (documentauthor doc))
                   forkIO $ do
                      newdoc <- sealDocument ctxhostpart ctxtime user doc
                      sendClosedEmails ctx newdoc
                   return  clearDoc
   
doctransPending2Canceled :: Context -> Document -> IO Document
doctransPending2Canceled ctx@Context{ctxtime,ctxipnumber} doc = do
  logErrorWithDefault (update $ UpdateDocumentStatus (documentid doc) Canceled ctxtime ctxipnumber) doc return


doctransPending2Timedout :: Context -> Document -> IO Document
doctransPending2Timedout ctx@Context{ctxtime,ctxipnumber} doc = do
   logErrorWithDefault (update $ UpdateDocumentStatus (documentid doc) Timedout ctxtime ctxipnumber) doc return
                    
sendInvitationEmails :: Context -> Document -> IO ()
sendInvitationEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendInvitationEmail1 ctx document)

sendInvitationEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendInvitationEmail1 ctx document signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail
                                                         }
                   , signatorymagichash } = signatorylink
      Document{documenttitle,documentid} = document
  mail <- invitationMail ctx signatoryemail signatoryname
             document signatorylink signatorymagichash
  let attachmentcontent = filepdf $ head $ documentfiles document
  sendMail $ mail { fullnameemails =  [(signatoryname,signatoryemail)]  , attachments = [(documenttitle,attachmentcontent)]} 

sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendClosedEmail1 ctx document)
  sendClosedAuthorEmail ctx document

sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorymagichash
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail }} = signatorylink
      Document{documenttitle,documentid} = document
  mail <- closedMail ctx signatoryemail signatoryname
             document signatorylink signatorymagichash
  let attachmentcontent = filepdf $ head $ documentfiles document
  sendMail $ mail { fullnameemails =  [(signatoryname,signatoryemail)] , attachments = [(documenttitle,attachmentcontent)]}

sendClosedAuthorEmail :: Context -> Document -> IO ()
sendClosedAuthorEmail ctx document = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  mail<- closedMailAuthor ctx (unEmail $ useremail authoruser) (userfullname authoruser)
             document
  let attachmentcontent = filepdf $ head $ documentfiles document
      Document{documenttitle,documentid} = document
  let email2 = signatoryemail $ documentauthordetails document
      email1 = unEmail $ useremail authoruser
      em = if email2/=BS.empty && email2/=email1
           then [(name2,email2)]
           else []
      name1 = userfullname authoruser
      name2 = signatoryname $ documentauthordetails document
  sendMail $ mail { fullnameemails = ([(name1,email1)] ++ em), attachments = [(documenttitle,attachmentcontent)]}
  
  
handleSign :: Kontra Response
handleSign = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime}) <- get
  msum [path $ \documentid -> 
            path $ \signatoryid -> 
                  path $ \magichash -> 
                      handleSignShow documentid signatoryid magichash
       , withUser $ do
          let u = userid $ fromJust ctxmaybeuser
          documents <- query $ GetDocumentsBySignatory u
          renderFromBody ctx TopNone kontrakcja (listDocuments ctxtime u documents)
       ]

signDocument :: DocumentID 
             -> SignatoryLinkID 
             -> Kontra Response
signDocument documentid 
             signatorylinkid1 = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
  getDataFnM (look "sign")
  do
     mdocument <- update $ SignDocument documentid signatorylinkid1 ctxtime ctxipnumber
     case (mdocument) of
      Left messege -> do
                       addFlashMsgText $ BS.fromString messege
                       let link = LinkMain
                       response <- webHSP (seeOtherXML $ show link)
                       seeOther (show LinkMain) response
      Right document -> do 
                        let isallsigned = all (isJust . maybesigninfo) (documentsignatorylinks document)
                        when isallsigned ((liftIO $ doctransPending2Closed ctx document) >> return ())
                        let link = LinkSigned documentid signatorylinkid1
                        response <- webHSP (seeOtherXML $ show link)
                        seeOther (show link) response
  
signatoryLinkFromDocumentByID document@Document{documentsignatorylinks} linkid = do
    let invitedlinks = filter (\x -> signatorylinkid x == linkid
                               {- && signatorymagichash x == magichash1 -})
                              documentsignatorylinks
    case invitedlinks of
      [invitedlink] -> return invitedlink
      _ -> mzero
    

landpageSignInvite ctx document = do
  renderFromBody ctx TopNone kontrakcja $ landpageSignInviteView ctx document

landpageSigned ctx document signatorylinkid = do
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  maybeuser <- query $ GetUserByEmail (Email $ signatoryemail (signatorydetails signatorylink))
  renderFromBody ctx TopEmpty kontrakcja $ landpageSignedView ctx document signatorylink (isJust maybeuser)

{-
 Here we need to save the document either under existing account or create a new account
 send invitation email and put the document in that account
-}
landpageSignedSave ctx@Context{ctxhostpart} document signatorylinkid = do
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  let details = signatorydetails signatorylink
  maybeuser <- query $ GetUserByEmail (Email $ signatoryemail details)
  let fullname = signatoryname details
  user <- case maybeuser of
            Nothing -> do -- create a new user
              password <- g "password"
              password2 <- g "password2"
              tos <- g "tos" -- this will fail if not checked
              -- FIXME: what to do when two passwords do not match?
              -- FIXME: what to do if passwords arent strong enough?
              when (password /= password2) $ error "Passwords do not mutch"
              when (not (isPasswordStrong password)) $ error "Password is too weak"
              let email = signatoryemail details
              user <- liftIO $ createUser ctxhostpart fullname email (Just password) Nothing
              return user
            Just user -> return user
  Just document2 <- update $ SaveDocumentForSignedUser (documentid document) (userid user) signatorylinkid
  -- should redirect
  renderFromBody ctx TopEmpty kontrakcja $ landpageLoginForSaveView ctx document2 signatorylink

landpageSaved (ctx@Context { ctxmaybeuser = Just user@User{userid} }) 
              document@Document{documentid}
              signatorylinkid = do
  Just document2 <- update $ SaveDocumentForSignedUser documentid userid signatorylinkid
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  renderFromBody ctx TopDocument kontrakcja $ landpageDocumentSavedView ctx document signatorylink


handleSignShow :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handleSignShow documentid 
               signatorylinkid1
               magichash1 = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
               
  msum [ DocControl.signDocument documentid signatorylinkid1
       , do 
           -- FIXME: this is so wrong on so many different levels...
          mdocument <- update $ MarkDocumentSeen documentid signatorylinkid1 ctxtime ctxipnumber
          document <- maybe mzero return mdocument
             
          let invitedlinks = filter (\x -> signatorylinkid x == signatorylinkid1
                                     && signatorymagichash x == magichash1)
                              (documentsignatorylinks document)
          invitedlink <- case invitedlinks of
                           [invitedlink] -> return invitedlink
                           _ -> mzero
          let wassigned = f invitedlink
              f (SignatoryLink {maybesigninfo}) = isJust maybesigninfo 
              authoruserid = unAuthor $ documentauthor document
          Just author <- query $ GetUserByUserID authoruserid
          let authorname = signatoryname $ documentauthordetails document
              invitedname = signatoryname $ signatorydetails $ invitedlink 
          renderFromBody ctx TopNone kontrakcja 
                 (showDocumentForSign (LinkSignDoc document invitedlink) 
                      document  ctxmaybeuser invitedlink wassigned)
       ]

handleIssue :: Kontra Response
handleIssue = 
    msum [ pathdb GetDocumentByDocumentID handleIssueShow
         , hget0 handleIssueGet
         , hpost0 handleIssuePost
         ]

freeLeftForUser :: User -> Kontra Int
freeLeftForUser user = do
  numdoc <- query $ GetNumberOfDocumentsOfUser user
  let freeleft = if numdoc >= 5 then 0 else 5 - numdoc
  return freeleft

handleIssueShow :: Document -> Kontra Response
handleIssueShow document@Document{ documentauthor
                                 , documentid
                                 } = do
  ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) <- get
                
  msum [ do
           methodM GET 
           freeleft <- freeLeftForUser user
           when (userid/=unAuthor documentauthor) mzero
           let toptab = if documentstatus document == Closed
                        then TopDocument
                        else TopNew
           renderFromBody ctx toptab kontrakcja 
                                    (showDocument user document False freeleft)
       , do
           methodM POST
           when (userid/=unAuthor documentauthor) mzero
           doc2 <- updateDocument ctx document
           
           if (documentstatus doc2 == Pending &&
               documentstatus document /= Pending) 
             then do
               -- FIXME: create KontraLink
               let link = "/landpage/signinvite/" ++ show documentid
               response <- webHSP (seeOtherXML link)
               seeOther link response
            else do 
             let link = ctxhostpart ++ show LinkIssue
             addFlashMsgHtml $ documentSavedForLaterFlashMessage doc2
             response <- webHSP (seeOtherXML link)
             seeOther link response

       , path $ \(_title::String) -> methodM GET >> do
           when (userid/=unAuthor documentauthor) mzero
           let file = safehead "handleIssueShow" (documentfiles document)
           let contents = filepdf file
           let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
           let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
           return res2
       ]

-- | Useful inside the RqData monad.  Gets the named input parameter
-- (either from a POST or a GET)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name
    = do 
#if MIN_VERSION_happstack_server(0,5,1)
         inputs <- asks (\(a,b,c) -> a ++ b)
#else
         inputs <- asks fst
#endif
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, value <- isname k]

getAndConcat :: String -> Kontra [BS.ByteString]
getAndConcat field = do
  values <- getDataFnM $ lookInputList field
  return $ map concatChunks values

updateDocument :: Context -> Document -> Kontra Document  
updateDocument ctx@Context{ctxtime} document@Document{documentid} = do
  signatoriesnames <- getAndConcat "signatoryname"
  signatoriescompanies <- getAndConcat "signatorycompany"
  signatoriesnumbers <- getAndConcat "signatorynumber"
  signatoriesemails <- getAndConcat "signatoryemail"
  daystosignstring <- getDataFnM (look "daystosign")
  daystosign <- readM daystosignstring

  invitetext <- g "invitetext"

  let signatories = zipWith4 sd signatoriesnames signatoriescompanies signatoriesnumbers signatoriesemails where
                                               sd n c no e = SignatoryDetails n c no e [] [] [] [] []

  -- FIXME: tell the user what happened!
  when (daystosign<1 || daystosign>99) mzero
  
  doc2 <- update $ UpdateDocument ctxtime documentid
          signatories daystosign invitetext

  msum 
     [ do getDataFnM (look "final")
          liftIO $ doctransPreparation2Pending ctx doc2
     , return doc2
     ]
    

handleIssueGet :: Kontra Response
handleIssueGet = do
  ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime}) <- get
  documents <- query $ GetDocumentsByUser (userid user) 
  renderFromBody ctx TopDocument kontrakcja (listDocuments ctxtime (userid user) documents)

gs :: String
#ifdef WINDOWS
gs = "c:\\Program Files\\gs\\gs8.60\\bin\\gswin32c.exe" 
#else
gs = "gs"
#endif

convertPdfToJpgPages :: FileID ->BS.ByteString ->IO JpegPages
convertPdfToJpgPages fileid content = do
  tmppath1 <- getTemporaryDirectory
  let tmppath = tmppath1 ++ "/" ++ show fileid
  createDirectoryIfMissing True tmppath
  let sourcepath = tmppath ++ "/source.pdf"
  BS.writeFile sourcepath content

  let gsproc = (proc gs [ "-sDEVICE=jpeg" 
                        , "-sOutputFile=" ++ tmppath ++ "/output-%d.jpg"
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , "-dTextAlphaBits=4"
                        , "-r144"
                        , sourcepath
                        ]) { std_out = CreatePipe
                           , std_err = CreatePipe
                           }
  (_, Just outhandle, Just errhandle, gsProcHandle) <- createProcess gsproc
  errcontent <- BS.hGetContents errhandle
  outcontent <- BS.hGetContents outhandle
                 
  exitcode <- waitForProcess gsProcHandle

  result <- case exitcode of
    ExitFailure _ -> return $ JpegPagesError (errcontent `BS.append` outcontent)
    ExitSuccess -> do
                  let pathofx x = tmppath ++ "/output-" ++ show x ++ ".jpg"
                  let exists1 x = doesFileExist (pathofx x)
                  let w (x:xs) = do
                                 g <- exists1 x 
                                 if g 
                                  then do
                                   h <- w xs
                                   return (x:h)
                                  else return []

                  listofpages <- w [1..]
                  x <- mapM (\x -> BS.readFile (pathofx x)) listofpages
                  return (JpegPages x)
  -- remove the directory with all the files now
  -- everything as been collected, process has ended, we are done!
  -- removeDirectoryRecursive tmppath
  return result
       

maybeScheduleRendering :: MVar (Map.Map FileID JpegPages) -> File -> IO JpegPages
maybeScheduleRendering mvar 
                       (file@File { fileid
                                  , filepdf 
                                  }) = do
  modifyMVar mvar $ \setoffilesrenderednow ->
      case Map.lookup fileid setoffilesrenderednow of
         Just pages -> return (setoffilesrenderednow, pages)
         Nothing -> do
           forkIO $ do
                -- putStrLn $ "Rendering " ++ show fileid
                -- FIXME: handle exceptions gracefully
                jpegpages <- convertPdfToJpgPages fileid filepdf
                modifyMVar_ mvar (\setoffilesrenderednow -> return (Map.insert fileid jpegpages setoffilesrenderednow))
           return (Map.insert fileid JpegPagesPending setoffilesrenderednow, JpegPagesPending)

handlePageOfDocument :: Document -> Kontra Response
handlePageOfDocument document@Document {documentfiles,documentid} = do
    Context{ctxnormalizeddocuments} <- get
    let pending JpegPagesPending = True
        pending _ = False
    case documentfiles of
      [] -> notFound (toResponse "temporary unavailable (document has no files)")
      f -> do
        b <- mapM (\file -> liftIO $ maybeScheduleRendering ctxnormalizeddocuments file) f
        if any pending b
           then notFound (toResponse "temporary unavailable (document has files pending for process)")
           else webHSP (DocView.showFilesImages2 $ zip f b)
    
handleDocumentUpload :: DocumentID -> BS.ByteString -> BS.ByteString -> IO ()
handleDocumentUpload docid content filename = do
  update $ AttachFile docid filename content

personFromSignatoryDetails :: SignatoryDetails -> Seal.Person
personFromSignatoryDetails details =
    Seal.Person { Seal.fullname = BS.toString $ signatoryname details 
                , Seal.company = BS.toString $ signatorycompany details
                , Seal.email = BS.toString $ signatoryemail details
                , Seal.number = BS.toString $ signatorynumber details
                }

personsFromDocument :: Document -> [(Seal.Person, (MinutesTime,Word32), (MinutesTime,Word32))]
personsFromDocument document = 
    let
        links = documentsignatorylinks document
        unSignInfo (si@SignInfo { signtime, signipnumber }) = (signtime,signipnumber)
        x (SignatoryLink{ signatorydetails
                        , maybesigninfo = Just (si@SignInfo { signtime, signipnumber })
                        , maybeseeninfo
                        })
             -- FIXME: this one should really have seentime always...
             = (personFromSignatoryDetails signatorydetails, unSignInfo $ maybe si id maybeseeninfo, (signtime,signipnumber))
        x link = trace (show link) $ error "SignatoryLink does not have all the necessary data"
    in map x links

sealSpecFromDocument :: String -> Document -> User -> String -> String -> Seal.SealSpec
sealSpecFromDocument hostpart document author@(User {userfullname,usercompanyname,usercompanynumber,useremail}) inputpath outputpath =
  let docid = unDocumentID (documentid document)
      (authorsigntime,authorsignipnumber) = 
          case documentmaybesigninfo document of
            Nothing -> error "signtime1"
            Just (SignInfo {signtime,signipnumber}) -> (signtime,signipnumber)
      authordetails = documentauthordetails document
      authorperson = if authordetails /= emptyDetails 
                     then personFromSignatoryDetails authordetails
                     else personFromSignatoryDetails (SignatoryDetails 
                                  { signatoryname = userfullname
                                  , signatorycompany = usercompanyname
                                  , signatorynumber = usercompanynumber
                                  , signatoryemail = unEmail $ useremail
                                  , signatorynameplacements = []
                                  , signatorycompanyplacements = []
                                  , signatorynumberplacements = []
                                  , signatoryemailplacements = []
                                  , signatoryotherfields = []
                                  })

      signatories = personsFromDocument document

      -- oh boy, this is really network byte order!
      formatIP :: Word32 -> String
      formatIP 0 = ""
      -- formatIP 0x7f000001 = ""
      formatIP x = " (IP: " ++ show ((x `shiftR` 0) .&. 255) ++
                   "." ++ show ((x `shiftR` 8) .&. 255) ++
                   "." ++ show ((x `shiftR` 16) .&. 255) ++
                   "." ++ show ((x `shiftR` 24) .&. 255) ++ ")"
      fst3 (a,b,c) = a
      snd3 (a,b,c) = b
      thd3 (a,b,c) = c
      persons = authorperson : (map fst3 signatories)
      paddeddocid = reverse $ take 20 $ (reverse (show docid) ++ repeat '0')
      initials = concatComma (map initialsOfPerson persons)
      initialsOfPerson (Seal.Person {Seal.fullname}) = map head (words fullname)
      -- 2. "Name of invited" granskar dokumentet online
      makeHistoryEntry (Seal.Person {Seal.fullname},(seentime2,seenipnumber2),(signtime2,signipnumber2)) = 
          [ Seal.HistEntry
            { Seal.histdate = show signtime2
            , Seal.histcomment = fullname ++ " undertecknar dokumentet online" ++ formatIP signipnumber2
            }
          , Seal.HistEntry
            { Seal.histdate = show seentime2
            , Seal.histcomment = fullname ++ " granskar dokumentet online" ++ formatIP seenipnumber2
            } 
          ]
      maxsigntime = maximum (authorsigntime : map (fst . thd3) signatories)
      firstHistEntries = 
          [ Seal.HistEntry
            { Seal.histdate = show authorsigntime
            , Seal.histcomment = Seal.fullname authorperson ++ 
                                 " undertecknar dokumentet." ++ formatIP authorsignipnumber
            }
          , Seal.HistEntry
            { Seal.histdate = show authorsigntime
            , Seal.histcomment = "En automatisk inbjudan skickas via e-post till samtliga parter."
            }
          ]
      lastHistEntry = Seal.HistEntry
                      { Seal.histdate = show maxsigntime
                      , Seal.histcomment = "Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande."
                      }

      concatComma = concat . intersperse ", "

      history = firstHistEntries ++ sort (concatMap makeHistoryEntry signatories) ++ [lastHistEntry]
      
      config = Seal.SealSpec 
            { Seal.input = inputpath
            , Seal.output = outputpath
            , Seal.documentNumber = paddeddocid
            , Seal.persons = persons
            , Seal.history = history
            , Seal.initials = initials
            , Seal.hostpart = hostpart
            }
      in config


sealDocument :: String -> MinutesTime -> User -> Document -> IO Document
sealDocument hostpart signtime1 author@(User {userfullname,usercompanyname,usercompanynumber,useremail}) document = do
  let (file@File {fileid,filename,filepdf,filejpgpages}) = 
           safehead "sealDocument" $ documentfiles document
  let docid = unDocumentID (documentid document)

  tmppath <- getTemporaryDirectory
  let tmpin = tmppath ++ "/in_" ++ show docid ++ ".pdf"
  let tmpout = tmppath ++ "/out_" ++ show docid ++ ".pdf"
  BS.writeFile tmpin filepdf
  let config = sealSpecFromDocument hostpart document author tmpin tmpout

  (code,stdout,stderr) <- readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))

  when (code /= ExitSuccess) $
       putStrLn "Cannot execute dist/build/pdfseal/pdfseal"

  newfilepdf <- BS.readFile tmpout
  let newfile = file {filepdf = newfilepdf, filejpgpages = JpegPagesPending}
  update $ ReplaceFile (documentid document) newfile
  


basename :: String -> String
basename filename = 
    case span (\x -> x/='\\' || x/='/') filename of
      (_,(_:rest)) -> basename rest
      _ -> fst (span ((/=) '.') filename) -- FIXME: take care of many dots in file name

handleIssuePost :: Kontra KontraLink
handleIssuePost = handleIssueNewDocument `mplus` handleIssueArchive

handleIssueNewDocument :: Kontra KontraLink
handleIssueNewDocument = do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    input@(Input content (Just filename) _contentType) <- getDataFnM (lookInput "doc")
    -- see if we have empty input, then there was no file selected
    if BSL.null content
       then do
         return LinkMain
        else do
          -- FIXME: here we have encoding issue
          -- Happstack gives use String done by BS.unpack, so BS.pack it here
          -- in our case it should be utf-8 as this is what we use everywhere
          let title = BSC.pack (basename filename) 
          freeleft <- freeLeftForUser user
          doc <- update $ NewDocument user title ctxtime (freeleft>0)
          liftIO $ forkIO $ handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ LinkIssueDoc doc


handleIssueArchive :: Kontra KontraLink
handleIssueArchive = do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    something <- getDataFnM (lookInput "archive")
    idstrings <- getDataFnM (lookInputList "doccheck")
    let Just ids = sequence $ map (readM . BSL.toString) idstrings
    update $ ArchiveDocuments ids
    return LinkIssue


showPage :: MinutesTime -> FileID -> Int -> Kontra Response
showPage modminutes fileid pageno = do
  Context{ctxnormalizeddocuments} <- get
  docmap <- liftIO $ readMVar ctxnormalizeddocuments
  case Map.lookup fileid docmap of
    Just (JpegPages pages) -> do
      let contents = pages !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
      let modtime = toUTCTime modminutes
      rq <- askRq                 -- FIXME: what?
      return $ ifModifiedSince modtime rq res2
    _ -> mzero

handleResend:: Kontra Response
handleResend = do
    ctx<- get
    pathdb GetDocumentByDocumentID $ \doc -> 
            path $ \signlinkid -> 
                    do
                      case (signlinkFromDocById doc signlinkid) of
                       Just signlink -> 
                               do 
                                mail <- liftIO $ remindMail ctx doc signlink
                                liftIO $ sendMail (mail {fullnameemails = [(signatoryname $ signatorydetails signlink,signatoryemail $ signatorydetails signlink )]})
                                addFlashMsgText ( remindMailFlashMessage doc signlink)
                                let link = show (LinkIssueDoc doc)
                                response <- webHSP (seeOtherXML link)
                                seeOther link response
                       Nothing -> mzero         

   where signlinkFromDocById doc sid = find (((==) sid) . signatorylinkid) (documentsignatorylinks  doc)

