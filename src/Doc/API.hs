module Doc.API (documentAPI) where

import Happstack.StaticRouting
import Text.JSON
import KontraMonad
--import Util.JSON
import Happstack.Server.Types
import Routing
import Doc.DocStateQuery
import Doc.DocStateData
import Doc.Model
import Doc.JSON
--import Control.Applicative
--import Control.Monad
import Control.Monad.Trans
import Misc
import Data.Maybe
import Util.MonadUtils

import Text.JSON.String

--import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Lazy as BSL
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Happstack.Server.RqData
import Doc.DocStorage
import qualified Doc.DocControl as DocControl
--import Doc.DocControl
--import Control.Concurrent.MVar

import DB.Classes
import File.Model
import MagicHash (MagicHash)
import Kontra
import Doc.DocUtils
import User.Model
import Company.Model
import API.Monad
import Control.Monad.Error
import qualified Log
import Stats.Control
import Control.Applicative
--import Data.String.Utils
--import MinutesTime
import EvidenceLog.Model

documentAPI :: Route (Kontra Response)
documentAPI = choice [
  dir "api" $ dir "document" $ hPostNoXToken $ toK0 $ documentNew,
--  dir "api" $ dir "document" $ hGet          $ toK1 $ documentView,
  dir "api" $ dir "document" $ hPostNoXToken $ toK6 $ documentUploadSignatoryAttachment,
  dir "api" $ dir "document" $ hDelete       $ toK6 $ documentDeleteSignatoryAttachment
  ]

documentNew :: Kontrakcja m => m Response
documentNew = api $ documentNewMultiPart
{-
  mjson <- lift $ getDataFn' (look "json")
  case mjson of
    Just _ -> documentWithJSON
    Nothing -> documentNewMultiPart

documentWithJSON :: Kontrakcja m => APIMonad m (Created JSValue)
documentWithJSON = do
  (user, actor) <- getAPIUser
  mcompany <- case usercompany user of
    Just companyid -> lift $ runDBQuery $ GetCompany companyid
    Nothing -> return Nothing

  jsonString <- apiGuardL (badInput "The request must contain a MIME part called 'json'." ) $ getDataFn' (look "json")
  json       <- apiGuard  (badInput "The 'json' part must be valid JSON."                 ) $ runGetJSON readJSValue jsonString
  dcr        <- apiGuard  (badInput "The JSON does not describe a document."              ) $ dcrFromJSON json

  -- exactly one author
  let aus = length [a | a <- dcrInvolved dcr, elem SignatoryAuthor $ irRole a]
  when (aus /= 1) $ do
    throwError $ badInput $ "Should have exactly one author; instead, has " ++ show aus

  -- the mainfile is attached
  (Input contentspec (Just _filename') _contentType) <- apiGuardL (badInput $ "The attachment described in mainfile \"" ++ dcrMainFile dcr ++ "\" was not found.")  $ getDataFn' (lookInput $ dcrMainFile dcr)

  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content

  let doctype = dcrType dcr
      title = dcrTitle dcr

  doc <- apiGuardL' $ runDBUpdate $ NewDocument user mcompany (BS.fromString title) doctype actor

  gscmd <- ctxgscmd <$> getContext
  content14 <- apiGuardL (badInput "PDF was invalid.") $ liftIO $ preCheckPDF gscmd (concatChunks content1)

  file <- lift $ runDBUpdate $ NewFile (BS.fromString title) content14
  _ <- apiGuardL' $ runDBUpdate (AttachFile (documentid doc) (fileid file) actor)
  
  when_ (length (dcrInvolved dcr) > 2) $
    lift $ runDBUpdate $ SetDocumentFunctionality (documentid doc) AdvancedFunctionality actor

  _ <- lift $ runDBUpdate $ SetDocumentIdentification (documentid doc) [EmailIdentification] actor

  let signatories = for (dcrInvolved dcr) $ \InvolvedRequest{irRole,irData} ->
        (SignatoryDetails{signatorysignorder = SignOrder 0, signatoryfields = irData},
         irRole)
  
  doc2 <- apiGuardL' $ runDBUpdate $ ResetSignatoryDetails (documentid doc) signatories actor
  
  _ <- lift $ DocControl.postDocumentChangeAction doc2 doc Nothing

  hostpart <- ctxhostpart <$> getContext
  return $ Created $ jsonDocumentForAuthor doc2 hostpart
-}
documentNewMultiPart :: Kontrakcja m => APIMonad m (Created JSValue)
documentNewMultiPart = do
  (user, actor) <- getAPIUser
  mcompany <- case usercompany user of
    Just companyid -> lift $ runDBQuery $ GetCompany companyid
    Nothing -> return Nothing
  
  doctypes <- apiGuardL (badInput "The MIME part 'type' must exist and must be an integer.") $ getDataFn' (look "type")
  
  doctypei <- apiGuard (badInput "The document type (param 'type') must be an integer.") $ maybeRead doctypes
  
  doctype <- apiGuard (badInput "The document type (param 'type') was not recognized.") $ toSafeEnumInt doctypei
  
  -- pdf exists  
  Input contentspec (Just filename') _contentType <- apiGuardL (badInput "The main file of the document must be attached in the MIME part 'file'.") $ getDataFn' (lookInput "file")
  
  let filename = (BS.fromString $ basename filename')
      
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  
  d1 <- apiGuardL' $ runDBUpdate $ NewDocument user mcompany filename doctype actor
  content <- apiGuardL (badInput "The PDF was invalid.") $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)
  file <- lift $ runDB $ dbUpdate $ NewFile filename content

  d2 <- apiGuardL' $ runDBUpdate $ AttachFile (documentid d1) (fileid file) actor
  _ <- lift $ addDocumentCreateStatEvents d2
  return $ Created $ jsonDocumentForAuthor d2 (ctxhostpart ctx)

data SignatoryResource = SignatoryResource
instance FromReqURI SignatoryResource where
    fromReqURI s = Just SignatoryResource <| s == "signatory" |> Nothing

data AttachmentResource = AttachmentResource
instance FromReqURI AttachmentResource where
    fromReqURI s = Just AttachmentResource <| s == "attachment" |> Nothing
    
data FileResource = FileResource
instance FromReqURI FileResource where
    fromReqURI s = Just FileResource <| s == "file" |> Nothing

data MetadataResource = MetadataResource
instance FromReqURI MetadataResource where
    fromReqURI s = Just MetadataResource <| s == "metadata" |> Nothing
 
getSigLinkID :: Kontrakcja m => APIMonad m (SignatoryLinkID, MagicHash)
getSigLinkID = do
  msignatorylink <- lift $ readField "signatorylinkid"
  mmagichash <- lift $ readField "magichash"
  case (msignatorylink, mmagichash) of
       (Just sl, Just mh) -> return (sl,mh)
       _ -> throwError $ badInput "The signatorylinkid or magichash were missing."
  
documentUploadSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentUploadSignatoryAttachment did _ sid _ aname _ = api $ do
  Log.debug $ "sigattachment ajax"
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL' $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  sl  <- apiGuard (forbidden "There is no signatory by that id.") $ getSigLinkFor doc sid
  let email = getEmail sl
  
  sigattach <- apiGuard (forbidden "There is not signatory attachment request of that name.") $ getSignatoryAttachment email (BS.fromString aname) doc
  
  -- attachment must have no file
  apiGuard (actionNotAvailable "There is already a file attached for that attachment request.") (isNothing $ signatoryattachmentfile sigattach)
  
  -- pdf exists in input param "file"
  (Input contentspec (Just filename) _contentType) <- apiGuardL (badInput "The attachment PDF must be in the MIME part named 'file'. It was not found.") $ getDataFn' (lookInput "file")
  
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext

  content <- apiGuardL (badInput "The PDF was invalid.") $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)
  
  file <- lift $ runDB $ dbUpdate $ NewFile (BS.fromString $ basename filename) content
  let actor = SignatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) (BS.toString email) slid
  d <- apiGuardL' $ runDBUpdate $ SaveSigAttachment (documentid doc) (BS.fromString aname) email (fileid file) actor
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment email (BS.fromString aname) d
  
  return $ Created $ jsonSigAttachmentWithFile sigattach' (Just file)

documentDeleteSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentDeleteSignatoryAttachment did _ sid _ aname _ = api $ do
  Context{ctxtime, ctxipnumber} <- getContext
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL' $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  
  sl <- apiGuard (forbidden "The signatory does not exist.") $ getSigLinkFor doc sid
  let email = getEmail sl
      muid  = maybesignatory sl
  
  
  -- sigattachexists
  sigattach <- apiGuard (forbidden "The attachment with that name does not exist for the signatory.") $ getSignatoryAttachment email (BS.fromString aname) doc

  -- attachment must have a file
  fileid <- apiGuard (actionNotAvailable "That signatory attachment request does not have a file uploaded for it, or it has been previously deleted.") $ signatoryattachmentfile sigattach

  d <- apiGuardL' $ runDBUpdate $ DeleteSigAttachment (documentid doc) email fileid 
       (SignatoryActor ctxtime ctxipnumber muid (BS.toString email) sid)
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment email (BS.fromString aname) d
  
  return $ jsonSigAttachmentWithFile sigattach' Nothing

