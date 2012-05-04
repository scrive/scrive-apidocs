module Doc.API (documentAPI) where

import Happstack.StaticRouting
import Text.JSON
import KontraMonad
--import Util.JSON
import Happstack.Server.SimpleHTTP (askRq)
import Happstack.Server.Types
import Routing
import Doc.DocStateQuery
import Doc.DocStateData
import Doc.Model
import Doc.JSON
import Control.Applicative
import Control.Logic
import Control.Monad.Trans
import Misc
import Data.Maybe

--import Text.JSON.String

--import qualified Data.ByteString as BS
--import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Lazy as BSL
import Util.Actor
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Happstack.Server.RqData
import Doc.DocStorage
import DB
--import Doc.DocControl
--import Control.Concurrent.MVar

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
--import Control.Applicative
--import Data.String.Utils
--import MinutesTime
documentAPI :: Route (KontraPlus Response)
documentAPI = choice [
  dir "api" $ dir "document" $ hPostNoXToken $ toK0 $ documentNew,

  -- /api/mainfile/{docid} ==> Change main file
  dir "api" $ dir "mainfile" $ hPostNoXToken $ toK1 $ documentChangeMainFile,

  dir "api" $ dir "document" $ hPostNoXToken $ toK6 $ documentUploadSignatoryAttachment,
  dir "api" $ dir "document" $ hDelete       $ toK6 $ documentDeleteSignatoryAttachment
  ]

documentNew :: Kontrakcja m => m Response
documentNew = api $ do
  rq <- lift $ askRq
  Log.debug $ show $ rqHeaders rq
  
  (user, actor) <- getAPIUser
  mcompany <- case usercompany user of
    Just companyid -> Just <$> (apiGuardL' $ dbQuery $ GetCompany companyid)
    Nothing -> return Nothing
    

  jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")

  json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                               Ok js -> Just js
                                                                               _     -> Nothing

  dcr <- either (throwError . badInput) return $ dcrFromJSON json
  
  let doctype = dcrType dcr
  
  -- pdf exists  
  Input contentspec (Just filename') _contentType <- apiGuardL (badInput "The main file of the document must be attached in the MIME part 'file'.") $ getDataFn' (lookInput "file")
  
  let filename = basename filename'
      
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  
  d1 <- apiGuardL' $ dbUpdate $ NewDocument user mcompany filename doctype 1 actor
  
  file <- lift $ dbUpdate $ NewFile filename $ concatChunks content1

  d2 <- apiGuardL' $ dbUpdate $ AttachFile (documentid d1) (fileid file) actor
  -- we really need to check SignatoryDetails before adding them

  d3 <- apiGuardL' $ dbUpdate $ ResetSignatoryDetails2 (documentid d2) (map (\ir -> (SignatoryDetails {
                                                                                                 signatorysignorder = SignOrder (toInteger $ fromMaybe 1 $ irSignOrder ir),
                                                                                                 signatoryfields = irData ir
                                                                                                 },
                                                                                     irRole ir,
                                                                                     irAttachments ir,
                                                                                     Nothing)) -- No CSV
                                                                                (dcrInvolved dcr))
                                                                                actor
  _ <- lift $ addDocumentCreateStatEvents d3 "web"
  return $ Created $ jsonDocumentForAuthor d3 (ctxhostpart ctx)

-- this one must be standard post with post params because it needs to
-- be posted from a browser form
-- Change main file, file stored in input "file" OR templateid stored in "template"
documentChangeMainFile :: Kontrakcja m => DocumentID -> m Response
documentChangeMainFile docid = api $ do
  ctx <- getContext
  aa <- apiGuard (forbidden "No user is logged in.") $ mkAuthorActor ctx
  doc <- apiGuardL forbidden' $ getDocByDocID docid
  apiGuard (forbidden "Logged in user is not author of document.") (isAuthor $ getSigLinkFor doc (ctxmaybeuser ctx))

  fileinput <- lift $ getDataFn' (lookInput "file")
  templateinput <- lift $ getDataFn' (look "template")

  fileid <- case (fileinput, templateinput) of
            (Just (Input contentspec (Just filename') _contentType), _) -> do
              content1 <- case contentspec of
                Left filepath -> liftIO $ BSL.readFile filepath
                Right content -> return content
                
              -- we need to downgrade the PDF to 1.4 that has uncompressed structure
              -- we use gs to do that of course
              content <- apiGuardL (badInput "PDF precheck failed.") $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)
              let filename = basename filename'
      
              fileid <$> (dbUpdate $ NewFile filename content)
            (_, Just templateids) -> do
              templateid <- apiGuard (badInput $ "Template id in bad format: " ++ templateids) $ maybeRead templateids
              temp <- apiGuardL' $ getDocByDocID templateid
              apiGuard (badInput "No template found for that id (or you don't have permissions).") $ listToMaybe $ documentfiles temp
            _ -> throwError $ badInput "This API call requires one of 'file' or 'template' POST parameters."
  
  _ <- apiGuardL' $ dbUpdate $ AttachFile docid fileid aa
  return ()


--documentView :: (Kontrakcja m) => DocumentID -> m Response
--documentView (_ :: DocumentID) = api $  undefined
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
  
  sigattach <- apiGuard (forbidden "There is no signatory attachment request of that name.") $ getSignatoryAttachment doc slid aname
  
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
  
  file <- lift $ dbUpdate $ NewFile (basename filename) content
  let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) email slid
  d <- apiGuardL' $ dbUpdate $ SaveSigAttachment (documentid doc) sid aname (fileid file) actor
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment d sid aname
  
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
  sigattach <- apiGuard (forbidden "The attachment with that name does not exist for the signatory.") $ getSignatoryAttachment doc sid aname

  -- attachment must have a file
  fileid <- apiGuard (actionNotAvailable "That signatory attachment request does not have a file uploaded for it, or it has been previously deleted.") $ signatoryattachmentfile sigattach

  d <- apiGuardL' $ dbUpdate $ DeleteSigAttachment (documentid doc) sid fileid
       (signatoryActor ctxtime ctxipnumber muid email sid)
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment d sid aname
  
  return $ jsonSigAttachmentWithFile sigattach' Nothing

