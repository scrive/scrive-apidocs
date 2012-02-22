module Doc.API (documentAPI) where

import Happstack.StaticRouting
import Text.JSON
import KontraMonad
import Util.JSON
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
--import qualified Codec.Text.IConv as IConv
import OAuth.Model
--import qualified Codec.MIME.Type as MIME
--import qualified Codec.MIME.Parse as MIME
--import qualified Codec.MIME.Utils as MIME

import Text.JSON.String

--import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Lazy as BSL
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Happstack.Server.RqData
import Doc.DocStorage
import qualified Doc.DocControl as DocControl
import Doc.DocControl

--import Control.Concurrent.MVar

import DB.Classes
import File.Model
import MagicHash (MagicHash)
import Kontra
import Doc.DocUtils
import User.Model
import Company.Model
import Happstack.Server.Monads
import API.Monad
import Control.Monad.Error
import qualified Log
import Stats.Control
import qualified Data.Map as Map
import Control.Applicative
--import Data.String.Utils
import OAuth.Parse
import MinutesTime
import EvidenceLog.Model

documentAPI :: Route (Kontra Response)
documentAPI = choice [
  dir "api" $ dir "document" $ hGet          $ toK0 $ documentList,
  dir "api" $ dir "document" $ hPostNoXToken $ toK0 $ documentNew,
--  dir "api" $ dir "document" $ hGet          $ toK1 $ documentView,
  dir "api" $ dir "document" $ hPostNoXToken $ toK6 $ documentUploadSignatoryAttachment,
  dir "api" $ dir "document" $ hDelete       $ toK6 $ documentDeleteSignatoryAttachment,
  dir "api" $ dir "document" $ hPostNoXToken $ toK2 $ documentChangeMetadata
  --dir "api" $ dir "login"    $ hPostNoXToken $ toK0 $ apiLogin
  ]

{-              
_apiLogin :: Kontrakcja m => m Response
_apiLogin = api $ do
  memail  <- getDataFn' (look "email")
  mpasswd <- getDataFn' (look "password")
  case (memail, mpasswd) of
    (Just email, Just passwd) -> do
      -- check the user things here
      maybeuser <- lift $ runDBQuery $ GetUserByEmail Nothing (Email $ BS.fromString email)
      case maybeuser of
        Just user@User{userpassword}
          | verifyPassword userpassword (BS.fromString passwd) -> do
            muuser <- runDBQuery $ GetUserByID (userid user)
            lift $ logUserToContext muuser
            apiOK jsempty
        _ -> apiForbidden
    _ -> apiBadInput
-}              

-- | Return a list of documents the logged in user can see
documentList :: Kontrakcja m => m Response
documentList = api $ do
  docs <- apiGuardL getDocsByLoggedInUser
  jdocs <- lift $ mapM jsonDocumentAndFiles docs
  return $ showJSON jdocs

-- this one must be standard post with post params because it needs to
-- be posted from a browser form; we will have a better one for more
-- capable clients that follows the api standards
documentNew :: Kontrakcja m => m Response
documentNew = api $ do

  Log.debug $ "hello!!!"

  mjson <- lift $ getDataFn' (look "json")
  case mjson of
    Just _ -> documentWithJSON
    Nothing -> documentNewMultiPart

documentWithJSON :: Kontrakcja m => APIMonad m (Created JSValue)
documentWithJSON = do
  time <- ctxtime <$> getContext
  ip <- ctxipnumber <$> getContext
  rq <- lift $ askRq

  let headers = rqHeaders rq
  Log.debug $ "Got headers: " ++ show headers

  HeaderPair _ auths <- apiGuard' Forbidden $ Map.lookup (BS.fromString "authorization") headers

  Log.debug $ "Got Authorization: headers: " ++ show auths

  auth <- apiGuard' Forbidden $ BS.toString <$> listToMaybe auths

  -- pull the data out of Authorization
  let params = splitAuthorization auth                 

  Log.debug $ "Split Authorization header into params: " ++ show params

  sigtype <- apiGuard' BadInput $ maybeRead =<< lookup "oauth_signature_method" params
  when (sigtype /= "PLAINTEXT") $ throwError BadInput

  (mapisecret, mtokensecret) <- apiGuard' Forbidden $ splitSignature =<< maybeRead =<< lookup "oauth_signature" params
  apisecret <- apiGuard' Forbidden mapisecret
  tokensecret <- apiGuard' Forbidden mtokensecret

  Log.debug $ "Got api secret: " ++ show apisecret
     
  apitoken    <- apiGuard' BadInput $ maybeRead =<< maybeRead =<< lookup "oauth_consumer_key" params
  accesstoken <- apiGuard' BadInput $ maybeRead =<< maybeRead =<< lookup "oauth_token" params

  Log.debug $ "token: " ++ show accesstoken
  (userid, apistring) <- apiGuardL' Forbidden $ runDBQuery $ GetUserIDForAPIWithPrivilege apitoken apisecret accesstoken tokensecret APIDocCreate
  
  Log.debug $ "userid: " ++ show userid

  user <- apiGuardL' Forbidden $ runDBQuery $ GetUserByID userid

  let actor = APIActor time ip userid (BS.toString $ getEmail user) apistring

  jsonString <- apiGuardL' BadInput $ getDataFn' (look "json")

  Log.debug $ "JSON: " ++ jsonString

  json <- apiGuard' BadInput $ runGetJSON readJSValue jsonString
  dcr <- apiGuard' BadInput $ dcrFromJSON json

  Log.debug $ "DCR: " ++ show dcr

  -- exactly one author
  let aus = [a | a <- dcrInvolved dcr, elem SignatoryAuthor $ irRole a]

  when (length aus /= 1) $ do
    Log.debug $ (show $ toSeconds time) ++ " Should have exactly one author; instead, has " ++ show aus
    throwError BadInput

  let [authorIR] = aus

  -- at least one signatory
  let sigs = length $ [p | p <- dcrInvolved dcr, elem SignatoryAuthor $ irRole p]
  when (1 > length (dcrInvolved dcr)) $ do
    Log.debug $ (show $ toSeconds time) ++ " Should have at least one signatory; instead, has " ++ show sigs
    throwError BadInput

  Log.debug "about to read input for file"

  -- the mainfile is attached
  --(Input contentspec (Just _filename') _contentType) <- apiGuardL' BadInput $ getDataFn' (lookInput $ dcrMainFile dcr)
  content1 <- apiGuardL' BadInput $ getDataFn' (look $ dcrMainFile dcr)
  Log.debug "just got input"

  --content1 <- case contentspec of
  --  Left filepath -> liftIO $ BSL.readFile filepath
  --  Right content -> return content

  Log.debug "got content"

  -- create document
  -- set to advanced
  -- add signatories
  -- send document

  mcompany <- case usercompany user of
    Just companyid -> lift $ runDBQuery $ GetCompany companyid
    Nothing -> return Nothing

  let userDetails = signatoryDetailsFromUser user mcompany

  -- check email, first name, and last name to make sure they match with the author
  when (not $ all id [sfValue u == sfValue j
                     | u <- signatoryfields userDetails
                     , j <- irData authorIR
                     , sfType u == sfType j
                     , sfType u `elem` [FirstNameFT, LastNameFT, EmailFT]
                     ]) $ do
    Log.debug $ (show $ toSeconds time) ++ " Author data does not match: " ++ show authorIR ++ " and " ++ show userDetails
    throwError Forbidden

  -- check that all signatories have first, last, and email
  when (not $ all ((3 ==) . length) [[v | v <- irData s
                                      , sfType v `elem` [FirstNameFT, LastNameFT, EmailFT]]
                                    | s <- dcrInvolved dcr]) $ do
    Log.debug $ (show $ toSeconds time) ++ " Minimum information not there for all signatories: " ++ show (dcrInvolved dcr)
    throwError BadInput

  let doctype = dcrType dcr
      title = dcrTitle dcr

  Log.debug "about to new document"      
  doc <- apiGuardL' ServerError $ runDBUpdate $ NewDocument user mcompany (BS.fromString title) doctype actor

  Log.debug "new document success"
  gscmd <- ctxgscmd <$> getContext
  content14 <- apiGuardL $ liftIO $ preCheckPDF gscmd (BS.fromString content1)

  file <- lift $ runDBUpdate $ NewFile (BS.fromString title) content14
  _ <- apiGuardL $ runDBUpdate (AttachFile (documentid doc) (fileid file) actor)
  
  _ <- lift $ runDBUpdate $ SetDocumentFunctionality (documentid doc) AdvancedFunctionality actor
  _ <- lift $ runDBUpdate $ SetDocumentIdentification (documentid doc) [EmailIdentification] actor

  let signatories = for (dcrInvolved dcr) $ \InvolvedRequest{irRole,irData} ->
        (SignatoryDetails{signatorysignorder = SignOrder 0, signatoryfields = irData},
         irRole)
  
  _ <- apiGuardL' BadInput $ runDBUpdate $ ResetSignatoryDetails (documentid doc) signatories actor
  
  doc2 <- apiGuardL' ServerError $ runDBUpdate $ PreparationToPending (documentid doc) actor
  lift $ markDocumentAuthorReadAndSeen doc2 actor
  _ <- lift $ DocControl.postDocumentChangeAction doc2 doc Nothing

  return $ Created $ jsonDocumentForAuthor doc2

documentNewMultiPart :: Kontrakcja m => APIMonad m (Created JSValue)
documentNewMultiPart = do
  user <- getAPIUser
  mcompany <- case usercompany user of
    Nothing -> return Nothing
    Just cid -> do
      a <- apiGuardL $ runDBQuery $ GetCompany cid
      return $ Just a
  
  doctypes <- apiGuardL' BadInput $ getDataFn' (look "type")
  
  doctypei <- apiGuard' BadInput $ maybeRead doctypes
  
  doctype <- apiGuard' BadInput $ toSafeEnumInt doctypei
  
  -- pdf exists  
  (Input contentspec (Just filename') _contentType) <- apiGuardL' BadInput $ getDataFn' (lookInput "file")
  
  let filename = (BS.fromString $ basename filename')
      
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  let now = ctxtime ctx
  
  let aa = AuthorActor now (ctxipnumber ctx) (userid user) (BS.toString $ getEmail user)
  d1 <- apiGuardL $ runDBUpdate $ NewDocument user mcompany filename doctype aa
  content <- apiGuardL' BadInput $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)
  file <- lift $ runDB $ dbUpdate $ NewFile filename content

  d2 <- apiGuardL $ runDBUpdate $ AttachFile (documentid d1) (fileid file) aa
  _ <- lift $ addDocumentCreateStatEvents d2
  return $ Created $ jsonDocumentForAuthor d2

documentChangeMetadata :: Kontrakcja m => DocumentID -> MetadataResource -> m Response
documentChangeMetadata docid _ = api $ do
  user <- getAPIUser  
  doc <- apiGuardL $ runDBQuery $ GetDocumentByDocumentID docid
  
  asl <- apiGuard $ getAuthorSigLink doc
  
  apiGuard' Forbidden (Just (userid user) == maybesignatory asl)
    
  rq <- lift askRq
    
  bdy <- apiGuardL $ liftIO $ takeRequestBody rq
  let jstring = BS.toString $ concatChunks $ unBody bdy
  
  json <- apiGuard $ decode jstring
  ctx <- getContext
  let now = ctxtime ctx
  let actor = AuthorActor now (ctxipnumber ctx) (userid user) (BS.toString $ getEmail user)      
  d <- case jsget "title" json of
    Left _ -> return doc
    Right (JSString s) ->
      apiGuardL $ runDBUpdate $ SetDocumentTitle docid (BS.fromString $ fromJSString s) actor
    Right _ -> throwError BadInput
      
  return $ jsonDocumentMetadata d

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
       _ -> throwError BadInput
  
documentUploadSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentUploadSignatoryAttachment did _ sid _ aname _ = api $ do
  Log.debug $ "sigattachment ajax"
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  sl  <- apiGuard $ getSigLinkFor doc sid
  let email = getEmail sl
  
  sigattach <- apiGuard' Forbidden $ getSignatoryAttachment email (BS.fromString aname) doc
  
  -- attachment must have no file
  apiGuard' ActionNotAvailable (isNothing $ signatoryattachmentfile sigattach)
  
  -- pdf exists in input param "file"
  (Input contentspec (Just filename) _contentType) <- apiGuardL' BadInput $ getDataFn' (lookInput "file")
  
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext

  content <- apiGuardL' BadInput $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)
  
  file <- lift $ runDB $ dbUpdate $ NewFile (BS.fromString $ basename filename) content
  let actor = SignatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) (BS.toString email) slid
  d <- apiGuardL $ runDBUpdate $ SaveSigAttachment (documentid doc) (BS.fromString aname) email (fileid file) actor
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  return $ Created $ jsonSigAttachmentWithFile sigattach' (Just file)

documentDeleteSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentDeleteSignatoryAttachment did _ sid _ aname _ = api $ do
  Context{ctxtime, ctxipnumber} <- getContext
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  
  sl <- apiGuard $ getSigLinkFor doc sid
  let email = getEmail sl
      muid  = maybesignatory sl
  
  
  -- sigattachexists
  sigattach <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) doc

  -- attachment must have a file
  fileid <- apiGuard' ActionNotAvailable $ signatoryattachmentfile sigattach

  d <- apiGuardL $ runDBUpdate $ DeleteSigAttachment (documentid doc) email fileid 
       (SignatoryActor ctxtime ctxipnumber muid (BS.toString email) sid)
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  return $ jsonSigAttachmentWithFile sigattach' Nothing

  
-- helpers

jsonDocumentAndFiles :: Kontrakcja m => Document -> m JSValue
jsonDocumentAndFiles doc = do
  --files <- getFilesByStatus doc
  return $ jsonDocumentForSignatory doc
  
