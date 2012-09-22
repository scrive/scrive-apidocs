-----------------------------------------------------------------------------
-- |
-- Module      :  API.UserAPI
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Simple api that allows some action to be done as system user, but thru the API.
-- Calls require authorisation with user email and password
--
-----------------------------------------------------------------------------

module API.UserAPI (userAPI) where

import API.API
import API.APICommons hiding (SignatoryTMP(..))
import DB.Classes
import Doc.DocControl
import Doc.DocState
import Company.Model
import User.Model
import Kontra
import Misc
import Doc.DocViewMail
import Mails.MailsData

import Happstack.State
import Happstack.Server(Response)
import Happstack.StaticRouting(Route, dir, choice)
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import Control.Monad.Reader
import Text.JSON
import qualified Data.ByteString as BS
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Util.MonadUtils
import Util.JSON
import Text.JSON.String
data UserAPIContext = UserAPIContext {wsbody :: APIRequestBody ,user :: User}
type UserAPIFunction m a = APIFunction m UserAPIContext a

instance APIContext UserAPIContext where
    apiContext  = do
        muser <- apiUser
        mbody <- runGetJSON readJSObject <$> getFieldWithDefault "" "body"
        case (muser, mbody)  of
             (Just u, Right b) -> return $ Right $ UserAPIContext { wsbody = b, user = u}
             (Nothing, _)            -> return $ Left $ (API_ERROR_LOGIN, "Not logged in")
             (_, Left s)             -> return $ Left $ (API_ERROR_PARSING, "Parsing error: " ++ s)

instance JSONContainer UserAPIContext where
    getJSON = wsbody
    setJSON j uapictx = uapictx {wsbody = j}

apiUser :: Kontrakcja m => m (Maybe User)
apiUser = do
    email <- getFieldUTFWithDefault BS.empty "email"
    muser <- runDBQuery $ GetUserByEmail Nothing (Email email)
    case muser of
        Nothing -> return Nothing
        Just user -> do
            passwd <- getFieldUTFWithDefault BS.empty "password"
            if (verifyPassword (userpassword user) passwd)
               then return $ Just user
               else return Nothing

userAPI :: Route (Kontra Response)
userAPI =  
  dir "api" $ dir "userapi" $ choice 
    [ apiCall "sendnewdocument" sendNewDocument
    , apiCall "sendFromTemplate" sendFromTemplate
    , apiCall "document" getDocument
    , apiCall "sendReminder" sendReminder
    , apiUnknownCall
    ]

sendReminder :: Kontrakcja m => UserAPIFunction m APIResponse
sendReminder = do
  ctx <- getContext
  doc <- getUserDoc
  let siglinkstoremind = [sl | sl <- documentsignatorylinks doc
                             , isSignatory sl
                             , not $ hasSigned sl]
  _ <- forM siglinkstoremind $ (\signlink -> do
                              mail <- mailDocumentRemind Nothing ctx doc signlink
                              scheduleEmailSendout (ctxesenforcer ctx) $ mail {
                                to = [getMailAddress signlink]
                                , mailInfo = Invitation  (documentid doc) (signatorylinkid signlink)
                                })
  return $ toJSObject []


getDocument :: Kontrakcja m => UserAPIFunction m APIResponse
getDocument = do
  doc <- getUserDoc
  api_doc <- api_document_read True (doc)
  return $ toJSObject [("document",api_doc)]


getUserDoc :: Kontrakcja m => UserAPIFunction m Document
getUserDoc = do
  author <- user <$> ask
  mdocument <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ fromJSONField "document_id"
  when (isNothing mdocument || (not $ isAuthor ((fromJust mdocument), author))) $
        throwApiError API_ERROR_NO_DOCUMENT "No document"
  return (fromJust mdocument)

sendFromTemplate :: Kontrakcja m => UserAPIFunction m APIResponse
sendFromTemplate = do
  author <- user <$> ask
  mcompany <- case usercompany author of
                Just companyid -> runDBQuery $ GetCompany companyid
                Nothing -> return Nothing
  ctx <- getContext
  temp <- getTemplate
  signatories <- getSignatories
  doc <- update $ SignableFromDocument temp
  let mauthorsiglink = getAuthorSigLink doc
  when (isNothing mauthorsiglink) $ throwApiError API_ERROR_OTHER "Template has no author."
  _ <- update $ SetDocumentFunctionality (documentid doc) AdvancedFunctionality (ctxtime ctx)
  _ <- update $ SetEmailIdentification (documentid doc) (ctxtime ctx)
  medoc <- update $ ResetSignatoryDetails (documentid doc) (((signatoryDetailsFromUser author mcompany) { signatorysignorder = SignOrder 0 }, 
                                                             [SignatoryPartner, SignatoryAuthor]): 
                                                            (zip signatories (repeat [SignatoryPartner]))) (ctxtime ctx)
  case medoc of
    Left _msg  -> throwApiError API_ERROR_OTHER "Problem with saving document."
    Right edoc -> do
      esdoc <- update $ PreparationToPending (documentid edoc) (ctxtime ctx)
      case esdoc of
        Left _msg   -> throwApiError API_ERROR_OTHER "Problem with sending document."
        Right sdoc -> do
          liftKontra $ postDocumentChangeAction sdoc doc Nothing
          return $ toJSObject [("document_id", JSString $ toJSString $ show (documentid sdoc))]

getTemplate :: Kontrakcja m => UserAPIFunction m Document
getTemplate = do
  author <- user <$> ask
  mtemplate <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ fromJSONField "template_id"
  when (isNothing mtemplate) $ throwApiError API_ERROR_NO_DOCUMENT "No template exists with this ID"
  let Just temp = mtemplate
  when (not $ isAuthor (temp, author)) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists with this ID"
  when (not $ isTemplate temp) $ throwApiError API_ERROR_OTHER "This document is not a template"
  return temp

sendNewDocument :: Kontrakcja m => UserAPIFunction m APIResponse
sendNewDocument = do
  author <- user <$> ask
  mcompany <- case usercompany author of
                Just companyid -> runDBQuery $ GetCompany companyid
                Nothing -> return Nothing
  mtitle <- fromJSONField "title"
  when (isNothing mtitle) $ throwApiError API_ERROR_MISSING_VALUE "There was no document title. Please add the title attribute (ex: title: \"mycontract\""
  let title = fromJust mtitle
  files <- getFiles
  when (length files /= 1) $ throwApiError API_ERROR_MISSING_VALUE "There has bad number of files. Exactly one file is required."
  let (filename, content) = head files
  signatories <- getSignatories
  when (Data.List.null signatories) $ throwApiError API_ERROR_MISSING_VALUE "There were no involved parties. At least one is needed."
  mtype <- liftMM (return . toSafeEnum) (fromJSONField "type")
  when (isNothing mtype) $ throwApiError API_ERROR_MISSING_VALUE "BAD DOCUMENT TYPE"
  let doctype = toDocumentType $ fromJust mtype
  --_msignedcallback <- fromJSONField "signed_callback"
  --_mnotsignedcallback <- fromJSONField "notsigned_callback"
  ctx <- getContext
  mnewdoc <- update $ NewDocument author mcompany title doctype (ctxtime ctx)
  when (isLeft mnewdoc) $ throwApiError API_ERROR_OTHER "Problem making doc, maybe company and user don't match."
  let newdoc = fromRight mnewdoc
  _ <- liftKontra $ handleDocumentUpload (documentid newdoc) content filename
  _ <- update $ SetDocumentFunctionality (documentid newdoc) AdvancedFunctionality (ctxtime ctx)
  _ <- update $ SetEmailIdentification (documentid newdoc) (ctxtime ctx)
  edoc <- update $ ResetSignatoryDetails (documentid newdoc) (((signatoryDetailsFromUser author mcompany) { signatorysignorder = SignOrder 0 }, 
                                                             [SignatoryPartner, SignatoryAuthor]): 
                                                            (zip signatories (repeat [SignatoryPartner]))) (ctxtime ctx)
  case edoc of
    Left _msg  -> throwApiError API_ERROR_OTHER "Problem with saving document."
    Right doc -> do
      esdoc <- update $ PreparationToPending (documentid doc) (ctxtime ctx)
      case esdoc of
        Left _msg   -> throwApiError API_ERROR_OTHER "Problem with sending document."
        Right sdoc -> do
          liftKontra $ postDocumentChangeAction sdoc doc Nothing
          return $ toJSObject [("document_id", JSString $ toJSString $ show (documentid sdoc))]

getSignatories :: Kontrakcja m => UserAPIFunction m [SignatoryDetails]
getSignatories = do
    minvolved  <- fromJSONLocal "involved" $ fromJSONLocalMap $ fmap toSignatoryDetails <$> getSignatoryTMP
    case minvolved of
        Nothing -> throwApiError API_ERROR_MISSING_VALUE "Problems with involved."
        Just involved -> return involved
