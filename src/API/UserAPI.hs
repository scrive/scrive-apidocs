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
import Doc.DocControl
import Doc.DocState
import Kontra
import Misc
import Doc.DocViewMail
import Mails.MailsData

import Happstack.State
import Happstack.Server
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

data UserAPIContext = UserAPIContext {wsbody :: APIRequestBody ,user :: User}
type UserAPIFunction m a = APIFunction m UserAPIContext a

instance Kontrakcja m => APIContext m UserAPIContext where
    body= wsbody
    newBody b ctx = ctx {wsbody = b}
    apiContext  = do
        muser <- apiUser
        mbody <- apiBody
        case (muser, mbody)  of
             (Just u, Right b) -> return $ Right $ UserAPIContext { wsbody = b, user = u}
             (Nothing, _)            -> return $ Left $ (API_ERROR_LOGIN, "Not logged in")
             (_, Left s)             -> return $ Left $ (API_ERROR_PARSING, "Parsing error: " ++ s)

apiUser :: Kontrakcja m => m (Maybe User)
apiUser = do
    email <- getFieldUTFWithDefault BS.empty "email"
    muser <- query $ GetUserByEmail Nothing (Email email)
    case muser of
        Nothing -> return Nothing
        Just user -> do
            passwd <- getFieldUTFWithDefault BS.empty "password"
            if (verifyPassword (userpassword user) passwd)
               then return $ Just user
               else return Nothing

userAPI :: Kontra Response
userAPI =  dir "api" $ dir "userapi" $ msum [
      apiCall "sendnewdocument" sendNewDocument   :: Kontrakcja m => m Response
    , apiCall "sendFromTemplate" sendFromTemplate :: Kontrakcja m => m Response
    , apiCall "document" getDocument              :: Kontrakcja m => m Response
    , apiCall "sendReminder" sendReminder         :: Kontrakcja m => m Response
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
  api_doc <- api_document True (doc)
  return $ toJSObject [("document",api_doc)]


getUserDoc :: Kontrakcja m => UserAPIFunction m Document
getUserDoc = do
  author <- user <$> ask
  mdocument <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "document_id"
  when (isNothing mdocument || (not $ isAuthor ((fromJust mdocument), author))) $
        throwApiError API_ERROR_NO_DOCUMENT "No document"
  return (fromJust mdocument)

sendFromTemplate :: Kontrakcja m => UserAPIFunction m APIResponse
sendFromTemplate = do
  author <- user <$> ask
  ctx <- getContext
  temp <- getTemplate
  signatories <- getSignatories
  doc <- update $ SignableFromDocument temp
  let mauthorsiglink = getAuthorSigLink doc
  when (isNothing mauthorsiglink) $ throwApiError API_ERROR_OTHER "Template has no author."
  let Just authorsiglink = mauthorsiglink
  let saccount = getSignatoryAccount author
  medoc <- update $
          UpdateDocument --really? This is ridiculous! Too many params
          (ctxtime ctx)
          (documentid doc)
          (documenttitle doc)
          (zip signatories (repeat [SignatoryPartner]))
          Nothing
          (documentinvitetext doc)
          ((signatorydetails authorsiglink) { signatorysignorder = SignOrder 0 }, signatoryroles authorsiglink, saccount)
          [EmailIdentification]
          Nothing
          AdvancedFunctionality
  case medoc of
    Left _msg  -> throwApiError API_ERROR_OTHER "Problem with saving document."
    Right edoc -> do
      liftIO $ print edoc
      esdoc <- update $ AuthorSendDocument
               (documentid edoc)
               (ctxtime ctx)
               (ctxipnumber ctx)
               Nothing
      case esdoc of
        Left _msg   -> throwApiError API_ERROR_OTHER "Problem with sending document."
        Right sdoc -> do
          liftIO $ print sdoc

          liftKontra $ postDocumentChangeAction sdoc doc Nothing
          return $ toJSObject [("document_id", JSString $ toJSString $ show (documentid sdoc))]

getTemplate :: Kontrakcja m => UserAPIFunction m Document
getTemplate = do
  author <- user <$> ask
  mtemplate <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "template_id"
  when (isNothing mtemplate) $ throwApiError API_ERROR_NO_DOCUMENT "No template exists with this ID"
  let Just temp = mtemplate
  when (not $ isAuthor (temp, author)) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists with this ID"
  when (not $ isTemplate temp) $ throwApiError API_ERROR_OTHER "This document is not a template"
  return temp


sendNewDocument :: Kontrakcja m => UserAPIFunction m APIResponse
sendNewDocument = do
  author <- user <$> ask
  mtitle <- apiAskBS "title"
  when (isNothing mtitle) $ throwApiError API_ERROR_MISSING_VALUE "There was no document title. Please add the title attribute (ex: title: \"mycontract\""
  let title = fromJust mtitle
  files <- getFiles
  when (length files /= 1) $ throwApiError API_ERROR_MISSING_VALUE "There has bad number of files. Exactly one file is required."
  let (filename, content) = head files
  signatories <- getSignatories
  when (Data.List.null signatories) $ throwApiError API_ERROR_MISSING_VALUE "There were no involved parties. At least one is needed."
  mtype <- liftMM (return . toSafeEnum) (apiAskInteger "type")
  when (isNothing mtype) $ throwApiError API_ERROR_MISSING_VALUE "BAD DOCUMENT TYPE"
  let doctype = toDocumentType $ fromJust mtype
  _msignedcallback <- apiAskBS "signed_callback"
  _mnotsignedcallback <- apiAskBS "notsigned_callback"
  ctx <- getContext
  newdoc <- update $ NewDocument author title doctype (ctxtime ctx)
  liftIO $ print newdoc
  _ <- liftKontra $ handleDocumentUpload (documentid newdoc) content filename
  let saccount = getSignatoryAccount author
  edoc <- update $
          UpdateDocument --really? This is ridiculous! Too many params
          (ctxtime ctx)
          (documentid newdoc)
          title
          (zip signatories (repeat [SignatoryPartner]))
          Nothing
          (documentinvitetext newdoc)
          ((signatoryDetailsFromUser author) { signatorysignorder = SignOrder 0 }, [SignatoryAuthor], saccount)
          [EmailIdentification]
          Nothing
          AdvancedFunctionality
  case edoc of
    Left _msg  -> throwApiError API_ERROR_OTHER "Problem with saving document."
    Right doc -> do
      liftIO $ print doc
      esdoc <- update $ AuthorSendDocument
               (documentid doc)
               (ctxtime ctx)
               (ctxipnumber ctx)
               Nothing
      case esdoc of
        Left _msg   -> throwApiError API_ERROR_OTHER "Problem with sending document."
        Right sdoc -> do
          liftIO $ print sdoc

          liftKontra $ postDocumentChangeAction sdoc doc Nothing
          return $ toJSObject [("document_id", JSString $ toJSString $ show (documentid sdoc))]

getSignatories :: Kontrakcja m => UserAPIFunction m [SignatoryDetails]
getSignatories = do
    minvolved  <- apiLocal "involved" $ apiMapLocal $ fmap toSignatoryDetails <$> getSignatoryTMP
    case minvolved of
        Nothing -> throwApiError API_ERROR_MISSING_VALUE "Problems with involved."
        Just involved -> return involved
