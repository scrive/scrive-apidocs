{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
module API.UserAPI (userAPI)
       
       {- 

// create and send a single new contract from a template
/coopdemo/api/sendFromTemplate

Example Request:

{ "templateid" : "43243242",
  "involved"   : [{"email"       : "dude@signatory.com",
                   "fstname"     : "John",
                   "sndname"     : "Wayne",
                   "company"     : "User",     //optional
                   "companynr"   : "32432432", //optional
                   "personalnr"  : "4324324",  //optional
                   "customfields" : [{"name"  : "BABS"},
                                     {"name"  : "Age",
                                      "value" : "23"}]}]}

// note: custom field names must match those in the template and all
// custom fields from the template must exist in the request

// note: if you leave out the value in the customfield, it will be
// left up to the signatory to fill out

// note: "signatories" has changed to "involved"

// note: involved is a list of signatories; author is not included

Example response:

{ "documentid" : "43265434" }

// get the document status for an existing document. also return all
// of the BABS numbers that have been filled in by signatories

/coopdemo/api/getDocumentStatusAndSignatories

Request example:

{ "documentid" : "765756765" }

Response example:

{ "documentid"     : "765756765",
  "documentstatus" : "Pending",
  "signatories" : [{"signed"      : true,
                    "email"       : "dude@signatory.com",
                    "fstname"     : "John",
                    "sndname"     : "Wayne",
                    "company"     : "User",     
                    "companynr"   : "32432432", 
                    "personalnr"  : "4324324",  
                    "customfields" : [{"name"  : "BABS",
                                       "value" : "432432423"},
                                      {"name"  : "Age",
                                       "value" : "23"}]},
                  {"signed"      : false,
                   "email"       : "gregor@mendel.com",
                   "fstname"     : "Gregor",
                   "sndname"     : "Mendel",
                   "company"     : "User",     
                   "companynr"   : "3533432", 
                   "personalnr"  : "123224",  
                   "customfields" : [{"name"  : "BABS"},
                                     {"name"  : "Age",
                                      "value" : ""}]}]}

// note: if a signatory has not signed, his fields may be empty. Use
// the "signed" property to see if he has signed.

// note: if no value is given for a custom field, it is because it has
// not been filled out.


-}
where

import API.API
import Doc.DocControl
import Doc.DocState
import Doc.DocUtils
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
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS

data UserAPIContext = UserAPIContext {wsbody :: APIRequestBody ,user :: User}
type UserAPIFunction a = APIFunction UserAPIContext a

instance APIContext UserAPIContext where
    body= wsbody
    newBody b ctx = ctx {wsbody = b}
    apiContext  = do
        muser <- apiUser
        mbody <- apiBody 
        case (muser, mbody)  of
             (Just u, Right b) -> return $ Right $ UserAPIContext { wsbody = b, user = u}
             (Nothing, _)            -> return $ Left $ (API_ERROR_LOGIN, "Not logged in")
             (_, Left s)             -> return $ Left $ (API_ERROR_PARSING, "Parsing error: " ++ s)

apiUser ::  Kontra (Maybe User)
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
userAPI =  dir "webshop" $ msum [ apiCall "sendnewdocument" sendNewDocument
                                 , apiCall "sendFromTemplate" sendFromTemplate
                                 , apiCall "getDocumentStatusAndSignatories" getDocumentStatusAndSignatories
                                 , apiCall "sendReminder" sendReminder
                                 , apiUnknownCall
                                 ]
           
getDocumentType :: Integer -> DocumentType
getDocumentType 1 = Signable Contract
getDocumentType 3 = Signable Offer
getDocumentType _ = error "Cannot create other types."

sendReminder :: UserAPIFunction APIResponse
sendReminder = do
  ctx <- askKontraContext  
  doc <- getDocument
  let siglinkstoremind = [sl | sl <- documentsignatorylinks doc
                             , isSignatory sl
                             , not $ hasSigned sl]
  _ <- forM siglinkstoremind $ (\signlink -> do
                              mail <- liftIO $  mailDocumentRemind (ctxtemplates ctx) Nothing ctx doc signlink
                              scheduleEmailSendout (ctxesenforcer ctx) $ mail {
                                to = [MailAddress {fullname = signatoryname $ signatorydetails signlink
                                                  , email = signatoryemail $ signatorydetails signlink}]
                                , mailInfo = Invitation  (documentid doc) (signatorylinkid signlink)
                                })
  return $ toJSObject [("documentid", JSString $ toJSString $ show (documentid doc))]  
                                

getDocumentStatusAndSignatories :: UserAPIFunction APIResponse
getDocumentStatusAndSignatories = do
  doc <- getDocument
  let signatories = [s|s <- documentsignatorylinks doc,
                     isSignatory s]
  return $ toJSObject [("documentid", JSString $ toJSString $ show $ documentid doc)
                      ,("documentstatus", JSString $ toJSString $ show $ documentstatus doc)
                      ,("signatories", JSArray $ map (sigToJSON) signatories)]

sigToJSON :: SignatoryLink -> JSValue  
sigToJSON siglink = JSObject $ toJSObject [("signed", JSBool $ isJust $ maybesigninfo siglink)
                               ,("email", JSString $ toJSString $ BS.toString $ signatoryemail $ signatorydetails siglink)
                               ,("fstname", JSString $ toJSString $ BS.toString $  signatoryfstname $ signatorydetails siglink)
                               ,("sndname", JSString $ toJSString $ BS.toString $  signatorysndname $ signatorydetails siglink)
                               ,("company", JSString $ toJSString $ BS.toString $  signatorycompany $ signatorydetails siglink)
                               ,("companynr", JSString $ toJSString $ BS.toString $  signatorycompanynumber $ signatorydetails siglink)
                               ,("personalnr", JSString $ toJSString $  BS.toString $ signatorypersonalnumber $ signatorydetails siglink)
                               ,("customfields", JSArray $ map fieldDefinitionToJSObject $ signatoryotherfields $ signatorydetails siglink)]

                                                                                  
fieldDefinitionToJSObject :: FieldDefinition -> JSValue
fieldDefinitionToJSObject fd = 
  JSObject $ toJSObject $ [("name", JSString $ toJSString $ BS.toString $ fieldlabel fd)]
            ++ if not (BS.null $ fieldvalue fd) || fieldfilledbyauthor fd 
               then [("value", JSString $ toJSString $ BS.toString $ fieldvalue fd)]
               else []
                               
  
getDocument :: UserAPIFunction Document
getDocument = do
  author <- user <$> ask  
  mdocumentid <- maybeReadM $ apiAskString "documentid"
  when (isNothing mdocumentid) $ throwApiError API_ERROR_MISSING_VALUE "The documentid was not given or was invalid."
  let Just documentid = mdocumentid
  mdoc <- query $ GetDocumentByDocumentID documentid
  when (isNothing mdoc) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists with this ID"
  let Just doc = mdoc
  -- same error message because we can't leak that the docid exists
  when (not $ isUserAuthor doc author) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists with this ID"
  return doc

sendFromTemplate :: UserAPIFunction APIResponse
sendFromTemplate = do
  author <- user <$> ask
  ctx <- askKontraContext  
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

          lift $ lift $ postDocumentChangeAction sdoc doc Nothing
          return $ toJSObject [("documentid", JSString $ toJSString $ show (documentid sdoc))]

getTemplate :: UserAPIFunction Document
getTemplate = do 
  author <- user <$> ask
  mtemplateid <- maybeReadM $ apiAskString "templateid"
  when (isNothing mtemplateid) $ throwApiError API_ERROR_MISSING_VALUE "The templateid was not given or was invalid."
  let Just templateid = mtemplateid
  mtemp <- query $ GetDocumentByDocumentID templateid
  when (isNothing mtemp) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists with this ID"
  let Just temp = mtemp
  -- same error message because we can't leak that the docid exists
  when (not $ isUserAuthor temp author) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists with this ID"
  when (not $ isTemplate temp) $ throwApiError API_ERROR_OTHER "This document is not a template"
  return temp
  
  
sendNewDocument :: UserAPIFunction APIResponse
sendNewDocument = do
  author <- user <$> ask
  mtitle <- apiAskBS "title"
  when (isNothing mtitle) $ throwApiError API_ERROR_MISSING_VALUE "There was no document title. Please add the title attribute (ex: title: \"mycontract\""
  let title = fromJust mtitle
  files <- getFiles
  when (Data.List.null files) $ throwApiError API_ERROR_MISSING_VALUE "There was no file uploaded. Exactly one file is required."
  when (length files > 1) $ throwApiError API_ERROR_MISSING_VALUE "There was more than one file uploaded. Exactly one file is required."
  let (filename, content) = head files
  signatories <- getSignatories
  when (Data.List.null signatories) $ throwApiError API_ERROR_MISSING_VALUE "There were no involved parties. At least one is needed."
  mdoctype <- apiAskInteger "type"  
  when (isNothing mdoctype) $ throwApiError API_ERROR_MISSING_VALUE "You forgot to add the document type. (ex: type: 1)"
  let doctype = fromJust mdoctype
  _msignedcallback <- apiAskBS "signed_callback"
  _mnotsignedcallback <- apiAskBS "notsigned_callback"
  ctx <- askKontraContext  
  --mnewdoc <- makeDocumentFromFile (getDocumentType doctype) (Input (Right content) (Just filename) "")
  newdoc <- update $ NewDocument author title (getDocumentType doctype) (ctxtime ctx)
  liftIO $ print newdoc
  _ <- lift $ lift $ handleDocumentUpload (documentid newdoc) content filename
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

          lift $ lift $ postDocumentChangeAction sdoc doc Nothing
          return $ toJSObject [("document_id", JSString $ toJSString $ show (documentid sdoc))]

getFiles :: UserAPIFunction [(BS.ByteString, BS.ByteString)]
getFiles = do
  mfiles <- apiLocal "files" $ apiMapLocal $ do
    name    <- apiAskBS     "name"
    content <- apiAskBase64 "content"
    when (isNothing name || isNothing content) $ throwApiError API_ERROR_MISSING_VALUE "Problems with files upload."
    return $ Just (fromJust name, fromJust content)    
  case mfiles of
    Nothing -> throwApiError API_ERROR_MISSING_VALUE "Problems with files upload."
    Just files -> return files

getSignatories :: UserAPIFunction [SignatoryDetails]
getSignatories = do
  minvolved <- apiLocal "involved" $ apiMapLocal $ do
    memail      <- apiAskBS "email"
    mfstname    <- apiAskBS "fstname"
    msndname    <- apiAskBS "sndname"
    mcompany    <- apiAskBS "company"
    mcompanynr  <- apiAskBS "companynr"
    mpersonalnr <- apiAskBS "personalnr"
    mfields     <- apiLocal "customfields" $ apiMapLocal $ do
      mname  <- apiAskBS "name"
      when (isNothing mname) $ throwApiError API_ERROR_MISSING_VALUE "Missing name in customfields."
      mvalue <- apiAskBS "value"
      return $ Just (fromJust mname, mvalue)
    when (isNothing memail || isNothing mfstname || isNothing msndname) $
      throwApiError API_ERROR_MISSING_VALUE "Problems with involved."
    return $ Just SignatoryDetails { signatoryfstname                  = fromJust mfstname
                                   , signatorysndname                  = fromJust msndname
                                   , signatorycompany                  = maybe BS.empty id mcompany
                                   , signatorypersonalnumber           = maybe BS.empty id mcompanynr
                                   , signatorycompanynumber            = maybe BS.empty id mpersonalnr
                                   , signatoryemail                    = fromJust memail
                                   , signatorysignorder                = SignOrder 1
                                   , signatoryfstnameplacements        = []
                                   , signatorysndnameplacements        = []
                                   , signatorycompanyplacements        = []
                                   , signatoryemailplacements          = []
                                   , signatorypersonalnumberplacements = []
                                   , signatorycompanynumberplacements  = []
                                   , signatoryotherfields              = maybe [] (map (\(name, mvalue) ->
                                                                                         FieldDefinition { fieldlabel = name,
                                                                                                           fieldvalue = maybe BS.empty id mvalue,
                                                                                                           fieldplacements = [],
                                                                                                           fieldfilledbyauthor = isJust mvalue
                                                                                                         })) mfields
                                   }
  case minvolved of
    Nothing -> throwApiError API_ERROR_MISSING_VALUE "Problems with involved."
    Just involved -> return involved
