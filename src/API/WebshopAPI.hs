{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  API.WebshopAPI
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Webshop API is simplest for of integration with SkrivaPa. 
-- Customer uses oryginall service to prepare a template of a document.
-- After he is ready to sign a contract with his client he provides SkrivaPa with 
-- all data to change template to real contract. We then just provide him with links 
-- for signatories to sign.
-----------------------------------------------------------------------------
module API.WebshopAPI
    ( 
       webshopAPI
    ) where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Doc.DocState
import Happstack.Server hiding (simpleHTTP,host,body)
import Happstack.State (query)
import KontraLink
import Misc
import System.IO.Unsafe
import Kontra
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified AppLogger as Log (error, security, debug)
import Happstack.State (update)
import PayEx.PayExInterface -- Import so at least we check if it compiles
import InputValidation
import Text.JSON
import Control.Monad.Reader
import API.API
import API.IntegrationAPIUtils
import Doc.DocUtils

{- | 
  Defining API schema
-}

data WebshopAPIContext = WebshopAPIContext {wsbody :: APIRequestBody ,user :: User}
type WebshopAPIFunction a = APIFunction WebshopAPIContext a

instance APIContext WebshopAPIContext where
    body= wsbody
    newBody b ctx = ctx {wsbody = b}
    apiContext  = do
        muser <- webshopUser
        mbody <- apiBody 
        case (muser, mbody)  of
             (Just user, Right body) -> return $ Right $ WebshopAPIContext {wsbody=body,user=user}
             (Nothing,_) -> return $ Left $ (API_ERROR_LOGIN ,"Not logged in")
             (_,Left s) -> return $ Left $ (API_ERROR_PARSING, "Parsing error: " ++ s)
    


webshopUser ::  Kontra (Maybe User)
webshopUser = do
    email <- getFieldUTFWithDefault BS.empty "email"
    muser <- query $ GetUserByEmail Nothing (Email email)
    case muser of
        Nothing -> return Nothing       
        Just user -> do
            passwd <- getFieldUTFWithDefault BS.empty "password"
            if (verifyPassword (userpassword user) passwd)
               then return $ Just user
               else return Nothing 

webshopAPI :: Kontra Response
webshopAPI =  dir "webshop" $ msum [
                  apiCall "new" newFromTemplate
                , apiUnknownCall
             ]

--- API calls starts here
            
newFromTemplate :: WebshopAPIFunction APIResponse
newFromTemplate = do
    template <- getTemplate
    signatories <- getSignatories
    document <- fillTemplate template signatories
    checkIfReadyToSend document
    signable <- update $ SignableFromDocument document 
    author <- user <$> ask
    ctx <- askKontraContext
    msenddoc <- update $ AuthorSendDocument (documentid signable) (ctxtime ctx) (ctxipnumber ctx) Nothing
    case msenddoc of
         Right senddoc -> return $ toJSObject [("links", JSArray $ map (JSString . toJSString) $ documentLinks senddoc)]
         Left s -> throwApiError API_ERROR_OTHER $ "Webshop can't send a document: " ++  s

documentLinks::Document -> [String]
documentLinks doc = 
    map (show . (LinkSignDoc $ doc)) (documentsignatorylinks doc)
        

fillTemplate::Document -> [SignatoryTMP] -> WebshopAPIFunction Document
fillTemplate doc (authorTMP:sigsTMP) = do
    let (author,sigs) = span siglinkIsAuthor (documentsignatorylinks doc)
    author' <- sequence $ fmap (mergeSignatoryWithTMP authorTMP) author 
    sigs' <- sequence $ zipWith mergeSignatoryWithTMP sigsTMP  sigs 
    return $ doc {documentsignatorylinks = author' ++ sigs'}
fillTemplate doc [] = throwApiError API_ERROR_OTHER "No signatory provided"

getTemplate::WebshopAPIFunction Document
getTemplate = do
    mtemplateid <- maybeReadM $ apiAskString "id"
    when (isNothing mtemplateid) $ throwApiError API_ERROR_MISSING_VALUE "No valid document ID provided"
    mdoc <- query $ GetDocumentByDocumentID(fromJust mtemplateid)
    when (isNothing mdoc) $ throwApiError API_ERROR_NO_DOCUMENT "No document exist with this ID"
    let doc = fromJust mdoc
    when (not $ isTemplate doc) $ throwApiError API_ERROR_OTHER "This document is not a template"
    user <- user <$> ask
    when (not $ isUserAuthor doc user) $ throwApiError API_ERROR_PERMISSION_ACTION "Current user is not an author of a document" 
    return doc
    
getSignatories::WebshopAPIFunction [SignatoryTMP]
getSignatories = fmap (fromMaybe []) $ apiLocal "signatories" $ apiMapLocal $ getSignatoryTMP
                
checkIfReadyToSend::Document -> WebshopAPIFunction ()
checkIfReadyToSend doc = do
    let siglinks = documentsignatorylinks doc
    liftIO $ putStrLn $ show siglinks
    let nosignatories = null $ filter (not . siglinkIsAuthor) siglinks
    when nosignatories $ throwApiError  API_ERROR_OTHER "At least one nonauthor signatory must be set"
    let bademail = any (not . isGood . asValidEmail . BS.toString . signatoryemail . signatorydetails ) siglinks
    when bademail $ throwApiError  API_ERROR_OTHER "One of signatories has a bad email"
    let nofstname = any (BS.null . signatoryfstname . signatorydetails ) siglinks
    when nofstname $ throwApiError  API_ERROR_OTHER "One of signatories has empty first name"
    let nosndname = any (BS.null . signatorysndname . signatorydetails ) siglinks
    when nosndname $ throwApiError  API_ERROR_OTHER "One of signatories has empty last name"
    
