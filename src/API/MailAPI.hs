
module API.MailAPI where


import Crypto
import Doc.DocState
import InputValidation
import InspectXML
import Kontra
import KontraLink
import Mails.MailsConfig
import Mails.SendGridEvents
import Mails.SendMail
import MinutesTime
import Misc
import PayEx.PayExInterface ()-- Import so at least we check if it compiles
import Redirect
import Routing
import Session
import User.UserView as UserView
import qualified Administration.AdministrationControl as Administration
import qualified AppLogger as Log (error, security, debug)
import qualified Contacts.ContactsControl as Contacts
import qualified Doc.DocControl as DocControl
import qualified ELegitimation.BankID as BankID
import qualified FlashMessage as F
import qualified MemCache
import qualified Payments.PaymentsControl as Payments
import qualified TrustWeaver as TW
import qualified User.UserControl as UserControl

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.State
import Data.Functor
import Data.List
import Data.Maybe
import Data.Word
import GHC.Int (Int64(..))
import Happstack.Server hiding (simpleHTTP, host)
import Happstack.Server.Internal.Cookie
import Happstack.State (query)
import Happstack.State (update)
import ListUtil
import Network.Socket
import System.Directory
import System.Time
import Text.JSON
import Text.JSON.String
import API.Service.ServiceControl

import Text.JSON.Types
import Text.StringTemplate.Base (StringTemplate(..))
import Text.StringTemplate.Classes (StFirst(..))
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import qualified Codec.Text.IConv as IConv
import InspectXMLInstances ()

maybeFail :: (Monad m) => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return 



{- 

{ "title": "Title of the document",
  "persons": [ 
           { "firstName": "Gracjan",
             "lastName": "Polak",
             "personNumber": "1412341234"
             "email": "gracjan@skrivapa.se"
           }]
}

-}

parseEmailMessageToParts :: BS.ByteString -> (MIME.MIMEValue, [(MIME.Type, BS.ByteString)])
parseEmailMessageToParts content = (mime, parts mime)
  where
    mime = MIME.parseMIMEMessage (BSC.unpack content)
    parts mimevalue = case MIME.mime_val_content mimevalue of 
        MIME.Single value -> [(MIME.mime_val_type mimevalue, BSC.pack value)]
        MIME.Multi more -> concatMap parts more
  
parseEmailMessage :: (Monad m, MonadIO m) => BS.ByteString -> m (Either String (JSValue,BS.ByteString,BS.ByteString,BS.ByteString))
parseEmailMessage content = runErrorT $ do
  let (mime, allParts) = parseEmailMessageToParts content 
      isPDF (tp,_) = MIME.mimeType tp == MIME.Application "pdf"
      isPlain (tp,_) = MIME.mimeType tp == MIME.Text "plain"
      typesOfParts = map fst allParts
  let pdfs = filter isPDF allParts
  let plains = filter isPlain allParts
  pdf <- case pdfs of
    [pdf] -> return pdf
    _ -> fail $ "Exactly one of attachments should be application/pdf, you have " ++ show typesOfParts
  plain <- case plains of
    [plain] -> return plain
    _ -> fail $ "Exactly one of attachments should be text/plain, you have " ++ show typesOfParts
  let pdfBinary = snd pdf
  let from = maybe BS.empty BS.fromString $ lookup "from" (MIME.mime_val_headers mime)
  let to = maybe BS.empty BS.fromString $ lookup "to" (MIME.mime_val_headers mime)
  
  let charset mimetype = maybe "us-ascii" id $ lookup "charset" (MIME.mimeParams mimetype)
  let recode mimetype content' = 
        case IConv.convertStrictly (charset mimetype) "UTF-8" (BSL.fromChunks [content']) of
          Left result' -> return $ BS.concat (BSL.toChunks (result'))
          Right errmsg -> fail (show $ IConv.reportConversionError errmsg) 
  recodedPlain <- recode (fst plain) (snd plain)   
    
  json <- (ErrorT . return) $ runGetJSON readJSObject (BS.toString recodedPlain)
  return (json,pdfBinary,from,to)
  

handleMailCommand :: JSValue -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Kontra (Either String DocumentID)
handleMailCommand (JSObject json) content from to = runErrorT $ do
  let username = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') $ BS.toString from
  -- 'extension' is a piece of data that is after + sign in email
  -- addres. example: api+1234@api.skrivapa.se here '1234' is
  -- extension and can be used as for example password
  let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') $ BS.toString to
    
  Context { ctxtime, ctxipnumber } <- lift get
  maybeUser <- lift $ query $ GetUserByEmail Nothing (Email $ BS.fromString username)
  (user :: User) <- maybeFail ("user '" ++ username ++ "' not found") maybeUser
  
  let title = case get_field json "title" of
        Just (JSString x) -> BS.fromString (fromJSString x)
        _ -> BS.fromString "Untitled document received by email"
  let apikey = case get_field json "apikey" of
        Just (JSString x) -> fromJSString x
        _ -> extension
      extractPerson :: (Monad m) => JSValue -> m SignatoryDetails
      extractPerson (JSObject x) = do
        (JSString email :: JSValue) <- maybeFail "'email' is required string field" $ get_field x "email" 
        JSString firstName <- maybeFail "'firstName' is required string field" $ get_field x "firstName" 
        JSString lastName <- maybeFail "'lastName' is required string field" $ get_field x "lastName" 
        company <- case get_field x "company" of
          Just (JSString v) -> return $ fromJSString v
          _ -> return ""
        personalNumber <- case get_field x "personalNumber" of
          Just (JSString v) -> return $ fromJSString v
          _ -> return ""
        companyNumber <- case get_field x "companyNumber" of
          Just (JSString v) -> return $ fromJSString v
          _ -> return ""
        return $ SignatoryDetails 
                               { signatoryfstname = BS.fromString (fromJSString firstName)
                               , signatorysndname = BS.fromString (fromJSString lastName)
                               , signatorycompany = BS.fromString company
                               , signatorypersonalnumber = BS.fromString personalNumber
                               , signatorycompanynumber = BS.fromString companyNumber
                               , signatoryemail = BS.fromString (fromJSString email)
                               , signatoryfstnameplacements = []
                               , signatorysndnameplacements = []
                               , signatorycompanyplacements = []
                               , signatoryemailplacements = []
                               , signatorypersonalnumberplacements = []
                               , signatorycompanynumberplacements = []
                               , signatoryotherfields = []
                               , signatorysignorder = SignOrder 1
                               }
      extractPerson _ = fail "'persons' is not a JavaScript object"
      
  case apikey of
    "" -> fail $ "Need to specify 'apikey' in JSON or after + sign in email address"
    "998877665544332211" -> return ()
    z -> fail $ "Apikey '" ++ z ++ "' invalid for account '" ++ username ++ "'"

  let toStr = BS.toString to
  doctype <- case get_field json "doctype" of
        Just (JSString x) -> case fromJSString x of
          "contract" -> return (Signable Contract)
          "offer" -> return (Signable Offer)
          "order" -> return (Signable Order)
          z -> fail $ "Unsupported document type '" ++ z ++ "', should be one of ['contract', 'offer', 'order']"
        _ -> if "contract" `isInfixOf` toStr
             then return $ Signable Contract
             else if "offer" `isInfixOf` toStr
                  then return $ Signable Offer
                  else if "order" `isInfixOf` toStr
                       then return $ Signable Order
                       else return $ Signable Contract

      
  JSArray personsField <- maybeFail "need to specify 'persons'" $ get_field json "persons"
  
  (persons :: [SignatoryDetails]) <- mapM extractPerson personsField
  let
      signatories = map (\p -> (p,[SignatoryPartner])) persons
      userDetails = signatoryDetailsFromUser user
        
  doc <- lift $ update $ NewDocument user title doctype ctxtime
  lift $ DocControl.handleDocumentUpload (documentid doc) content title
  _ <- ErrorT $ update $ UpdateDocument ctxtime (documentid doc) title 
    signatories Nothing BS.empty (userDetails, [SignatoryPartner, SignatoryAuthor], getSignatoryAccount user) [EmailIdentification] Nothing AdvancedFunctionality
  
  newdocument <- ErrorT $ update $ AuthorSendDocument (documentid doc) ctxtime ctxipnumber Nothing
  lift $ DocControl.markDocumentAuthorReadAndSeen newdocument ctxtime ctxipnumber
  lift $ DocControl.postDocumentChangeAction newdocument doc Nothing
  return (documentid doc)
handleMailCommand _ _ _ _ = return (Left ("needs JSON object as top level value"))
  
handleMailAPI :: Kontra Response
handleMailAPI = do
    Input contentspec (Just _filename) _contentType <- getDataFnM (lookInput "mail")
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
            
    -- content at this point is basically full MIME email as it was received by SMTP server
    -- can be tested using curl -X POST -F mail=@mail.eml http://url.com/mailapi/
    eresult <- parseEmailMessage (concatChunks content)
    case eresult of
      Left msg -> do
        Log.debug $ "handleMailAPI: " ++ msg
        return $ toResponse msg
      Right (json, pdf, from, to) -> do
        result' <- handleMailCommand json pdf from to
        case result' of
          Right docid -> do
            let rjson = makeObj [ ("status", JSString (toJSString "success"))
                                , ("message", JSString (toJSString ("Document #" ++ show docid ++ " created")))
                                , ("documentid", JSString (toJSString (show docid)))
                                ]
            return $ toResponse $ showJSValue rjson []
          Left msg -> do
            Log.debug $ msg
            let rjson = makeObj [ ("status", JSString (toJSString "error"))
                                , ("message", JSString (toJSString msg))
                                ]
            return $ toResponse $ showJSValue rjson []
