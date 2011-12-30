module API.MailAPI (
      handleMailAPI

    ) where

--import DB.Classes
--import Doc.DocState
--import Company.Model
--import User.Model
import Kontra
--import Misc
--import qualified AppLogger as Log (debug)
--import qualified Doc.DocControl as DocControl
--import Control.Monad.Reader
--import InputValidation
--import Control.Monad.Error
--import Data.Functor
--import Data.List
--import Data.Maybe
--import Happstack.Server hiding (simpleHTTP, host)
--import Happstack.StaticRouting(Route, Path)
--import Happstack.State (update)
--import Text.JSON
--import Text.JSON.String
--import Codec.MIME.Decode
--import qualified Codec.MIME.Parse as MIME
--import qualified Codec.MIME.Type as MIME
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as BSC
--import qualified Data.ByteString.Lazy as BSL
--import qualified Data.ByteString.UTF8 as BS
--import Data.Either

import ScriveByMail.Control
--import ScriveByMail.View
--import ScriveByMail.Parse

--import qualified Codec.Text.IConv as IConv
import InspectXMLInstances ()
--import API.API
--import API.APICommons
--import Data.Char
--import Mails.MailsData as MD
--import Data.String.Utils
--import Util.StringUtil
--import Doc.DocViewMail
--import Doc.DocProcess
--import Doc.DocStateData
--import Doc.DocState

--import ScriveByMail.Control

--import Text.JSON.Types
import Util.JSON
import Doc.JSON

                                     
handleMailAPI :: Kontrakcja m => m String
handleMailAPI = do
  let _ = dcrFromJSON jsempty
  handleScriveByMail

{-
data MailAPIContext = MailAPIContext { ibody :: APIRequestBody
                                     , icontent :: BS.ByteString
                                     , ifrom :: BS.ByteString
                                     , ito :: BS.ByteString
                                     }
type MailAPIFunction m a = APIFunction m MailAPIContext a

instance APIContext MailAPIContext where
    apiContext = apiContextForMail

instance JSONContainer MailAPIContext where
    getJSON = ibody
    setJSON j mapictx = mapictx {ibody = j}


apiContextForMail :: Kontrakcja m => m (Either (API_ERROR, String) MailAPIContext)
apiContextForMail = do
    ctx <- getContext
    Input contentspec (Just _filename) _contentType <- getDataFnM (lookInput "mail")
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    -- content at this point is basically full MIME email as it was received by SMTP server
    -- can be tested using curl -X POST -F mail=@mail.eml http://url.com/mailapi/
    eresult <- parseEmailMessage (concatChunks content)
    case eresult of
      Left (from,msg) -> do
        Log.debug $ "handleMailAPI: " ++ msg
        let from' = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') $ BS.toString from
        case (asValidEmail from') of 
             Good _ -> scheduleEmailSendout (ctxesenforcer ctx) $ mailMailApiError (MD.MailAddress {MD.fullname=BS.empty, MD.email = BS.fromString from'}) msg
             _ -> return ()
        return $ Left (API_ERROR_PARSING, "Cannot parse email API call: " ++ msg)
      Right (json, pdf, from, to) -> do
        Log.debug $ "json again: " ++ show json
        return $ Right $ MailAPIContext { ibody    = json
                                        , icontent = pdf
                                        , ifrom    = from
                                        , ito      = to
                                        }



mailAPI :: (Kontrakcja m, Path m (m Response) Response Response) => Route (m Response)
mailAPI = apiCall "mailapi" handleMailCommand

maybeFail :: (Monad m) => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return
-}
{-

{ "title": "Title of the document",
  "involved": [
           { "fstname": "Gracjan",
             "sndname": "Polak",
             "personalnumber": "1412341234",
             "email": "gracjan@skrivapa.se"
           }]
}

-}
{-
parseEmailMessageToParts :: BS.ByteString -> (MIME.MIMEValue, [(MIME.Type, BS.ByteString)])
parseEmailMessageToParts content = (mime, parts mime)
  where
    mime = MIME.parseMIMEMessage (BSC.unpack content)
    parts mimevalue = case MIME.mime_val_content mimevalue of
        MIME.Single value -> [(MIME.mime_val_type mimevalue, BSC.pack value)]
        MIME.Multi more -> concatMap parts more

parseEmailMessage :: (Monad m, MonadIO m) => BS.ByteString -> m (Either (BS.ByteString,String) (JSValue,BS.ByteString,BS.ByteString,BS.ByteString))
parseEmailMessage content = 
  let 
  (mime, allParts) = parseEmailMessageToParts content
  isPDF (tp,_) = MIME.mimeType tp == MIME.Application "pdf"
  isPlain (tp,_) = MIME.mimeType tp == MIME.Text "plain"
  typesOfParts = map fst allParts
  pdfs = filter isPDF allParts
  plains = filter isPlain allParts
  from = maybe BS.empty BS.fromString $ lookup "from" (MIME.mime_val_headers mime)
  to   = maybe BS.empty BS.fromString $ lookup "to"   (MIME.mime_val_headers mime)
  isOutlook = maybe False ("Outlook" `isInfixOf`) $ lookup "x-mailer" (MIME.mime_val_headers mime) 
  subject = decodeWords $ BS.toString $ maybe BS.empty BS.fromString $ lookup "subject" (MIME.mime_val_headers mime)
  charset mimetype = maybe "us-ascii" id $ lookup "charset" (MIME.mimeParams mimetype)
  recode mimetype content' =
        case IConv.convertStrictly (charset mimetype) "UTF-8" (BSL.fromChunks [content']) of
          Left result' -> return $ BS.concat (BSL.toChunks (result'))
          Right errmsg -> throwError (show $ IConv.reportConversionError errmsg)
  extendWithFrom (Left l) = Left (from,l)
  extendWithFrom (Right r) = Right r
  in
 liftM extendWithFrom $ runErrorT $ do        
  Log.debug $ "Types of mime parts: " ++ show typesOfParts 
  pdf <- case pdfs of
    [pdf] -> return pdf
    _ -> throwError $ "Exactly one of attachments should be application/pdf, you have " ++ show typesOfParts
  plain <- case plains of
    [plain] -> return plain
    _ -> throwError $ "Exactly one of attachments should be text/plain, you have " ++ show typesOfParts
  let pdfBinary = snd pdf
  recodedPlain' <- recode (fst plain) (snd plain)
  let recodedPlain = (replace "\r\n\r\n" "\r\n" $ BS.toString recodedPlain') <| isOutlook |> BS.toString recodedPlain'
  Log.debug $ "recodedPlain body: " ++ recodedPlain
  json <- (ErrorT . return) $ if '{' == (head $ strip recodedPlain)
                              then runGetJSON readJSObject recodedPlain
                              else parseSimpleEmail subject recodedPlain
  Log.debug $ "Json returned from parsing: " ++ show json
  return (json,pdfBinary,from,to)

-- handleMailCommand :: JSValue -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Kontra (Either String DocumentID)
-- handleMailCommand (JSObject json) content from to = runErrorT $ do
handleMailCommand :: Kontrakcja m => MailAPIFunction m (JSObject JSValue)
handleMailCommand = do
    Log.debug "top of handleMailCommand"
    MailAPIContext { ifrom = from, ito = to, icontent = content } <- ask
    let username = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') $ BS.toString from
    -- 'extension' is a piece of data that is after + sign in email
    -- addres. example: api+1234@api.skrivapa.se here '1234' is
    -- extension and can be used as for example password
    let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') $ BS.toString to

    ctxx@Context { ctxtime, ctxipnumber } <- getContext
    user <- runDBQuery (GetUserByEmail Nothing (Email $ BS.fromString username))
      >>= maybeFail ("User '" ++ username ++ "' not found")

    mmailapi <- runDBQuery $ GetUserMailAPI $ userid user
    when (isNothing mmailapi) $
        fail $ "User '" ++ username ++ "' hasn't enabled mail api"
    let Just mailapi = mmailapi

    title <- fromMaybe (BS.fromString "Untitled document received by email") <$> (fromJSONField "title")

    apikey <- fromMaybe extension <$> (fromJSONField "apikey")

    case apikey of
        "" -> fail $ "Need to specify 'apikey' in JSON or after + sign in email address"
        k | (show $ umapiKey mailapi) == k -> return ()
        k -> fail $ "Apikey '" ++ k ++ "' invalid for account '" ++ username ++ "'"

    

    let toStr = BS.toString to
    mdoctype <- fromJSONField "doctype"
    doctype <- case mdoctype of
        Just "contract" -> return (Signable Contract)
        Just "offer" -> return (Signable Offer)
        Just "order" -> return (Signable Order)
        Just z -> fail $ "Unsupported document type '" ++ z ++ "', should be one of ['contract', 'offer', 'order']"
        -- magic below defaults to (Signable Contract), but looks up first True in a list of options
        _ -> return $ fromJust $ lookup True
             [ ("contract" `isInfixOf` toStr, Signable Contract)
             , ("offer" `isInfixOf` toStr,    Signable Offer)
             , ("order" `isInfixOf` toStr,    Signable Order)
             , (True,                         Signable Contract)
             ]

    when (umapiDailyLimit mailapi == umapiSentToday mailapi) $ do
        fail $ "Daily limit of documents for user '" ++ username ++ "' has been reached"
    _ <- runDBUpdate $ SetUserMailAPI (userid user) $ Just mailapi {
        umapiSentToday = umapiSentToday mailapi + 1
    }
    Log.debug "here"
    (involvedTMP) <- fmap (fromMaybe []) $ (fromJSONLocal "involved" $ fromJSONLocalMap $ getSignatoryTMP)
    let (involved :: [SignatoryDetails]) = map toSignatoryDetails involvedTMP

    Log.debug $ show involved
    let signatories = map (\p -> (p,[SignatoryPartner])) involved
    mcompany <- case usercompany user of
                  Just companyid -> runDBQuery $ GetCompany companyid
                  Nothing -> return Nothing
    let userDetails = signatoryDetailsFromUser user mcompany
    let authorrole = [SignatoryAuthor] <| Just True == getValueForProcess doctype processauthorsend |> [SignatoryAuthor, SignatoryPartner]

    (eitherdoc :: Either String Document) <- liftIO $ update $ NewDocument user mcompany title doctype ctxtime
    (doc :: Document) <- case eitherdoc of
                           Left errmsg -> return (error errmsg)
                           Right document -> return document
    (_ :: ()) <- liftKontra $ DocControl.handleDocumentUploadNoLogin (documentid doc) content title
    _errs <- lefts <$> (liftIO $ sequence $ [update $ SetEmailIdentification (documentid doc) ctxtime,
                                            update $ SetDocumentTitle (documentid doc) title ctxtime,
                                            update $ SetDocumentAdvancedFunctionality (documentid doc) ctxtime,
                                            update $ ResetSignatoryDetails (documentid doc) ((userDetails, authorrole):signatories) ctxtime])

    (eithernewdocument :: Either String Document) <- update $ PreparationToPending (documentid doc) ctxtime
    (newdocument :: Document) <- case eithernewdocument of
                                     Left errmsg -> return (error errmsg)
                                     Right document -> return document

    (_ :: ()) <- liftKontra $ DocControl.markDocumentAuthorReadAndSeen newdocument ctxtime ctxipnumber
    (_ :: ()) <- liftKontra $ DocControl.postDocumentChangeAction newdocument doc Nothing
    let docid = documentid doc

    _ <- sendMailAPIConfirmEmail ctxx newdocument

    let (rjson :: JSObject JSValue) = toJSObject [ ("status", JSString (toJSString "success"))
                                                 , ("message", JSString (toJSString ("Document #" ++ show docid ++ " created")))
                                                 , ("documentid", JSString (toJSString (show docid)))
                                                 ]
    return rjson


-}
