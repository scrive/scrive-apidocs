module API.MailAPI (
      handleMailCommand
    , mailAPI
    ) where

import Doc.DocState
import Company.CompanyState
import Kontra
import Misc
import PayEx.PayExInterface ()-- Import so at least we check if it compiles
import qualified AppLogger as Log (debug)
import qualified Doc.DocControl as DocControl
import Control.Monad.Reader

import Control.Monad.Error
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host)
import Happstack.State (query, update)
import Text.JSON
import Text.JSON.String
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS

import qualified Codec.Text.IConv as IConv
import InspectXMLInstances ()
import MinutesTime
import API.API
import API.APICommons

data MailAPIContext = MailAPIContext { ibody :: APIRequestBody
                                     , icontent :: BS.ByteString
                                     , ifrom :: BS.ByteString
                                     , ito :: BS.ByteString
                                     }
type MailAPIFunction m a = APIFunction m MailAPIContext a

instance APIContext MailAPIContext where
    body = ibody
    newBody b ctx = ctx {ibody = b}
    apiContext = apiContextForMail


apiContextForMail :: Kontrakcja m => m (Either (API_ERROR, String) MailAPIContext)
apiContextForMail = do
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
        return $ Left (API_ERROR_PARSING, "Cannot parse email API call: " ++ msg)
      Right (json, pdf, from, to) -> do
                        return $ Right $ MailAPIContext { ibody = json
                                                        , icontent = pdf
                                                        , ifrom = from
                                                        , ito = to
                                                        }

mailAPI :: Kontrakcja m => m Response
mailAPI = apiCall "mailapi" handleMailCommand

maybeFail :: (Monad m) => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return

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

-- handleMailCommand :: JSValue -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Kontra (Either String DocumentID)
-- handleMailCommand (JSObject json) content from to = runErrorT $ do
handleMailCommand :: Kontrakcja m => MailAPIFunction m (JSObject JSValue)
handleMailCommand = do
    MailAPIContext { ifrom = from, ito = to, icontent = content } <- ask
    let username = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') $ BS.toString from
    -- 'extension' is a piece of data that is after + sign in email
    -- addres. example: api+1234@api.skrivapa.se here '1234' is
    -- extension and can be used as for example password
    let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') $ BS.toString to

    Context { ctxtime, ctxipnumber } <- getContext
    (maybeUser :: Maybe User) <- liftIO $ query $ GetUserByEmail Nothing (Email $ BS.fromString username)
    (user :: User) <- maybeFail ("User '" ++ username ++ "' not found") maybeUser

    when (isNothing $ usermailapi user) $
        fail $ "User '" ++ username ++ "' hasn't enabled mail api"
    let Just mailapi = usermailapi user

    title <- fromMaybe (BS.fromString "Untitled document received by email") <$> (apiAskBS "title")

    apikey <- fromMaybe extension <$> (apiAskString "apikey")

    case apikey of
        "" -> fail $ "Need to specify 'apikey' in JSON or after + sign in email address"
        k | (show $ umapiKey mailapi) == k -> return ()
        k -> fail $ "Apikey '" ++ k ++ "' invalid for account '" ++ username ++ "'"

    let toStr = BS.toString to
    mdoctype <- apiAskString "doctype"
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

    today <- asInt <$> liftIO getMinutesTime
    if today == umapiLastSentDate mailapi
       then do
           when (umapiDailyLimit mailapi == umapiSentToday mailapi) $ do
               fail $ "Daily limit of documents for user '" ++ username ++ "' has been reached"
           let senttoday = umapiSentToday mailapi + 1
           _ <- liftIO $ update $ SetUserMailAPI (userid user) $ Just mailapi {
               umapiSentToday = senttoday
           }
           return ()
       else do
           _ <- liftIO $ update $ SetUserMailAPI (userid user) $ Just mailapi {
                 umapiLastSentDate = today
               , umapiSentToday = 1
           }
           return ()

    (involvedTMP) <- fmap (fromMaybe []) $ (apiLocal "involved" $ apiMapLocal $ getSignatoryTMP)
    let (involved :: [SignatoryDetails]) = map toSignatoryDetails involvedTMP

    let signatories = map (\p -> (p,[SignatoryPartner])) involved
    mcompany <- case usercompany user of --TODO EM maybe make signatoryDetailsFromUser figure out the company
                  Just companyid -> query $ GetCompany companyid
                  Nothing -> return Nothing
    let userDetails = signatoryDetailsFromUser user mcompany

    (doc :: Document) <- liftIO $ update $ NewDocument user title doctype ctxtime
    (_ :: ()) <- liftKontra $ DocControl.handleDocumentUpload (documentid doc) content title
    (_ :: Either String Document) <- liftIO $ update $ UpdateDocument ctxtime (documentid doc) title
                                     signatories Nothing BS.empty
                                    (userDetails, [SignatoryPartner, SignatoryAuthor], userid user, usercompany user)
                                    [EmailIdentification] Nothing AdvancedFunctionality

    (eithernewdocument :: Either String Document) <- update $ AuthorSendDocument (documentid doc) ctxtime ctxipnumber Nothing

    (newdocument :: Document) <- case eithernewdocument of
                                     Left errmsg -> return (error errmsg)
                                     Right document -> return document

    (_ :: ()) <- liftKontra $ DocControl.markDocumentAuthorReadAndSeen newdocument ctxtime ctxipnumber
    (_ :: ()) <- liftKontra $ DocControl.postDocumentChangeAction newdocument doc Nothing
    let docid = documentid doc
    let (rjson :: JSObject JSValue) = toJSObject [ ("status", JSString (toJSString "success"))
                                                 , ("message", JSString (toJSString ("Document #" ++ show docid ++ " created")))
                                                 , ("documentid", JSString (toJSString (show docid)))
                                                 ]
    return rjson

