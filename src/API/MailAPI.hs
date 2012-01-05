module API.MailAPI (
      handleMailAPI,
      parseEmailMessageToParts,
      charset
    ) where

import MinutesTime
import DB.Classes
import Company.Model
import User.Model
import Kontra
import Misc
import qualified AppLogger as Log (jsonMailAPI, mailAPI, scrivebymailfailure)
import qualified Doc.DocControl as DocControl

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Control.Applicative
import Happstack.Server hiding (simpleHTTP, host)

import Text.JSON
import Text.JSON.String
import Codec.MIME.Decode
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import Data.Either
import Doc.DocUtils
import ScriveByMail.Control
import Doc.Transitory

--import ScriveByMail.Parse

import qualified Codec.Text.IConv as IConv
import InspectXMLInstances ()
--import API.APICommons
import Data.Char
import Doc.DocStateData
--import Doc.Model
--import Doc.DocState
--import Util.JSON
import Doc.JSON

parseEmailMessageToParts :: BS.ByteString -> (MIME.MIMEValue, [(MIME.Type, BS.ByteString)])
parseEmailMessageToParts content = (mime, parts mime)
  where
    mime = MIME.parseMIMEMessage (BSC.unpack content)
    parts mimevalue = case MIME.mime_val_content mimevalue of
        MIME.Single value -> [(MIME.mime_val_type mimevalue, BSC.pack value)]
        MIME.Multi more -> concatMap parts more

charset :: MIME.Type -> String
charset mimetype = fromMaybe "us-ascii" $ lookup "charset" (MIME.mimeParams mimetype)

handleMailAPI :: Kontrakcja m => m String
handleMailAPI = do
  Input contentspec _ _ <- getDataFnM (lookInput "mail")
  content <- concatChunks <$> case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content

  let (mime, allParts) = parseEmailMessageToParts content

      isPDF (tp,_) = MIME.mimeType tp == MIME.Application "pdf" ||
                     (MIME.mimeType tp == MIME.Application "octet-stream" &&
                      maybe False (isSuffixOf ".pdf" . map toLower) (lookup "name" (MIME.mimeParams tp)))
      isPlain (tp,_) = MIME.mimeType tp == MIME.Text "plain"

      pdfs = filter isPDF allParts
      plains = filter isPlain allParts

      from = fromMaybe "" $ lookup "from" (MIME.mime_val_headers mime)
      to   = fromMaybe "" $ lookup "to"   (MIME.mime_val_headers mime)

      isOutlook = maybe False ("Outlook" `isInfixOf`) (lookup "x-mailer" (MIME.mime_val_headers mime)) ||
                  maybe False ("Exchange" `isInfixOf`) (lookup "x-mimeole" (MIME.mime_val_headers mime))

      subject = decodeWords $ BS.toString $ maybe BS.empty BS.fromString $ lookup "subject" (MIME.mime_val_headers mime)

  -- access control

  let username = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') from
    -- 'extension' is a piece of data that is after + sign in email
    -- addres. example: api+1234@api.skrivapa.se here '1234' is
    -- extension and can be used as for example password
  let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') to

  muser <- runDBQuery (GetUserByEmail Nothing (Email $ BS.fromString username))
  when (isNothing muser) $ do
    Log.mailAPI $ "User does not exist: " ++ username
    mzero

  let Just user = muser

  mmailapi <- runDBQuery $ GetUserMailAPI $ userid user
  when (isNothing mmailapi) $ do
    Log.mailAPI $ "User has not enabled api: " ++ username
    mzero

  let Just mailapi = mmailapi

  when (maybeRead extension /= Just (umapiKey mailapi)) $ do
    Log.mailAPI $ "User api key does not match: " ++ username ++ " key: " ++ extension
    mzero

  -- at this point, the user has been authenticated
  -- we can start sending him emails about errors

  -- also, we have enough to begin differentiating between Scrive by
  -- Mail and JSON api since the JSON mail api sends back different
  -- errors (in JSON format), we need to split it off here.

  case plains of
    [p] | isJSON p -> jsonMailAPI mailapi username user pdfs plains content
    _              -> scriveByMail mailapi username user to subject isOutlook pdfs plains content

isJSON :: (MIME.Type, BS.ByteString) -> Bool
isJSON plain =
  case IConv.convertStrictly (charset (fst plain)) "UTF-8" (BSL.fromChunks [(snd plain)]) of
    Left r -> isPrefixOf "{" $ strip $ BS.toString $ BS.concat $ BSL.toChunks r
    _      -> False

jsonMailAPI :: (Kontrakcja m) =>
                UserMailAPI
                -> String
                -> User
                -> [(MIME.Type, BS.ByteString)]
                -> [(MIME.Type, BS.ByteString)]
                -> BS.ByteString
                -> m String
jsonMailAPI mailapi username user pdfs plains content = do
  Context{ctxtime, ctxipnumber} <- getContext
  when (umapiDailyLimit mailapi <= umapiSentToday mailapi) $ do
    Log.jsonMailAPI $ "Daily limit of documents for user '" ++ username ++ "' has been reached"
    -- ignore it
    mzero

  when (length plains /= 1) $ do
    Log.jsonMailAPI $ "Wrong number of plain text attachments."
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    mzero

  let [plain] = plains

  recodedPlain' <- case IConv.convertStrictly (charset (fst plain)) "UTF-8" (BSL.fromChunks [(snd plain)]) of
    Left result' -> return $ BS.concat (BSL.toChunks (result'))
    Right errmsg -> do
      let msg = (show $ IConv.reportConversionError errmsg)

      Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
      Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

      mzero

  let ejson = runGetJSON readJSValue $ BS.toString recodedPlain'

  when (isLeft ejson) $ do
    let Left msg = ejson

    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

    -- should probably send a bad json error message
    mzero

  let Right json = ejson
      edcr = dcrFromJSON json

  when (isLeft edcr) $ do
    let Left msg = edcr

    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

    -- should probably send a bad request message
    mzero

  let Right dcr = edcr

  -- we should check that the request makes sense

  -- exactly one author
  let aus = [a | a <- dcrInvolved dcr, elem SignatoryAuthor $ irRole a]

  when (length aus /= 1) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Should have exactly one author; instead, has " ++ show aus
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    mzero

  let [authorIR] = aus

  -- at least one signatory
  let sigs = length $ [p | p <- dcrInvolved dcr, elem SignatoryAuthor $ irRole p]
  when (1 > length (dcrInvolved dcr)) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Should have at least one signatory; instead, has " ++ show sigs
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    mzero

  -- the mainfile is attached
  let mpdf = getByAttachmentName (dcrMainFile dcr) pdfs

  when (isNothing mpdf) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Missing pdf attachment: " ++ show (dcrMainFile dcr)
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

    mzero

  let Just pdf = mpdf

  let pdfBinary = snd pdf

  -- create document
  -- set to advanced
  -- add signatories
  -- send document

  mcompany <- case usercompany user of
    Just companyid -> runDBQuery $ GetCompany companyid
    Nothing -> return Nothing

  let userDetails = signatoryDetailsFromUser user mcompany

  -- check email, first name, and last name to make sure they match with the author
  when (not $ all id [sfValue u == sfValue j
                     | u <- signatoryfields userDetails
                     , j <- irData authorIR
                     , sfType u == sfType j
                     , sfType u `elem` [FirstNameFT, LastNameFT, EmailFT]
                     ]) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Author data does not match: " ++ show authorIR ++ " and " ++ show userDetails
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    mzero

  -- check that all signatories have first, last, and email
  when (not $ all ((3 ==) . length) [[v | v <- irData s
                                      , sfType v `elem` [FirstNameFT, LastNameFT, EmailFT]]
                                    | s <- dcrInvolved dcr]) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Minimum information not there for all signatories: " ++ show (dcrInvolved dcr)
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    mzero

  let doctype = dcrType dcr
      title = dcrTitle dcr

  edoc <- doc_update $ NewDocument user mcompany (BS.fromString title) doctype ctxtime

  when (isLeft edoc) $ do
    let Left msg = edoc

    Log.jsonMailAPI $ "Could not create document: " ++ msg

    mzero

  let Right doc = edoc

  _ <- DocControl.handleDocumentUploadNoLogin (documentid doc) pdfBinary (BS.fromString title)
  _ <- doc_update $ SetDocumentAdvancedFunctionality (documentid doc) ctxtime
  _ <- doc_update $ SetEmailIdentification (documentid doc) ctxtime

  let signatories = for (dcrInvolved dcr) $ \InvolvedRequest{irRole,irData} ->
        (SignatoryDetails{signatorysignorder = SignOrder 0, signatoryfields = irData},
         irRole)

  errs <- lefts <$> (sequence $ [doc_update $ ResetSignatoryDetails (documentid doc) signatories ctxtime])

  when ([] /= errs) $ do
    Log.jsonMailAPI $ "Could not set up document: " ++ (intercalate "; " errs)

    mzero

  edoc2 <- doc_update $ PreparationToPending (documentid doc) ctxtime

  when (isLeft edoc2) $ do
    Log.jsonMailAPI $ "Could not got to pending document: " ++ (intercalate "; " errs)

    mzero

  let Right doc2 = edoc2
      docid = documentid doc2
  _ <- DocControl.markDocumentAuthorReadAndSeen doc2 ctxtime ctxipnumber
  _ <- DocControl.postDocumentChangeAction doc2 doc Nothing

  _ <- runDBUpdate $ SetUserMailAPI (userid user) $ Just mailapi {
    umapiSentToday = umapiSentToday mailapi + 1
  }

  -- sendMailAPIConfirmEmail ctx doc2

  return $ show docid

getByAttachmentName :: String -> [(MIME.Type, BS.ByteString)] -> Maybe (MIME.Type, BS.ByteString)
getByAttachmentName name ps =
  find byname ps
    where byname p = case lookup "name" (MIME.mimeParams $ fst p) of
            Just n' -> name == n'
            _       -> False
