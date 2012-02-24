module API.MailAPI (
      handleMailAPI,
      parseEmailMessageToParts,
      charset
    ) where

import Redirect
import File.Model
import Doc.DocStorage
import MinutesTime
import DB.Classes
import Company.Model
import User.Model
import Kontra
import KontraLink
import Misc
import qualified Log (jsonMailAPI, mailAPI, scrivebymailfailure)
import qualified Doc.DocControl as DocControl

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
import Doc.Model
import qualified Codec.Text.IConv as IConv
import InspectXMLInstances ()
import Data.String.Utils
import Data.Char
import Doc.DocStateData

import Doc.JSON

import EvidenceLog.Model
import Util.HasSomeUserInfo
--import Util.SignatoryLinkUtils

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


  hostpart <- ctxhostpart <$> getContext
  locale <- ctxlocale <$> getContext
  ctx <- getContext
  let username = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') from
    -- 'extension' is a piece of data that is after + sign in email
    -- addres. example: api+1234@api.skrivapa.se here '1234' is
    -- extension and can be used as for example password
  let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') to

  muser <- runDBQuery (GetUserByEmail Nothing (Email $ BS.fromString $ map toLower username))
  when (isNothing muser) $ do
    Log.mailAPI $ "User does not exist: " ++ username
    sendMailAPIErrorEmail ctx username $ "<p>The address from which you sent the email (" ++ username ++ ") is not a registered Scrive User account. If you would like to sign up to use Scrive, please visit <a href='" ++  hostpart ++ show (LinkHome locale) ++ "'>the Scrive homepage</a>.</p>"    
    mzero

  let Just user = muser

  mmailapi <- runDBQuery $ GetUserMailAPI $ userid user
  when (isNothing mmailapi) $ do
    Log.mailAPI $ "User has not enabled api: " ++ username
    sendMailAPIErrorEmail ctx username $ "<p>The address from which you sent the email (" ++ username ++ ") is a registered Scrive User account but Scrive by Mail has not been activated on the account. If you would like to sign up to use Scrive by Mail, please visit <a href='" ++  hostpart ++ show LinkUserMailAPI ++ "'>the Scrive by Mail settings page</a>.</p>"    
    mzero

  let Just mailapi = mmailapi

  when (maybeRead extension /= Just (umapiKey mailapi)) $ do
    Log.mailAPI $ "User api key does not match: " ++ username ++ " key: " ++ extension
    sendMailAPIErrorEmail ctx username $ "<p>I have just received an email from " ++ username ++ " requesting to create a document. Unfortunately, the Scrive by Mail email address it was sent to was invalid for this account. Please visit is a registered Scrive User account but the email address you sent to is not . If you would like to sign up to use Scrive, please visit <a href='" ++  hostpart ++ show LinkUserMailAPI ++ "'>the Scrive by Mail settings page</a> to note your personal Scrive by Mail email address.</p>"
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
  ctx@Context{ctxtime} <- getContext
  when (umapiDailyLimit mailapi <= umapiSentToday mailapi) $ do
    Log.jsonMailAPI $ "Daily limit of documents for user '" ++ username ++ "' has been reached"
    sendMailAPIErrorEmail ctx username $ "<p>For your own protection, Scrive Mail API sets a daily limit on how many emails you can send out. Your daily Scrive Mail API limit has been reached. To reset your daily limit, please visit " ++ ctxhostpart ctx ++ show LinkUserMailAPI ++ " .<p>"    
    mzero

  when (length plains /= 1) $ do
    Log.jsonMailAPI $ "Wrong number of plain text attachments."
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>Please ensure that there is exactly one plain text attachment. In the message you sent, there were " ++ show (length plains) ++ ".</p>"
    mzero

  let [plain] = plains

  recodedPlain' <- case IConv.convertStrictly (charset (fst plain)) "UTF-8" (BSL.fromChunks [(snd plain)]) of
    Left result' -> return $ BS.concat (BSL.toChunks (result'))
    Right errmsg -> do
      let msg = (show $ IConv.reportConversionError errmsg)

      Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
      Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

      sendMailAPIErrorEmail ctx username $ "<p>I do not know what the problem is. Perhaps try again with a different email program or contact <a href='mailto:eric@scrive.com'>Mail API Support</a> for help.</p>"

      mzero

  let ejson = runGetJSON readJSValue $ BS.toString recodedPlain'

  when (isLeft ejson) $ do
    let Left msg = ejson

    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>I could not understand the email you sent me. The JSON did not parse: <br /><br />\n" ++ msg ++ "<br /><br />\nPlease correct the problems and try again.</p>"
    mzero

  let Right json = ejson
      edcr = dcrFromJSON json

  when (isLeft edcr) $ do
    let Left msg = edcr

    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>I could not understand the email you sent me. The JSON parsed, but there were problems: <br /><br />\n" ++ msg ++ "<br /><br />\nPlease correct the problems and try again.</p>"
    -- should probably send a bad request message
    mzero

  let Right dcr = edcr

  -- we should check that the request makes sense

  -- exactly one author
  let aus = [a | a <- dcrInvolved dcr, elem SignatoryAuthor $ irRole a]

  when (length aus /= 1) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Should have exactly one author; instead, has " ++ show aus
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>There should be exactly one author (role 1 or 2), but there were " ++ show (length aus) ++ ". Please correct the problem and try again.</p>"
    mzero

  let [authorIR] = aus

  -- at least one signatory
  let sigs = length $ [p | p <- dcrInvolved dcr, elem SignatoryPartner $ irRole p]
  when (1 > sigs) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Should have at least one signatory; instead, has " ++ show sigs
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>There should be at least one signatory (role 2 or 5), but there were " ++ show sigs ++ ". Please correct the problem and try again.</p>"
    mzero

  -- the mainfile is attached
  let mpdf = getByAttachmentName (strip $ dcrMainFile dcr) pdfs

  when (isNothing mpdf) $ do
    let attnames = catMaybes $ map (lookup "name" . MIME.mimeParams . fst) pdfs 
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Missing pdf attachment: " ++ show (dcrMainFile dcr)
    Log.jsonMailAPI $ "Here are the attachments: " ++ show (map fst pdfs)
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>In the JSON, you requested the pdf called '" ++ show (dcrMainFile dcr) ++ "' to be considered the main file of the document. I could not find the pdf called '" ++ show (dcrMainFile dcr) ++ "' in the email attachments. The following pdfs were attached: '" ++ intercalate "', '" attnames ++ "'. Please check the file names and JSON and try again.</p>"
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

  let fieldstring FirstNameFT = "fstname"
      fieldstring LastNameFT  = "sndname"
      fieldstring EmailFT     = "email"
      fieldstring _           = ""

  -- check email, first name, and last name to make sure they match with the author
  let mismatched = [(fieldstring $ sfType u, sfValue u, sfValue j)
                   | u <- signatoryfields userDetails
                   , j <- irData authorIR
                   , sfType u == sfType j
                   , sfType u `elem` [FirstNameFT, LastNameFT, EmailFT]
                   , sfValue u /= sfValue j
                   ]
  when (not $ null mismatched) $ do
    let mm (n, u, j) = "<p>In JSON, " ++ show n ++ " was set to " ++ show j ++ " but in user account, it is set to " ++ show u ++ ".</p>"
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Author data does not match: " ++ show authorIR ++ " and " ++ show userDetails
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>The author information in the JSON does not match the information from your user account.</p>" ++ concatMap mm mismatched
    mzero

  -- check that all signatories have first, last, and email
  when (not $ all ((3 ==) . length) [[v | v <- irData s
                                        , sfType v `elem` [FirstNameFT, LastNameFT, EmailFT]]
                                    | s <- dcrInvolved dcr]) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Minimum information not there for all signatories: " ++ show (dcrInvolved dcr)
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

    sendMailAPIErrorEmail ctx username $ "<p>All involved parties need <i>fstname</i>, <i>sndname</i>, and <i>email</i> entries in the JSON.</p>"
    mzero

  let doctype = dcrType dcr
      title = dcrTitle dcr
      actor = MailAPIActor ctxtime (userid user) (BS.toString $ getEmail user)
      
  edoc <- runDBUpdate $ NewDocument user mcompany (BS.fromString title) doctype actor

  when (isLeft edoc) $ do
    let Left msg = edoc

    Log.jsonMailAPI $ "Could not create document: " ++ msg

    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not create your document. I do not know what is wrong. You can try again or you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkUpload) ++ "\">click here</a> to use the web interface.</p>"

    mzero

  let Right doc = edoc

  content14 <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) pdfBinary
  file <- runDB $ dbUpdate $ NewFile (BS.fromString title) content14
  _ <- guardRightM $ runDBUpdate (AttachFile (documentid doc) (fileid file) actor)
  
  _ <- runDBUpdate $ SetDocumentFunctionality (documentid doc) AdvancedFunctionality actor
  _ <- runDBUpdate $ SetDocumentIdentification (documentid doc) [EmailIdentification] actor

  let signatories = for (dcrInvolved dcr) $ \InvolvedRequest{irRole,irData} ->
        (SignatoryDetails{signatorysignorder = SignOrder 0, signatoryfields = irData},
         irRole)

  errs <- lefts <$> (sequence $ [runDBUpdate $ ResetSignatoryDetails (documentid doc) signatories actor])

  when ([] /= errs) $ do
    Log.jsonMailAPI $ "Could not set up document: " ++ (intercalate "; " errs)
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what is wrong. I created it in Scrive, but I cannot get it ready to send. If you want to see your document, you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"
    mzero

  edoc2 <- runDBUpdate $ PreparationToPending (documentid doc) actor
  when (isLeft edoc2) $ do
    Log.jsonMailAPI $ "Could not got to pending document: " ++ (intercalate "; " errs)
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what's wrong. Your document is created and ready to be sent. To see your document and send it yourself, <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"
    mzero

  let Right doc2 = edoc2
      docid = documentid doc2
  markDocumentAuthorReadAndSeen doc2
  _ <- DocControl.postDocumentChangeAction doc2 doc Nothing

  _ <- runDBUpdate $ SetUserMailAPI (userid user) $ Just mailapi {
    umapiSentToday = umapiSentToday mailapi + 1
  }

  sendMailAPIConfirmEmail ctx doc2

  return $ show docid

getByAttachmentName :: String -> [(MIME.Type, BS.ByteString)] -> Maybe (MIME.Type, BS.ByteString)
getByAttachmentName name ps =
  find byname ps
    where byname p = case lookup "name" (MIME.mimeParams $ fst p) of
            Just n' -> name == decodeWords n'
            _       -> False
