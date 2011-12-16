module ScriveByMail.Control 
       (
         handleScriveByMail,
         sendMailAPIConfirmEmail,
         parseEmailMessageToParts
       )
       
       where

import Kontra
import Happstack.Server.Types 
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Happstack.Server
import Misc
import Data.List
import Codec.MIME.Decode
import MinutesTime
import Data.String.Utils
import Data.Maybe
import qualified Codec.Text.IConv as IConv
import ScriveByMail.Parse
import Doc.DocStateData
import Util.HasSomeUserInfo
import User.Model
import DB.Classes
import Company.Model
import Doc.DocUtils
import Happstack.State hiding (extension)
import Doc.DocState
import Data.Either
import qualified Doc.DocControl as DocControl
import Templates.Templates
import qualified AppLogger as Log (scrivebymail, scrivebymailfailure)
import ScriveByMail.View
import Util.SignatoryLinkUtils
import Mails.MailsData
import KontraLink

import Control.Applicative

import Control.Monad

checkThat :: String -> Bool -> Maybe String
checkThat s b = Nothing <| b |> Just s

parseEmailMessageToParts :: BS.ByteString -> (MIME.MIMEValue, [(MIME.Type, BS.ByteString)])
parseEmailMessageToParts content = (mime, parts mime)
  where
    mime = MIME.parseMIMEMessage (BSC.unpack content)
    parts mimevalue = case MIME.mime_val_content mimevalue of
        MIME.Single value -> [(MIME.mime_val_type mimevalue, BSC.pack value)]
        MIME.Multi more -> concatMap parts more

handleScriveByMail :: Kontrakcja m => m String
handleScriveByMail = do
  Input contentspec _ _ <- getDataFnM (lookInput "mail")
  content <- concatChunks <$> case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
    
  ctx@Context{ctxtime, ctxipnumber} <- getContext
  
  let (mime, allParts) = parseEmailMessageToParts content
      
      isPDF (tp,_) = MIME.mimeType tp == MIME.Application "pdf" || MIME.mimeType tp == MIME.Application "octet-stream"
      isPlain (tp,_) = MIME.mimeType tp == MIME.Text "plain"
  
      typesOfParts = map fst allParts
      pdfs = filter isPDF allParts
      plains = filter isPlain allParts
  
      from = fromMaybe "" $ lookup "from" (MIME.mime_val_headers mime)
      to   = fromMaybe "" $ lookup "to"   (MIME.mime_val_headers mime)
  
      isOutlook = maybe False ("Outlook" `isInfixOf`) (lookup "x-mailer" (MIME.mime_val_headers mime)) ||
                  maybe False ("Exchange" `isInfixOf`) (lookup "x-mimeole" (MIME.mime_val_headers mime))
      
      subject = decodeWords $ BS.toString $ maybe BS.empty BS.fromString $ lookup "subject" (MIME.mime_val_headers mime)
      
      charset mimetype = maybe "us-ascii" id $ lookup "charset" (MIME.mimeParams mimetype)
      
  -- access control
  
  let username = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') from
    -- 'extension' is a piece of data that is after + sign in email
    -- addres. example: api+1234@api.skrivapa.se here '1234' is
    -- extension and can be used as for example password
  let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') to

  muser <- runDBQuery (GetUserByEmail Nothing (Email $ BS.fromString username))
  when (isNothing muser) $ do
    Log.scrivebymail $ "User does not exist: " ++ username
    mzero

  let Just user = muser
      
  mmailapi <- runDBQuery $ GetUserMailAPI $ userid user
  when (isNothing mmailapi) $ do
    Log.scrivebymail $ "User has not enabled api: " ++ username
    mzero
  
  let Just mailapi = mmailapi
  
  when (maybeRead extension /= Just (umapiKey mailapi)) $ do
    Log.scrivebymail $ "User api key does not match: " ++ username ++ " key: " ++ extension
    mzero
    
  -- at this point, the user has been authenticated
  -- we can start sending him emails about errors
  
  when (umapiDailyLimit mailapi <= umapiSentToday mailapi) $ do
    Log.scrivebymail $ "Daily limit of documents for user '" ++ username ++ "' has been reached"
    
    sendMailAPIErrorEmail ctx username $ "<p>For your own protection, Scrive by Mail sets a daily limit on how many emails you can send out. Your daily Scrive by Mail limit has been reached. To reset your daily limit, please visit " ++ show LinkUserMailAPI ++ " .<p>"
    
    mzero

  
  let (doctype, arole) = caseOf [ ("contract" `isInfixOf` to, (Signable Contract, [SignatoryAuthor, SignatoryPartner]))
                                , ("offer"    `isInfixOf` to, (Signable Offer,    [SignatoryAuthor]))
                                , ("order"    `isInfixOf` to, (Signable Order,    [SignatoryAuthor]))
                                ]                             (Signable Contract, [SignatoryAuthor, SignatoryPartner])
  
  Log.scrivebymail $ "Types of mime parts: " ++ show typesOfParts
  let errors1 = catMaybes [
        checkThat "Exactly one PDF should be attached."        $ length pdfs   == 1,
        checkThat "Exactly one text should be in the message." $ length plains == 1
        ]
                
  when ([] /= errors1) $ do
    let errorstring = intercalate "<br />\n" errors1
  
    Log.scrivebymail $ (show $ toSeconds ctxtime) ++ " " ++ errorstring
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
  
    -- send error email
    sendMailAPIErrorEmail ctx username $ "<p>Please ensure that the following condition is met: <br /> " ++ errorstring ++ "</p>"
  
    mzero
  
  let [plain] = plains
      [pdf]   = pdfs
      
  recodedPlain' <- case IConv.convertStrictly (charset (fst plain)) "UTF-8" (BSL.fromChunks [(snd plain)]) of
    Left result' -> return $ BS.concat (BSL.toChunks (result'))
    Right errmsg -> do
      let msg = (show $ IConv.reportConversionError errmsg)
      
      Log.scrivebymail $ (show $ toSeconds ctxtime) ++ " " ++ msg
      Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
          
      -- send error email          
      sendMailAPIErrorEmail ctx username $ "<p>I do not know what the problem is. Perhaps try again with a different email program.</p>"
      mzero
      
  let recodedPlain = (replace "\r\n\r\n" "\r\n" $ BS.toString recodedPlain') <| isOutlook |> BS.toString recodedPlain'
      eparseresult = parseSimpleEmail subject recodedPlain

  when (isLeft eparseresult) $ do
    let Left msg = eparseresult
    
    Log.scrivebymail $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    
    -- send error mail
    sendMailAPIErrorEmail ctx username $ "<p>I could not understand the email you sent me. Here are the things I did not understand: <br /><br />\n" ++ msg ++ "<br /><br />\nPlease correct the problems and try again.</p>"
    
    mzero
    
  let Right (title, sigdets) = eparseresult
  
  let pdfBinary = snd pdf

  -- create document
  -- set to advanced
  -- add signatories
  -- send document
  
  let signatories = map (\p -> (p, [SignatoryPartner])) sigdets
      
  mcompany <- case usercompany user of
    Just companyid -> runDBQuery $ GetCompany companyid
    Nothing -> return Nothing
  
  let userDetails = signatoryDetailsFromUser user mcompany

  edoc <- update $ NewDocument user mcompany (BS.fromString title) doctype ctxtime
  
  when (isLeft edoc) $ do
    let Left msg = edoc
    
    Log.scrivebymail $ "Could not create document: " ++ msg
    
    -- send email saying sorry, there was some weird error
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not create your document. I do not know what is wrong. You can try again or you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkUpload) ++ "\">click here</a> to use the web interface.</p>"
    
    mzero
    
  let Right doc = edoc
      
  _ <- DocControl.handleDocumentUploadNoLogin (documentid doc) pdfBinary (BS.fromString title)
  _ <- update $ SetDocumentAdvancedFunctionality (documentid doc) ctxtime
  _ <- update $ SetEmailIdentification (documentid doc) ctxtime
  
  errs <- lefts <$> (sequence $ [update $ ResetSignatoryDetails (documentid doc) ((userDetails, arole):signatories) ctxtime])
          
  when ([] /= errs) $ do
    Log.scrivebymail $ "Could not set up document: " ++ (intercalate "; " errs)
    
    -- send sorry email
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what is wrong. I created it in Scrive, but I cannot get it ready to send. If you want to see your document, you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"
    
    mzero

  edoc2 <- update $ PreparationToPending (documentid doc) ctxtime
  
  when (isLeft edoc2) $ do
    Log.scrivebymail $ "Could not got to pending document: " ++ (intercalate "; " errs)
    
    -- send sorry email
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what's wrong. Your document is created and ready to be sent. To see your document and send it yourself, <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"
    
    mzero
  
  let Right doc2 = edoc2
      docid = documentid doc2
  _ <- DocControl.markDocumentAuthorReadAndSeen doc2 ctxtime ctxipnumber
  _ <- DocControl.postDocumentChangeAction doc2 doc Nothing
    
  _ <- runDBUpdate $ SetUserMailAPI (userid user) $ Just mailapi {
    umapiSentToday = umapiSentToday mailapi + 1
  }

  sendMailAPIConfirmEmail ctx doc2

  return $ show docid

sendMailAPIConfirmEmail :: TemplatesMonad m => Context -> Document -> m ()
sendMailAPIConfirmEmail ctx document =
  case getAuthorSigLink document of
    Nothing -> error "No author in Document"
    Just authorsl -> do
      mail <- mailMailAPIConfirm ctx document authorsl
      scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress authorsl] }

sendMailAPIErrorEmail :: TemplatesMonad m => Context -> String -> String -> m ()
sendMailAPIErrorEmail ctx email msg = do
  mail <- mailMailApiError ctx msg
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress (BS.fromString email) (BS.fromString email)] }
