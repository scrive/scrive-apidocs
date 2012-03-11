module ScriveByMail.Control 
       (
         scriveByMail,
         sendMailAPIConfirmEmail,
         sendMailAPIErrorEmail,
         markDocumentAuthorReadAndSeen
       )
       
       where

import File.Model
import Kontra
import KontraError (internalError)
--import Happstack.Server.Types 
import Doc.DocStorage
import Control.Monad.Trans
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
--import qualified Codec.MIME.Parse as MIME
--import qualified Codec.MIME.Type as MIME
--import Happstack.Server
import Misc
import Data.List
--import Codec.MIME.Decode
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
import Doc.Model
import Data.Either
import qualified Doc.DocControl as DocControl
import qualified Log (scrivebymail, scrivebymailfailure)
import ScriveByMail.View
import Util.SignatoryLinkUtils
import Mails.SendMail
import KontraLink
import qualified Codec.MIME.Type as MIME
import Redirect

import EvidenceLog.Model

--import Data.Char

import Control.Applicative

import Control.Monad

checkThat :: String -> Bool -> Maybe String
checkThat s b = Nothing <| b |> Just s

charset :: MIME.Type -> String
charset mimetype = fromMaybe "us-ascii" $ lookup "charset" (MIME.mimeParams mimetype)        

scriveByMail :: (Kontrakcja m) =>
                UserMailAPI
                -> String
                -> User
                -> String
                -> String
                -> Bool
                -> [(MIME.Type, BS.ByteString)]
                -> [(MIME.Type, BS.ByteString)]
                -> BS.ByteString
                -> m String
scriveByMail mailapi username user to subject isOutlook pdfs plains content = do
  ctx@Context{ctxtime} <- getContext
  -- at this point, the user has been authenticated
  -- we can start sending him emails about errors
  
  let (doctype, arole) = caseOf [ ("contract" `isInfixOf` to, (Signable Contract, [SignatoryAuthor, SignatoryPartner]))
                                , ("offer"    `isInfixOf` to, (Signable Offer,    [SignatoryAuthor]))
                                , ("order"    `isInfixOf` to, (Signable Order,    [SignatoryAuthor]))
                                ]                             (Signable Contract, [SignatoryAuthor, SignatoryPartner])
      
  when (umapiDailyLimit mailapi <= umapiSentToday mailapi) $ do
    Log.scrivebymail $ "Daily limit of documents for user '" ++ username ++ "' has been reached"
    
    sendMailAPIErrorEmail ctx username $ "<p>For your own protection, Scrive by Mail sets a daily limit on how many emails you can send out. Your daily Scrive by Mail limit has been reached. To reset your daily limit, please visit " ++ show LinkUserMailAPI ++ " .<p>"
    
    internalError
  
  --Log.scrivebymail $ "Types of mime parts: " ++ show typesOfParts
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
  
    internalError
  
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
      internalError
      
  let recodedPlain = (replace "\r\n\r\n" "\r\n" $ BS.toString recodedPlain') <| isOutlook |> BS.toString recodedPlain'
      eparseresult = parseSimpleEmail subject recodedPlain

  when (isLeft eparseresult) $ do
    let Left msg = eparseresult
    
    Log.scrivebymail $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    
    -- send error mail
    sendMailAPIErrorEmail ctx username $ "<p>I could not understand the email you sent me. Here are the things I did not understand: <br /><br />\n" ++ msg ++ "<br /><br />\nPlease correct the problems and try again.</p>"
    
    internalError
    
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

  let actor = MailAPIActor ctxtime (userid user) (getEmail user)
  edoc <- runDBUpdate $ NewDocument user mcompany title doctype 0 actor
  
  when (isLeft edoc) $ do
    let Left msg = edoc
    
    Log.scrivebymail $ "Could not create document: " ++ msg
    
    -- send email saying sorry, there was some weird error
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not create your document. I do not know what is wrong. You can try again or you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkUpload) ++ "\">click here</a> to use the web interface.</p>"
    
    internalError
    
  let Right doc = edoc
      
  content14 <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) pdfBinary
  file <- runDB $ dbUpdate $ NewFile title content14
  _ <- guardRightM $ runDBUpdate (AttachFile (documentid doc) (fileid file) actor)
  _ <- runDBUpdate $ SetDocumentFunctionality (documentid doc) AdvancedFunctionality actor
  _ <- runDBUpdate $ SetDocumentIdentification (documentid doc) [EmailIdentification] actor
  errs <- lefts <$> (sequence $ [runDBUpdate $ ResetSignatoryDetails (documentid doc) ((userDetails, arole):signatories) actor])
          
  when ([] /= errs) $ do
    Log.scrivebymail $ "Could not set up document: " ++ (intercalate "; " errs)
    
    -- send sorry email
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what is wrong. I created it in Scrive, but I cannot get it ready to send. If you want to see your document, you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"
    
    internalError
    
  edoc2 <- runDBUpdate $ PreparationToPending (documentid doc) actor           
  
  when (isLeft edoc2) $ do
    Log.scrivebymail $ "Could not got to pending document: " ++ (intercalate "; " errs)
    
    -- send sorry email
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what's wrong. Your document is created and ready to be sent. To see your document and send it yourself, <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"
    
    internalError
  
  let Right doc2 = edoc2
      docid = documentid doc2
  markDocumentAuthorReadAndSeen doc2
  _ <- DocControl.postDocumentChangeAction doc2 doc Nothing
    
  _ <- runDBUpdate $ SetUserMailAPI (userid user) $ Just mailapi {
    umapiSentToday = umapiSentToday mailapi + 1
  }

  sendMailAPIConfirmEmail ctx doc2

  return $ show docid

sendMailAPIConfirmEmail :: Kontrakcja m => Context -> Document -> m ()
sendMailAPIConfirmEmail ctx document =
  case getAuthorSigLink document of
    Nothing -> error "No author in Document"
    Just authorsl -> do
      mail <- mailMailAPIConfirm ctx document authorsl
      scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress authorsl] }

sendMailAPIErrorEmail :: Kontrakcja m => Context -> String -> String -> m ()
sendMailAPIErrorEmail ctx email msg = do
  mail <- mailMailApiError ctx msg
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress email email] }

markDocumentAuthorReadAndSeen :: Kontrakcja m => Document -> m ()
markDocumentAuthorReadAndSeen doc@Document{documentid} = do
  let Just sl@SignatoryLink{signatorylinkid, signatorymagichash, maybesignatory} =
        getAuthorSigLink doc
  time <- ctxtime <$> getContext
  _ <- runDBUpdate $ MarkInvitationRead documentid signatorylinkid 
       (MailAPIActor time (fromJust maybesignatory) (getEmail sl))
  _ <- runDBUpdate $ MarkDocumentSeen documentid signatorylinkid signatorymagichash 
       (MailAPIActor time (fromJust maybesignatory) (getEmail sl))
  return ()

