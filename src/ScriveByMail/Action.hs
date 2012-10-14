module ScriveByMail.Action
    (
     doMailAPI
    ,parseEmailMessageToParts
    )

    where

import Company.Model
import Control.Logic
import DB
import Doc.DocStateData
import Doc.Rendering
import Doc.DocUtils
import Doc.JSON
import Doc.Model
import File.Model
import Kontra
import KontraLink
import MagicHash
import Mails.SendMail
import MinutesTime
import Utils.Either
import Utils.Prelude
import Utils.Read
import Redirect
import ScriveByMail.Model
import ScriveByMail.Parse
import ScriveByMail.View
import Stats.Control
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified Log (scrivebymail, scrivebymailfailure, mailAPI, jsonMailAPI)

import Codec.MIME.Decode
import qualified Codec.MIME.QuotedPrintable as PQ
import qualified Codec.MIME.Base64 as Base64
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.String.Utils
import System.FilePath
import Text.JSON
import Text.JSON.String
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Internal as BSI

checkThat :: String -> Bool -> Maybe String
checkThat s b = Nothing <| b |> Just s

charset :: MIME.Type -> String
charset mimetype = fromMaybe "us-ascii" $ lookup "charset" (MIME.mimeParams mimetype)

-- Some mail clients apparently don't surround the email address with <>, so treat
-- them separately
parseEmail :: String -> String
parseEmail from = if '<' `elem` from
                  then strip $ map toLower $ takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') from
                  else strip from


doMailAPI :: Kontrakcja m => BS.ByteString -> m (Maybe Document)
doMailAPI content = do
  let (mime, allParts) = parseEmailMessageToParts content

      isPDF (tp,_) = MIME.mimeType tp == MIME.Application "pdf" ||
                     (MIME.mimeType tp == MIME.Application "octet-stream" &&
                      maybe False (isSuffixOf ".pdf" . map toLower . decodeWordsMIME) (lookup "name" (MIME.mimeParams tp)))
      isPlain (tp,_) = MIME.mimeType tp == MIME.Text "plain"

      pdfs = filter isPDF allParts
      plains = filter isPlain allParts

      from = fromMaybe "" $ lookup "from" (MIME.mime_val_headers mime)
      to   = fromMaybe "" $ lookup "to"   (MIME.mime_val_headers mime)

      isOutlook = maybe False ("Outlook" `isInfixOf`) (lookup "x-mailer" (MIME.mime_val_headers mime)) ||
                  maybe False ("Exchange" `isInfixOf`) (lookup "x-mimeole" (MIME.mime_val_headers mime))

      subject = decodeWordsMIME $ BS.toString $ maybe BS.empty BS.fromString $ lookup "subject" (MIME.mime_val_headers mime)

  -- access control

  ctx <- getContext
  let username = parseEmail from
    -- 'extension' is a piece of data that is after + sign in email
    -- addres. example: api+1234@api.skrivapa.se here '1234' is
    -- extension and can be used as for example password
  let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') to

  muser <- dbQuery $ GetUserByEmail $ Email username

  minfo <- case muser of
    Just user -> do
      mmailapi <- dbQuery $ GetUserMailAPI $ userid user
      case mmailapi of
        Just mailapi | Just (umapiKey mailapi) == maybeRead extension -> do
          -- we have a user with matching mailapi key
          return $ Just (user, UserMailAPIInfo mailapi)
        _ -> case usercompany user of
               Just companyid -> do
                 mcmailapi <- dbQuery $ GetCompanyMailAPI companyid
                 case mcmailapi of
                   Just cmailapi | Just (umapiKey cmailapi) == maybeRead extension -> do
                     -- we have a user with company with matching company mailapi key
                     return $ Just (user, CompanyMailAPIInfo cmailapi)
                   _ -> do
                      Log.mailAPI $ "email from: " ++ from ++ " to " ++ to ++ "; User exists but key is wrong; Company exists but key is wrong; not sending error because of opportunity for abuse."
                      return Nothing
               _ -> do
                   Log.mailAPI $ "email from: " ++ from ++ " to " ++ to ++ "; User exists but key is wrong; Company does not exist; not sending error because of opportunity for abuse."
                   return Nothing
    _ ->  -- no user for this address
      case break (== '@') $ map toLower username of
        (_, '@':domain) -> do
          mcompany <- dbQuery $ GetCompanyByEmailDomain domain
          case mcompany of
            Just company -> do
              mcmailapi <- dbQuery $ GetCompanyMailAPI (companyid company)
              case mcmailapi of
                Just cmailapi | Just (umapiKey cmailapi) == maybeRead extension -> do
                  -- we have a non-user with company email domain with matching company mailapi key
                  -- we need to store the email, send a confirmation to the company admins, a message to the sender
                  mdelay <- dbUpdate $ AddMailAPIDelay username content (companyid company) (ctxtime ctx)
                  case mdelay of
                    Just (delayid, key, True) -> do
                      cusers <- dbQuery $ GetCompanyAccounts (companyid company)
                      forM_ (filter useriscompanyadmin cusers) $ \admin -> do
                        sendMailAPIDelayAdminEmail (getEmail admin) username delayid key (ctxtime ctx)
                      return ()
                    Just (_, _, False) -> return () -- no need to do anything; we already have sent an admin confirmation
                    Nothing -> do
                      Log.scrivebymail $ "Could not create delay; email: " ++ username ++ " companyid: " ++ show (companyid company)
                      return () -- this means we had an error
                  sendMailAPIDelayUserEmail username
                  return Nothing
                _ -> do
                  Log.mailAPI $ "email from: " ++ from ++ " to " ++ to ++ "; User does not exist; domain matches but key is wrong; not sending error because of opportunity for abuse."
                  return Nothing
            Nothing -> do
                  Log.mailAPI $ "email from: " ++ from ++ " to " ++ to ++ "; User does not exist; domain does not match; not sending error because of opportunity for abuse."
                  return Nothing
        _ -> do
          Log.mailAPI $ "email from: " ++ from ++ " to " ++ to ++ "; User does not exist; domain does not match a company: " ++ username ++ "; not sending error because of opportunity for abuse."
          return Nothing

  -- at this point, the user has been authenticated
  -- we can start sending him emails about errors

  -- also, we have enough to begin differentiating between Scrive by
  -- Mail and JSON api since the JSON mail api sends back different
  -- errors (in JSON format), we need to split it off here.

  case (minfo, plains) of
    (Just (user, mailapi), [p]) | isJSON p -> Just <$> jsonMailAPI mailapi username user pdfs plains content
    (Just (user, mailapi), _)              -> Just <$> scriveByMail mailapi username user to subject isOutlook pdfs plains content
    _                                      -> return Nothing

isJSON :: (MIME.Type, BS.ByteString) -> Bool
isJSON plain =
  case IConv.convertStrictly (charset (fst plain)) "UTF-8" (BSL.fromChunks [(snd plain)]) of
    Left r -> isPrefixOf "{" $ strip $ BS.toString $ BS.concat $ BSL.toChunks r
    _      -> False

{- |

   Parse and send a message using the simple human mail api.

   We have already authenticated the user.

-}
scriveByMail :: (Kontrakcja m) =>
                TaggedMailAPIInfo
                -> String
                -> User
                -> String
                -> String
                -> Bool
                -> [(MIME.Type, BS.ByteString)]
                -> [(MIME.Type, BS.ByteString)]
                -> BS.ByteString
                -> m Document
scriveByMail mailapi username user to subject isOutlook pdfs plains content = do
  ctx@Context{ctxtime} <- getContext

  let authorRole = (True, False)
      fullRole = (True, True)
      (doctype, arole) = caseOf [ ("contract" `isInfixOf` to, (Signable Contract, fullRole))
                                , ("offer"    `isInfixOf` to, (Signable Offer,    authorRole))
                                , ("order"    `isInfixOf` to, (Signable Order,    authorRole))
                                ]                             (Signable Contract, fullRole)

  when (umapiDailyLimit (unTagMailAPIInfo mailapi) <= umapiSentToday (unTagMailAPIInfo mailapi)) $ do
    Log.scrivebymail $ "Daily limit of documents for user '" ++ username ++ "' has been reached"

    sendMailAPIErrorEmail ctx username $ "<p>For your own protection, Scrive by Mail sets a daily limit on how many emails you can send out. Your daily Scrive by Mail limit has been reached. To reset your daily limit, please visit " ++ show LinkUserMailAPI ++ " .<p>"

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

  let recodedPlain = (replace "\r\n\r\n" "\r\n" $ BS.toString recodedPlain') <| isOutlook |> BS.toString recodedPlain'
      eparseresult = parseSimpleEmail subject recodedPlain

  when (isLeft eparseresult) $ do
    let Left msg = eparseresult

    Log.scrivebymail $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

    -- send error mail
    sendMailAPIErrorEmail ctx username $ "<p>I could not understand the email you sent me. Here are the things I did not understand: <br /><br />\n" ++ msg ++ "<br /><br />\nPlease correct the problems and try again.</p>"

  let Right (title, sigdets) = eparseresult

  let pdfBinary = snd pdf

  -- create document
  -- set to advanced
  -- add signatories
  -- send document

  let signatories = map (\p -> p { signatoryispartner = True }) sigdets

  mcompany <- case usercompany user of
    Just companyid -> dbQuery $ GetCompany companyid
    Nothing -> return Nothing

  let userDetails = signatoryDetailsFromUser user mcompany arole

  let actor = mailAPIActor ctxtime (userid user) (getEmail user)
  mdoc <- dbUpdate $ NewDocument user mcompany title doctype 0 actor

  when (isNothing mdoc) $ do

    -- send email saying sorry, there was some weird error
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not create your document. I do not know what is wrong. You can try again or you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkUpload) ++ "\">click here</a> to use the web interface.</p>"

  let Just doc = mdoc

  content14 <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) pdfBinary
  file <- dbUpdate $ NewFile title content14
  _ <- guardTrueM $ dbUpdate (AttachFile (documentid doc) (fileid file) actor)
  _ <- dbUpdate $ SetDocumentAuthenticationMethod (documentid doc) StandardAuthentication actor
  _ <- dbUpdate $ SetDocumentDeliveryMethod (documentid doc) EmailDelivery actor
  res <- (sequence $ [dbUpdate $ ResetSignatoryDetails (documentid doc) (userDetails:signatories) actor])

  when (not $ and res) $ do
    -- send sorry email
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what is wrong. I created it in Scrive, but I cannot get it ready to send. If you want to see your document, you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"

  is_pending <- dbUpdate $ PreparationToPending (documentid doc) actor
  when (not is_pending) $ do
    -- send sorry email
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what's wrong. Your document is created and ready to be sent. To see your document and send it yourself, <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"

  _ <- dbUpdate $ SetDocumentInviteTime (documentid doc) ctxtime actor
  -- if previous step succeeded, document must be in the database
  Just enddoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  _ <- addDocumentCreateStatEvents enddoc "mailapi+simple"
  --markDocumentAuthorReadAndSeen enddoc
  --_ <- DocControl.postDocumentChangeAction doc2 doc Nothing

  _ <- case (mailapi, usercompany user) of
    (CompanyMailAPIInfo _, Just companyid) -> dbUpdate $ IncrementCompanyMailAPI companyid
    (UserMailAPIInfo _, _) -> dbUpdate $ IncrementUserMailAPI (userid user)
    _ -> do
      Log.scrivebymail $ "Company API Key with user with no companyid?"
      return Nothing

  sendMailAPIConfirmEmail ctx enddoc

  return enddoc

sendMailAPIConfirmEmail :: Kontrakcja m => Context -> Document -> m ()
sendMailAPIConfirmEmail ctx document =
  case getAuthorSigLink document of
    Nothing -> error "No author in Document"
    Just authorsl -> do
      mail <- mailMailAPIConfirm ctx document authorsl
      scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress authorsl] }

-- | Roll back DB operations; commit an error email to DB; throw error.
sendMailAPIErrorEmail :: Kontrakcja m => Context -> String -> String -> m a
sendMailAPIErrorEmail ctx email msg = do
  dbRollback
  mail <- mailMailApiError ctx msg
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress email email] }
  dbCommit -- Needed because ActionControl will roll back on the error we'll throw
  internalError

sendMailAPIDelayAdminEmail :: Kontrakcja m => String -> String -> Int64 -> MagicHash -> MinutesTime -> m ()
sendMailAPIDelayAdminEmail adminemail email delayid key now = do
  ctx <- getContext
  mail <- mailMailApiDelayAdmin ctx adminemail email delayid key now
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress adminemail adminemail] }

sendMailAPIDelayUserEmail :: Kontrakcja m => String -> m ()
sendMailAPIDelayUserEmail email = do
  ctx  <- getContext
  mail <- mailMailApiDelayUser ctx email
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress email email] }

{- Removed because we should not be marking invitation and seen for author.
markDocumentAuthorReadAndSeen :: Kontrakcja m => Document -> m ()
markDocumentAuthorReadAndSeen doc@Document{documentid} = do
  let Just sl@SignatoryLink{signatorylinkid, signatorymagichash, maybesignatory} =
        getAuthorSigLink doc
  time <- ctxtime <$> getContext
  _ <- dbUpdate $ MarkInvitationRead documentid signatorylinkid
       (mailAPIActor time (fromJust maybesignatory) (getEmail sl))
  _ <- dbUpdate $ MarkDocumentSeen documentid signatorylinkid signatorymagichash
       (mailAPIActor time (fromJust maybesignatory) (getEmail sl))
  return ()
-}
parseEmailMessageToParts :: BS.ByteString -> (MIME.MIMEValue, [(MIME.Type, BS.ByteString)])
parseEmailMessageToParts content = (mime, parts mime)
  where
    mime = MIME.parseMIMEMessage (BSC.unpack content)
    parts mimevalue = case MIME.mime_val_content mimevalue of
        MIME.Single value -> [(MIME.mime_val_type mimevalue, BSC.pack value)]
        MIME.Multi more -> concatMap parts more

data TaggedMailAPIInfo = CompanyMailAPIInfo MailAPIInfo
                       | UserMailAPIInfo    MailAPIInfo

unTagMailAPIInfo :: TaggedMailAPIInfo -> MailAPIInfo
unTagMailAPIInfo (CompanyMailAPIInfo i) = i
unTagMailAPIInfo (   UserMailAPIInfo i) = i


{- |

   Parse and send email based on JSON.

   The user is already authenticated.

-}
jsonMailAPI :: (Kontrakcja m) =>
                TaggedMailAPIInfo
                -> String
                -> User
                -> [(MIME.Type, BS.ByteString)]
                -> [(MIME.Type, BS.ByteString)]
                -> BS.ByteString
                -> m Document
jsonMailAPI mailapi username user pdfs plains content = do
  ctx@Context{ctxtime} <- getContext
  when (umapiDailyLimit (unTagMailAPIInfo mailapi) <= umapiSentToday (unTagMailAPIInfo mailapi)) $ do
    Log.jsonMailAPI $ "Daily limit of documents for user '" ++ username ++ "' has been reached"
    sendMailAPIErrorEmail ctx username $ "<p>For your own protection, Scrive Mail API sets a daily limit on how many emails you can send out. Your daily Scrive Mail API limit has been reached. To reset your daily limit, please visit " ++ ctxhostpart ctx ++ show LinkUserMailAPI ++ " .</p>"
  when (length plains /= 1) $ do
    Log.jsonMailAPI $ "Wrong number of plain text attachments."
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>Please ensure that there is exactly one plain text attachment. In the message you sent, there were " ++ show (length plains) ++ ".</p>"

  let [plain] = plains

  recodedPlain' <- case IConv.convertStrictly (charset (fst plain)) "UTF-8" (BSL.fromChunks [(snd plain)]) of
    Left result' -> return $ BS.concat (BSL.toChunks (result'))
    Right errmsg -> do
      let msg = (show $ IConv.reportConversionError errmsg)

      Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
      Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

      sendMailAPIErrorEmail ctx username $ "<p>I do not know what the problem is. Perhaps try again with a different email program or contact <a href='mailto:eric@scrive.com'>Mail API Support</a> for help.</p>"
  let jsonString = BS.toString recodedPlain'
      
  -- some mail clients insert carriage returns + newlines wherever they want, even 
  -- in the middle of JSON strings! -- Eric
  let ejson = runGetJSON readJSValue $ replace "\n" "" $ replace "\r" "" jsonString

  when (isLeft ejson) $ do
    let Left msg = ejson

    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>I could not understand the email you sent me. The JSON did not parse: <br /><br />\n" ++ msg ++ "<br /><br />\nPlease correct the problems and try again.</p>"

  let Right json = ejson
      edcr = dcrFromJSON json

  when (isLeft edcr) $ do
    let Left msg = edcr

    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " " ++ msg
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>I could not understand the email you sent me. The JSON parsed, but there were problems: <br /><br />\n" ++ msg ++ "<br /><br />\nPlease correct the problems and try again.</p>"
    -- should probably send a bad request message

  let Right dcr = edcr

  -- we should check that the request makes sense

  -- exactly one author
  let aus = [a | a <- dcrInvolved dcr, irIsAuthor a]

  when (length aus /= 1) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Should have exactly one author; instead, has " ++ show aus
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>There should be exactly one author (role 1 or 2), but there were " ++ show (length aus) ++ ". Please correct the problem and try again.</p>"

  let [authorIR] = aus

  -- at least one signatory
  let sigs = length $ [p | p <- dcrInvolved dcr, irIsPartner p]
  when (1 > sigs) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Should have at least one signatory; instead, has " ++ show sigs
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    sendMailAPIErrorEmail ctx username $ "<p>There should be at least one signatory (role 2 or 5), but there were " ++ show sigs ++ ". Please correct the problem and try again.</p>"

  -- the mainfile is attached
  mpdf <- case (dcrMainFile dcr, pdfs) of
            (Just mainfilename, _) -> return $ getByAttachmentName (strip mainfilename) pdfs
            (Nothing, [pdf])       -> return $ Just pdf
            _                      -> return Nothing

  when (isNothing mpdf) $ do
    let attnames = catMaybes $ map (lookup "name" . MIME.mimeParams . fst) pdfs
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Missing pdf attachment: " ++ show (dcrMainFile dcr)
    Log.jsonMailAPI $ "Here are the attachments: " ++ show (map fst pdfs)
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content
    case dcrMainFile dcr of
      Just mainfilename ->
          sendMailAPIErrorEmail ctx username $ "<p>In the JSON, you requested the pdf called '" ++ mainfilename ++ "' to be considered the main file of the document. I could not find the pdf called '" ++ mainfilename ++ "' in the email attachments. The following pdfs were attached: '" ++ intercalate "', '" attnames ++ "'. Please check the file names and JSON and try again.</p>"
      Nothing | length pdfs == 0 ->
          sendMailAPIErrorEmail ctx username $ "<p>There were no PDFs attached to the document. Please attach a PDF that will be considered the main file of the document.</p>"
      _ ->
          sendMailAPIErrorEmail ctx username $ "<p>There were multiple PDFs attached to the document. In order to disambiguate them, please add a JSON parameter like this: <code>\"mainfile\" : { \"name\" : \"filename.pdf\" }</code> where filename.pdf is the name of the attached file. Alternatively, please only attach one PDF.</p>"

  let Just pdf = mpdf

  let pdfBinary = snd pdf

  -- create document
  -- set to advanced
  -- add signatories
  -- send document

  mcompany <- case usercompany user of
    Just companyid -> dbQuery $ GetCompany companyid
    Nothing -> return Nothing

  let userDetails = signatoryDetailsFromUser user mcompany (False, False)

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

  -- check that all signatories have first, last, and email
  when (not $ all ((3 ==) . length) [[v | v <- irData s
                                        , sfType v `elem` [FirstNameFT, LastNameFT, EmailFT]]
                                    | s <- dcrInvolved dcr]) $ do
    Log.jsonMailAPI $ (show $ toSeconds ctxtime) ++ " Minimum information not there for all signatories: " ++ show (dcrInvolved dcr)
    Log.scrivebymailfailure $ "\n####### "++ (show $ toSeconds ctxtime) ++ "\n" ++ BS.toString content

    sendMailAPIErrorEmail ctx username $ "<p>All involved parties need <i>fstname</i>, <i>sndname</i>, and <i>email</i> entries in the JSON.</p>"

  let doctype = dcrType dcr
      title = maybe (takeBaseName $ getAttachmentFilename $ fst pdf) decodeWordsMIME $ dcrTitle dcr
      actor = mailAPIActor ctxtime (userid user) (getEmail user)

  mdoc <- dbUpdate $ NewDocument user mcompany title doctype 0 actor

  when (isNothing mdoc) $ do
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not create your document. I do not know what is wrong. You can try again or you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkUpload) ++ "\">click here</a> to use the web interface.</p>"

  let Just doc = mdoc

  content14 <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) pdfBinary
  file <- dbUpdate $ NewFile title content14
  _ <- guardTrueM $ dbUpdate (AttachFile (documentid doc) (fileid file) actor)

  _ <- dbUpdate $ SetDocumentAuthenticationMethod (documentid doc) StandardAuthentication actor
  _ <- dbUpdate $ SetDocumentDeliveryMethod (documentid doc) EmailDelivery actor

  let signatories = for (dcrInvolved dcr) $ \InvolvedRequest{..} ->
        (SignatoryDetails {
            signatorysignorder = SignOrder 0
          , signatoryfields = irData
          , signatoryisauthor = irIsAuthor
          , signatoryispartner = irIsPartner
        })

  res <- (sequence $ [dbUpdate $ ResetSignatoryDetails (documentid doc) signatories actor])

  when (not $ and res) $ do
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what is wrong. I created it in Scrive, but I cannot get it ready to send. If you want to see your document, you can <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"

  is_pending <- dbUpdate $ PreparationToPending (documentid doc) actor
  when (not is_pending) $ do
    sendMailAPIErrorEmail ctx username $ "<p>I apologize, but I could not forward your document. I do not know what's wrong. Your document is created and ready to be sent. To see your document and send it yourself, <a href=\"" ++ ctxhostpart ctx ++ (show $ LinkIssueDoc (documentid doc)) ++ "\">click here</a>.</p>"

  _ <- dbUpdate $ SetDocumentInviteTime (documentid doc) ctxtime actor
  -- if previous step succeeded, document must be in the database
  Just enddoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  _ <- addDocumentCreateStatEvents enddoc "mailapi+json"
  --markDocumentAuthorReadAndSeen enddoc
  --_ <- DocControl.postDocumentChangeAction doc2 doc Nothing

  _ <- case (mailapi, usercompany user) of
    (CompanyMailAPIInfo _, Just companyid) -> dbUpdate $ IncrementCompanyMailAPI companyid
    (UserMailAPIInfo _, _) -> dbUpdate $ IncrementUserMailAPI (userid user)
    _ -> do
      Log.jsonMailAPI $ "Company API Key with user with no companyid?"
      return Nothing

  sendMailAPIConfirmEmail ctx enddoc

  return enddoc

getByAttachmentName :: String -> [(MIME.Type, BS.ByteString)] -> Maybe (MIME.Type, BS.ByteString)
getByAttachmentName name ps =
  find byname ps
    where byname p = case lookup "name" (MIME.mimeParams $ fst p) of
            Just n' -> name == decodeWordsMIME n'
            _       -> False

getAttachmentFilename :: MIME.Type -> String
getAttachmentFilename tp = case lookup "filename" (MIME.mimeParams tp) of
                             Just s -> decodeWordsMIME s
                             _ -> case lookup "name" (MIME.mimeParams tp) of
                                    Just s -> decodeWordsMIME s
                                    _ -> "document.pdf"


-- | This is a quickfix. MIME packege does not support UTF-8 encoding
decodeWordsMIME :: String -> String
decodeWordsMIME s =  if (utfString `isInfixOf` (toLower <$> s) || utfBString `isInfixOf` (toLower <$> s))
                        then unwords $ map decodeWordsMIMEUTF $ words s
                        else decodeWords s

decodeWordsMIMEUTF :: String -> String                        
decodeWordsMIMEUTF s = if (utfString `isPrefixOf` (toLower <$> s))
                          then BS.toString  $ BS.pack $ map BSI.c2w $ PQ.decode $ takeWhile ((/=) '?') $ drop (length utfString) s
                          else if (utfBString `isPrefixOf` (toLower <$> s))
                            then BS.toString  $ BS.pack $ map BSI.c2w $ Base64.decodeToString $ takeWhile ((/=) '?') $ drop (length utfString) s
                            else s

utfString :: String
utfString = "=?utf-8?q?"

utfBString :: String
utfBString = "=?utf-8?b?"
