{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-orphans -fcontext-stack=50 -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP #-}

module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
  , isDeletableDocument -- fromUtils
  , anyInvitationUndelivered
  , undeliveredSignatoryLinks
  , insertDocumentAsIs
  , toDocumentProcess

  , AddDocumentAttachment(..)
  , AddInvitationEvidence(..)
  , AdminOnlySaveForUser(..)
  , ArchiveDocument(..)
  , AttachCSVUpload(..)
  , AttachFile(..)
  , AttachSealedFile(..)
  , CancelDocument(..)
  , ChangeMainfile(..)
  , ChangeSignatoryEmailWhenUndelivered(..)
  , CloseDocument(..)
  , DeleteSigAttachment(..)
  , DocumentFromSignatoryData(..)
  , ErrorDocument(..)
  , GetDeletedDocumentsByUser(..)
  , GetDocumentByDocumentID(..)
  , GetDocumentStats(..)
  , GetDocumentStatsByUser(..)
  , GetDocuments(..)
  , GetDocumentsByAuthor(..)
  , GetDocumentsByCompanyAndTags(..)
  , GetDocumentsBySignatory(..)
  , GetSignatoryLinkIDs(..)
  , GetTimeoutedButPendingDocuments(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , NewDocument(..)
  , PendingToAwaitingAuthor(..)
  , PreparationToPending(..)
  , ReallyDeleteDocument(..)
  , RejectDocument(..)
  , RemoveDaysToSign(..)
  , RemoveDocumentAttachment(..)
  -- , RemoveSignatoryCompany(..)
  -- , RemoveSignatoryUser(..)
  , ResetSignatoryDetails(..)
  , ResetSignatoryDetails2(..)
  , RestartDocument(..)
  , RestoreArchivedDocument(..)
  , SaveDocumentForUser(..)
  , SaveSigAttachment(..)
  , SetDaysToSign(..)
  , SetDocumentAdvancedFunctionality(..)
  , SetDocumentInviteTime(..)
  , SetDocumentLocale(..)
  , SetDocumentTags(..)
  , SetDocumentTimeoutTime(..)
  , SetDocumentTitle(..)
  , SetDocumentUI(..)
  , SetElegitimationIdentification(..)
  , SetEmailIdentification(..)
  , SetInvitationDeliveryStatus(..)
  , SetInviteText(..)
  -- , SetSignatoryCompany(..)
  -- , SetSignatoryUser(..)
  , SignDocument(..)
  , SignLinkFromDetailsForTest(..)
  , SignableFromDocumentIDWithUpdatedAuthor(..)
  , StoreDocumentForTesting(..)
  , TemplateFromDocument(..)
  , TimeoutDocument(..)
  , UpdateFields(..)
  , UpdateSigAttachments(..)
  , SetDocumentModificationData(..)
  -- , FixBug510ForDocument(..)
  --, MigrateDocumentSigAccounts(..)
  ) where

import API.Service.Model
import DB.Classes
--import DB.Derive
import DB.Fetcher
import DB.Types
import DB.Utils
import File.File
import File.FileID
--import User.Region
import Doc.DocUtils
import User.UserID
import User.Model
import Company.Model
import MinutesTime
import Doc.DocStateData
import Doc.Invariants
import Data.Data
import Database.HDBC
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe
import Misc
import Data.Convertible
import Data.List
import qualified Data.Map as Map
import Doc.Tables
import Control.Applicative
import Util.SignatoryLinkUtils
--import Doc.DocStateUtils
import Doc.DocProcess
import Doc.DocStateCommon
import qualified Log
import System.Random (randomIO)
--import Happstack.Server
--import Happstack.State
--import Happstack.Util.Common
--import Numeric
--import Data.Int
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Exception as E
import File.Model

import EvidenceLog.Model
import Util.HasSomeUserInfo

data SqlField = SqlField String String SqlValue

instance Convertible SqlValue SqlValue where
    safeConvert = return . id

sqlField :: (Convertible v SqlValue) => String -> v -> SqlField
sqlField name value = SqlField name "" (toSql value)

sqlFieldType :: (Convertible v SqlValue) => String -> String -> v -> SqlField
sqlFieldType name xtype value = SqlField name xtype (toSql value)

sqlLogAppend :: MinutesTime -> String -> SqlField
sqlLogAppend time text = sqlFieldType "log" "append" $ unlines [show $ DocumentLogEntry time $ BS.fromString text]

runInsertStatement :: String -> [SqlField] -> DB Integer
runInsertStatement tableName fields =
  runInsertStatementWhere tableName fields "" []


runInsertStatementWhere :: String -> [SqlField] -> String -> [SqlValue] -> DB Integer
runInsertStatementWhere tableName fields xwhere values = do
  (r,_s) <- runInsertStatementWhereReturning tableName fields xwhere values []
  return r

mkInsertStatementWhereReturning :: String -> [SqlField] -> String -> [SqlValue] -> [String] -> String
mkInsertStatementWhereReturning tableName fields xwhere _values returnFields =
   "INSERT INTO " ++ tableName ++
   " (" ++ concat (intersperse "," (map name fields)) ++ ")" ++
   " SELECT " ++ concat (intersperse "," (map xtype fields)) ++
   (case xwhere of
      [] -> ""
      _ -> " WHERE " ++ xwhere) ++
   (case returnFields of
      [] -> ""
      _ -> " RETURNING " ++ concat (intersperse ", " returnFields))
   where
     name (SqlField x _ _) = x
     xtype (SqlField _ x _) = insertType x
     insertType "" = "?"
     insertType "timestamp" = "?"
     insertType "base64" = "decode(?, 'base64')"
     insertType ytype = error $ "mkInsertStatement: invalid insert type " ++ ytype


runInsertStatementWhereReturning :: String -> [SqlField] -> String -> [SqlValue] -> [String] -> DB (Integer, Statement)
runInsertStatementWhereReturning tableName fields xwhere values returnFields = do
  wrapDB $ \conn -> (doit conn `E.catch` handle)

  where
    doit conn = do
      st <- prepare conn statement
      r <- execute st params
      return (r, st)
    handle e = E.throwIO $ SQLError { DB.Classes.originalQuery = statement
                                    , sqlError = e
                                    , queryParams = params
                                    }
    value (SqlField _ _ v) = [v]
    params = concatMap value fields ++ values
    statement = mkInsertStatementWhereReturning tableName fields xwhere values returnFields

-- here we can add encoding and better error reporting in case conversion fails
mkUpdateStatement :: String -> [SqlField] -> String
mkUpdateStatement tableName fields =
   "UPDATE " ++ tableName ++
   " SET " ++ concat (intersperse "," (map one fields)) ++ " "
   where
     name (SqlField x _ _) = x
     xtype (SqlField _ "" _) = "?"
     xtype (SqlField _ "timestamp" _) = "?"
     xtype (SqlField _ "base64" _) = "decode(?, 'base64')"
     xtype (SqlField name' "append" _) = name' ++ "||?"
     xtype (SqlField _ x _) = error $ "mkUpdateStatement: invalid insert type " ++ x
     one field = name field ++ "=" ++ xtype field


runUpdateStatement :: String -> [SqlField] -> String -> [SqlValue] -> DB Integer
runUpdateStatement tableName fields whereClause whereValues = do
  wrapDB $ \conn -> (run conn statement params) `E.catch`
        (\e -> liftIO $ E.throwIO $ SQLError { DB.Classes.originalQuery = statement
                                             , sqlError = e
                                             , queryParams = params
                                             })
  where
    value (SqlField _ _ v) = [v]
    params = concatMap value fields ++ whereValues
    statement = mkUpdateStatement tableName fields ++ whereClause


getOneDocumentAffected :: String -> Integer -> DocumentID -> DB (Either String Document)
getOneDocumentAffected text r did =
  case r of
    0 -> do
      return (Left (text ++ " did not affect any rows"))
    1 -> do
      mnewdoc <- dbQuery $ GetDocumentByDocumentID did
      case mnewdoc of
        Nothing -> return (Left ("Document #" ++ show did ++ " diappeared after " ++ text))
        Just newdoc -> return (Right newdoc)
    _ -> do
      -- here we really want to abort transaction, as we have affected more rows that we wanted
      -- something is seriously wrong!
      liftIO $ E.throwIO TooManyObjects { DB.Classes.originalQuery = ""
                                        , queryParams = []
                                        , tmoExpected = 1
                                        , tmoGiven = fromIntegral r
                                        }


toDocumentSimpleType :: DocumentType -> Int
toDocumentSimpleType (Signable _) = 1
toDocumentSimpleType (Template _) = 2
toDocumentSimpleType (Attachment) = 3
toDocumentSimpleType (AttachmentTemplate) = 4

toDocumentProcess :: DocumentType -> Maybe DocumentProcess
toDocumentProcess (Signable p) = Just p
toDocumentProcess (Template p) = Just p
toDocumentProcess (Attachment) = Nothing
toDocumentProcess (AttachmentTemplate) = Nothing

unimplemented :: String -> a
unimplemented msg = error ("Unimplemented in Doc/Model: " ++ msg)


checkEqualBy :: (Eq b, Show b) => String -> (a -> b) -> a -> a -> Maybe (String, String, String)
checkEqualBy name func obj1 obj2
  | func obj1 /= func obj2 = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

checkEqualByAllowSecondNothing :: (Eq b, Show b) => String -> (a -> Maybe b) -> a -> a -> Maybe (String, String, String)
checkEqualByAllowSecondNothing name func obj1 obj2
  | func obj1 /= func obj2 && (not (isNothing (func obj2))) = Just (name, show (func obj1), show (func obj2))
  | otherwise              = Nothing

assertEqualDocuments :: (Monad m, MonadIO m) => Document -> Document -> m ()
assertEqualDocuments d1 d2 | null inequalities = return ()
                           | otherwise = do
                                Log.debug message
                                error message
  where
    message = "Documents aren't equal in " ++ concat (map showInequality inequalities)
    showInequality (name,obj1,obj2) = name ++ ": \n" ++ obj1 ++ "\n" ++ obj2 ++ "\n"
    sl1 = sort $ documentsignatorylinks d1
    sl2 = sort $ documentsignatorylinks d2
    checkSigLink s1 s2 = map (\f -> f s1 s2)
                         [ checkEqualBy "signatorylinkid" signatorylinkid
                         , checkEqualBy "signatorydetails" signatorydetails
                         , checkEqualBy "signatorymagichash" signatorymagichash
                         , checkEqualByAllowSecondNothing "maybesignatory" maybesignatory
                         , checkEqualByAllowSecondNothing "maybesupervisor" maybesupervisor
                         , checkEqualBy "maybecompany" maybecompany
                         , checkEqualBy "maybesigninfo" maybesigninfo
                         , checkEqualBy "maybeseeninfo" maybeseeninfo
                         , checkEqualBy "maybereadinvite" maybereadinvite
                         , checkEqualBy "invitationdeliverystatus" invitationdeliverystatus
                         , checkEqualBy "signatorysignatureinfo" signatorysignatureinfo
                         , checkEqualBy "signatoryroles" (sort . signatoryroles)
                         , checkEqualBy "signatorylinkdeleted" signatorylinkdeleted
                         , checkEqualBy "signatorylinkreallydeleted" signatorylinkreallydeleted
                         , checkEqualBy "signatorylinkcsvupload" signatorylinkcsvupload
                         ]

    inequalities = catMaybes $ map (\f -> f d1 d2)
                   [ checkEqualBy "documentid" documentid
                   , checkEqualBy "documenttitle" documenttitle
                   --, checkEqualBy "documentfiles" documentfiles -- Mariusz: I skipped this test, since for migration I put drop files that are no avaible in database (broken)
                   , checkEqualBy "documentsealedfiles" documentsealedfiles
                   , checkEqualBy "documentstatus" documentstatus
                   , checkEqualBy "documenttype" documenttype
                   , checkEqualBy "documentfunctionality" documentfunctionality
                   , checkEqualBy "documentctime" documentctime
                   , checkEqualBy "documentmtime" documentmtime
                   , checkEqualBy "documentdaystosign" documentdaystosign
                   , checkEqualBy "documenttimeouttime" documenttimeouttime
                   , checkEqualBy "documentinvitetime" documentinvitetime
                   , checkEqualBy "documentlog" documentlog
                   , checkEqualBy "documentinvitetext" documentinvitetext
                   , checkEqualBy "documentallowedidtypes" (nub . documentallowedidtypes)
                   , checkEqualBy "documentcancelationreason" documentcancelationreason
                   , checkEqualBy "documentrejectioninfo" documentrejectioninfo
                   , checkEqualBy "documenttags" documenttags
                   , checkEqualBy "documentservice" documentservice
                   , checkEqualBy "documentdeleted" documentdeleted
                   , checkEqualBy "documentauthorattachments" documentauthorattachments
                   , checkEqualBy "documentsignatoryattachments" documentsignatoryattachments
                   , checkEqualBy "documentui" documentui
                   , checkEqualBy "documentregion" documentregion
                   , checkEqualBy "documentsignatorylinks count" (length . documentsignatorylinks)
                   ] ++
                   concat (zipWith checkSigLink sl1 sl2)



decodeRowAsDocument :: DocumentID
                    -> BS.ByteString
                    -> Maybe FileID
                    -> Maybe FileID
                    -> DocumentStatus
                    -> Maybe String
                    -> Int
                    -> Maybe DocumentProcess
                    -> DocumentFunctionality
                    -> MinutesTime
                    -> MinutesTime
                    -> Maybe Int
                    -> Maybe TimeoutTime
                    -> Maybe MinutesTime
                    -> Maybe IPAddress
                    -> [DocumentLogEntry]
                    -> BS.ByteString
                    -> [IdentificationType]
                    -> Maybe CancelationReason
                    -> Maybe MinutesTime
                    -> Maybe SignatoryLinkID
                    -> Maybe BS.ByteString
                    -> [DocumentTag]
                    -> Maybe ServiceID
                    -> Bool
                    -> Maybe BS.ByteString
                    -> Region
                    -> Either DBException Document
decodeRowAsDocument did
                    title
                    file_id
                    sealed_file_id
                    status
                    error_text
                    simple_type
                    process
                    functionality
                    ctime
                    mtime
                    days_to_sign
                    timeout_time
                    invite_time
                    invite_ip
                    dlog
                    invite_text
                    allowed_id_types
                    cancelationreason
                    rejection_time
                    rejection_signatory_link_id
                    rejection_reason
                    tags
                    service
                    deleted
                    --authorattachments
                    --signatoryattachments
                    mail_footer
                    region = (Right $ Document { documentid = did
                                               , documenttitle = title
                                               , documentsignatorylinks = []
                                               , documentfiles = maybeToList file_id
                                               , documentsealedfiles = maybeToList sealed_file_id
                                               , documentstatus = case (status, error_text) of
                                                                    (DocumentError{}, Just text) -> DocumentError text
                                                                    (DocumentError{}, Nothing) -> DocumentError "document error"
                                                                    _ -> status
                                               , documenttype = case (simple_type, process) of
                                                                  (1, Just p) -> Signable p
                                                                  (2, Just p) -> Template p
                                                                  (3, _) -> Attachment
                                                                  (4, _) -> AttachmentTemplate
                                                                  (_,_) -> error "Illegal simpletype"
                                               , documentfunctionality = functionality
                                               , documentctime = ctime
                                               , documentmtime = mtime
                                               , documentdaystosign = days_to_sign
                                               , documenttimeouttime = timeout_time
                                               , documentinvitetime = case invite_time of
                                                                        Nothing -> Nothing
                                                                        Just t -> Just (SignInfo t (fromMaybe unknownIPAddress invite_ip))
                                               , documentlog = dlog
                                               , documentinvitetext = invite_text
                                               , documentallowedidtypes = allowed_id_types
                                               , documentcancelationreason = cancelationreason
                                               , documentsharing = Private
                                               , documentrejectioninfo = case (rejection_time, rejection_signatory_link_id, rejection_reason) of
                                                                           (Just t, Just sl, mr) -> Just (t, sl, fromMaybe BS.empty mr)
                                                                           _ -> Nothing
                                               , documenttags = tags
                                               , documentservice = service
                                               , documentdeleted = deleted
                                               , documentauthorattachments = []
                                               , documentsignatoryattachments = []
                                               , documentui = DocumentUI mail_footer
                                               , documentregion = region
                                               }) :: Either DBException Document

selectDocumentsSelectors :: [String]
selectDocumentsSelectors = [ "id"
                           , "title"
                           , "file_id"
                           , "sealed_file_id"
                           , "status"
                           , "error_text"
                           , "type"
                           , "process"
                           , "functionality"
                           , "ctime"
                           , "mtime"
                           , "days_to_sign"
                           , "timeout_time"
                           , "invite_time"
                           , "invite_ip"
                           , "log"
                           , "invite_text"
                           , "allowed_id_types"
                           , "cancelation_reason"
                           , "rejection_time"
                           , "rejection_signatory_link_id"
                           , "rejection_reason"
                           , "tags"
                           , "service_id"
                           , "deleted"
                           --authorattachments
                           --signatoryattachments
                           , "mail_footer"
                           , "region"
                           ]

selectDocumentsSQL :: String
selectDocumentsSQL = "SELECT " ++ concat (intersperse "," selectDocumentsSelectors) ++ " FROM documents "

fetchDocuments :: Statement -> IO [Document]
fetchDocuments st = do
  fetchValues st decodeRowAsDocument


selectSignatoryLinksSelectors :: [String]
selectSignatoryLinksSelectors = [ "id"
                                , "document_id"
                                , "user_id"
                                , "company_id"
                                , "fields"
                                , "sign_order"
                                , "token"
                                , "sign_time"
                                , "sign_ip"
                                , "seen_time"
                                , "seen_ip"
                                , "read_invitation"
                                , "invitation_delivery_status"
                                , "signinfo_text"
                                , "signinfo_signature"
                                , "signinfo_certificate"
                                , "signinfo_provider"
                                , "signinfo_first_name_verified"
                                , "signinfo_last_name_verified"
                                , "signinfo_personal_number_verified"
                                , "roles"
                                , "csv_title"
                                , "csv_contents"
                                , "csv_signatory_index"
                                , "deleted"
                                , "really_deleted"
                                ]

selectSignatoryLinksSQL :: String
selectSignatoryLinksSQL = "SELECT " ++ concat (intersperse "," selectSignatoryLinksSelectors) ++ " FROM signatory_links "

decodeRowAsSignatoryLinkWithDocumentID :: SignatoryLinkID
                         -> DocumentID
                         -> Maybe UserID
                         -> Maybe CompanyID
                         -> [SignatoryField]
                         -> SignOrder
                         -> MagicHash
                         -> Maybe MinutesTime
                         -> Maybe IPAddress
                         -> Maybe MinutesTime
                         -> Maybe IPAddress
                         -> Maybe MinutesTime
                         -> MailsDeliveryStatus
                         -> Maybe String
                         -> Maybe String
                         -> Maybe String
                         -> Maybe SignatureProvider
                         -> Maybe Bool
                         -> Maybe Bool
                         -> Maybe Bool
                         -> [SignatoryRole]
                         -> Maybe BS.ByteString
                         -> Maybe [[BS.ByteString]]
                         -> Maybe Int
                         -> Bool
                         -> Bool
                         -> Either DBException (DocumentID,SignatoryLink)
decodeRowAsSignatoryLinkWithDocumentID slid
                         document_id
                         user_id
                         company_id
                         fields
                         sign_order
                         token
                         sign_time
                         sign_ip
                         seen_time
                         seen_ip
                         read_invitation
                         invitation_delivery_status
                         signinfo_text
                         signinfo_signature
                         signinfo_certificate
                         signinfo_provider
                         signinfo_first_name_verified
                         signinfo_last_name_verified
                         signinfo_personal_number_verified
                         roles
                         csv_title
                         csv_contents
                         csv_signatory_index
                         deleted
                         really_deleted =
    (return $ (document_id,SignatoryLink
    { signatorylinkid = slid
    , signatorydetails = SignatoryDetails
                         { signatorysignorder = sign_order
                         , signatoryfields = fields
                         }
    , signatorymagichash = token
    , maybesignatory     = user_id
    , maybesupervisor    = Nothing
    , maybecompany       = company_id
    , maybesigninfo      = case (sign_time, sign_ip) of
                             (Just st, Just sip) -> Just (SignInfo st sip)
                             _ -> Nothing
    , maybeseeninfo      = case (seen_time, seen_ip) of
                             (Just st, Just sip) -> Just (SignInfo st sip)
                             _ -> Nothing
    , maybereadinvite    = read_invitation
    , invitationdeliverystatus = invitation_delivery_status
    , signatorysignatureinfo = do -- Maybe Monad
        signinfo_text' <- signinfo_text
        signinfo_signature' <- signinfo_signature
        signinfo_certificate' <- signinfo_certificate
        signinfo_provider' <- signinfo_provider
        signinfo_first_name_verified' <- signinfo_first_name_verified
        signinfo_last_name_verified' <- signinfo_last_name_verified
        signinfo_personal_number_verified' <- signinfo_personal_number_verified
        return $ SignatureInfo { signatureinfotext        = signinfo_text'
                               , signatureinfosignature   = signinfo_signature'
                               , signatureinfocertificate = signinfo_certificate'
                               , signatureinfoprovider    = signinfo_provider'
                               , signaturefstnameverified = signinfo_first_name_verified'
                               , signaturelstnameverified = signinfo_last_name_verified'
                               , signaturepersnumverified = signinfo_personal_number_verified'
                               }

    , signatoryroles     = roles
    , signatorylinkdeleted  = deleted
    , signatorylinkreallydeleted = really_deleted
    , signatorylinkcsvupload =
      case (csv_title, csv_contents, csv_signatory_index) of
        (Just t, Just c, Just si) -> Just (CSVUpload t c si)
        _ -> Nothing
    })) :: Either DBException (DocumentID,SignatoryLink)

fetchSignatoryLinks :: Statement -> IO [SignatoryLink]
fetchSignatoryLinks st = do
  values <- fetchValues st decodeRowAsSignatoryLinkWithDocumentID
  return (map snd values)

fetchSignatoryLinksWithDocuments :: Statement -> IO [(DocumentID,SignatoryLink)]
fetchSignatoryLinksWithDocuments st = do
  fetchValues st decodeRowAsSignatoryLinkWithDocumentID

insertSignatoryLinkAsIs :: DocumentID -> SignatoryLink -> DB (Maybe SignatoryLink)
insertSignatoryLinkAsIs documentid link = do
  ruserid <- case maybesignatory link of
    Nothing -> return Nothing
    Just userid1 -> do
      muser <- dbQuery $ GetUserByID userid1
      case muser of
        Nothing ->
          do
            Just doc <- dbQuery $ GetDocumentByDocumentID documentid
            Log.server $ "User " ++ show (maybesignatory link) ++ " of document #" ++
               show documentid ++ " '" ++ BS.toString (documenttitle doc) ++ "' does not exist, setting to NULL"
            return Nothing
        Just _ -> return (Just userid1)

  --liftIO $ print link
  (_, st) <- runInsertStatementWhereReturning "signatory_links"
                            [ sqlField "id" $ signatorylinkid link
                            , sqlField "document_id" documentid
                            , sqlField "user_id" $ ruserid
                            , sqlField "roles" $ signatoryroles link
                            , sqlField "company_id" $ maybecompany link
                            , sqlField "token" $ signatorymagichash link
                            , sqlField "fields" $ signatoryfields $ signatorydetails link
                            , sqlField "sign_order"$ signatorysignorder $ signatorydetails link
                            , sqlField "sign_time" $ signtime `fmap` maybesigninfo link
                            , sqlField "sign_ip" $ signipnumber `fmap` maybesigninfo link
                            , sqlField "seen_time" $ signtime `fmap` maybeseeninfo link
                            , sqlField "seen_ip" $ signipnumber `fmap` maybeseeninfo link
                            , sqlField "read_invitation" $ maybereadinvite link
                            , sqlField "invitation_delivery_status" $ invitationdeliverystatus link
                            , sqlField "signinfo_text" $ signatureinfotext `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_signature" $ signatureinfosignature `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_certificate" $ signatureinfocertificate `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_provider" $ signatureinfoprovider `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_first_name_verified" $ signaturefstnameverified `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_last_name_verified" $ signaturelstnameverified `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_personal_number_verified" $ signaturepersnumverified `fmap` signatorysignatureinfo link
                            , sqlField "csv_title" $ csvtitle `fmap` signatorylinkcsvupload link
                            , sqlField "csv_contents" $ csvcontents `fmap` signatorylinkcsvupload link
                            , sqlField "csv_signatory_index" $ csvsignatoryindex `fmap` signatorylinkcsvupload link
                            , sqlField "deleted" $ signatorylinkdeleted link
                            , sqlField "really_deleted" $ signatorylinkreallydeleted link
                            ]
                            "NOT EXISTS (SELECT * FROM signatory_links WHERE id = ? AND document_id = ?)"
                            [ toSql (signatorylinkid link), toSql documentid ]
                            selectSignatoryLinksSelectors

  links <- liftIO $ fetchSignatoryLinks st

  case links of
    [newlink] -> return (Just newlink)
    [] -> return Nothing
    _ -> return Nothing -- should throw an exception probably

selectAuthorAttachmentsSelectors :: [String]
selectAuthorAttachmentsSelectors = [ "document_id"
                                   , "file_id"
                                   ]

selectAuthorAttachmentsSQL :: String
selectAuthorAttachmentsSQL = "SELECT " ++ concat (intersperse "," selectAuthorAttachmentsSelectors) ++ " FROM author_attachments "


decodeRowAsAuthorAttachment :: DocumentID
                            -> FileID
                            -> Either DBException (DocumentID, AuthorAttachment)
decodeRowAsAuthorAttachment document_id
                            file_id =
  return ( document_id
         , AuthorAttachment
           { authorattachmentfile = file_id
           })

fetchAuthorAttachmentsWithDocumentID :: Statement -> IO [(DocumentID,AuthorAttachment)]
fetchAuthorAttachmentsWithDocumentID st = do
  fetchValues st decodeRowAsAuthorAttachment

fetchAuthorAttachments :: Statement -> IO [AuthorAttachment]
fetchAuthorAttachments st = do
  values <- fetchAuthorAttachmentsWithDocumentID st
  return (map snd values)

insertAuthorAttachmentAsIs :: DocumentID -> AuthorAttachment -> DB (Maybe AuthorAttachment)
insertAuthorAttachmentAsIs documentid attach = do
  (_, st) <- runInsertStatementWhereReturning "author_attachments"
                            [ sqlField "file_id" $ authorattachmentfile attach
                            , sqlField "document_id" documentid
                            ]
                            "NOT EXISTS (SELECT * FROM author_attachments WHERE file_id = ? AND document_id = ?)"
                            [ toSql (authorattachmentfile attach), toSql documentid ]
                            selectAuthorAttachmentsSelectors

  links <- liftIO $ fetchAuthorAttachments st

  case links of
    [newattach] -> return (Just newattach)
    [] -> return Nothing
    _ -> return Nothing -- should throw an exception probably



selectSignatoryAttachmentsSelectors :: [String]
selectSignatoryAttachmentsSelectors = [ "document_id"
                                      , "file_id"
                                      , "email"
                                      , "name"
                                      , "description"
                                      ]

selectSignatoryAttachmentsSQL :: String
selectSignatoryAttachmentsSQL = "SELECT " ++ concat (intersperse "," selectAuthorAttachmentsSelectors) ++ " FROM author_attachments "


decodeRowAsSignatoryAttachment :: DocumentID
                               -> Maybe FileID
                               -> BS.ByteString
                               -> BS.ByteString
                               -> BS.ByteString
                               -> Either DBException (DocumentID, SignatoryAttachment)
decodeRowAsSignatoryAttachment document_id
                               file_id
                               email
                               name
                               description =
   return ( document_id
          , SignatoryAttachment { signatoryattachmentfile = file_id
                                , signatoryattachmentemail = email
                                , signatoryattachmentname = name
                                , signatoryattachmentdescription = description
                                })

fetchSignatoryAttachmentsWithDocumentID :: Statement -> IO [(DocumentID,SignatoryAttachment)]
fetchSignatoryAttachmentsWithDocumentID st = do
  fetchValues st decodeRowAsSignatoryAttachment

fetchSignatoryAttachments :: Statement -> IO [SignatoryAttachment]
fetchSignatoryAttachments st = do
  values <- fetchValues st decodeRowAsSignatoryAttachment
  return (map snd values)

insertSignatoryAttachmentAsIs :: DocumentID -> SignatoryAttachment -> DB (Maybe SignatoryAttachment)
insertSignatoryAttachmentAsIs documentid attach = do
  (_, st) <- runInsertStatementWhereReturning "signatory_attachments"
                            [ sqlField "file_id" $ signatoryattachmentfile attach
                            , sqlField "document_id" $ documentid
                            , sqlField "email" $ signatoryattachmentemail attach
                            , sqlField "name" $ signatoryattachmentname attach
                            , sqlField "description" $ signatoryattachmentdescription attach
                            ]
                            "NOT EXISTS (SELECT * FROM signatory_attachments WHERE file_id = ? AND email = ?)"
                            [ toSql $ signatoryattachmentfile attach
                            , toSql $ signatoryattachmentemail attach
                            ]
                            selectSignatoryAttachmentsSelectors

  links <- liftIO $ fetchSignatoryAttachments st

  case links of
    [newattach] -> return (Just newattach)
    [] -> return Nothing
    _ -> return Nothing -- should throw an exception probably


insertDocumentAsIs :: Document -> DB (Maybe Document)
insertDocumentAsIs document = do
    let Document { documentid
                 , documenttitle
                 , documentsignatorylinks
                 , documentfiles
                 , documentsealedfiles
                 , documentstatus
                 , documenttype
                 , documentfunctionality
                 , documentctime
                 , documentmtime
                 , documentdaystosign
                 , documenttimeouttime
                 , documentinvitetime
                 , documentlog
                 , documentinvitetext
                 , documentallowedidtypes
                 , documentcancelationreason
                 , documentrejectioninfo
                 , documenttags
                 , documentservice
                 , documentdeleted
                 , documentauthorattachments
                 , documentsignatoryattachments
                 , documentui
                 , documentregion
                 } = document
        simpletype = toDocumentSimpleType documenttype
        process = toDocumentProcess documenttype
    files <-  sequence $ map (dbQuery . GetFileByFileID)  documentfiles
    let fileLost = (length $ concatMap maybeToList files) <  length documentfiles
    when (fileLost) $
        Log.error $ "!!!!MIGRATION WARN: Document  " ++ (show documentid) ++ " has files ("++ show documentfiles ++ "), but they are not in database. FileID will be dropped."
            ++ "Document was created "++ show documentctime
    (_,st) <- runInsertStatementWhereReturning "documents"
                                     [ sqlField "id" documentid
                                     , sqlField "title" documenttitle
                                     , sqlField "tags" documenttags
                                     , sqlField "file_id" $ Nothing<| fileLost |> (listToMaybe documentfiles)
                                     , sqlField "sealed_file_id" (listToMaybe documentsealedfiles)
                                     , sqlField "status" documentstatus
                                     , sqlField "error_text" $ case documentstatus of
                                                                 DocumentError msg -> toSql msg
                                                                 _ -> SqlNull
                                     , sqlField "type" simpletype
                                     , sqlField "process" process

                                     , sqlField "functionality" documentfunctionality
                                     , sqlField "ctime" documentctime
                                     , sqlField "mtime" documentmtime
                                     , sqlField "days_to_sign" documentdaystosign
                                     , sqlField "timeout_time" documenttimeouttime
                                     , sqlField "invite_time" $ signtime `fmap` documentinvitetime
                                     , sqlField "invite_ip" (fmap signipnumber documentinvitetime)
                                     , sqlField "invite_text" documentinvitetext
                                     , sqlField "log" documentlog
                                     , sqlField "allowed_id_types" documentallowedidtypes
                                     , sqlField "cancelation_reason" documentcancelationreason
                                     , sqlField "rejection_time" $ fst3 `fmap` documentrejectioninfo
                                     , sqlField "rejection_signatory_link_id" $ snd3 `fmap` documentrejectioninfo
                                     , sqlField "rejection_reason" $ thd3 `fmap` documentrejectioninfo
                                     , sqlField "service_id" documentservice
                                     , sqlField "deleted" documentdeleted
                                     -- , toSql documentauthorattachments      -- many to many
                                     -- , toSql documentsignatoryattachments   -- many to many
                                     , sqlField "mail_footer" $ documentmailfooter $ documentui  -- should go into separate table?
                                     , sqlField "region" documentregion
                                     , sqlField "sharing" Private -- this is unused, but does not have default and needs to be specifed here
                                     ]
                                     "NOT EXISTS (SELECT * FROM documents WHERE id = ?)"
                                     [toSql documentid]
                                     selectDocumentsSelectors

    docs <- liftIO $ fetchDocuments st

    case docs of
      [] -> return Nothing
      [doc] -> do
        mlinks <- mapM (insertSignatoryLinkAsIs documentid) documentsignatorylinks
        mauthorattachments <- mapM (insertAuthorAttachmentAsIs documentid) documentauthorattachments
        msignatoryattachments <- mapM (insertSignatoryAttachmentAsIs documentid) documentsignatoryattachments
        if any isNothing mlinks || any isNothing mauthorattachments || any isNothing msignatoryattachments
         then return Nothing
         else do
          let newdocument = doc { documentsignatorylinks = catMaybes mlinks
                                , documentauthorattachments = catMaybes mauthorattachments
                                , documentsignatoryattachments = catMaybes msignatoryattachments
                                }
          assertEqualDocuments document newdocument
          return (Just newdocument)
      _ -> error "XXX"

insertNewDocument :: Document -> DB Document
insertNewDocument doc = do
  wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
  docid <- DocumentID <$> getUniqueID tableDocuments
  now <- liftIO $ getMinutesTime
  let docWithId = doc {documentid = docid, documentmtime  = now, documentctime = now}
  newdoc <- insertDocumentAsIs docWithId
  case newdoc of
    Just d -> return d
    Nothing -> error "insertNewDocument failed for some reason"


-- Create new document based on existing one
newFromDocument :: (Document -> Document) -> DocumentID -> DB (Either String Document)
newFromDocument f docid = do
  mdoc <- dbQuery $ GetDocumentByDocumentID docid
  case mdoc of
      Just doc -> fmap Right $ insertNewDocument $ f doc
      Nothing -> return $ Left $ "Document " ++ show docid ++ " does not exist"

{- |
    The existance of this function is wrong.  What it means is that storing
    maybesignatory and maybecompany on the signatory links is the wrong way of doing it,
    and there should be something else for hooking accounts to sig links that doesn't
    involve editing all the docs as a user moves between private and company accounts.
-}
data (Actor a, Show a, Eq a, Ord a) => AdminOnlySaveForUser a = AdminOnlySaveForUser DocumentID User a
                                                              deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (AdminOnlySaveForUser a) (Either String Document) where
  dbUpdate (AdminOnlySaveForUser did user actor) = do
    r <- runUpdateStatement "signatory_links"
                       [ sqlField "company_id" $ usercompany user
                       ]
                       ("WHERE document_id = ? " ++
                        " AND user_id = ? ")
                       [ toSql did
                       , toSql (userid user)
                       ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      AdminOnlySaveForUserEvidence
      ("Document saved for user with email " ++ show (getEmail user) ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "AdminOnlySaveForUser" r did

data (Actor a, Show a, Eq a, Ord a) => ArchiveDocument a = ArchiveDocument User DocumentID a
                                                         deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (ArchiveDocument a) (Either String Document) where
  dbUpdate (ArchiveDocument user did actor) = do
    r <- case (usercompany user, useriscompanyadmin user) of
           (Just cid, True) ->
              runUpdateOnArchivableDoc "WHERE company_id = ?" [toSql cid]
           _ ->
              runUpdateOnArchivableDoc "WHERE user_id = ?" [toSql $ userid user]
    -- a supervisor could delete both their own and another subaccount's links
    -- on the same document, so this would mean the sig link count affected
    -- is more than 1. see bug 1195.
    let fudgedr = if r==0 then 0 else 1

    let forstr = case (usercompany user, useriscompanyadmin user) of
          (Just cid, True) -> "company with admin email " ++ show (getEmail user)
          _ -> "user with email " ++ show (getEmail user)
    when (fudgedr == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      ArchiveDocumentEvidence
      ("Moved document to rubbish bin for " ++ forstr ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "ArchiveDocument" fudgedr did

    where
      runUpdateOnArchivableDoc whereClause whereFields =
        runUpdateStatement "signatory_links"
                          [sqlField "deleted" True]
                          (whereClause ++ " AND document_id = ? AND EXISTS (SELECT * FROM documents WHERE id = ? AND status <> ? AND status <> ?)")
                          (whereFields ++
                              [ toSql did
                              , toSql did
                              , toSql Pending
                              , toSql AwaitingAuthor
                              ])


data (Actor a, Show a, Eq a, Ord a) => AttachCSVUpload a = AttachCSVUpload DocumentID SignatoryLinkID CSVUpload a
                       deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (AttachCSVUpload a) (Either String Document) where
  dbUpdate (AttachCSVUpload did slid csvupload actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot AttachCSVUpload document " ++ show did ++ " because it does not exist"
      Just document -> do
        case documentstatus document of
          Preparation -> do
                     r <- runUpdateStatement "signatory_links"
                          [ {- sqlField "mtime" time
                          , -} sqlField "csv_title" $ csvtitle csvupload
                          , sqlField "csv_signatory_index" $ csvsignatoryindex csvupload
                          , sqlField "csv_contents" $ csvcontents csvupload
                          ]
                         "WHERE document_id = ? AND signatory_links.id = ? AND deleted = FALSE AND ((roles & ?)=0)"
                         [ toSql did
                         , toSql slid
                         , toSql [SignatoryAuthor]
                         ]
                     when (r == 1) $
                       ignore $ dbUpdate $ InsertEvidenceEvent
                       AttachCSVUploadEvidence
                       ("Attached CSV (" ++ show (csvtitle csvupload) ++ ") to document by " ++ actorWho actor ++ ".")
                       (Just did)
                       actor
                     getOneDocumentAffected "AttachCSVUpload" r did

          _ -> return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be Preparation"

data (Actor a, Show a, Eq a, Ord a) => AttachFile a = AttachFile DocumentID FileID a
                  deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (AttachFile a) (Either String Document) where
  dbUpdate (AttachFile did fid a) = do
    let time = actorTime a
    r <- runUpdateStatement "documents"
         [ sqlField "mtime" time
         , sqlField "file_id" fid
         , sqlLogAppend time ("Attached main file " ++ show fid)
         ]
         "WHERE id = ? AND status = ?" [ toSql did, toSql Preparation ]
    when (r == 1) $ do
      ignore $ dbUpdate $ InsertEvidenceEvent
        AttachFileEvidence
        ("Uploaded main PDF by " ++ actorWho a ++ ".")
        (Just did)
        a
    getOneDocumentAffected "AttachFile" r did

data (Actor a, Show a, Eq a, Ord a) => AttachSealedFile a = AttachSealedFile DocumentID FileID a
                                                          deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (AttachSealedFile a) (Either String Document) where
  dbUpdate (AttachSealedFile did fid actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "mtime" $ time
         , sqlField "sealed_file_id" $ fid
         , sqlLogAppend time ("Attached sealed file " ++ show fid)
         ]
         "WHERE id = ? AND status = ?" [ toSql did, toSql Closed ]
    when (r == 1) $ 
      ignore $ dbUpdate $ InsertEvidenceEvent
      AttachSealedFileEvidence
      ("Sealed file attached by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "AttachSealedFile" r did

data (Actor a, Show a, Eq a, Ord a) => CancelDocument a = CancelDocument DocumentID CancelationReason a
                      deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (CancelDocument a) (Either String Document) where
  dbUpdate (CancelDocument did reason actor) = do
    let mtime = actorTime actor
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because it does not exist"
      Just document ->
        case checkCancelDocument document of
          [] -> do
            let logmsg = case actorIP actor of
                  Just ipaddress -> "Document canceled from " ++ formatIP ipaddress ++ " ."
                  Nothing        -> "Document canceled."
            r <- runUpdateStatement "documents"
                 [ sqlField "status" Canceled
                 , sqlField "mtime" mtime
                 , sqlField "cancelation_reason" reason
                 , sqlLogAppend mtime logmsg
                 ]
                "WHERE id = ? AND type = ?" [ toSql did, toSql (toDocumentSimpleType (Signable undefined)) ]
            when (r == 1) $ 
              case reason of
                ManualCancel -> 
                  ignore $ dbUpdate $ InsertEvidenceEvent
                  CancelDocumentEvidence
                  ("The document was canceled by " ++ actorWho actor ++ ".")
                  (Just did)
                  actor

                ELegDataMismatch _ sid fn ln num -> do
                  let Just sl = getSigLinkFor document sid
                      trips = [("first name",      getFirstName      sl, fn)
                              ,("last name",       getLastName       sl, ln)
                              ,("personal number", getPersonalNumber sl, num)]
                      uneql = filter (\(_,a,b)->a/=b) trips
                      msg = intercalate "; " $ map (\(f,s,e)->f ++ " from document was " ++ show s ++ " but from e-legitimation was " ++ show e) uneql
                  ignore $ dbUpdate $ InsertEvidenceEvent
                    CancelDocumenElegEvidence
                    ("The document was canceled due to a mismatch with e-legitimation data by " ++ actorWho actor ++ ". Reason: " ++ msg ++ ".")
                    (Just did)
                    actor

            getOneDocumentAffected "CancelDocument" r did

            -- return $ Right $ document { documentstatus = Closed
            --                          , documentmtime  = time
            --                          } `appendHistory` [DocumentHistoryClosed time ipaddress]
          s -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because " ++ concat s

data (Actor a, Show a, Eq a, Ord a) => ChangeMainfile a = ChangeMainfile DocumentID FileID a
instance (Actor a, Show a, Eq a, Ord a)=> DBUpdate (ChangeMainfile a) (Either String Document) where
  dbUpdate (ChangeMainfile did fid actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot ChangeMainfile document " ++ show did ++ " because it does not exist"
      Just document -> do
        let fieldname = if (documentstatus document == Closed || allHadSigned document)
                        then "sealed_file_id"
                        else "file_id"
        r <- runUpdateStatement "documents"
                 [ sqlField fieldname $ fid
                 ]
             "WHERE id = ?" [ toSql did ]
        when (r == 1) $ do
          ignore $ dbUpdate $ InsertEvidenceEvent
            ChangeMainfileEvidence
            ("Main file was changed by " ++ actorWho actor ++ ".")
            (Just did)
            actor
        getOneDocumentAffected "ChangeMainfile" r did
    where
        allHadSigned doc = all (hasSigned ||^ (not . isSignatory)) $ documentsignatorylinks doc

data (Actor a, Show a, Eq a, Ord a) => ChangeSignatoryEmailWhenUndelivered a = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) BS.ByteString a
                                           deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (ChangeSignatoryEmailWhenUndelivered a) (Either String Document) where
  dbUpdate (ChangeSignatoryEmailWhenUndelivered did slid muser email actor) = do
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    let setEmail signatoryfields =
         map (\sf -> case sfType sf of
                 EmailFT -> sf { sfValue = email }
                 _       -> sf) signatoryfields

    let Just sl = getSigLinkFor doc slid
        oldemail = getEmail sl
    r <- runUpdateStatement "signatory_links"
                       [ sqlField "invitation_delivery_status" Unknown
                       , sqlField "fields" $ setEmail $ signatoryfields $ signatorydetails sl
                       , sqlField "user_id" $ fmap userid muser
                       , sqlField "company_id" $ muser >>= usercompany
                       ]
                       ("WHERE EXISTS (SELECT * FROM documents WHERE documents.id = signatory_links.document_id AND (documents.status = ? OR documents.status = ?))" ++
                        " AND document_id = ? " ++
                        " AND id = ? ")
                       [ toSql Pending
                       , toSql AwaitingAuthor
                       , toSql did
                       , toSql slid
                       ]
    when (r == 1) $ 
      ignore $ dbUpdate $ InsertEvidenceEvent
      ChangeSignatoryEmailWhenUndeliveredEvidence
      ("Changed the email address for signatory from " ++ show oldemail ++ " to " ++ show email ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
   
    getOneDocumentAffected "ChangeSignatoryEmailWhenUndelivered" r did

data (Actor a, Show a, Eq a, Ord a) => PreparationToPending a = PreparationToPending DocumentID a
                     deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (PreparationToPending a) (Either String Document) where
  dbUpdate (PreparationToPending docid actor) = do
    let time = actorTime actor
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPreparationToPending document of
          [] -> do
            let mtt = (\days -> (days * 24 *60) `minutesAfter` time) <$> documentdaystosign document
                t = case mtt of
                  Just tt -> " Timeout time set to " ++ formatMinutesTimeUTC tt ++ " UTC."
                  _ -> ""
            r <- runUpdateStatement "documents"
                 [ sqlField "status" $ Pending
                 , sqlField "mtime" time
                 , sqlField "timeout_time" mtt
                 , sqlLogAppend time ("Document put into Pending state")
                 ]
                "WHERE id = ? AND type = ?" [ toSql docid, toSql (toDocumentSimpleType (Signable undefined))]
            when (r == 1) $
              ignore $ dbUpdate $ InsertEvidenceEvent
              PreparationToPendingEvidence
              ("Document was put into Pending state by " ++ actorWho actor ++ "." ++ t)
              (Just docid)
              actor
            getOneDocumentAffected "PreparationToPending" r docid
          s -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because " ++ concat s


data (Actor a, Show a, Eq a, Ord a) => CloseDocument a = CloseDocument DocumentID a
                     deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (CloseDocument a) (Either String Document) where
  dbUpdate (CloseDocument docid actor) = do
    let time = actorTime actor
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot Close document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkCloseDocument document of
          [] -> do
            r <- runUpdateStatement "documents"
                 [ sqlField "status" $ Closed
                 , sqlField "mtime" time
                 , sqlLogAppend time ("Document closed")
                 ]
                "WHERE id = ? AND type = ?" [ toSql docid, toSql (toDocumentSimpleType (Signable undefined)) ]
            when (r == 1) $
              ignore $ dbUpdate $ InsertEvidenceEvent 
              CloseDocumentEvidence
              ("The document was closed by " ++ actorWho actor ++ " after all signatories had signed.")
              (Just docid)
              actor
            getOneDocumentAffected "CloseDocument" r docid

            -- return $ Right $ document { documentstatus = Closed
            --                          , documentmtime  = time
            --                          } `appendHistory` [DocumentHistoryClosed time ipaddress]
          s -> return $ Left $ "Cannot CloseDocument " ++ show docid ++ " because " ++ concat s

data (Actor a, Show a, Eq a, Ord a) => DeleteSigAttachment a = DeleteSigAttachment DocumentID BS.ByteString FileID a
                           deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (DeleteSigAttachment a) (Either String Document) where
  dbUpdate (DeleteSigAttachment did email fid actor) = do
    r <- runUpdateStatement "signatory_attachments"
                         [ sqlField "file_id" SqlNull
                         ]
                         "WHERE document_id = ? AND email = ? AND file_id = ?"
                         [ toSql did
                         , toSql email
                         , toSql fid
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      DeleteSigAttachmentEvidence
      ("Signatory attachment for signatory with email " ++ show email ++ " was deleted by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "DeleteSigAttachment" r did

data (Actor a, Show a, Eq a, Ord a) => DocumentFromSignatoryData a = DocumentFromSignatoryData DocumentID Int BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString [BS.ByteString] a
                                 deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (DocumentFromSignatoryData a) (Either String Document) where
  dbUpdate (DocumentFromSignatoryData docid sigindex fstname sndname email company personalnumber companynumber fieldvalues actor) = do
    ed <- newFromDocument toNewDoc docid    
    when (isRight ed) $ 
      let Right d = ed 
          Just sl = getAuthorSigLink d
      in do
        copyEvidenceLogToNewDocument docid (documentid d)
        ignore $ dbUpdate $ InsertEvidenceEvent
          AuthorUsesCSVEvidence
          ("Document created from CSV file by " ++ actorWho actor ++ ".")
          (Just $ documentid d)
          actor
    return ed
   where
    now = actorTime actor 
    toNewDoc :: Document -> Document
    toNewDoc d = d { documentsignatorylinks = map toNewSigLink (documentsignatorylinks d)
                    , documenttype = newDocType $ documenttype d
                    , documentsignatoryattachments = map replaceCSV (documentsignatoryattachments d)
                    , documentctime = now
                    , documentmtime = now
                    }
    replaceCSV :: SignatoryAttachment -> SignatoryAttachment
    replaceCSV sa =
      if signatoryattachmentemail sa == BS.fromString "csv"
      then sa { signatoryattachmentemail = email }
      else sa
    newDocType :: DocumentType -> DocumentType
    newDocType (Signable p) = Signable p
    newDocType (Template p) = Signable p
    newDocType dt = dt
    toNewSigLink :: SignatoryLink -> SignatoryLink
    toNewSigLink sl
      | isJust (signatorylinkcsvupload sl) = pumpData sl { signatorylinkcsvupload = Nothing }
      | otherwise = sl
    pumpData :: SignatoryLink -> SignatoryLink
    pumpData siglink = replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues

data (Actor a, Show a, Eq a, Ord a) => ErrorDocument a = ErrorDocument DocumentID String a
                     deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (ErrorDocument a) (Either String Document) where
  dbUpdate (ErrorDocument docid errmsg actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot ErrorDocument document " ++ show docid ++ " because it does not exist"
      Just document ->
        case [] of
          [] -> do
            r <- runUpdateStatement "documents"
                 [ sqlField "status" $ DocumentError errmsg
                 , sqlField "error_text" $ errmsg
                 ]
                "WHERE id = ?" [ toSql docid ]
            when (r == 1) $
              ignore $ dbUpdate $ InsertEvidenceEvent
              ErrorDocumentEvidence
              ("The following error occured on the document: " ++ errmsg)
              (Just docid)
              actor
            getOneDocumentAffected "ErrorDocument" r docid

          s -> return $ Left $ "Cannot ErrorDocument document " ++ show docid ++ " because " ++ concat s

data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser User
                                 deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByUser [Document] where
  dbQuery (GetDeletedDocumentsByUser user) = do
    docs <- selectDocumentsBySignatory (userid user) True
    return docs

selectDocuments :: String -> [SqlValue] -> DB [Document]
selectDocuments select values = do
    kPrepare $ "CREATE TEMP TABLE docs ON COMMIT DROP AS " ++ select
    _ <- kExecute values

    kPrepare "SELECT * FROM docs"
    _ <- kExecute []

    docs <- kFetchAll decodeRowAsDocument

    kPrepare $ "SELECT " ++ concat (intersperse "," selectSignatoryLinksSelectors) ++
               " FROM signatory_links WHERE document_id IN (SELECT id FROM docs) ORDER BY document_id, internal_insert_order"
    _ <- kExecute []
    sls <- kFetchAll decodeRowAsSignatoryLinkWithDocumentID

    kPrepare $ "SELECT " ++ concat (intersperse "," selectAuthorAttachmentsSelectors) ++
               " FROM author_attachments WHERE document_id IN (SELECT id FROM docs) ORDER BY document_id"
    _ <- kExecute []
    ats <- kFetchAll decodeRowAsAuthorAttachment

    kPrepare $ "SELECT " ++ concat (intersperse "," selectSignatoryAttachmentsSelectors) ++
               " FROM signatory_attachments WHERE document_id IN (SELECT id FROM docs) ORDER BY document_id"
    _ <- kExecute []

    sas <- kFetchAll decodeRowAsSignatoryAttachment

    kPrepare $ "DROP TABLE docs"
    _ <- kExecute []


    let makeListOfSecond :: (a,b) -> (a,[b])
        makeListOfSecond (a,b) = (a,[b])
        makeMap::(Eq a) => [(a,b)] -> Map.Map a [b]
        makeMap x = Map.fromAscListWith (flip (++)) $ map makeListOfSecond x
        sls_map = makeMap sls
        ats_map = makeMap ats
        sas_map = makeMap sas

        findEmpty :: Document -> Map.Map DocumentID [a] -> [a]
        findEmpty doc mapx = maybe [] id (Map.lookup (documentid doc) mapx)

        fillIn doc = doc { documentsignatorylinks       = findEmpty doc sls_map
                         , documentauthorattachments    = findEmpty doc ats_map
                         , documentsignatoryattachments = findEmpty doc sas_map
                         }

    return $ map fillIn docs


data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentByDocumentID (Maybe Document) where
  dbQuery (GetDocumentByDocumentID did) = do
    docs <- selectDocuments (selectDocumentsSQL ++ " WHERE id = ? AND deleted = FALSE") [toSql did]
    case docs of
      [doc] -> return (Just doc)
      _ -> return Nothing

data GetDocumentStats = GetDocumentStats
                        deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStats DocStats where
  dbQuery (GetDocumentStats) = do
  undeleteddocs <- selectDocuments (selectDocumentsSQL ++ " WHERE deleted=FALSE") []
  let signatureCountForDoc :: Document -> Int
      signatureCountForDoc doc = length $ filter (isJust . maybesigninfo) (documentsignatorylinks doc)
  return $ DocStats
      { doccount = (length undeleteddocs)
      , signaturecount = sum $ map signatureCountForDoc undeleteddocs
      , signaturecount1m = 0
      , signaturecount2m = 0
      , signaturecount3m = 0
      , signaturecount6m = 0
      , signaturecount12m = 0
      }


data GetDocumentStatsByUser = GetDocumentStatsByUser User MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStatsByUser DocStats where
  dbQuery (GetDocumentStatsByUser user time) = do
  docs    <- dbQuery $ GetDocumentsByAuthor (userid user)
  sigdocs <- dbQuery $ GetDocumentsBySignatory user
  let signaturecount'    = length $ allsigns
      signaturecount1m'  = length $ filter (isSignedNotLaterThanMonthsAgo 1)  $ allsigns
      signaturecount2m'  = length $ filter (isSignedNotLaterThanMonthsAgo 2)  $ allsigns
      signaturecount3m'  = length $ filter (isSignedNotLaterThanMonthsAgo 3)  $ allsigns
      signaturecount6m'  = length $ filter (isSignedNotLaterThanMonthsAgo 6)  $ allsigns
      signaturecount12m' = length $ filter (isSignedNotLaterThanMonthsAgo 12) $ allsigns
      isSignedNotLaterThanMonthsAgo m d = monthsBefore m time < documentmtime d
      allsigns = filter (\d -> hasSigned $ getSigLinkFor d (userid user)) sigdocs
  return DocStats { doccount          = length docs
                  , signaturecount    = signaturecount'
                  , signaturecount1m  = signaturecount1m'
                  , signaturecount2m  = signaturecount2m'
                  , signaturecount3m  = signaturecount3m'
                  , signaturecount6m  = signaturecount6m'
                  , signaturecount12m = signaturecount12m'
                  }

data GetDocuments = GetDocuments (Maybe ServiceID)
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocuments [Document] where
  dbQuery (GetDocuments mserviceid) = do
    selectDocuments (selectDocumentsSQL ++ " WHERE (?::TEXT IS NULL AND service_id IS NULL) OR (service_id = ?)")  [toSql mserviceid,toSql mserviceid]

selectDocumentsBySignatoryLink :: String -> [SqlValue] -> DB [Document]
selectDocumentsBySignatoryLink condition values = do
    selectDocuments (selectDocumentsSQL ++ " WHERE EXISTS (SELECT TRUE FROM signatory_links WHERE documents.id = document_id AND " ++ condition ++ ") ORDER BY mtime DESC") values

{- |
    All documents authored by the user that have never been deleted.
-}
data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
                            deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByAuthor [Document] where
  dbQuery (GetDocumentsByAuthor uid) = do
    selectDocumentsBySignatoryLink ("signatory_links.deleted = FALSE AND signatory_links.user_id = ? AND ((signatory_links.roles & ?)<>0) ORDER BY mtime DESC") [toSql uid, toSql [SignatoryAuthor]]


{- |
    Fetches documents by company and tags, this won't return documents that have been deleted (so ones
    that would appear in the recycle bin//trash can.)  It also makes sure to respect the sign order in
    cases where the company is linked via a signatory that hasn't yet been activated.
-}
data GetDocumentsByCompanyAndTags = GetDocumentsByCompanyAndTags (Maybe ServiceID) CompanyID [DocumentTag]
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompanyAndTags [Document] where
  dbQuery (GetDocumentsByCompanyAndTags mservice companyid doctags) = do
        docs <- selectDocumentsBySignatoryLink ("signatory_links.deleted = FALSE AND " ++
                                                "signatory_links.company_id = ? AND " ++
                                                activatedSQL ++ " AND " ++
                                                "(signatory_links.roles = ? OR signatory_links.roles = ?) AND " ++
                                                "((?::TEXT IS NULL AND service_id IS NULL) OR (service_id = ?)) ")
                [ toSql companyid,
                  toSql [SignatoryAuthor],
                  toSql [SignatoryAuthor, SignatoryPartner],
                  toSql mservice,
                  toSql mservice
                ]
        return (filter hasTags docs)
    where hasTags doc = all (`elem` (documenttags doc)) doctags

activatedSQL :: String
activatedSQL = "(NOT EXISTS (" ++ subselect ++ ")) "
  where
    subselect = "SELECT 1 FROM signatory_links AS sl2 " ++
                "WHERE signatory_links.document_id = sl2.document_id " ++
                "  AND ((sl2.roles & 1) <> 0) " ++
                "  AND sl2.sign_time IS NULL " ++
                "  AND sl2.sign_order < signatory_links.sign_order "

selectDocumentsBySignatory :: UserID -> Bool -> DB [Document]
selectDocumentsBySignatory userid deleted = do
    docs <- selectDocumentsBySignatoryLink
            ("    signatory_links.deleted = " ++ show deleted ++ " " ++
             "AND signatory_links.really_deleted = FALSE " ++
             (if deleted
              then ""
              else "AND " ++ activatedSQL) ++
             "AND (   signatory_links.user_id = ? " ++
             "     OR EXISTS (SELECT TRUE FROM users " ++
             "                WHERE users.id = ? " ++
             "                  AND signatory_links.company_id = users.company_id " ++
             "                  AND users.is_company_admin = TRUE))")
             [ toSql userid
             , toSql userid
             ]
    return docs

{- |
    All documents where the user is a signatory that are not deleted.  An author is a type
    of signatory, so authored documents are included too.
    This also filters so that documents where a user is a signatory, but that signatory
    has not yet been activated according to the document's sign order, are excluded.
-}
data GetDocumentsBySignatory = GetDocumentsBySignatory User
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsBySignatory [Document] where
  dbQuery (GetDocumentsBySignatory user) = do
    docs <- selectDocumentsBySignatory (userid user) False
    return docs


data GetSignatoryLinkIDs = GetSignatoryLinkIDs
                           deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetSignatoryLinkIDs [SignatoryLinkID] where
  dbQuery (GetSignatoryLinkIDs) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT id FROM signatory_links"
    result <- execute st []
    fetchValues st decodeRowAsSignatoryLinkID
    where
      decodeRowAsSignatoryLinkID :: SignatoryLinkID -> Either DBException SignatoryLinkID
      decodeRowAsSignatoryLinkID slid = return slid

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
                                       deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetTimeoutedButPendingDocuments [Document] where
  dbQuery (GetTimeoutedButPendingDocuments mtime) = do
        selectDocuments (selectDocumentsSQL ++ " WHERE status = ? AND timeout_time IS NOT NULL AND timeout_time < ?")
                      [ toSql Pending
                      , toSql mtime
                      ]

data (Actor a, Show a, Eq a, Ord a) => MarkDocumentSeen a = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (MarkDocumentSeen a) (Either String Document) where
  -- should this check whether actor matches siglinkid?
  dbUpdate (MarkDocumentSeen did signatorylinkid1 mh actor) = do
    let time = actorTime actor
        ipnumber = fromMaybe (IPAddress 0) $ actorIP actor
        txt = case actorIP actor of
          Just _ ->
            "GET Request made to secret link for signatory with id " ++ show signatorylinkid1 ++ " by " ++ actorWho actor ++ "."
          Nothing ->
            "Marking document seen for signatory with id " ++ show signatorylinkid1 ++ " by " ++ actorWho actor ++ "."
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "seen_time" time
                         , sqlField "seen_ip" ipnumber
                         ]
                         "WHERE id = ? AND document_id = ? AND token = ? AND seen_time IS NULL AND sign_time IS NULL AND EXISTS (SELECT * FROM documents WHERE id = ? AND type = ? AND status <> ? AND status <> ?)"
                         [ toSql signatorylinkid1
                         , toSql did
                         , toSql mh
                         , toSql did
                         , toSql (toDocumentSimpleType (Signable Contract)) -- Contract part should be ignored
                         , toSql Preparation
                         , toSql Closed
                         ]
                         
    -- it's okay if we don't update the doc because it's been seen or signed already
    -- (see jira #1194)
    let fudgedr = if r==0 then 1 else r
    --but we should update the log                   
    when (fudgedr == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      MarkDocumentSeenEvidence
      txt
      (Just did)
      actor
    getOneDocumentAffected "MarkDocumentSeen" r did

data (Actor a, Show a, Eq a, Ord a) => AddInvitationEvidence a = AddInvitationEvidence DocumentID SignatoryLinkID a
                          deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (AddInvitationEvidence a) (Either String Document) where
  dbUpdate (AddInvitationEvidence docid slid actor) = do
  -- modifySignable docid $ \document ->
  -- case checkAddEvidence document slid of
  --  [] -> let Just sds = signatorydetails <$> getSigLinkFor document slid
  --        in Right $ document { documentinvitetime = Just (SignInfo time ipnumber) }
  --           `appendHistory` [DocumentHistoryInvitationSent time ipnumber [sds]]
  --  s -> Left $ "Document " ++ show documentid ++ " cannot have evidence attached for signatory " ++ show slid ++ " because " ++ concat s
    mdoc <- dbQuery $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Left "no such document"
      Just doc -> do
        case getSigLinkFor doc slid of
          Just sl -> do
            let eml = BS.toString $ getEmail sl
            ignore $ dbUpdate $ InsertEvidenceEvent
              InvitationEvidence
              ("Invitation sent to " ++ eml ++ " by " ++ actorWho actor ++ ".")
              (Just docid)
              actor
            return $ Right doc
          Nothing -> 
            return $ Left $ "SignatoryLinkID " ++ show slid ++ " does not exist in document with id " ++ show docid

data (Actor a, Show a, Eq a, Ord a) => MarkInvitationRead a = MarkInvitationRead DocumentID SignatoryLinkID a
                          deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (MarkInvitationRead a) (Either String Document) where
  dbUpdate (MarkInvitationRead did linkid actor) = do
    mdoc <- dbQuery $ GetDocumentByDocumentID did
    case mdoc of
      Nothing -> return $ Left "no such document"
      Just doc -> case getSigLinkFor doc linkid of
        Nothing -> return $ Left "no such siglink id"
        Just sl -> do
          let time = actorTime actor
              eml  = BS.toString $ getEmail sl
          r <- runUpdateStatement "signatory_links"
               [ sqlField "read_invitation" time
               ]
               "WHERE id = ? AND document_id = ? AND read_invitation IS NULL"
               [ toSql linkid
               , toSql did
               ]
          when (r == 1) $
            ignore $ dbUpdate $ InsertEvidenceEvent
            MarkInvitationReadEvidence
            ("Invitation sent to " ++ show eml ++ " was opened, as reported by " ++ actorWho actor ++ ".")
            (Just did)
            actor
          getOneDocumentAffected "MarkInvitationRead" r did

data (Actor a, Show a, Eq a, Ord a) => NewDocument a = NewDocument User (Maybe Company) BS.ByteString DocumentType a
                 deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (NewDocument a) (Either String Document) where
  dbUpdate (NewDocument user mcompany title documenttype actor) = do
  let ctime = actorTime actor  
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do
      wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
      wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      did <- DocumentID <$> getUniqueID tableDocuments

      let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                        then [SignatoryAuthor]
                        else [SignatoryPartner, SignatoryAuthor]
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO randomIO

      let authorlink0 = signLinkFromDetails'
                        (signatoryDetailsFromUser user mcompany)
                        authorRoles linkid magichash

      let authorlink = authorlink0 {
                         maybesignatory = Just $ userid user,
                         maybecompany = usercompany user }

      let doc = blankDocument
                { documentid                   = did
                , documenttitle                = title
                , documentsignatorylinks       = [authorlink]
                , documenttype                 = documenttype
                , documentregion               = getRegion user
                , documentfunctionality        = newDocumentFunctionality documenttype user
                , documentctime                = ctime
                , documentmtime                = ctime
                , documentservice              = userservice user
                , documentauthorattachments    = []
                , documentsignatoryattachments = []
                , documentallowedidtypes       = [EmailIdentification]
                , documentui                   = (documentui blankDocument) {documentmailfooter = BS.fromString <$> (customfooter $ usersettings user)}
                } `appendHistory` [DocumentHistoryCreated ctime]

      case invariantProblems ctime doc of
        Nothing -> do

           midoc <- insertDocumentAsIs doc
           case midoc of
             Just doc' -> do
               ignore $ dbUpdate $ InsertEvidenceEvent           
                 NewDocumentEvidence
                 ("Document created by " ++ actorWho actor ++ ".")
                 (Just $ documentid doc')
                 actor
               return $ Right doc'
             Nothing -> do
               Log.debug $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
               return $ Left $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
        Just a -> do
           Log.debug $ "insertDocumentAsIs invariants violated: " ++ show a
           return $ Left $ "insertDocumentAsIs invariants violated: " ++ show a

data (Actor a, Show a, Eq a, Ord a) => ReallyDeleteDocument a = ReallyDeleteDocument User DocumentID a
                             deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (ReallyDeleteDocument a) (Either String Document) where
  dbUpdate (ReallyDeleteDocument user did actor) = do
    -- I don't like this: we should do this on the DB side, not pass
    -- in a User which could be old. It should be done within a
    -- transaction. -EN
    r <- case (usercompany user, useriscompanyadmin user) of
           (Just cid, True) ->
             runUpdateOnDeletableDoc "WHERE company_id = ?" [toSql cid]
           _ ->
             runUpdateOnDeletableDoc "WHERE user_id = ? AND company_id is NULL" [toSql $ userid user]
    let txt = case (usercompany user, useriscompanyadmin user) of
          (Just cid, True) -> "the company with admin email " ++ show (getEmail user)
          _ -> "the user with email " ++ show (getEmail user)
    when (r == 1) $ do
      ignore $ dbUpdate $ InsertEvidenceEvent
        ReallyDeleteDocumentEvidence
        ("The document was removed from the rubbish bin for " ++ txt ++ " by " ++ actorWho actor ++ ".")
        (Just did)
        actor
    getOneDocumentAffected "ReallyDeleteDocument" r did
    where
      runUpdateOnDeletableDoc whereClause whereFields =
        runUpdateStatement "signatory_links"
                           [sqlField "really_deleted" True]
                           (whereClause ++ " AND document_id = ? AND deleted = TRUE AND EXISTS (SELECT * FROM documents WHERE id = ?)")
                           (whereFields ++
                              [ toSql did
                              , toSql did
                              ])

data (Actor a, Show a, Eq a, Ord a) => RejectDocument a = RejectDocument DocumentID SignatoryLinkID (Maybe BS.ByteString) a
                      deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (RejectDocument a) (Either String Document) where
  dbUpdate (RejectDocument docid slid customtext actor) = do
    let time = actorTime actor
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because it does not exist"
      Just document -> case getSigLinkFor document slid of
        Nothing -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because signatory link id does not exist."
        Just sl ->
          case checkRejectDocument document slid of
            [] -> do
              r <- runUpdateStatement "documents"
                   [ sqlField "status" Rejected
                   , sqlField "mtime" time
                   , sqlField "rejection_time" time
                   , sqlField "rejection_reason" customtext
                   , sqlField "rejection_signatory_link_id" slid
                   , sqlLogAppend time ("Document rejected")
                   ]
                   "WHERE id = ?" [toSql docid]
              let eml = BS.toString $ getEmail sl
              when (r == 1) $
                ignore $ dbUpdate $ InsertEvidenceEvent
                RejectDocumentEvidence
                ("Document rejected for signatory with email " ++ show eml ++ " by " ++ actorWho actor ++ ".")
                (Just docid)
                actor
              getOneDocumentAffected "RejectDocument" r docid
            s -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because " ++ concat s

data (Actor a, Show a, Eq a, Ord a) => RestartDocument a = RestartDocument Document a
                       deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (RestartDocument a) (Either String Document) where
  dbUpdate (RestartDocument doc actor) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> do
        ed <- newFromDocument (const newdoc) (documentid doc)
        case ed of
          Left s -> return $ Left s
          Right d -> do
            ignore $ dbUpdate $ InsertEvidenceEvent
              RestartDocumentEvidence
              ("Document restarted by " ++ actorWho actor ++ ". New document has id " ++ show (documentid d) ++ ".")
              (Just $ documentid doc)
              actor
            copyEvidenceLogToNewDocument (documentid doc) (documentid d)
            ignore $ dbUpdate $ InsertEvidenceEvent
              RestartDocumentEvidence
              ("Document restarted from document with id " ++ (show $ documentid doc) ++ " by " ++ actorWho actor ++ ".")
              (Just $ documentid d)
              actor
            return $ Right d
      other -> return other
   where
    tryToGetRestarted :: DB (Either String Document)
    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
      else do
             let time = actorTime actor
                 ipnumber = fromMaybe (IPAddress 0) $ actorIP actor
             doc' <- clearSignInfofromDoc
             let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
             return $ Right doc''
    clearSignInfofromDoc :: DB Document
    clearSignInfofromDoc = do
      let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x, signatorylinkid x)) $ documentsignatorylinks doc
          Just asl = getAuthorSigLink doc
      newSignLinks <- flip mapM signatoriesDetails $ do \(a,b,c) -> do
                                                             magichash <- liftIO randomIO

                                                             return $ signLinkFromDetails' a b c magichash
      let Just authorsiglink0 = find isAuthor newSignLinks
          authorsiglink = authorsiglink0 {
                            maybesignatory = maybesignatory asl,
                            maybecompany = maybecompany asl
                          }
          othersiglinks = filter (not . isAuthor) newSignLinks
          newsiglinks = authorsiglink : othersiglinks
      return doc {documentstatus = Preparation,
                  documenttimeouttime = Nothing,
                  documentsignatorylinks = newsiglinks
                 }

data (Actor a, Show a, Eq a, Ord a) => RestoreArchivedDocument a = RestoreArchivedDocument User DocumentID a
                                deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (RestoreArchivedDocument a) (Either String Document) where
  dbUpdate (RestoreArchivedDocument user did actor) = do
    r <- case (usercompany user, useriscompanyadmin user) of
           (Just cid, True) ->
             runUpdateOnRestorableDoc "WHERE company_id = ?" [toSql cid]
           _ ->
             runUpdateOnRestorableDoc "WHERE user_id = ?" [toSql $ userid user]
    let txt = case (usercompany user, useriscompanyadmin user) of
          (Just cid, True) -> "the company with admin email " ++ show (getEmail user)
          _ -> "the user with email " ++ show (getEmail user)
    ignore $ dbUpdate $ InsertEvidenceEvent
      RestoreArchivedDocumentEvidence
      ("Document restored from the rubbish bin for " ++ txt ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "RestoreArchivedDocument" r did
    where
      runUpdateOnRestorableDoc whereClause whereFields =
        runUpdateStatement "signatory_links"
                          [sqlField "deleted" False]
                          (whereClause ++ " AND document_id = ? AND really_deleted = FALSE AND EXISTS (SELECT * FROM documents WHERE id = ?)")
                          (whereFields ++
                              [ toSql did
                              , toSql did
                              ])
{- |
    Links up a signatory link to a user account.  This should happen when
      \1. a document moves from preparation to pending more
      \2. a signer creates an account after signing to save their document
      \3. the email of a signatory is corrected to that of an existing user
-}
data (Actor a, Show a, Eq a, Ord a) => SaveDocumentForUser a = SaveDocumentForUser DocumentID User SignatoryLinkID a
                           deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SaveDocumentForUser a) (Either String Document) where
  dbUpdate (SaveDocumentForUser did user@User{userid, usercompany} slid actor) = do
    r <- runUpdateStatement "signatory_links"
                          [ sqlField "user_id" userid
                          , sqlField "company_id" usercompany
                          ]
                          ("WHERE document_id = ? AND id = ?")
                          [ toSql did
                          , toSql slid
                          ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SaveDocumentForUserEvidence
      ("Saving document to user account with email " ++ show (getEmail user) ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SaveDocumentForUser" r did

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
data (Actor a, Show a, Eq a, Ord a) => SaveSigAttachment a = SaveSigAttachment DocumentID BS.ByteString BS.ByteString FileID a
                         deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SaveSigAttachment a) (Either String Document) where
  dbUpdate (SaveSigAttachment did name email fid actor) = do
    r <- runUpdateStatement "signatory_attachments"
                         [ sqlField "file_id" fid
                         ]
                         "WHERE document_id = ? AND email = ? AND file_id IS NULL AND name = ? "
                         [ toSql did
                         , toSql email
                         , toSql name
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SaveSigAttachmentEvidence
      ("Saving attachment with name " ++ show name ++ " for signatory with email " ++ show email ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SaveSigAttachment" r did

data (Actor a, Show a, Eq a, Ord a) => SetDocumentTags a = SetDocumentTags DocumentID [DocumentTag] a
                       deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDocumentTags a) (Either String Document) where
  dbUpdate (SetDocumentTags did doctags actor) = do
    r <- runUpdateStatement "documents"
         [ sqlField "tags" $ doctags
         -- , sqlField "mtime" time
         ]
         "WHERE id = ?" [ toSql did ]
    let tagstr = intercalate "; " $ map (\(DocumentTag k v)-> BS.toString k ++ "=" ++ BS.toString v) doctags
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDocumentTagsEvidence
      ("Document tags set to " ++ show tagstr ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTags" r did

data (Actor a, Show a, Eq a, Ord a) => SetDocumentInviteTime a = SetDocumentInviteTime DocumentID MinutesTime a
                       deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDocumentInviteTime a) (Either String Document) where
  dbUpdate (SetDocumentInviteTime did invitetime actor) = do
    let ipaddress  = fromMaybe (IPAddress 0) $ actorIP actor
    r <- runUpdateStatement "documents"
         [ sqlField "invite_time" invitetime,
           sqlField "invite_ip" ipaddress
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDocumentInviteTimeEvidence
      ("Document invite time set to " ++ formatMinutesTimeUTC invitetime ++ " UTC by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentInviteTime" r did

data (Actor a, Show a, Eq a, Ord a) => SetDocumentTimeoutTime a = SetDocumentTimeoutTime DocumentID MinutesTime a
                              deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDocumentTimeoutTime a) (Either String Document) where
  dbUpdate (SetDocumentTimeoutTime did timeouttime actor) = do
    r <- runUpdateStatement "documents"
                         [ sqlField "timeout_time" timeouttime
                         ]
                         "WHERE id = ? AND deleted = FALSE AND type = ?"
                         [ toSql did
                         , toSql (toDocumentSimpleType (Signable undefined))
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDocumentTimeoutTimeEvidence
      ("Document timeout time set to " ++ formatMinutesTimeUTC timeouttime ++ " UTC by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTimeoutTime" r did

data (Actor a, Show a, Eq a, Ord a) => SetSignatoryCompany a = SetSignatoryCompany DocumentID SignatoryLinkID CompanyID a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetSignatoryCompany a) (Either String Document) where
  dbUpdate (SetSignatoryCompany did slid cid actor) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "company_id" cid
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetSignatoryCompanyEvidence
      ("Signatory with id " ++ show slid ++ " was associated to company with id " ++ show cid ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetSignatoryCompany" r did

data (Actor a, Show a, Eq a, Ord a) => RemoveSignatoryCompany a = RemoveSignatoryCompany DocumentID SignatoryLinkID a
                                                                deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (RemoveSignatoryCompany a) (Either String Document) where
  dbUpdate (RemoveSignatoryCompany did slid actor) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "company_id" SqlNull
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      RemoveSignatoryCompanyEvidence
      ("Signatory with id " ++ show slid ++ " was dissociated from company by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "RemoveSignatoryCompany" r did

data (Actor a, Show a, Eq a, Ord a) => SetSignatoryUser a = SetSignatoryUser DocumentID SignatoryLinkID UserID a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetSignatoryUser a) (Either String Document) where
  dbUpdate (SetSignatoryUser did slid uid actor) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "user_id" uid
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetSignatoryUserEvidence
      ("Signatory with id " ++ show slid ++ " was associated with user with id " ++ show uid ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetSignatoryUser" r did

data (Actor a, Show a, Eq a, Ord a) => RemoveSignatoryUser a = RemoveSignatoryUser DocumentID SignatoryLinkID a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (RemoveSignatoryUser a) (Either String Document) where
  dbUpdate (RemoveSignatoryUser did slid actor) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "user_id" SqlNull
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      RemoveSignatoryUserEvidence
      ("Signatory with id " ++ show slid ++ " was dissociated from its user by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "RemoveSignatoryUser" r did

data (Actor a, Show a, Eq a, Ord a) => SetInviteText a = SetInviteText DocumentID BS.ByteString a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetInviteText a) (Either String Document) where
  dbUpdate (SetInviteText did text actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "invite_text" $ text
         , sqlField "mtime" time
         , sqlLogAppend time ("Invite text set")
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetInvitationTextEvidence
      ("Invitation text set to " ++ show text ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetInviteText" r did

data (Actor a, Show a, Eq a, Ord a) => SetDaysToSign a = SetDaysToSign DocumentID Int a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDaysToSign a) (Either String Document) where
  dbUpdate (SetDaysToSign did days actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "days_to_sign" $ days
         , sqlField "mtime" time
         , sqlLogAppend time ("Days to sign set to " ++ show days)
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDaysToSignEvidence
      ("Days to sign set to " ++ show days ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDaysToSign" r did

data (Actor a, Show a, Eq a, Ord a) => RemoveDaysToSign a = RemoveDaysToSign DocumentID a
                                                          deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (RemoveDaysToSign a) (Either String Document) where
  dbUpdate (RemoveDaysToSign did actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "days_to_sign" $ SqlNull
         , sqlField "mtime" time
         , sqlLogAppend time ("Removed days to sign")
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      RemoveDaysToSignEvidence
      ("Days to sign removed by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "RemoveDaysToSign" r did

data (Actor a, Show a, Eq a, Ord a) => SetDocumentAdvancedFunctionality a = SetDocumentAdvancedFunctionality DocumentID a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDocumentAdvancedFunctionality a) (Either String Document) where
  dbUpdate (SetDocumentAdvancedFunctionality did actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "functionality" AdvancedFunctionality
         , sqlField "mtime" time
         , sqlLogAppend time ("Document changed to advanced functionality")
         ]
         "WHERE id = ? AND functionality <> ?" [ toSql did, toSql AdvancedFunctionality ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDocumentAdvancedFunctionalityEvidence
      ("Document set to advanced functionality by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentAdvancedFunctionality" r did


data (Actor a, Show a, Eq a, Ord a) => SetDocumentTitle a = SetDocumentTitle DocumentID BS.ByteString a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDocumentTitle a) (Either String Document) where
  dbUpdate (SetDocumentTitle did doctitle actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "title" $ doctitle
         , sqlField "mtime" time
         , sqlLogAppend time ("Document title changed")
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDocumentTitleEvidence
      ("Document title set to " ++ show doctitle ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentTitle" r did

data (Actor a, Show a, Eq a, Ord a) => SetDocumentLocale a = SetDocumentLocale DocumentID Locale a
                        deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDocumentLocale a) (Either String Document) where
  dbUpdate (SetDocumentLocale did locale actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "region" $ getRegion locale
         , sqlField "mtime" time
         , sqlLogAppend time ("Document locale changed")
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDocumentLocaleEvidence
      ("Document locale set to " ++ show locale ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentLocale" r did

data (Actor a, Show a, Eq a, Ord a) => SetDocumentUI a = SetDocumentUI DocumentID DocumentUI a
                     deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetDocumentUI a) (Either String Document) where
  dbUpdate (SetDocumentUI did documentui actor) = do
    r <- runUpdateStatement "documents"
         [ sqlField "mail_footer" $ documentmailfooter documentui
         , sqlField "mtime" $ actorTime actor           
         ]
         "WHERE id = ?" [ toSql did ]
    let txt = case documentmailfooter documentui of
          Nothing -> "Document mail footer removed by " ++ actorWho actor ++ "."
          Just footer -> "Document mail footer set to " ++ show footer ++ " by " ++ actorWho actor ++ "."
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetDocumentUIEvidence
      txt
      (Just did)
      actor
    getOneDocumentAffected "SetDocumentUI" r did

data (Actor a, Show a, Eq a, Ord a) => SetInvitationDeliveryStatus a = SetInvitationDeliveryStatus DocumentID SignatoryLinkID MailsDeliveryStatus a
                                   deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetInvitationDeliveryStatus a) (Either String Document) where
  dbUpdate (SetInvitationDeliveryStatus did slid status actor) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "invitation_delivery_status" status
                         ]
                         "WHERE id = ? AND document_id = ? AND EXISTS (SELECT * FROM documents WHERE id = ? AND type = ?)"
                         [ toSql slid
                         , toSql did
                         , toSql did
                         , toSql (toDocumentSimpleType (Signable undefined))
                         ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetInvitationDeliveryStatusEvidence
      ("Delivery status for signatory with id " ++ show slid ++ " set to " ++ show status ++ " by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetInvitationDeliveryStatus" r did


data (Actor a, Show a, Eq a, Ord a) => SignDocument a = SignDocument DocumentID SignatoryLinkID MagicHash (Maybe SignatureInfo) a
                                                      deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SignDocument a) (Either String Document) where
  dbUpdate (SignDocument docid slid mh msiginfo actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkSignDocument document slid mh of
          [] -> do
            let ipnumber = fromMaybe (IPAddress 0) $ actorIP actor
                time     = actorTime actor
            r <- runUpdateStatement "signatory_links"
                      [ sqlField "sign_ip"   ipnumber
                      , sqlField "sign_time" time
                      , sqlField "signinfo_text" $ signatureinfotext `fmap` msiginfo
                      , sqlField "signinfo_signature" $ signatureinfosignature `fmap` msiginfo
                      , sqlField "signinfo_certificate" $ signatureinfocertificate `fmap` msiginfo
                      , sqlField "signinfo_provider" $ signatureinfoprovider `fmap` msiginfo
                      , sqlField "signinfo_first_name_verified" $ signaturefstnameverified `fmap` msiginfo
                      , sqlField "signinfo_last_name_verified" $ signaturelstnameverified `fmap` msiginfo
                      , sqlField "signinfo_personal_number_verified" $ signaturepersnumverified `fmap` msiginfo
                 ]
                "WHERE id = ? AND document_id = ?" [ toSql slid
                                                   , toSql docid
                                                   ]
            let using = case msiginfo of
                  Nothing -> ""
                  Just (SignatureInfo { signatureinfotext
                                      , signatureinfosignature
                                      , signatureinfocertificate
                                      , signatureinfoprovider
                                      , signaturefstnameverified
                                      , signaturelstnameverified
                                      , signaturepersnumverified
                                      }) -> let ps = case signatureinfoprovider of
                                                  BankIDProvider -> "BankID"
                                                  TeliaProvider -> "Telia"
                                                  NordeaProvider -> "Nordea"
                                                pairs = [("first name", signaturefstnameverified)
                                                        ,("last name", signaturelstnameverified)
                                                        ,("personal number", signaturepersnumverified)]
                                                pairstrue = filter (\(_,t)->t) pairs
                                                vs = intercalate "; " $ map (\(s,_) -> s) pairstrue
                                                vstring = case pairstrue of
                                                  [] -> "No fields were verified."
                                                  _ -> "The following fields were verified: " ++ vs
                                            in " using e-legitimation. The signed text was " 
                                               ++ show signatureinfotext 
                                               ++ ". The provider was " ++ ps ++ ". " 
                                               ++ vstring
            when (r == 1) $
              ignore $ dbUpdate $ InsertEvidenceEvent
              SignDocumentEvidence
              ("Document signed by " ++ actorWho actor ++ using ++ ".")
              (Just docid)
              actor
            getOneDocumentAffected "SignDocument" r docid
          s -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because " ++ concat s

data (Actor a, Show a, Eq a, Ord a) => ResetSignatoryDetails a = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] a
                                  deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (ResetSignatoryDetails a) (Either String Document) where
  dbUpdate (ResetSignatoryDetails documentid signatories actor) = 
    dbUpdate (ResetSignatoryDetails2 documentid (map (\(a,b) -> (a,b,Nothing)) signatories) actor)


data (Actor a, Show a, Eq a, Ord a) => ResetSignatoryDetails2 a = ResetSignatoryDetails2 DocumentID [(SignatoryDetails, [SignatoryRole], Maybe CSVUpload)] a
                                  deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (ResetSignatoryDetails2 a) (Either String Document) where
  dbUpdate (ResetSignatoryDetails2 documentid signatories actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID documentid
    case mdocument of
      Nothing -> return $ Left $ "ResetSignatoryDetails: document #" ++ show documentid ++ " does not exist"
      Just document ->
        case checkResetSignatoryData document signatories of
          [] -> do

            kPrepare "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
            kExecute []
            kPrepare "DELETE FROM signatory_links WHERE document_id = ?"
            kExecute [toSql documentid]

            let mauthorsiglink = getAuthorSigLink document
            flip mapM signatories $ \(details, roles, mcsvupload) -> do
                     linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

                     magichash <- liftIO randomIO

                     let link' = (signLinkFromDetails' details roles linkid magichash)
                                 { signatorylinkcsvupload = mcsvupload }
                         link = if isAuthor link'
                                then link' { maybesignatory = maybe Nothing maybesignatory mauthorsiglink
                                           , maybecompany   = maybe Nothing maybecompany   mauthorsiglink
                                           }
                                else link'
                     r1 <- insertSignatoryLinkAsIs documentid link
                     when (isJust r1) $
                       ignore $ dbUpdate $ InsertEvidenceEvent
                       ResetSignatoryDetailsEvidence
                       ("Signatory details for signatory with email " ++ show (getEmail link) ++ " by " ++ actorWho actor ++ ".")
                       (Just documentid)
                       actor

                     when (not (isJust r1)) $
                          error "ResetSignatoryDetails signatory_links did not manage to insert a row"

            Just newdocument <- dbQuery $ GetDocumentByDocumentID documentid
            let moldcvsupload = msum (map (\(_,_,a) -> a) signatories)
            let mnewcsvupload = msum (map (signatorylinkcsvupload) (documentsignatorylinks newdocument))

            when (moldcvsupload /= mnewcsvupload) $ do
                     Log.error $ "ResetSignatoryDetails2 csvupload differs: " ++ show moldcvsupload ++ " vs " ++ show mnewcsvupload
                     error $ "error in ResetSignatoryDetails2"
            return $ Right newdocument

          s -> return $ Left $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s


data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignLinkFromDetailsForTest SignatoryLink where
  dbUpdate (SignLinkFromDetailsForTest details roles) = do
      wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO randomIO

      let link = signLinkFromDetails' details
                        roles linkid magichash

      return link

data (Actor a, Show a, Eq a, Ord a) => SignableFromDocument a = SignableFromDocument Document a
                                                              deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SignableFromDocument a) Document where
  -- NOTE TO MERGER: I removed this in another branch. If there's a
  -- conflict in a merge, get rid of this whole DBUpdate -- Eric
  dbUpdate (SignableFromDocument document actor ) = do
    d <- insertNewDocument $ templateToDocument document
    ignore $ dbUpdate $ InsertEvidenceEvent
      SignableFromDocumentEvidence
      ("Document created from template by " ++ actorWho actor ++ ".")
      (Just (documentid d))
      actor
    return d

data (Actor a, Show a, Eq a, Ord a) => SignableFromDocumentIDWithUpdatedAuthor a = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID a
                                                                                 deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SignableFromDocumentIDWithUpdatedAuthor a) (Either String Document) where
  dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid actor) =
      if fmap companyid mcompany /= usercompany user
        then return $ Left "company and user don't match"
        else do
          let time = actorTime actor
          r <- (flip newFromDocument) docid $ \doc ->
            (templateToDocument doc) {
              documentsignatorylinks = map replaceAuthorSigLink (documentsignatorylinks doc)
                                       -- FIXME: Need to remove authorfields?
              , documentctime = time
              , documentmtime = time
              }
          case r of 
            Right d -> do
              copyEvidenceLogToNewDocument docid (documentid d)
              ignore $ dbUpdate $ InsertEvidenceEvent
                SignableFromDocumentIDWithUpdatedAuthorEvidence
                ("Document created from template with id " ++ show docid ++ " by " ++ actorWho actor ++ ".")
                (Just $ documentid d)
                actor
              return r
            Left _ -> return r
    where replaceAuthorSigLink :: SignatoryLink -> SignatoryLink
          replaceAuthorSigLink sl
            | isAuthor sl = replaceSignatoryUser sl user mcompany
            | otherwise = sl


data StoreDocumentForTesting = StoreDocumentForTesting Document
                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate StoreDocumentForTesting DocumentID where
  dbUpdate (StoreDocumentForTesting document) = do
    -- FIXME: this requires more thinking...
    wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
    did <- DocumentID <$> getUniqueID tableDocuments
    Just doc <- insertDocumentAsIs (document { documentid = did })
    return (documentid doc)

{-
   FIXME: this is so wrong on so many different levels
   - should set mtime
   - should not change type or copy this doc into new doc
-}
data (Actor a, Show a, Eq a, Ord a) => TemplateFromDocument a = TemplateFromDocument DocumentID a
                            deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (TemplateFromDocument a) (Either String Document) where
  dbUpdate (TemplateFromDocument did actor) = do
    r <- runUpdateStatement "documents"
         [ sqlField "status" Preparation
         , sqlField "type" $ toDocumentSimpleType (Template undefined)
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      TemplateFromDocumentEvidence
      ("Document converted to template by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "TemplateFromDocument" r did

data (Actor a, Show a, Eq a, Ord a) => TimeoutDocument a = TimeoutDocument DocumentID a
                       deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (TimeoutDocument a) (Either String Document) where
  dbUpdate (TimeoutDocument did actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "status" $ Timedout
         , sqlField "mtime" time
         , sqlLogAppend time ("Document timed out")
         ]
         "WHERE id = ? AND type = ? AND status = ?" [ toSql did
                                                    , toSql (toDocumentSimpleType (Signable undefined))
                                                    , toSql Pending
                                                    ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      TimeoutDocumentEvidence
      ("Document timed out by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "TimeoutDocument" r did

data (Actor a, Show a, Eq a, Ord a) => SetEmailIdentification a = SetEmailIdentification DocumentID a
                      deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetEmailIdentification a) (Either String Document) where
  dbUpdate (SetEmailIdentification did actor) = do
    let time = actorTime actor
    r <- runUpdateStatement "documents"
         [ sqlField "allowed_id_types" $ [EmailIdentification]
         , sqlField "mtime" time
         , sqlLogAppend time ("Email identification set")
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetEmailIdentificationEvidence
      ("Document identification type set to Email by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetEmailIdentification" r did

data (Actor a, Show a, Eq a, Ord a) => SetElegitimationIdentification a = SetElegitimationIdentification DocumentID a
                      deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (SetElegitimationIdentification a) (Either String Document) where
  dbUpdate (SetElegitimationIdentification did actor) = do
    let time = actorTime actor    
    r <- runUpdateStatement "documents"
         [ sqlField "allowed_id_types" $ [ELegitimationIdentification]
         , sqlField "mtime" time
         , sqlLogAppend time ("E-leg identification set")
         ]
         "WHERE id = ?" [ toSql did ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      SetElegitimationIdentificationEvidence
      ("Document identification type set to E-Legitimation by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    getOneDocumentAffected "SetElegitimationIdentification" r did


data (Actor a, Show a, Eq a, Ord a) => UpdateFields a = UpdateFields DocumentID SignatoryLinkID [(BS.ByteString, BS.ByteString)] a
                      deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (UpdateFields a) (Either String Document) where
  dbUpdate (UpdateFields did slid fields actor) = do
  Just document <- dbQuery $ GetDocumentByDocumentID did
  case checkUpdateFields document slid of
    [] -> do
      let updateSigField sf =
                let updateF n = case lookup n fields of
                      Just v  -> sf { sfValue = v }
                      Nothing -> sf
                in case sfType sf of
                  CompanyFT        -> updateF $ BS.fromString "sigco"
                  PersonalNumberFT -> updateF $ BS.fromString "sigpersnr"
                  CompanyNumberFT  -> updateF $ BS.fromString "sigcompnr"
                  CustomFT label _ -> updateF label
                  _                -> sf

      let Just sl = getSigLinkFor document slid
          eml     = BS.toString $ getEmail sl
      r <- runUpdateStatement "signatory_links"
                       [ sqlField "fields" $ map updateSigField $ signatoryfields $ signatorydetails sl
                       ]
                       ("WHERE EXISTS (SELECT * FROM documents WHERE documents.id = signatory_links.document_id AND (documents.status = ? OR documents.status = ?))" ++
                        " AND document_id = ? " ++
                        " AND id = ? ")
                       [ toSql Pending
                       , toSql AwaitingAuthor
                       , toSql did
                       , toSql slid
                       ]
      when (r == 1) $ forM_ fields $ \(n, v) -> 
        dbUpdate $ InsertEvidenceEvent
        UpdateFieldsEvidence
        ("Information for signatory with email " ++ show eml ++ " for field " ++ show n ++ " was set to " ++ show v ++ " by " ++ actorWho actor ++ ".")
        (Just did)
        actor
      getOneDocumentAffected "UpdateFields" r did

    s -> return $ Left $ "Cannot updateFields on document " ++ show did ++ " because " ++ concat s

data (Actor a, Show a, Eq a, Ord a) => PendingToAwaitingAuthor a = PendingToAwaitingAuthor DocumentID a
                      deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (PendingToAwaitingAuthor a) (Either String Document) where
  dbUpdate (PendingToAwaitingAuthor docid actor) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PendingToAwaitingAuthor document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPendingToAwaitingAuthor document of
          [] -> do
            let time = actorTime actor
            r <- runUpdateStatement "documents"
                 [ sqlField "status" AwaitingAuthor
                 , sqlField "mtime" time
                 , sqlLogAppend time ("Changed to AwaitingAuthor status")
                 ]
                "WHERE id = ? AND type = ?" [ toSql docid, toSql (toDocumentSimpleType (Signable undefined)) ]
            when (r == 1) $
              ignore $ dbUpdate $ InsertEvidenceEvent
              PendingToAwaitingAuthorEvidence
              ("Document moved from Pending to AwaitingAuthor status when all signatories had signed but author by " ++ actorWho actor ++ ".")
              (Just docid)
              actor

            getOneDocumentAffected "PendingToAwaitingAuthor" r docid

            -- return $ Right $ document { documentstatus = Closed
            --                          , documentmtime  = time
            --                          } `appendHistory` [DocumentHistoryClosed time ipaddress]
          s -> return $ Left $ "Cannot PendingToAwaitingAuthor document " ++ show docid ++ " because " ++ concat s



data (Actor a, Show a, Eq a, Ord a) => AddDocumentAttachment a = AddDocumentAttachment DocumentID FileID a
                                 deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (AddDocumentAttachment a) (Either String Document) where
  dbUpdate (AddDocumentAttachment did fid actor) = do
            r <- wrapDB $ \conn -> do
                   run conn ("INSERT INTO author_attachments(document_id, file_id) (SELECT ?, ? WHERE" ++
                         " EXISTS (SELECT TRUE FROM documents WHERE id = ? AND status = ?))")
                      [ toSql did
                      , toSql fid
                      , toSql did
                      , toSql Preparation
                      ]
            when (r == 1) $
              ignore $ dbUpdate $ InsertEvidenceEvent
              AddDocumentAttachmentEvidence
              ("File with ID " ++ show fid ++ " attached to Document by " ++ actorWho actor ++ ".")
              (Just did)
              actor
            getOneDocumentAffected "AddDocumentAttachment" r did

data (Actor a, Show a, Eq a, Ord a) => RemoveDocumentAttachment a = RemoveDocumentAttachment DocumentID FileID a
                                 deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (RemoveDocumentAttachment a) (Either String Document) where
  dbUpdate (RemoveDocumentAttachment did fid actor) = do
    r <- wrapDB $ \conn -> run conn "DELETE FROM author_attachments WHERE document_id = ? AND file_id = ? AND EXISTS (SELECT TRUE FROM documents WHERE id = ? AND status = ?)"
                                 [ toSql did
                                 , toSql fid
                                 , toSql did
                                 , toSql Preparation
                                 ]
    when (r == 1) $
      ignore $ dbUpdate $ InsertEvidenceEvent
      RemoveDocumentAttachmentEvidence
      ("File with ID " ++ show fid ++ " removed from document by " ++ actorWho actor ++ ".")
      (Just did)
      actor
                                 
    m <- dbQuery $ GetDocumentByDocumentID did
    case m of
      Just doc -> case documentstatus doc of
        Preparation -> return $ Right doc
        _           -> return $ Left "bad document status"
      Nothing -> return $ Left "no such document"

data (Actor a, Show a, Eq a, Ord a) => UpdateSigAttachments a = UpdateSigAttachments DocumentID [SignatoryAttachment] a
                            deriving (Eq, Ord, Show, Typeable)
instance (Actor a, Show a, Eq a, Ord a) => DBUpdate (UpdateSigAttachments a) (Either String Document) where
  dbUpdate (UpdateSigAttachments did sigatts actor) = do
    r <- wrapDB $ \conn -> run conn "DELETE FROM signatory_attachments WHERE document_id = ?" [toSql did]
    when (r > 0) $ 
      ignore $ dbUpdate $ InsertEvidenceEvent
      RemoveSigAttachmentsEvidence
      ("All signatory attachments removed by " ++ actorWho actor ++ ".")
      (Just did)
      actor
    flip mapM sigatts doInsert
    getOneDocumentAffected "UpdateSigAttachments" 1 did
    where
         doInsert (SignatoryAttachment { signatoryattachmentfile
                                       , signatoryattachmentemail
                                       , signatoryattachmentname
                                       , signatoryattachmentdescription
                                       }) = do
           r <- runInsertStatement "signatory_attachments"
                [ sqlField "file_id"     $ signatoryattachmentfile
                , sqlField "email"       $ signatoryattachmentemail
                , sqlField "name"        $ signatoryattachmentname
                , sqlField "description" $ signatoryattachmentdescription
                , sqlField "document_id" $ did
                ]
           when (r == 1) $
             ignore $ dbUpdate $ InsertEvidenceEvent
             AddSigAttachmentEvidence
             ("Signatory attachment request for " ++ show signatoryattachmentname ++ " from signatory with email " ++ show signatoryattachmentemail ++ " by " ++ actorWho actor ++ ".")
             (Just did)
             actor
           
           return r

-- For users lists in adminonly
selectUsersAndStatsSQL :: String
selectUsersAndStatsSQL = "SELECT "
  -- User:
  ++ "  u.id AS userid"
  ++ ", encode(u.password, 'base64')"
  ++ ", encode(u.salt, 'base64')"
  ++ ", u.is_company_admin"
  ++ ", u.account_suspended"
  ++ ", u.has_accepted_terms_of_service"
  ++ ", u.signup_method"
  ++ ", u.service_id"
  ++ ", u.company_id"
  ++ ", u.first_name"
  ++ ", u.last_name"
  ++ ", u.personal_number"
  ++ ", u.company_position"
  ++ ", u.phone"
  ++ ", u.mobile"
  ++ ", u.email"
  ++ ", u.preferred_design_mode"
  ++ ", u.lang"
  ++ ", u.region"
  ++ ", u.customfooter"
  -- Company:
  ++ ", c.id AS company_id"
  ++ ", c.external_id"
  ++ ", c.service_id"
  ++ ", c.name"
  ++ ", c.number"
  ++ ", c.address"
  ++ ", c.zip"
  ++ ", c.city"
  ++ ", c.country"
  -- Doc and signature for stats:
  ++ ", d.mtime"
  ++ ", d.id as docid"
  ++ ", sl.sign_time"
  ++ ", sl.roles"
  ++ "  FROM users u"
  ++ "  LEFT JOIN companies c ON u.company_id = c.id"
  ++ "  LEFT JOIN signatory_links sl "
  ++ "    ON sl.user_id = u.id"
  ++ "    AND sl.deleted = FALSE"
  ++ "    AND (sl.roles & 255) <> 0"
  ++ "  LEFT JOIN documents d "
  ++ "    ON d.id = sl.document_id"
  ++ "  WHERE u.deleted = FALSE"
  ++ "  ORDER BY u.first_name || ' ' || u.last_name ASC, u.email ASC, userid ASC, docid ASC"

fetchUsersAndStats :: Statement
                   -> IO [(User, Maybe Company, ( Maybe MinutesTime
                                                , Maybe DocumentID
                                                , Maybe MinutesTime
                                                , Maybe [SignatoryRole]))]
fetchUsersAndStats st = fetchValues st decoder
  where
    decoder :: UserID                   -- u.id
            -> Maybe Binary             -- encode(u.password, 'base64')
            -> Maybe Binary             -- encode(u.salt, 'base64')
            -> Bool                     -- u.is_company_admin
            -> Bool                     -- u.account_suspended
            -> Maybe MinutesTime        -- u.has_accepted_terms_of_service
            -> SignupMethod             -- u.signup_method
            -> Maybe ServiceID          -- u.service_id
            -> Maybe CompanyID          -- u.company_id
            -> BS.ByteString            -- u.first_name
            -> BS.ByteString            -- u.last_name
            -> BS.ByteString            -- u.personal_number
            -> BS.ByteString            -- u.company_position
            -> BS.ByteString            -- u.phone
            -> BS.ByteString            -- u.mobile
            -> Email                    -- u.email
            -> Maybe DesignMode         -- u.preferred_design_mode
            -> Lang                     -- u.lang
            -> Region                   -- u.region
            -> Maybe String             -- u.customfooter
            -> Maybe CompanyID          -- c.id AS company_id
            -> Maybe ExternalCompanyID  -- c.external_id
            -> Maybe ServiceID          -- c.service_id
            -> Maybe BS.ByteString      -- c.name
            -> Maybe BS.ByteString      -- c.number
            -> Maybe BS.ByteString      -- c.address
            -> Maybe BS.ByteString      -- c.zip
            -> Maybe BS.ByteString      -- c.city
            -> Maybe BS.ByteString      -- c.country
            -> Maybe MinutesTime        -- d.mtime
            -> Maybe DocumentID         -- d.id as docid
            -> Maybe MinutesTime        -- sl.sign_time
            -> Maybe [SignatoryRole]    -- sl.roles
            -> Either DBException (User, Maybe Company, ( Maybe MinutesTime
                                                        , Maybe DocumentID
                                                        , Maybe MinutesTime
                                                        , Maybe [SignatoryRole]))
    decoder uid
            password
            salt
            is_company_admin
            account_suspended
            has_accepted_terms_of_service
            signup_method
            service_id
            company_id
            first_name
            last_name
            personal_number
            company_position
            phone
            mobile
            email
            preferred_design_mode
            lang
            region
            customfooter
            cid
            eid
            sid
            name
            number
            address
            zip'
            city
            country
            mtime
            docid
            sign_time
            roles = return (
                User {
                       userid = uid
                     , userpassword = case (password, salt) of
                         (Just pwd, Just salt') -> Just Password {
                                                       pwdHash = pwd
                                                     , pwdSalt = salt'
                                                     }
                         _                      -> Nothing
                     , useriscompanyadmin = is_company_admin
                     , useraccountsuspended = account_suspended
                     , userhasacceptedtermsofservice = has_accepted_terms_of_service
                     , usersignupmethod = signup_method
                     , userinfo = UserInfo { userfstname = first_name
                                           , usersndname = last_name
                                           , userpersonalnumber = personal_number
                                           , usercompanyposition = company_position
                                           , userphone = phone
                                           , usermobile = mobile
                                           , useremail = email
                                           }
                     , usersettings = UserSettings { preferreddesignmode = preferred_design_mode
                                                   , locale = mkLocale region lang
                                                   , customfooter = customfooter
                                                   }
                     , userservice = service_id
                     , usercompany = company_id
                     }
        , case cid of
            (Just _) -> Just Company { companyid = fromJust cid
                                     , companyexternalid = eid
                                     , companyservice = sid
                                     , companyinfo = CompanyInfo {
                                           companyname = fromJust name
                                         , companynumber = fromJust number
                                         , companyaddress = fromJust address
                                         , companyzip = fromJust zip'
                                         , companycity = fromJust city
                                         , companycountry = fromJust country
                                         }
                                     }
            _        -> Nothing
        , case docid of
            (Just _) -> (mtime,docid,sign_time,roles)
            _        -> (Nothing, Nothing, Nothing, Nothing)
        )

sumUserStats :: [(User, Maybe Company, ( Maybe MinutesTime
                                       , Maybe DocumentID
                                       , Maybe MinutesTime
                                       , Maybe [SignatoryRole]))]
             -> MinutesTime
             -> [(User, Maybe Company, DocStats)]
sumUserStats [] _ = []
sumUserStats list time = map transform' $ groupBy sameUserID' $ sortWith (\(a,_,_)->a) list
  where
    sameUserID' = (\(user,_,_) (user2,_,_) -> userid user == userid user2)
    transform' []                  = error "sumUserStats: empty list grouped"
    transform' l@((user,mcom,_):_) = snd $ foldl' (\(lastDocId,(u,m,stats)) (_,_,st) ->
                                                      let (i,s) = addToStats' stats st lastDocId
                                                      in (i,(u,m,s)))
                                                  (DocumentID 0,(user,mcom,DocStats 0 0 0 0 0 0 0)) l
    addToStats' :: DocStats
                -> (
                     Maybe MinutesTime
                   , Maybe DocumentID
                   , Maybe MinutesTime
                   , Maybe [SignatoryRole]
                   )
                -> DocumentID
                -> (DocumentID,DocStats)
    addToStats' (DocStats cnt scnt s1cnt s2cnt s3cnt s6cnt s12cnt)
                (Just mtime, Just docid, sign_time, Just role) lastid =
        ( if isJust sign_time then docid else lastid
        , DocStats {
              doccount           = cnt + (if SignatoryAuthor `elem` role then 1 else 0)
            , signaturecount     = if isDiffrentSignedDoc then scnt + 1 else scnt
            , signaturecount1m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 1  mtime then s1cnt  + 1 else s1cnt
            , signaturecount2m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 2  mtime then s2cnt  + 1 else s2cnt
            , signaturecount3m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 3  mtime then s3cnt  + 1 else s3cnt
            , signaturecount6m   = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 6  mtime then s6cnt  + 1 else s6cnt
            , signaturecount12m  = if isDiffrentSignedDoc && isSignedNotLaterThanMonthsAgo 12 mtime then s12cnt + 1 else s12cnt
         })
      where
        isDiffrentSignedDoc = isJust sign_time && docid /= lastid
    addToStats' docstats _ lastid = (lastid, docstats)
    isSignedNotLaterThanMonthsAgo m t = monthsBefore m time < t

data GetUsersAndStats = GetUsersAndStats MinutesTime
instance DBQuery GetUsersAndStats [(User, Maybe Company, DocStats)] where
  dbQuery (GetUsersAndStats time) = wrapDB $ \conn -> do
    st  <- prepare conn $ selectUsersAndStatsSQL
    _   <- executeRaw st
    uas <- fetchUsersAndStats st
    return $ sumUserStats uas time

data SetDocumentModificationData = SetDocumentModificationData DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentModificationData (Either String Document) where
  dbUpdate (SetDocumentModificationData did time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "mtime" time]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDocumentModificationData" r did

