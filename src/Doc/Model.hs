{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-orphans -fcontext-stack=50 -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP #-}

module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
  , isShared -- fromUtils
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
  , GetDeletedDocumentsByCompany(..)
  , GetDeletedDocumentsByUser(..)
  , GetDocumentByDocumentID(..)
  , GetDocumentStats(..)
  , GetDocumentStatsByUser(..)
  , GetDocuments(..)
  , GetDocumentsByAuthor(..)
  , GetDocumentsByCompany(..)
  , GetDocumentsByCompanyAndTags(..)
  , GetDocumentsBySignatory(..)
  , GetDocumentsByUser(..)
  , GetDocumentsSharedInCompany(..)
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
  , RestartDocument(..)
  , RestoreArchivedDocument(..)
  , SaveDocumentForUser(..)
  , SaveSigAttachment(..)
  , SetCSVSigIndex(..)
  , SetDaysToSign(..)
  , SetDocumentAdvancedFunctionality(..)
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
  , ShareDocument(..)
  , SignDocument(..)
  , SignLinkFromDetailsForTest(..)
  , SignableFromDocument(..)
  , SignableFromDocumentIDWithUpdatedAuthor(..)
  , StoreDocumentForTesting(..)
  , TemplateFromDocument(..)
  , TimeoutDocument(..)
  , UpdateFields(..)
  , UpdateSigAttachments(..)
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
import qualified Data.ByteString.UTF8 as BS
import qualified Mails.MailsUtil as Mail
import Data.Maybe
import Misc
import Data.Convertible
import Data.List
import Data.Int
import Data.Word
import Mails.MailsUtil
import Doc.Tables
import Control.Applicative
import Util.SignatoryLinkUtils
--import Doc.DocStateUtils
import Doc.DocProcess
import Doc.DocStateCommon
import qualified AppLogger as Log
import System.Random
--import Happstack.Server
--import Happstack.State
--import Happstack.Util.Common
--import Numeric
--import Data.Int
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Exception as E
import File.Model

data SqlField = SqlField String String SqlValue

instance Convertible SqlValue SqlValue where
    safeConvert = return . id

sqlField :: (Convertible v SqlValue) => String -> v -> SqlField
sqlField name value = SqlField name "" (toSql value)

sqlFieldType :: (Convertible v SqlValue) => String -> String -> v -> SqlField
sqlFieldType name xtype value = SqlField name xtype (toSql value)

sqlLogAppend :: MinutesTime -> String -> SqlField
sqlLogAppend time text = sqlFieldType "log" "append" $ show $ DocumentLogEntry time $ BS.fromString text

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
                   , checkEqualBy "documentcsvupload" documentcsvupload
                   , checkEqualBy "documentcancelationreason" documentcancelationreason
                   , checkEqualBy "documentsharing" documentsharing
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
                    -> Maybe Int32
                    -> [DocumentLogEntry]
                    -> BS.ByteString
                    -> [IdentificationType]
                    -> Maybe BS.ByteString
                    -> Maybe [[BS.ByteString]]
                    -> Maybe Int
                    -> Maybe CancelationReason
                    -> DocumentSharing
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
                    csv_title
                    csv_contents
                    csv_signatory_index
                    cancelationreason
                    sharing
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
                                                                        Just t -> Just (SignInfo t (maybe 0 fromIntegral invite_ip))
                                               , documentlog = dlog
                                               , documentinvitetext = invite_text
                                               , documentallowedidtypes = allowed_id_types
                                               , documentcsvupload = case (csv_title, csv_contents, csv_signatory_index) of
                                                                       (Just t, Just c, Just si) -> Just (CSVUpload t c si)
                                                                       _ -> Nothing
                                               , documentcancelationreason = cancelationreason
                                               , documentsharing = sharing
                                               , documentrejectioninfo = case (rejection_time, rejection_signatory_link_id, rejection_reason) of
                                                                           (Just t, Just r, Just sl) -> Just (t, r, sl)
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
                           , "csv_title"
                           , "csv_contents"
                           , "csv_signatory_index"
                           , "cancelation_reason"
                           , "sharing"
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
                                , "deleted"
                                , "really_deleted"
                                ]

selectSignatoryLinksSQL :: String
selectSignatoryLinksSQL = "SELECT " ++ concat (intersperse "," selectSignatoryLinksSelectors) ++ " FROM signatory_links "

decodeRowAsSignatoryLinkWithDocumnetID :: SignatoryLinkID
                         -> DocumentID
                         -> Maybe UserID
                         -> Maybe CompanyID
                         -> [SignatoryField]
                         -> SignOrder
                         -> MagicHash
                         -> Maybe MinutesTime
                         -> Maybe Int32
                         -> Maybe MinutesTime
                         -> Maybe Int32
                         -> Maybe MinutesTime
                         -> Mail.MailsDeliveryStatus
                         -> Maybe String
                         -> Maybe String
                         -> Maybe String
                         -> Maybe SignatureProvider
                         -> Maybe Bool
                         -> Maybe Bool
                         -> Maybe Bool
                         -> [SignatoryRole]
                         -> Bool
                         -> Bool
                         -> Either DBException (DocumentID,SignatoryLink)                        
decodeRowAsSignatoryLinkWithDocumnetID slid
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
                             (Just st, Just sip) -> Just (SignInfo st (fromIntegral sip))
                             _ -> Nothing
    , maybeseeninfo      = case (seen_time, seen_ip) of
                             (Just st, Just sip) -> Just (SignInfo st (fromIntegral sip))
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
    })) :: Either DBException (DocumentID,SignatoryLink)         

    
decodeRowAsSignatoryLink :: SignatoryLinkID
                         -> DocumentID
                         -> Maybe UserID
                         -> Maybe CompanyID
                         -> [SignatoryField]
                         -> SignOrder
                         -> MagicHash
                         -> Maybe MinutesTime
                         -> Maybe Int32
                         -> Maybe MinutesTime
                         -> Maybe Int32
                         -> Maybe MinutesTime
                         -> Mail.MailsDeliveryStatus
                         -> Maybe String
                         -> Maybe String
                         -> Maybe String
                         -> Maybe SignatureProvider
                         -> Maybe Bool
                         -> Maybe Bool
                         -> Maybe Bool
                         -> [SignatoryRole]
                         -> Bool
                         -> Bool
                         -> Either DBException SignatoryLink    
decodeRowAsSignatoryLink slid
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
                         deleted
                         really_deleted =
  snd <$> decodeRowAsSignatoryLinkWithDocumnetID  slid
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
                         deleted
                         really_deleted


fetchSignatoryLinks :: Statement -> IO [SignatoryLink]
fetchSignatoryLinks st = do
  fetchValues st decodeRowAsSignatoryLink

fetchSignatoryLinksWithDocuments :: Statement -> IO [(DocumentID,SignatoryLink)]
fetchSignatoryLinksWithDocuments st = do
  fetchValues st decodeRowAsSignatoryLinkWithDocumnetID
  
insertSignatoryLinkAsIs :: DocumentID -> SignatoryLink -> DB (Maybe SignatoryLink)
insertSignatoryLinkAsIs documentid link = do
  let toSigned :: Word32 -> Int32 
      toSigned = fromIntegral

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
                            , sqlField "sign_ip" $ (toSigned . signipnumber) `fmap` maybesigninfo link
                            , sqlField "seen_time" $ signtime `fmap` maybeseeninfo link
                            , sqlField "seen_ip" $ (toSigned . signipnumber) `fmap` maybeseeninfo link
                            , sqlField "read_invitation" $ maybereadinvite link
                            , sqlField "invitation_delivery_status" $ invitationdeliverystatus link
                            , sqlField "signinfo_text" $ signatureinfotext `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_signature" $ signatureinfosignature `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_certificate" $ signatureinfocertificate `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_provider" $ signatureinfoprovider `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_first_name_verified" $ signaturefstnameverified `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_last_name_verified" $ signaturelstnameverified `fmap` signatorysignatureinfo link
                            , sqlField "signinfo_personal_number_verified" $ signaturepersnumverified `fmap` signatorysignatureinfo link
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
    let toSigned :: Word32 -> Int32 
        toSigned = fromIntegral
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
                 , documentcsvupload
                 , documentcancelationreason
                 , documentsharing
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
                                     , sqlField "invite_ip" (fmap (toSigned . signipnumber) documentinvitetime)
                                     , sqlField "invite_text" documentinvitetext
                                     , sqlField "log" documentlog
                                     , sqlField "allowed_id_types" documentallowedidtypes
                                     , sqlField "csv_title" $ csvtitle `fmap` documentcsvupload
                                     , sqlField "csv_contents" $ csvcontents `fmap` documentcsvupload
                                     , sqlField "csv_signatory_index" $ csvsignatoryindex `fmap` documentcsvupload
                                     , sqlField "cancelation_reason" documentcancelationreason
                                     , sqlField "sharing" documentsharing
                                     , sqlField "rejection_time" $ fst3 `fmap` documentrejectioninfo
                                     , sqlField "rejection_signatory_link_id" $ snd3 `fmap` documentrejectioninfo
                                     , sqlField "rejection_reason" $ thd3 `fmap` documentrejectioninfo
                                     , sqlField "service_id" documentservice
                                     , sqlField "deleted" documentdeleted
                                     -- , toSql documentauthorattachments      -- many to many
                                     -- , toSql documentsignatoryattachments   -- many to many
                                     , sqlField "mail_footer" $ documentmailfooter $ documentui  -- should go into separate table?
                                     , sqlField "region" documentregion
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
data AdminOnlySaveForUser = AdminOnlySaveForUser DocumentID User
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AdminOnlySaveForUser (Either String Document) where
  dbUpdate (AdminOnlySaveForUser did user) = do
    r <- runUpdateStatement "signatory_links" 
                       [ sqlField "company_id" $ usercompany user
                       ]
                       ("WHERE document_id = ? " ++
                        " AND user_id = ? ")
                       [ toSql did
                       , toSql (userid user)
                       ]
                        
    getOneDocumentAffected "AdminOnlySaveForUser" r did

data ArchiveDocument = ArchiveDocument User DocumentID
                         deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocument (Either String Document) where
  dbUpdate (ArchiveDocument user did) = do
    r <- case (usercompany user, useriscompanyadmin user) of
           (Just cid, True) ->
              runUpdateOnArchivableDoc "WHERE company_id = ?" [toSql cid]
           _ ->
              runUpdateOnArchivableDoc "WHERE user_id = ?" [toSql $ userid user]
    getOneDocumentAffected "ArchiveDocument" r did
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


data AttachCSVUpload = AttachCSVUpload DocumentID CSVUpload
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachCSVUpload (Either String Document) where
  dbUpdate (AttachCSVUpload did csvupload) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot PreparationToPending document " ++ show did ++ " because it does not exist"
      Just document -> do
        let msigindex = checkCSVSigIndex
                    (documentsignatorylinks document)
                    (csvsignatoryindex csvupload)
        case (msigindex, documentstatus document) of
          (Left s, _) -> return $ Left s
          (Right _, Preparation) -> do
                     r <- runUpdateStatement "documents"
                          [ {- sqlField "mtime" time
                          , -} sqlField "csv_title" $ csvtitle csvupload
                          , sqlField "csv_signatory_index" $ csvsignatoryindex csvupload
                          , sqlField "csv_contents" $ show $ csvcontents csvupload
                          ]
                         "WHERE id = ? AND status = ? AND deleted = FALSE" [ toSql did, toSql Preparation ]
                     getOneDocumentAffected "AttachCSVUpload" r did

          _ -> return $ Left $ "Document #" ++ show documentid ++ " is in " ++ show (documentstatus document) ++ " state, must be"


data AttachFile = AttachFile DocumentID FileID MinutesTime
                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachFile (Either String Document) where
  dbUpdate (AttachFile did fid time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "mtime" time
         , sqlField "file_id" $ fid
         , sqlLogAppend time ("Attached main file " ++ show fid)
         ]
         "WHERE id = ? AND status = ?" [ toSql did, toSql Preparation ]
    getOneDocumentAffected "AttachFile" r did

data AttachSealedFile = AttachSealedFile DocumentID FileID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachSealedFile (Either String Document) where
  dbUpdate (AttachSealedFile did fid time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "mtime" $ time
         , sqlField "sealed_file_id" $ fid
         , sqlLogAppend time ("Attached sealed file " ++ show fid)
         ]
         "WHERE id = ? AND status = ?" [ toSql did, toSql Closed ]
    getOneDocumentAffected "AttachSealedFile" r did

data CancelDocument = CancelDocument DocumentID CancelationReason MinutesTime Word32
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CancelDocument (Either String Document) where
  dbUpdate (CancelDocument did reason mtime ipaddress) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID did
    case mdocument of
      Nothing -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because it does not exist"
      Just document ->
        case checkCancelDocument document of
          [] -> do
            r <- runUpdateStatement "documents"
                 [ sqlField "status" $ Canceled
                 , sqlField "mtime" mtime
                 , sqlField "cancelation_reason" $ reason
                 , sqlLogAppend mtime ("Document canceled from " ++ formatIP ipaddress)
                 ]
                "WHERE id = ? AND type = ?" [ toSql did, toSql (toDocumentSimpleType (Signable undefined)) ]
            getOneDocumentAffected "CancelDocument" r did

            -- return $ Right $ document { documentstatus = Closed
            --                          , documentmtime  = time
            --                          } `appendHistory` [DocumentHistoryClosed time ipaddress]
          s -> return $ Left $ "Cannot CancelDocument document " ++ show did ++ " because " ++ concat s

data ChangeMainfile = ChangeMainfile DocumentID FileID
instance DBUpdate ChangeMainfile (Either String Document) where
  dbUpdate (ChangeMainfile did fid) = do
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
        getOneDocumentAffected "ChangeMainfile" r did
    where
        allHadSigned doc = all (hasSigned ||^ (not . isSignatory)) $ documentsignatorylinks doc

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) BS.ByteString
                                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ChangeSignatoryEmailWhenUndelivered (Either String Document) where
  dbUpdate (ChangeSignatoryEmailWhenUndelivered did slid muser email) = do
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    let setEmail signatoryfields = 
         map (\sf -> case sfType sf of
                               EmailFT -> sf { sfValue = email }
                               _       -> sf) signatoryfields

    let signlinks = documentsignatorylinks doc
        Just sl = find ((== slid) . signatorylinkid) signlinks

    r <- runUpdateStatement "signatory_links" 
                       [ sqlField "invitation_delivery_status" Unknown
                       , sqlField "fields" $ setEmail $ signatoryfields $ signatorydetails sl
                       , sqlField "user_id" $ fmap userid muser
                       , sqlField "company_id" $ muser >>= usercompany
                       ]
                       ("WHERE EXISTS (SELECT * FROM documents WHERE documents.id = signatory_links.document_id AND (documents.status = ? OR documents.status = ?))" ++ 
                        " AND document_id = ? " ++
                        " AND id = ? ")
                       [ toSql Pending, toSql AwaitingAuthor
                       , toSql did
                       , toSql slid
                       ]
                        
    getOneDocumentAffected "ChangeSignatoryEmailWhenUndelivered" r did

data PreparationToPending = PreparationToPending DocumentID MinutesTime
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PreparationToPending (Either String Document) where
  dbUpdate (PreparationToPending docid time) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPreparationToPending document of
          [] -> do
            r <- runUpdateStatement "documents"
                 [ sqlField "status" $ Pending
                 , sqlField "mtime" time
                 , sqlField "timeout_time" $ (\days -> (days * 24 *60) `minutesAfter` time) <$> documentdaystosign document
                 , sqlLogAppend time ("Document put into Pending state")
                 ]
                "WHERE id = ? AND type = ?" [ toSql docid, toSql (toDocumentSimpleType (Signable undefined))]
            getOneDocumentAffected "PreparationToPending" r docid
          s -> return $ Left $ "Cannot PreparationToPending document " ++ show docid ++ " because " ++ concat s


data CloseDocument = CloseDocument DocumentID MinutesTime Word32
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CloseDocument (Either String Document) where
  dbUpdate (CloseDocument docid time ipaddress) = do
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
            getOneDocumentAffected "CloseDocument" r docid

            -- return $ Right $ document { documentstatus = Closed
            --                          , documentmtime  = time
            --                          } `appendHistory` [DocumentHistoryClosed time ipaddress]
          s -> return $ Left $ "Cannot CloseDocument " ++ show docid ++ " because " ++ concat s

data DeleteSigAttachment = DeleteSigAttachment DocumentID BS.ByteString FileID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteSigAttachment (Either String Document) where
  dbUpdate (DeleteSigAttachment did email fid) = do
    r <- runUpdateStatement "signatory_attachments"
                         [ sqlField "file_id" SqlNull
                         ]
                         "WHERE document_id = ? AND email = ? AND file_id = ?"
                         [ toSql did
                         , toSql email
                         , toSql fid
                         ]
    getOneDocumentAffected "DeleteSigAttachment" r did

data DocumentFromSignatoryData = DocumentFromSignatoryData DocumentID Int BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString [BS.ByteString]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DocumentFromSignatoryData (Either String Document) where
  dbUpdate (DocumentFromSignatoryData docid sigindex fstname sndname email company personalnumber companynumber fieldvalues) = do
        newFromDocument toNewDoc docid
   where
    toNewDoc :: Document -> Document
    toNewDoc d = d { documentsignatorylinks = map snd . map toNewSigLink . zip [0..] $ (documentsignatorylinks d)
                    , documentcsvupload = Nothing
                    , documentsharing = Private
                    , documenttype = newDocType $ documenttype d
                    , documentsignatoryattachments = map replaceCSV (documentsignatoryattachments d)
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
    toNewSigLink :: (Int, SignatoryLink) -> (Int, SignatoryLink)
    toNewSigLink (i, sl)
      | i==sigindex = (i, pumpData sl)
      | otherwise = (i, sl)
    pumpData :: SignatoryLink -> SignatoryLink
    pumpData siglink = replaceSignatoryData siglink fstname sndname email company personalnumber companynumber fieldvalues

data ErrorDocument = ErrorDocument DocumentID String
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ErrorDocument (Either String Document) where
  dbUpdate (ErrorDocument docid errmsg) = do
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
            getOneDocumentAffected "ErrorDocument" r docid

          s -> return $ Left $ "Cannot ErrorDocument document " ++ show docid ++ " because " ++ concat s

data GetDeletedDocumentsByCompany = GetDeletedDocumentsByCompany User
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByCompany [Document] where
  dbQuery (GetDeletedDocumentsByCompany user) = do
    case useriscompanyadmin user of
      True ->
        selectDocuments (selectDocumentsSQL ++ " WHERE EXISTS (SELECT * FROM signatory_links WHERE signatory_links.deleted = TRUE AND company_id = ? AND really_deleted = FALSE AND service_id = ? AND documents.id = document_id) ORDER BY mtime DESC")
                      [ toSql (usercompany user)
                      , toSql (userservice user)
                      ]
      False -> return []

data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser User
                                 deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByUser [Document] where
  dbQuery (GetDeletedDocumentsByUser user) = do
    selectDocuments (selectDocumentsSQL ++
                     " WHERE EXISTS (SELECT * FROM signatory_links WHERE signatory_links.deleted = TRUE AND really_deleted = FALSE AND user_id = ? AND documents.id = document_id) ORDER BY mtime DESC")
                      [ toSql (userid user)
                      ]

selectDocuments :: String -> [SqlValue] -> DB [Document]
selectDocuments select values = wrapDB $ \conn -> do
    docs <- (do
              st <- prepare conn select
              _ <- execute st values
              fetchDocuments st) `E.catch` handle select     
    sls <- (do
               stx <- prepare conn $ "SELECT " ++ concat (intersperse "," selectSignatoryLinksSelectors) ++ " FROM signatory_links WHERE document_id IN (SELECT id FROM ("++select++") AS doc)"
               _ <- execute stx values
               fetchSignatoryLinksWithDocuments stx) `E.catch` handle select                      
    ats <- (do
               stx <- prepare conn $ "SELECT " ++ concat (intersperse "," selectAuthorAttachmentsSelectors) ++ " FROM author_attachments WHERE document_id IN (SELECT id FROM ("++select++") AS doc)"
               _ <- execute stx values
               fetchAuthorAttachmentsWithDocumentID stx) `E.catch` handle select                      
    sas <- (do
               stx <- prepare conn $ "SELECT " ++ concat (intersperse "," selectSignatoryAttachmentsSelectors) ++ " FROM signatory_attachments WHERE document_id IN (SELECT id FROM ("++select++") AS doc)"
               _ <- execute stx values
               fetchSignatoryAttachmentsWithDocumentID stx) `E.catch` handle select                      
    return $ joinDocuments3 (joinDocuments2 (joinDocuments docs sls) ats) sas
  where
    handle :: String -> SqlError -> IO a
    handle statement e = E.throwIO $ SQLError { DB.Classes.originalQuery = statement
                                              , queryParams = values
                                              , sqlError = e
                                              }
    joinDocuments::[Document] -> [(DocumentID,SignatoryLink)] -> [Document]
    joinDocuments docs ((did,s):sl) = joinDocuments (addToOne docs did s) sl
    joinDocuments docs [] = docs
    addToOne [] did s = []
    addToOne (d:ds) did s = if (documentid d == did)
                             then (d {documentsignatorylinks = documentsignatorylinks d ++ [s] } : ds)
                             else d:(addToOne ds did s)

    joinDocuments2::[Document] -> [(DocumentID,AuthorAttachment)] -> [Document]
    joinDocuments2 docs ((did,s):sl) = joinDocuments2 (addToOne2 docs did s) sl
    joinDocuments2 docs [] = docs
    addToOne2 [] did s = []
    addToOne2 (d:ds) did s = if (documentid d == did)
                             then (d {documentauthorattachments = documentauthorattachments d ++ [s] } : ds)
                             else d:(addToOne2 ds did s)

    joinDocuments3::[Document] -> [(DocumentID,SignatoryAttachment)] -> [Document]
    joinDocuments3 docs ((did,s):sl) = joinDocuments3 (addToOne3 docs did s) sl
    joinDocuments3 docs [] = docs
    addToOne3 [] did s = []
    addToOne3 (d:ds) did s = if (documentid d == did)
                             then (d {documentsignatoryattachments = documentsignatoryattachments d ++ [s] } : ds)
                             else d:(addToOne3 ds did s)
                             
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
  docs <- dbQuery $ GetDocumentsByUser user
  sigdocs <- dbQuery $ GetDocumentsBySignatory user
  let signaturecount' = length $ allsigns
      signaturecount1m' = length $ filter (isSignedNotLaterThanMonthsAgo 1) $ allsigns
      signaturecount2m' = length $ filter (isSignedNotLaterThanMonthsAgo 2) $ allsigns
      signaturecount3m' = length $ filter (isSignedNotLaterThanMonthsAgo 3) $ allsigns
      signaturecount6m' = length $ filter (isSignedNotLaterThanMonthsAgo 6) $ allsigns
      signaturecount12m' = length $ filter (isSignedNotLaterThanMonthsAgo 12) $ allsigns
      timeMonthsAgo m = (-m * 30 * 24 * 60) `minutesAfter` time
      isSignedNotLaterThanMonthsAgo m = (timeMonthsAgo m <) . documentmtime
      allsigns = filter (isSigned . relevantSigLink) sigdocs
      relevantSigLink :: Document -> Maybe SignatoryLink
      relevantSigLink doc = listToMaybe $ filter (isSigLinkFor $ userid user) (documentsignatorylinks doc)
      isSigned :: Maybe SignatoryLink -> Bool
      isSigned = maybe False (isJust . maybesigninfo)
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

data GetDocumentsByCompany = GetDocumentsByCompany User
                             deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompany [Document] where
  dbQuery (GetDocumentsByCompany user) = do
    case (useriscompanyadmin user, usercompany user) of
      (True, Just companyid) -> do
        docs <- selectDocumentsBySignatoryLink ("signatory_links.company_id = ?")
                      [ toSql (usercompany user)
                      ]
        return $ filterDocsWhereActivated companyid . filterDocsWhereDeleted False companyid $ docs
      _ -> return []

{- |
    Fetches documents by company and tags, this won't return documents that have been deleted (so ones
    that would appear in the recycle bin//trash can.)  It also makes sure to respect the sign order in
    cases where the company is linked via a signatory that hasn't yet been activated.
-}
data GetDocumentsByCompanyAndTags = GetDocumentsByCompanyAndTags (Maybe ServiceID) CompanyID [DocumentTag]
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompanyAndTags [Document] where
  dbQuery (GetDocumentsByCompanyAndTags mservice companyid doctags) = do
        docs <- selectDocumentsBySignatoryLink ("signatory_links.deleted = FALSE AND signatory_links.company_id = ?")
                      [ toSql companyid
                      ]
        let docs' = filterDocsWhereActivated companyid . filterDocsWhereDeleted False companyid $ docs
        return docs' -- (filter hasTags docs')
    where hasTags doc = not (null (intersect (documenttags doc) doctags))

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
    docs <- selectDocumentsBySignatoryLink ("signatory_links.deleted = FALSE AND signatory_links.user_id = ? AND ((signatory_links.roles & ?)<>0)")
                                     [toSql (userid user), {- toSql [SignatoryPartner] -} iToSql 255]
    return $ filterDocsWhereActivated (userid user) docs

{- |
    All documents which are saved for the user which have never been deleted.
    This doesn't respect sign order, so should be used carefully.
    This also makes sure that the documents match the user's service.
-}
data GetDocumentsByUser = GetDocumentsByUser User
                          deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByUser [Document] where
  dbQuery (GetDocumentsByUser user) = do
        selectDocumentsBySignatoryLink ("signatory_links.deleted = FALSE AND signatory_links.user_id = ? AND ((signatory_links.roles & ?)<>0)")
                                         [toSql (userid user), toSql [SignatoryAuthor]]

data GetDocumentsSharedInCompany = GetDocumentsSharedInCompany User
                                   deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsSharedInCompany [Document] where
  dbQuery (GetDocumentsSharedInCompany User{usercompany, userservice}) = do
    case usercompany of
      Just companyid -> do
        documents <- selectDocuments (selectDocumentsSQL ++
                                      " WHERE deleted = FALSE" ++
                                      "   AND sharing = ?" ++
                                      "   AND service_id = ?" ++
                                      "   AND EXISTS (SELECT 1 FROM signatory_links WHERE document_id = id AND company_id = ?) ORDER BY mtime DESC")
                       [ toSql Shared
                       , toSql (userservice)
                       , toSql companyid
                       ]

        return $ filter ((== Shared) . documentsharing) . filterDocsWhereActivated companyid . filterDocsWhereDeleted False companyid $ documents
      _ -> return []

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

data MarkDocumentSeen = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash MinutesTime Word32
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkDocumentSeen (Either String Document) where
  dbUpdate (MarkDocumentSeen did signatorylinkid1 mh time ipnumber) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "seen_time" time
                         , sqlField "seen_ip" ipnumber
                         ]
                         "WHERE id = ? AND document_id = ? AND token = ? AND EXISTS (SELECT * FROM documents WHERE id = ? AND type = ? AND status <> ? AND status <> ?)"
                         [ toSql signatorylinkid1
                         , toSql did
                         , toSql mh
                         , toSql did
                         , toSql (toDocumentSimpleType (Signable undefined))
                         , toSql Preparation
                         , toSql Closed
                         ]
    getOneDocumentAffected "MarkDocumentSeen" r did

data AddInvitationEvidence = AddInvitationEvidence DocumentID SignatoryLinkID MinutesTime Word32
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddInvitationEvidence (Either String Document) where
  dbUpdate (AddInvitationEvidence docid _slid _time _ipnumber) = do
  -- modifySignable docid $ \document ->
  -- case checkAddEvidence document slid of
  --  [] -> let Just sds = signatorydetails <$> getSigLinkFor document slid
  --        in Right $ document { documentinvitetime = Just (SignInfo time ipnumber) }
  --           `appendHistory` [DocumentHistoryInvitationSent time ipnumber [sds]]
  --  s -> Left $ "Document " ++ show documentid ++ " cannot have evidence attached for signatory " ++ show slid ++ " because " ++ concat s
    mdoc <- dbQuery $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Left "no such document"
      Just doc -> return $ Right doc




data MarkInvitationRead = MarkInvitationRead DocumentID SignatoryLinkID MinutesTime
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkInvitationRead (Either String Document) where
  dbUpdate (MarkInvitationRead did linkid time) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "read_invitation" time
                         ]
                         "WHERE id = ? AND document_id = ? AND read_invitation = NULL"
                         [ toSql linkid
                         , toSql did
                         ]
    getOneDocumentAffected "MarkInvitationRead" r did

data NewDocument = NewDocument User (Maybe Company) BS.ByteString DocumentType MinutesTime
                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate NewDocument (Either String Document) where
  dbUpdate (NewDocument user mcompany title documenttype ctime) = do
  if fmap companyid mcompany /= usercompany user
    then return $ Left "company and user don't match"
    else do
      wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
      did <- DocumentID <$> getUniqueID tableDocuments

      let authorRoles = if ((Just True) == getValueForProcess documenttype processauthorsend)
                        then [SignatoryAuthor]
                        else [SignatoryPartner, SignatoryAuthor]
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO $ MagicHash <$> randomRIO (0,maxBound)

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
             Just doc' -> return $ Right doc'
             Nothing -> do
                        Log.debug $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
                        return $ Left $ "insertDocumentAsIs could not insert document #" ++ show (documentid doc) ++ " in NewDocument"
        Just a -> do
           Log.debug $ "insertDocumentAsIs invariants violated: " ++ show a
           return $ Left $ "insertDocumentAsIs invariants violated: " ++ show a



data ReallyDeleteDocument = ReallyDeleteDocument User DocumentID
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ReallyDeleteDocument (Either String Document) where
  dbUpdate (ReallyDeleteDocument user did) = do
    r <- case (usercompany user, useriscompanyadmin user) of
           (Just cid, True) ->
             runUpdateOnDeletableDoc "WHERE company_id = ?" [toSql cid]
           _ ->
             runUpdateOnDeletableDoc "WHERE user_id = ? AND company_id is NULL" [toSql $ userid user]
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

data RejectDocument = RejectDocument DocumentID SignatoryLinkID MinutesTime Word32 (Maybe BS.ByteString)
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RejectDocument (Either String Document) where
  dbUpdate (RejectDocument docid slid time ipnumber customtext) =do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkRejectDocument document slid of
          [] ->
            do
              r <- runUpdateStatement "documents"
                                               [ sqlField "status" Rejected
                                               , sqlField "mtime" time
                                               , sqlField "rejection_time" time
                                               , sqlField "rejection_reason" customtext
                                               , sqlField "rejection_signatory_link_id" slid
                                               , sqlLogAppend time ("Document rejected")
                                               ]
                                               "WHERE id = ?" [toSql docid]
              getOneDocumentAffected "RejectDocument" r docid
          s -> return $ Left $ "Cannot RejectDocument document " ++ show docid ++ " because " ++ concat s

data RestartDocument = RestartDocument Document User MinutesTime Word32
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestartDocument (Either String Document) where
  dbUpdate (RestartDocument doc user time ipnumber) = do
    mndoc <- tryToGetRestarted
    case mndoc of
      Right newdoc -> newFromDocument (const newdoc) (documentid doc)
      other -> return other
   where
    tryToGetRestarted :: DB (Either String Document)
    tryToGetRestarted =
      if (documentstatus doc `notElem` [Canceled, Timedout, Rejected])
      then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
      else if (not $ isAuthor (doc, user))
           then return $ Left $ "Can't restart document if you are not it's author"
           else do
             doc' <- clearSignInfofromDoc
             let doc'' = doc' `appendHistory` [DocumentHistoryRestarted time ipnumber]
             return $ Right doc''
    clearSignInfofromDoc :: DB Document
    clearSignInfofromDoc = do
      let signatoriesDetails = map (\x -> (signatorydetails x, signatoryroles x, signatorylinkid x)) $ documentsignatorylinks doc
          Just asl = getAuthorSigLink doc
      newSignLinks <- flip mapM signatoriesDetails $ do \(a,b,c) -> do
                                                             magichash <- liftIO $ MagicHash <$> randomRIO (0,maxBound)

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

data RestoreArchivedDocument = RestoreArchivedDocument User DocumentID
                                deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestoreArchivedDocument (Either String Document) where
  dbUpdate (RestoreArchivedDocument user did) = do
    r <- case (usercompany user, useriscompanyadmin user) of
           (Just cid, True) ->
             runUpdateOnRestorableDoc "WHERE company_id = ?" [toSql cid]
           _ ->
             runUpdateOnRestorableDoc "WHERE user_id = ?" [toSql $ userid user]
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
data SaveDocumentForUser = SaveDocumentForUser DocumentID User SignatoryLinkID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveDocumentForUser (Either String Document) where
  dbUpdate (SaveDocumentForUser did User{userid, usercompany} slid) = do
    r <- runUpdateStatement "signatory_links"
                          [ sqlField "user_id" userid
                          , sqlField "company_id" usercompany
                          ]
                          ("WHERE document_id = ? AND id = ?")
                          [ toSql did
                          , toSql slid
                          ]
    getOneDocumentAffected "SaveDocumentForUser" r did

{- |
    Saves a signatory attachment to a document.
    If there's a problem such as the document isn't in a pending or awaiting author state,
    or the document does not exist a Left is returned.
-}
data SaveSigAttachment = SaveSigAttachment DocumentID BS.ByteString BS.ByteString FileID
                         deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveSigAttachment (Either String Document) where
  dbUpdate (SaveSigAttachment did name email fid) = do
    r <- runUpdateStatement "signatory_attachments"
                         [ sqlField "file_id" fid
                         ]
                         "WHERE document_id = ? AND email = ? AND file_id IS NULL AND name = ? "
                         [ toSql did
                         , toSql email
                         , toSql name
                         ]
    getOneDocumentAffected "SaveSigAttachment" r did


data SetDocumentTags = SetDocumentTags DocumentID [DocumentTag]
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTags (Either String Document) where
  dbUpdate (SetDocumentTags did doctags) = do
    r <- runUpdateStatement "documents"
         [ sqlField "tags" $ doctags
         -- , sqlField "mtime" time
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDocumentTags" r did


data SetDocumentTimeoutTime = SetDocumentTimeoutTime DocumentID MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTimeoutTime (Either String Document) where
  dbUpdate (SetDocumentTimeoutTime did timeouttime) = do
    r <- runUpdateStatement "documents"
                         [ sqlField "timeout_time" timeouttime
                         ]
                         "WHERE id = ? AND deleted = FALSE AND type = ?"
                         [ toSql did
                         , toSql (toDocumentSimpleType (Signable undefined))
                         ]
    getOneDocumentAffected "SetDocumentTimeoutTime" r did

data SetSignatoryCompany = SetSignatoryCompany DocumentID SignatoryLinkID CompanyID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryCompany (Either String Document) where
  dbUpdate (SetSignatoryCompany did slid cid) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "company_id" cid
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    getOneDocumentAffected "SetSignatoryCompany" r did

data RemoveSignatoryCompany = RemoveSignatoryCompany DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryCompany (Either String Document) where
  dbUpdate (RemoveSignatoryCompany did slid) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "company_id" SqlNull
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    getOneDocumentAffected "RemoveSignatoryCompany" r did

data SetSignatoryUser = SetSignatoryUser DocumentID SignatoryLinkID UserID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryUser (Either String Document) where
  dbUpdate (SetSignatoryUser did slid uid) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "user_id" uid
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    getOneDocumentAffected "SetSignatoryUser" r did

data RemoveSignatoryUser = RemoveSignatoryUser DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryUser (Either String Document) where
  dbUpdate (RemoveSignatoryUser did slid) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "user_id" SqlNull
                         ]
                         "WHERE id = ? AND document_id = ?"
                         [ toSql slid
                         , toSql did
                         ]
    getOneDocumentAffected "RemoveSignatoryUser" r did

data SetInviteText = SetInviteText DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInviteText (Either String Document) where
  dbUpdate (SetInviteText did text time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "invite_text" $ text
         , sqlField "mtime" time
         , sqlLogAppend time ("Invite text set")
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetInviteText" r did

data SetDaysToSign = SetDaysToSign DocumentID Int MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDaysToSign (Either String Document) where
  dbUpdate (SetDaysToSign did days time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "days_to_sign" $ days
         , sqlField "mtime" time
         , sqlLogAppend time ("Days to sign set to " ++ show days)
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDaysToSign" r did

data RemoveDaysToSign = RemoveDaysToSign DocumentID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDaysToSign (Either String Document) where
  dbUpdate (RemoveDaysToSign did time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "days_to_sign" $ SqlNull
         , sqlField "mtime" time
         , sqlLogAppend time ("Removed days to sign")
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "RemoveDaysToSign" r did

data SetDocumentAdvancedFunctionality = SetDocumentAdvancedFunctionality DocumentID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentAdvancedFunctionality (Either String Document) where
  dbUpdate (SetDocumentAdvancedFunctionality did time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "functionality" AdvancedFunctionality
         , sqlField "mtime" time
         , sqlLogAppend time ("Document changed to advanced functionality")
         ]
         "WHERE id = ? AND functionality <> ?" [ toSql did, toSql AdvancedFunctionality ]
    getOneDocumentAffected "SetDocumentAdvancedFunctionality" r did


data SetDocumentTitle = SetDocumentTitle DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTitle (Either String Document) where
  dbUpdate (SetDocumentTitle did doctitle time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "title" $ doctitle
         , sqlField "mtime" time
         , sqlLogAppend time ("Document title changed")
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDocumentTitle" r did

data SetDocumentLocale = SetDocumentLocale DocumentID Locale MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentLocale (Either String Document) where
  dbUpdate (SetDocumentLocale did locale time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "region" $ getRegion locale
         , sqlField "mtime" time
         , sqlLogAppend time ("Document locale changed")
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDocumentLocale" r did

data SetDocumentUI = SetDocumentUI DocumentID DocumentUI
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentUI (Either String Document) where
  dbUpdate (SetDocumentUI did documentui) = do
    r <- runUpdateStatement "documents"
         [ sqlField "mail_footer" $ documentmailfooter documentui
         -- , sqlField "mtime" time
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetDocumentUI" r did


data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus DocumentID SignatoryLinkID Mail.MailsDeliveryStatus
                                   deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInvitationDeliveryStatus (Either String Document) where
  dbUpdate (SetInvitationDeliveryStatus did slid status) = do
    r <- runUpdateStatement "signatory_links"
                         [ sqlField "invitation_delivery_status" status
                         ]
                         "WHERE id = ? AND document_id = ? AND EXISTS (SELECT * FROM documents WHERE id = ? AND type = ?)"
                         [ toSql slid
                         , toSql did
                         , toSql did
                         , toSql (toDocumentSimpleType (Signable undefined))
                         ]
    getOneDocumentAffected "SetInvitationDeliveryStatus" r did


data ShareDocument = ShareDocument DocumentID
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ShareDocument (Either String Document) where
  dbUpdate (ShareDocument did) = do
    r <- runUpdateStatement "documents"
         [ sqlField "sharing" $ Shared
         ]
         "WHERE id = ? AND deleted = FALSE" [ toSql did ]
    getOneDocumentAffected "ShareDocument" r did


data SignDocument = SignDocument DocumentID SignatoryLinkID MagicHash MinutesTime Word32 (Maybe SignatureInfo)
                    deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignDocument (Either String Document) where
  dbUpdate (SignDocument docid slid mh time ipnumber msiginfo) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkSignDocument document slid mh of
          [] -> do
            r <- runUpdateStatement "signatory_links"
                      [ sqlField "sign_ip" $ ipnumber
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
            getOneDocumentAffected "SignDocument" r docid
          s -> return $ Left $ "Cannot SignDocument document " ++ show docid ++ " because " ++ concat s


data ResetSignatoryDetails = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] MinutesTime
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ResetSignatoryDetails (Either String Document) where
  dbUpdate (ResetSignatoryDetails documentid signatories time) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID documentid
    case mdocument of
      Nothing -> return $ Left "document does not exist"
      Just document ->
        case checkResetSignatoryData document signatories of
          [] -> do

            wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
            wrapDB $ \conn -> run conn "DELETE FROM signatory_links WHERE document_id = ?" [toSql documentid]

            let mauthorsiglink = getAuthorSigLink document
            flip mapM signatories $ \(details, roles) -> do
                     linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

                     magichash <- liftIO $ MagicHash <$> randomRIO (0,maxBound)

                     let link' = signLinkFromDetails' details roles linkid magichash
                         link = if isAuthor link'
                                then link' { maybesignatory = maybe Nothing maybesignatory mauthorsiglink
                                           , maybecompany   = maybe Nothing maybecompany   mauthorsiglink
                                           }
                                else link'
                     r1 <- runInsertStatement "signatory_links"
                           [ sqlField "id" $ signatorylinkid link
                           , sqlField "document_id" documentid
                           , sqlField "user_id" $ maybesignatory link
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
                           , sqlField "deleted" $ signatorylinkdeleted link
                           , sqlField "really_deleted" $ signatorylinkreallydeleted link
                           ]
                     when (r1 /= 1) $
                          error "ResetSignatoryDetails signatory_links did not manage to insert a row"

            Just newdocument <- dbQuery $ GetDocumentByDocumentID documentid
            return $ Right newdocument

          s -> return $ Left $ "cannot reset signatory details on document " ++ show documentid ++ " because " ++ intercalate ";" s


data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignLinkFromDetailsForTest SignatoryLink where
  dbUpdate (SignLinkFromDetailsForTest details roles) = do
      wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
      linkid <- SignatoryLinkID <$> getUniqueID tableSignatoryLinks

      magichash <- liftIO $ MagicHash <$> randomRIO (0,maxBound)

      let link = signLinkFromDetails' details
                        roles linkid magichash

      return link

data SignableFromDocument = SignableFromDocument Document
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocument Document where
  dbUpdate (SignableFromDocument document) = do
    insertNewDocument $ templateToDocument document


data SignableFromDocumentIDWithUpdatedAuthor = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID MinutesTime
                                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocumentIDWithUpdatedAuthor (Either String Document) where
  dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid time) =
      if fmap companyid mcompany /= usercompany user
        then return $ Left "company and user don't match"
        else do
          (flip newFromDocument) docid $ \doc ->
            (templateToDocument doc) {
                                 documentsignatorylinks = map replaceAuthorSigLink (documentsignatorylinks doc)
                                -- FIXME: Need to remove authorfields?
                                , documentctime = time
                                }
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
data TemplateFromDocument = TemplateFromDocument DocumentID
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TemplateFromDocument (Either String Document) where
  dbUpdate (TemplateFromDocument did) = do
    r <- runUpdateStatement "documents"
         [ sqlField "status" Preparation
         , sqlField "type" $ toDocumentSimpleType (Template undefined)
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "TemplateFromDocument" r did


data TimeoutDocument = TimeoutDocument DocumentID MinutesTime
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TimeoutDocument (Either String Document) where
  dbUpdate (TimeoutDocument did time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "status" $ Timedout
         , sqlField "mtime" time
         , sqlLogAppend time ("Document timed out")
         ]
         "WHERE id = ? AND type = ? AND status = ?" [ toSql did
                                                    , toSql (toDocumentSimpleType (Signable undefined))
                                                    , toSql Pending
                                                    ]
    getOneDocumentAffected "TimeoutDocument" r did

data SetCSVSigIndex = SetCSVSigIndex DocumentID Int MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetCSVSigIndex (Either String Document) where
  dbUpdate (SetCSVSigIndex did csvsigindex time) = do
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    case documentfunctionality doc of
      BasicFunctionality -> return $ Left $ "Cannot set csvindex on basic functionality document " ++ show did
      AdvancedFunctionality -> 
        case documentcsvupload doc of
          Nothing -> return $ Left $ "There is no csv upload for document " ++ show did
          Just cu -> 
            case checkCSVSigIndex (documentsignatorylinks doc) csvsigindex of
              Left s -> return $ Left s
              Right i -> do
                r <- runUpdateStatement "documents"
                     [ sqlField "csv_signatory_index" $ csvsigindex
                     , sqlField "mtime" time
                     ]
                    "WHERE id = ?" [ toSql did ]
                getOneDocumentAffected "SetCSVSigIndex" r did

data SetEmailIdentification = SetEmailIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetEmailIdentification (Either String Document) where
  dbUpdate (SetEmailIdentification did time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "allowed_id_types" $ [EmailIdentification]
         , sqlField "mtime" time
         , sqlLogAppend time ("Email identification set")
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetEmailIdentification" r did

data SetElegitimationIdentification = SetElegitimationIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetElegitimationIdentification (Either String Document) where
  dbUpdate (SetElegitimationIdentification did time) = do
    r <- runUpdateStatement "documents"
         [ sqlField "allowed_id_types" $ [ELegitimationIdentification]
         , sqlField "mtime" time
         , sqlLogAppend time ("E-leg identification set")
         ]
         "WHERE id = ?" [ toSql did ]
    getOneDocumentAffected "SetElegitimationIdentification" r did


data UpdateFields  = UpdateFields DocumentID SignatoryLinkID [(BS.ByteString, BS.ByteString)]
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateFields (Either String Document) where
  dbUpdate (UpdateFields did slid fields) = do
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

      let signlinks = documentsignatorylinks document
          Just sl = find ((== slid) . signatorylinkid) signlinks

      r <- runUpdateStatement "signatory_links" 
                       [ sqlField "fields" $ map updateSigField $ signatoryfields $ signatorydetails sl
                       ]
                       ("WHERE EXISTS (SELECT * FROM documents WHERE documents.id = signatory_links.document_id AND (documents.status = ? OR documents.status = ?))" ++ 
                        " AND document_id = ? " ++
                        " AND id = ? ")
                       [ toSql Pending, toSql AwaitingAuthor
                       , toSql did
                       , toSql slid
                       ]
                        
      getOneDocumentAffected "ChangeSignatoryEmailWhenUndelivered" r did

    s -> return $ Left $ "Cannot updateFields on document " ++ show did ++ " because " ++ concat s

data PendingToAwaitingAuthor = PendingToAwaitingAuthor DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PendingToAwaitingAuthor (Either String Document) where
  dbUpdate (PendingToAwaitingAuthor docid time) = do
    mdocument <- dbQuery $ GetDocumentByDocumentID docid
    case mdocument of
      Nothing -> return $ Left $ "Cannot PendingToAwaitingAuthor document " ++ show docid ++ " because it does not exist"
      Just document ->
        case checkPendingToAwaitingAuthor document of
          [] -> do
            r <- runUpdateStatement "documents"
                 [ sqlField "status" $ AwaitingAuthor
                 , sqlField "mtime" time
                 , sqlLogAppend time ("Changed to AwaitingAuthor status")
                 ]
                "WHERE id = ? AND type = ?" [ toSql docid, toSql (toDocumentSimpleType (Signable undefined)) ]
            getOneDocumentAffected "PendingToAwaitingAuthor" r docid

            -- return $ Right $ document { documentstatus = Closed
            --                          , documentmtime  = time
            --                          } `appendHistory` [DocumentHistoryClosed time ipaddress]
          s -> return $ Left $ "Cannot PendingToAwaitingAuthor document " ++ show docid ++ " because " ++ concat s



data AddDocumentAttachment = AddDocumentAttachment DocumentID FileID
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddDocumentAttachment (Either String Document) where
  dbUpdate (AddDocumentAttachment did fid) = do
            r <- wrapDB $ \conn -> do
                   run conn ("INSERT INTO author_attachments(document_id, file_id) (SELECT ?, ? WHERE" ++
                         " EXISTS (SELECT TRUE FROM documents WHERE id = ? AND status = ?))")
                      [ toSql did
                      , toSql fid
                      , toSql did
                      , toSql Preparation
                      ]
            getOneDocumentAffected "AddDocumentAttachment" r did

data RemoveDocumentAttachment = RemoveDocumentAttachment DocumentID FileID
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDocumentAttachment (Either String Document) where
  dbUpdate (RemoveDocumentAttachment did fid) = do
            r <- wrapDB $ \conn -> run conn "DELETE FROM author_attachments WHERE document_id = ? AND file_id = ? AND EXISTS (SELECT TRUE FROM documents WHERE id = ? AND status = ?)"
                      [ toSql did
                      , toSql fid
                      , toSql did
                      , toSql Preparation
                      ]
            m <- dbQuery $ GetDocumentByDocumentID did
            case m of
              Just doc -> case documentstatus doc of
                            Preparation -> return $ Right doc
                            _ -> return $ Left "bad document status"
              Nothing -> return $ Left "no such document"


data UpdateSigAttachments = UpdateSigAttachments DocumentID [SignatoryAttachment] MinutesTime
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateSigAttachments (Either String Document) where
  dbUpdate (UpdateSigAttachments did sigatts time) = do
    wrapDB $ \conn -> run conn "DELETE FROM signatory_attachments WHERE document_id = ?" [toSql did]
    flip mapM sigatts doInsert
    getOneDocumentAffected "UpdateSigAttachments" 1 did
    where
         doInsert (SignatoryAttachment { signatoryattachmentfile
                                       , signatoryattachmentemail
                                       , signatoryattachmentname
                                       , signatoryattachmentdescription
                                       }) = do
           r <- runInsertStatement "signatory_attachments"
                [ sqlField "file_id" $ signatoryattachmentfile
                , sqlField "email" $ signatoryattachmentemail
                , sqlField "name" $ signatoryattachmentname
                , sqlField "description" $ signatoryattachmentdescription
                , sqlField "document_id" $ did
                ]
           return r
