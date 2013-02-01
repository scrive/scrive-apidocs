-- | Precog event processor for the async event machinery.
--
--   Events are logged to /$UID/events/$NAME, properties to /$UID/properties
--   and documents to /$UID/documents/$DOCID. Due to the way Precog works,
--   we get a history of all historical properties and documents, kept as an
--   array.
module ThirdPartyStats.Precog where
import Control.Monad.IO.Class
import Data.Monoid
import Data.String
import Text.JSON
import ThirdPartyStats.Core
import ThirdPartyStats.Utils
import Precog.Ingest as Precog
import Precog.Result
import MinutesTime
import User.Model (unEmail)
import Doc.DocumentID (fromDocumentID)
import Company.CompanyID (fromCompanyID)
import Data.Time.Clock.POSIX

processPrecogEvent :: MonadIO m
                   => PrecogCredentials
                   -> EventName
                   -> [EventProperty]
                   -> m ProcRes
processPrecogEvent creds (NamedEvent evt) props
  | Just (uid, props') <- extractUID props = do
    let mcid = extractCompanyID props'
        company = maybe "nocompany" (\(cid, _) -> toPath cid) mcid
        path = mconcat [company,toPath uid,"events",fromString $ noSpaces evt]
    res <- liftIO $ asyncIngest creds path Nothing [jsonObj props']
    case res of
      HTTPError   reason -> return (Failed reason)
      PrecogError reason -> return (Failed reason)
      Success            -> return OK
  | otherwise = do
    return (Failed "Tried to track Precog event without user ID!")
processPrecogEvent creds SetUserProps props
  | Just (uid, props') <- extractUID props = do
    let mcid = extractCompanyID props'
        company = maybe "nocompany" (\(cid, _) -> toPath cid) mcid
        path = mconcat [company, toPath uid, "properties"]
    res <- liftIO $ asyncIngest creds path Nothing [jsonObj props']
    case res of
      HTTPError   reason -> return (Failed reason)
      PrecogError reason -> return (Failed reason)
      Success            -> return OK
  | otherwise = do
    return (Failed "Tried to set Precog user props without user ID!")
processPrecogEvent creds (UploadDocInfo docjson) props = do
  let mact = do
        (uid, props') <- extractUID props
        (did, _) <- extractDocID props'
        let mcid = extractCompanyID props'
            company = maybe "nocompany" (\(cid, _) -> toPath cid) mcid
            path = mconcat [company, toPath uid, "documents", toPath did]
        return $ asyncIngest creds path Nothing [toJSObject [("doc", docjson)]]
  case mact of
    Just act -> do
      res <- liftIO act
      case res of
        HTTPError   reason -> return (Failed reason)
        PrecogError reason -> return (Failed reason)
        Success            -> return OK
    _ ->
      return (Failed "Tried to send doc to precog without doc or user ID!")

-- | Replace all spaces in a string with underscores.
noSpaces :: String -> String
noSpaces = map spaceToUnderscore
  where
    spaceToUnderscore ' ' = '_'
    spaceToUnderscore c   = c

jsonObj :: [EventProperty] -> JSObject JSValue
jsonObj = toJSObject . map jsonProperty

jsonProperty :: EventProperty -> (String, JSValue)
jsonProperty (MailProp mail) = ("email", JSString $ toJSString $ unEmail mail)
jsonProperty (IPProp ip)     = ("ip", JSString $ toJSString $ show ip)
jsonProperty (NameProp name) = ("name", JSString $ toJSString name)
jsonProperty (TimeProp t)    = ("time", timeToJSON t)
jsonProperty (UserIDProp _)  = error "User ID prop in the wrong place!"
jsonProperty (DocIDProp did) = ("docid", showJSON (fromDocumentID did))
jsonProperty (CompanyIDProp cid) = ("companyid", showJSON (fromCompanyID cid))
jsonProperty (SomeProp k v)  = (k, toJSONVal v)
  where
    toJSONVal (PVNumber d)      = showJSON d
    toJSONVal (PVString str)    = JSString $ toJSString str
    toJSONVal (PVBool b)        = JSBool b
    toJSONVal (PVMinutesTime t) = timeToJSON t

timeToJSON :: MinutesTime -> JSValue
timeToJSON t = showJSON $ (round $ utcTimeToPOSIXSeconds $ toUTCTime t :: Int)
