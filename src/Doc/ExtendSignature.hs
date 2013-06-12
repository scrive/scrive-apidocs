module Doc.ExtendSignature
  ( extendSignatures
  , sealMissingSignatures
  ) where

import ActionQueue.Scheduler (SchedulerData, getGlobalTemplates, sdAppConf)
import Amazon (AmazonMonad)
import AppConf (guardTimeConf, hostpart, mailsConfig, brandedDomains)
import BrandedDomains (findBrandedDomain, bdurl)
import Context (MailContext(..))
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG (CryptoRNG)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import DB (getMinutesTime, MonadDB, dbQuery)
import Doc.Action (sendClosedEmails)
import Doc.DocSeal (digitallySealDocument, digitallyExtendDocument)
import Doc.DocStateData (Document(..), DocumentStatus(..))
import Doc.DocUtils (documentsealedfileM)
import Doc.Model (GetDocuments(..), DocumentDomain(..), DocumentFilter(..), GetDocumentByDocumentID(..))
import Doc.SealStatus (SealStatus(..))
import User.Model (GetUserByEmail(..), Email(..), userassociateddomain)
import File.File (filename)
import File.Storage (getFileContents)
import IPAddress (noIP)
import qualified Log
import MinutesTime (MinutesTime, toCalendarTimeInUTC, fromClockTime)
import System.FilePath ((</>))
import System.Time (toClockTime, CalendarTime(..), Month(..))
import Templates (runTemplatesT)
import User.Lang (getLang)
import Util.HasSomeUserInfo (getEmail)
import Util.SignatoryLinkUtils (getAuthorSigLink)
import Utils.Directory (withSystemTempDirectory')

latest_publication_time :: MonadDB m => m MinutesTime
latest_publication_time = do
  now <- toCalendarTimeInUTC `fmap` getMinutesTime
  let fifteenth = now{ ctDay = 15, ctHour = 0, ctMin = 0, ctSec = 0, ctPicosec = 0 }
  return $ fromClockTime $ toClockTime $
    if now > fifteenth
    then fifteenth
    else if ctMonth fifteenth == January
         then fifteenth{ ctYear = pred (ctYear fifteenth)
                       , ctMonth = December
                       }
         else fifteenth{ ctMonth = pred (ctMonth fifteenth)}


sealMissingSignatures :: (CryptoRNG m, MonadIO m, AmazonMonad m, Log.MonadLog m, MonadDB m, MonadBaseControl IO m, MonadReader SchedulerData m)
                      => m ()
sealMissingSignatures = do
  docs <- dbQuery $ GetDocuments [DocumentsOfWholeUniverse]
            [ DocumentFilterStatuses [Closed]
            , DocumentFilterBySealStatus [Missing]
            ] [] (0,100)
  forM_ docs $ \doc -> do
    case documentsealstatus doc of
      Just Missing -> sealDocument doc
      _ -> return ()

extendSignatures :: (MonadBaseControl IO m, MonadReader SchedulerData m, CryptoRNG m, AmazonMonad m, MonadDB m) => m ()
extendSignatures = do
  lpt <- latest_publication_time
  Log.debug $ "extendSignatures: latest publication time is " ++ show lpt
  docs <- dbQuery $ GetDocuments [DocumentsOfWholeUniverse]
            [ DocumentFilterStatuses [Closed]
            , DocumentFilterByLatestSignTimeBefore lpt
            , DocumentFilterBySealStatus
              [ Guardtime{ extended = False, private = True }
              , Guardtime{ extended = False, private = False }
              ]
            ] [] (0,100)
  let f [] = return ()
      f (d:ds) = case documentsealstatus d of
                   Just (Guardtime{ extended = False }) -> do
                     ok <- extendDocumentSeal d
                     if ok then do
                       f ds
                      else do
                       Log.debug $ "Guardtime extension of " ++ show (documentid d) ++ " not possible - skipping the remanining documents in queue."
                   _ -> f ds
  f docs

sealDocument :: (CryptoRNG m, MonadIO m, AmazonMonad m, MonadReader SchedulerData m, MonadBaseControl IO m, MonadDB m)
             => Document -> m ()
sealDocument doc = do
  Just file <- documentsealedfileM doc
  withSystemTempDirectory' ("ExtendSignature-" ++ show (documentid doc) ++ "-") $ \tmppath -> do
    content <- getFileContents file
    let mainpath = tmppath </> "main.pdf"
    liftIO $ BS.writeFile mainpath content
    now <- getMinutesTime
    appConf <- asks sdAppConf
    let gtconf = guardTimeConf appConf
    templates <- getGlobalTemplates
    _ <- flip runReaderT templates $ digitallySealDocument False now gtconf (documentid doc) mainpath (filename file)
    Just doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)
    case documentsealstatus doc' of
      Just Guardtime{} -> do
        mauthor <- maybe (return Nothing) (dbQuery . GetUserByEmail) $
                   Email . getEmail <$> getAuthorSigLink doc'
        let mbd = flip findBrandedDomain (brandedDomains appConf) =<< userassociateddomain =<< mauthor
        let mctx = MailContext { mctxhostpart = fromMaybe (hostpart appConf) (bdurl <$> mbd)
                               , mctxmailsconfig = mailsConfig appConf
                               , mctxlang = documentlang doc'
                               , mctxcurrentBrandedDomain = mbd
                               , mctxipnumber = noIP
                               , mctxtime = now
                               , mctxmaybeuser = Nothing
                               }
        runTemplatesT (getLang doc', templates) $ sendClosedEmails mctx doc' True
      s -> Log.error $ "Still failing to seal document # " ++ show (documentid doc') ++ ": " ++ show s
    return ()
    -- TODO: re-send the document to signatories saying that this is a corrected version

extendDocumentSeal :: (MonadBaseControl IO m, MonadReader SchedulerData m, CryptoRNG m, MonadDB m, AmazonMonad m)
                   => Document -> m Bool
extendDocumentSeal doc = do
  Just file <- documentsealedfileM doc
  withSystemTempDirectory' ("ExtendSignature-" ++ show (documentid doc) ++ "-") $ \tmppath -> do
    content <- getFileContents file
    let sealedpath = tmppath </> "sealed.pdf"
    liftIO $ BS.writeFile sealedpath content
    now <- getMinutesTime
    gtconf <- asks (guardTimeConf . sdAppConf)
    templates <- getGlobalTemplates
    flip runReaderT templates $ digitallyExtendDocument now gtconf (documentid doc) sealedpath (filename file)
    -- Here, we have the option of notifying signatories of the
    -- extended version.  However: customers that choose to create an
    -- account will be able to access the extended version in their
    -- archive, and we consider that sufficient at the moment.
    --
    -- We may consider putting in an option in a user's account
    -- settings where the user can opt in to get extended versions
    -- mailed.
    --
    -- We can also provide a service /extend (similar to /verify), by
    -- which signatories can get their documents extended.  Or the
    -- /verify service can detect and provide an extended version if
    -- the verified document was extensible.
