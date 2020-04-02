module Doc.Sealing.Consumer (
    DocumentSealing
  , documentSealing
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Aeson
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class
import System.Timeout.Lifted

import BrandedDomain.Model
import DB
import DB.PostgreSQL
import Doc.Action
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Sealing.Model
import EventStream.Class
import FileStorage
import GuardTime
import Log.Identifier
import MailContext
import PdfToolsLambda.Conf
import PdfToolsLambda.Monad
import Templates
import User.Lang
import qualified MailContext.Internal as I

data DocumentSealing = DocumentSealing {
    dsDocumentID      :: !DocumentID
  , dsBrandedDomainID :: !BrandedDomainID
  , dsAttempts        :: !Int32
  }

documentSealing
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFail m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadEventStream m
     )
  => GuardTimeConf
  -> PdfToolsLambdaEnv
  -> KontrakcjaGlobalTemplates
  -> ConnectionSourceM m
  -> Text
  -> Int
  -> ConsumerConfig m DocumentID DocumentSealing
documentSealing guardTimeConf pdfToolsLambdaEnv templates pool mailNoreplyAddress maxRunningJobs
  = ConsumerConfig
    { ccJobsTable           = "document_sealing_jobs"
    , ccConsumersTable      = "document_sealing_consumers"
    , ccJobSelectors        = ["id", "branded_domain_id", "attempts"]
    , ccJobFetcher = \(did, bdid, attempts) -> DocumentSealing { dsDocumentID = did
                                                               , dsBrandedDomainID = bdid
                                                               , dsAttempts = attempts
                                                               }
    , ccJobIndex            = dsDocumentID
    , ccNotificationChannel = Just documentSealingNotificationChannel
    , ccNotificationTimeout = 60 * 1000000 -- 1 minute
    , ccMaxRunningJobs      = maxRunningJobs
    , ccProcessJob          =
      \docsealing@DocumentSealing {..} -> do
        logInfo "Document sealing started" $ object [identifier dsDocumentID]
        mres <- timeout fiveMins . withPostgreSQL pool . withDocumentID dsDocumentID $ do
          logInfo_ "Document lock acquired"
          now0 <- currentTime
          bd   <- dbQuery $ GetBrandedDomainByID dsBrandedDomainID
          doc  <- theDocument
          let lang = getLang doc
              mc   = I.MailContext { lang               = lang
                                   , brandedDomain      = bd
                                   , time               = now0
                                   , mailNoreplyAddress = mailNoreplyAddress
                                   }
          logInfo_ "Running postDocumentClosedActions"
          resultisok <-
            runGuardTimeConfT guardTimeConf
            . runPdfToolsLambdaT pdfToolsLambdaEnv
            . runTemplatesT (lang, templates)
            . runMailContextT mc
            $ postDocumentClosedActions True False

          if resultisok
            then return $ Ok Remove
            else Failed <$> onFailure Nothing docsealing
        case mres of
          Nothing  -> fail "Document sealing timed out"
          Just res -> do
            logInfo_ "Document sealing finished"
            return res
    , ccOnException         = onFailure . Just
    }
  where
    fiveMins :: Int
    fiveMins = 5 * 60 * 1000000

    onFailure (mex :: Maybe SomeException) DocumentSealing {..} = do
      when (dsAttempts > 1) $ do
        logAttention "Document sealing failed more than 1 time"
          $ object
              [ identifier dsDocumentID
              , "attempt_count" .= dsAttempts
              , "exception" .= show mex
              ]
      return . RerunAfter . attemptToDelay $ dsAttempts

    attemptToDelay 1 = iminutes 5
    attemptToDelay 2 = iminutes 10
    attemptToDelay 3 = iminutes 30
    attemptToDelay 4 = ihours 1
    attemptToDelay 5 = ihours 2
    attemptToDelay 6 = ihours 4
    attemptToDelay 7 = ihours 8
    attemptToDelay 8 = ihours 16
    attemptToDelay _ = idays 1
