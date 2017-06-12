module Administration.Invoicing
    ( InvoicingReport(..)
    , uploadInvoicing) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Int
import Data.Time.Calendar
import Data.Time.Clock
import Database.PostgreSQL.PQTypes
import Log
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BSL

import Chargeable.Model
import DB
import Doc.Data.Document (DocumentType(..))
import Doc.Data.DocumentStatus (DocumentStatus(..))
import KontraPrelude
import Log.Utils
import MinutesTime
import Partner.Model (PartnerID)
import SFTPConfig
import Util.CSVUtil (CSV(..), renderCSV)
import Utils.Directory (withSystemTempDirectory')
import Utils.IO (sftpTransfer)

uploadInvoicing :: forall m. (MonadDB m, MonadLog m, MonadIO m, MonadBaseControl IO m) =>
                SFTPConfig
                -> m ()
uploadInvoicing sftpConfig = do
    logInfo_ "Generating invoicing report"
    report <- dbQuery . InvoicingReport =<< currentTime
    let csvReport = renderCSV $ csvHeader report : csvContent report
    withSystemTempDirectory' "invoicing_reports" $ \tmpDir -> do
      let filePath = tmpDir </> (csvFilename report)
      liftIO $ BSL.writeFile filePath csvReport
      logInfo_ "Sending invoicing report to sftp server"
      (exitCode, _stdout, stderr) <-  sftpTransfer sftpConfig filePath
      case exitCode of
        ExitFailure ec1 -> do
          logInfo "SFTP transfer of invoicing failed" $ object [
              "curl_exitcode" .= show ec1
            , "stderr" `equalsExternalBSL`  stderr
            ]
        _ -> logInfo_ "SFTP transfer of invoicing finished"


data InvoicingReport = InvoicingReport UTCTime
instance (MonadDB m) => DBQuery m InvoicingReport CSV where
  query (InvoicingReport time) = do
    let periodFrom = beginingOfMonthUTC . (daysBefore 1) $ time
    let periodTo = beginningOfDay time
    runSQL_ $ invoicingQry periodFrom periodTo
    csvRows <- fetchMany fetchInvoicing
    return CSV {
        csvHeader = csvColumnHeadings
      , csvContent = csvRows
      , csvFilename = "invoicing_report_" <>
          (filter (/= '-') (showGregorian . utctDay $ periodFrom)) <>
          "_" <>
          (filter (/= '-') (showGregorian . utctDay $ periodTo)) <>
          ".csv"
      }

csvColumnHeadings :: [String]
csvColumnHeadings = [
    "Company name"
  , "Company ID"
  , "Company number"
  , "Partner ID"
  , "Partner name"
  , "Company admin"
  , "Payment plan"
  , "First doc signed"
  , "All docs"
  , "Docs sent"
  , "Docs closed"
  , "Sigs closed"
  , "SMSes sent"
  , "SMSes sent (physical)"
  , "Swedish BankID signatures"
  , "Swedish BankID authorization"
  , "Norwegian BankID authorization"
  , "Danish NemID authorization"
  , "Telia SMSes sent (physical)"
  , "Users at start of period"
  , "Users at end of period"
  , "Users activated during period"
  , "Users deleted during period"
  , "Start date"
  , "End date"
  ]

-- The outer `SELECT` is to be able to name the result and refer to its
-- constituent columns in a `WHERE`-clause even if they are the result of
-- a correlated subquery.
invoicingQry :: UTCTime -> UTCTime -> SQL
invoicingQry fromDate toDate =
    "SELECT * FROM (" <+>
      "SELECT companies.name AS \"Company name\"" <+>
      ", (companies.id :: TEXT) AS \"Company ID\"" <+>
      ", (companies.number :: TEXT) AS \"Company number\"" <+>
      ", (companies.partner_id) AS \"Partner ID\"" <+>
      ", ((" <+>
        "SELECT name" <+>
          "FROM partners" <+>
        "WHERE partners.id = companies.partner_id" <+>
        ") :: TEXT) AS \"Partner name\"" <+>
      ", ((" <+>
        "SELECT email" <+>
          "FROM users" <+>
        "WHERE users.is_company_admin" <+>
          "AND users.company_id = companies.id" <+>
        "LIMIT 1" <+>
        ") :: TEXT) AS \"Company admin\"" <+>
      ", (CASE companies.payment_plan" <+>
          "WHEN 0 THEN 'free'" <+>
          "WHEN 1 THEN 'one'" <+>
          "WHEN 2 THEN 'team'" <+>
          "WHEN 3 THEN 'enterprise'" <+>
          "WHEN 4 THEN 'trial'" <+>
        "END :: TEXT) AS PaymentPlan" <+>
      ", (SELECT MIN(d.mtime)::timestamptz" <+>
          "FROM documents d" <+>
          "JOIN signatory_links sl ON d.author_id = sl.id" <+>
          "JOIN users u            ON sl.user_id = u.id" <+>
          "WHERE d.type = " <?> Signable <+>
            "AND d.status = " <?> Closed <+>
            "AND u.company_id = companies.id" <+>
        ") AS \"First doc signed\"" <+>
      ", (SELECT count(*)" <+>
          "FROM documents" <+>
          "WHERE EXISTS (SELECT TRUE" <+>
                          "FROM signatory_links" <+>
                          "JOIN users ON users.id = signatory_links.user_id" <+>
                        "WHERE documents.author_id = signatory_links.id" <+>
                          "AND users.company_id = companies.id)) AS \"All docs\"" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CIStartingDocument <+>
            "AND chi.time >=" <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> " ) AS \"Docs sent\"" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CIClosingDocument <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS DocsClosed" <+>
       ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
            "FROM chargeable_items chi" <+>
           "WHERE chi.company_id = companies.id" <+>
             "AND chi.type = " <?> CIClosingSignature <+>
             "AND chi.time >= " <?> fromDate <+>
             "AND chi.time < " <?> toDate <+> ") AS SigsClosed" <+>
      ", (SELECT count(*)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CISMS <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS SMSSent" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CISMS <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS SMSSentPhysical" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CISEBankIDSignature <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS SwedishBankIDSignatures" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CISEBankIDAuthentication <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS SwedishBankIDAuthorization" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CINOBankIDAuthentication <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS NorwegianBankIDAuthorization" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CIDKNemIDAuthentication <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS DanishNemIDAuthorization" <+>
      ", (SELECT coalesce(sum(chi.quantity), 0)" <+>
          "FROM chargeable_items chi" <+>
          "WHERE chi.company_id = companies.id" <+>
            "AND chi.type = " <?> CISMSTelia <+>
            "AND chi.time >= " <?> fromDate <+>
            "AND chi.time < " <?> toDate <+> ") AS \"Telia SMSes sent (physical)\"" <+>
      ", (SELECT count(*)" <+>
          "FROM users" <+>
          "WHERE (users.deleted IS NULL OR users.deleted > " <?> fromDate <+> ")" <+>
            "AND users.email NOT LIKE '%@scrive.com'" <+>
            "AND users.company_id = companies.id" <+>
            "AND users.has_accepted_terms_of_service <= " <?> fromDate <+> ") AS UsersStart" <+>
      ", (SELECT count(*)" <+>
          "FROM users" <+>
          "WHERE (users.deleted IS NULL OR users.deleted > " <?> toDate <+> ")" <+>
            "AND users.email NOT LIKE '%@scrive.com'" <+>
            "AND users.company_id = companies.id" <+>
            "AND users.has_accepted_terms_of_service < " <?> toDate <+> ") AS UsersEnd" <+>
      ", (SELECT count(*)" <+>
          "FROM users" <+>
          "WHERE users.company_id = companies.id" <+>
            "AND users.email NOT LIKE '%@scrive.com'" <+>
            "AND has_accepted_terms_of_service >= " <?> fromDate <+>
            "AND has_accepted_terms_of_service < " <?> toDate <+> ") AS \"Users activated during period\"" <+>
      ", (SELECT count(*)" <+>
          "FROM users" <+>
          "WHERE users.company_id = companies.id" <+>
            "AND users.email NOT LIKE '%@scrive.com'" <+>
            "AND users.deleted >= " <?> fromDate <+>
            "AND users.deleted < " <?> toDate <+> ") AS \"Users deleted during period\"" <+>
  ", (SELECT " <?> fromDate <+> ") AS DateFrom" <+>
  ", (SELECT " <?> toDate <+> ") AS DateTo" <+>
  "FROM companies" <+>
  ") as report" <+>
  "WHERE report.PaymentPlan <> 'free'" <+>
  "AND (" <+>
        "report.SigsClosed > 0" <+>
    "OR report.DocsClosed > 0" <+>
    "OR report.SMSSent > 0" <+>
    "OR report.SwedishBankIDSignatures > 0" <+>
    "OR report.SwedishBankIDAuthorization > 0" <+>
    "OR report.NorwegianBankIDAuthorization > 0" <+>
    "OR report.DanishNemIDAuthorization > 0" <+>
    "OR report.UsersStart > 0" <+>
    "OR report.UsersEnd > 0" <+>
  ")" <+>
  "ORDER BY 1, 2;"

fetchInvoicing :: ( String
                  , String
                  , String
                  , PartnerID
                  , String
                  , String
                  , String
                  , UTCTime
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , Int64
                  , UTCTime
                  , UTCTime
                  )
               -> [String]
fetchInvoicing ( companyName
               , companyId
               , companyNumber
               , partnerId
               , partnerName
               , companyAdmin
               , paymentPlan
               , firstDocSigned
               , allDocs
               , docsSent
               , docsClosed
               , sigsClosed
               , smsSent
               , smsSentPhysical
               , seBankIDSigs
               , seBankIDAuths
               , noBankIDAuths
               , dkNemIDAuths
               , smsSentPhysicalTelia
               , usersAtStart
               , usersAtEnd
               , usersActivated
               , usersDeleted
               , tFrom
               , tTo
               ) =
               [ companyName
               , companyId
               , companyNumber
               , show partnerId
               , partnerName
               , companyAdmin
               , paymentPlan
               , showGregorian . utctDay $ firstDocSigned
               , show allDocs
               , show docsSent
               , show docsClosed
               , show sigsClosed
               , show smsSent
               , show smsSentPhysical
               , show seBankIDSigs
               , show seBankIDAuths
               , show noBankIDAuths
               , show dkNemIDAuths
               , show smsSentPhysicalTelia
               , show usersAtStart
               , show usersAtEnd
               , show usersActivated
               , show usersDeleted
               , showGregorian . utctDay $ tFrom
               , showGregorian . utctDay $ tTo
               ]
