module EID.Signature.Migrations (eidSignaturesAddProviderNetsNOBankID) where

import DB
import EID.Signature.Tables

-- the previous version of the check didn't handle adding new providers
eidSignaturesAddProviderNetsNOBankID :: MonadDB m => Migration m
eidSignaturesAddProviderNetsNOBankID = Migration {
    mgrTableName = tblName tableEIDSignatures
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "eid_signatures"  $ map sqlDropCheck  [
          Check "eid_signatures_ocsp_response_well_defined" ""
        ]
      runQuery_ $ sqlAlterTable "eid_signatures"  $ map sqlAddCheck [
          Check "eid_signatures_ocsp_response_well_defined" $
            "(provider <= 3 OR provider >= 6) AND ocsp_response IS NULL OR (provider = 4 OR provider = 5) AND ocsp_response IS NOT NULL"
        ]
  }
