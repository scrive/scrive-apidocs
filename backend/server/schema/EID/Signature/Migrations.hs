module EID.Signature.Migrations (
    addSignatoryIPToEIDSignatures
  , eidSignaturesAddProviderNetsNOBankID
) where

import DB
import EID.Signature.Tables

-- the previous version of the check didn't handle adding new providers
eidSignaturesAddProviderNetsNOBankID :: MonadDB m => Migration m
eidSignaturesAddProviderNetsNOBankID = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable "eid_signatures" $ map
        sqlDropCheck
        ["eid_signatures_ocsp_response_well_defined"]
      runQuery_ $ sqlAlterTable "eid_signatures" $ map
        sqlAddValidCheck
        [ tblCheck
            { chkName      = "eid_signatures_ocsp_response_well_defined"
            , chkCondition =
              "(provider <= 3 OR provider >= 6) AND ocsp_response IS NULL OR (provider = 4 OR provider = 5) AND ocsp_response IS NOT NULL"
            }
        ]
  }

addSignatoryIPToEIDSignatures :: MonadDB m => Migration m
addSignatoryIPToEIDSignatures = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableEIDSignatures)
                       [ sqlAddColumn $ tblColumn { colName     = "signatory_ip"
                                                  , colType     = TextT
                                                  , colNullable = True
                                                  }
                       ]
  }
