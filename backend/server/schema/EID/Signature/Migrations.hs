module EID.Signature.Migrations (
    addSignatoryIPToEIDSignatures
  , eidSignaturesAddProviderNetsNOBankID
  , addSignatoryDobAndEmailToEIDSignatures
  , dropEmailFromEIDSignatures
  , addEidJson
  , eidSignaturesAddProviderEIDServiceSEBankID
  , removeEidJson
  , addFieldsForVerimi
  , eidSignaturesUpdatePersonalNumberCheck
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
      runQuery_ . sqlAlterTable "eid_signatures" $ map
        sqlDropCheck
        ["eid_signatures_ocsp_response_well_defined"]
      runQuery_ . sqlAlterTable "eid_signatures" $ map
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

addSignatoryDobAndEmailToEIDSignatures :: MonadDB m => Migration m
addSignatoryDobAndEmailToEIDSignatures = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 3
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableEIDSignatures)
        [ sqlAddColumn $ tblColumn { colName     = "signatory_date_of_birth"
                                   , colType     = TextT
                                   , colNullable = True
                                   }
        , sqlAddColumn
          $ tblColumn { colName = "signatory_email", colType = TextT, colNullable = True }
        ]
      runQuery_ $ sqlAlterTable (tblName tableEIDSignatures)
                                [sqlAlterColumn "signature" "DROP NOT NULL"]
      runQuery_ $ sqlAlterTable (tblName tableEIDSignatures)
                                [sqlAlterColumn "data" "DROP NOT NULL"]
  }

dropEmailFromEIDSignatures :: MonadDB m => Migration m
dropEmailFromEIDSignatures = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 4
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable (tblName tableEIDSignatures)
                                               [sqlDropColumn "signatory_email"]
  }

addEidJson :: MonadDB m => Migration m
addEidJson = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 5
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableEIDSignatures)
                       [ sqlAddColumn
                           $ tblColumn { colName = "eid_service_json", colType = TextT }
                       ]
  }


eidSignaturesAddProviderEIDServiceSEBankID :: MonadDB m => Migration m
eidSignaturesAddProviderEIDServiceSEBankID = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 6
  , mgrAction    =
    StandardMigration $ do
      runQuery_ . sqlAlterTable "eid_signatures" $ map
        sqlDropCheck
        ["eid_signatures_ocsp_response_well_defined"]
      runQuery_ . sqlAlterTable "eid_signatures" $ map
        sqlAddValidCheck
        [ tblCheck
            { chkName      = "eid_signatures_ocsp_response_well_defined"
            , chkCondition =
              "(provider <= 3 OR provider >= 6) AND ocsp_response IS NULL OR (provider = 4 OR provider = 5 OR provider = 12) AND ocsp_response IS NOT NULL"
            }
        ]
  }

removeEidJson :: MonadDB m => Migration m
removeEidJson = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 7
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable (tblName tableEIDSignatures)
                                               [sqlDropColumn "eid_service_json"]
  }

addFieldsForVerimi :: MonadDB m => Migration m
addFieldsForVerimi = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 8
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableEIDSignatures)
        [ sqlAddColumn $ tblColumn { colName = "signatory_email", colType = TextT }
        , sqlAddColumn $ tblColumn { colName = "signatory_phone_number", colType = TextT }
        ]
  }

eidSignaturesUpdatePersonalNumberCheck :: MonadDB m => Migration m
eidSignaturesUpdatePersonalNumberCheck = Migration
  { mgrTableName = tblName tableEIDSignatures
  , mgrFrom      = 9
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "eid_signatures"
        [sqlDropCheck "eid_signatures_signatory_personal_number_well_defined"]

      runQuery_ $ sqlAlterTable
        "eid_signatures"
        [ sqlAddValidCheck $ tblCheck
            { chkName      = "eid_signatures_signatory_personal_number_well_defined"
            , chkCondition =
              "(provider = ANY (ARRAY[1, 2, 3, 4, 13])) = (signatory_personal_number IS NULL)"
            }
        ]
  }
