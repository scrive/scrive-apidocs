module EvidenceLog.Tables 
       (
         tableEvidenceLog
       )
       where

import Database.HDBC

import DB.Classes
import DB.Model

{- |

Description of the table to store the evidence log.

The main principle of this table is that it should be as independent
as possible from other tables to avoid migration conflicts. Hence the
lack of foreign keys.

-}
tableEvidenceLog :: Table
tableEvidenceLog = Table {
  tblName = "evidence_log"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("id",                SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just False}),
       ("document_id",       SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just True}),
       ("user_id",           SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just True}),
       ("email",             SqlColDesc { colType     = SqlVarCharT
                                        , colNullable = Just True}),
       ("time",              SqlColDesc { colType     = SqlTimestampWithZoneT
                                        , colNullable = Just False}),
       ("request_ip_v4",      SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just True}),
       ("request_ip_v6",      SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just True}),
       ("signatory_link_id", SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just True}),
       ("text",              SqlColDesc { colType     = SqlVarCharT
                                        , colNullable = Just False}),
       ("event_type",        SqlColDesc { colType     = SqlIntegerT
                                        , colNullable = Just False}),
       ("version_id",        SqlColDesc { colType     = SqlVarCharT
                                        , colNullable = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE evidence_log ("
          ++ "  id            BIGSERIAL   NOT NULL"
          ++ ", document_id   BIGINT          NULL"          
          ++ ", user_id       BIGINT          NULL"
          ++ ", email         VARCHAR         NULL"
          ++ ", time          TIMESTAMPTZ NOT NULL"
          ++ ", request_ip_v4 BIGINT          NULL"
          ++ ", request_ip_v6 BIGINT          NULL"          
          ++ ", signatory_link_id BIGINT      NULL"
          ++ ", text          VARCHAR     NOT NULL"
          ++ ", event_type    INT         NOT NULL"
          ++ ", version_id    VARCHAR     NOT NULL"
          ++ ", CONSTRAINT pk_evidence_log PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \_conn -> return ()
  }


