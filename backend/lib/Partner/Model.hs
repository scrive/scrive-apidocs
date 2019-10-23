module Partner.Model (
  PartnerID
, Partner(..)
, GetPartners(..)
, GetPartnerByID(..)
, InsertPartnerForTests(..)
, unsafePartnerID
, unPartnerID
) where

import Control.Monad.Catch
import Log

import DB
import Partner.Partner
import UserGroup.Types

fetchPartner :: (PartnerID, String, Bool, Maybe UserGroupID) -> Partner
fetchPartner (pid, pname, pdef, mugid) =
  Partner { ptID = pid, ptName = pname, ptDefaultPartner = pdef, ptUserGroupID = mugid }

partnerSelector :: [SQL]
partnerSelector = ["id", "name", "default_partner", "user_group_id"]

data GetPartners = GetPartners
instance (MonadDB m, MonadLog m) => DBQuery m GetPartners [Partner] where
  query (GetPartners) = do
    runQuery_ . sqlSelect "partners" $ do
      mapM_ sqlResult $ partnerSelector
      sqlOrderBy "id"
    fetchMany fetchPartner

data GetPartnerByID = GetPartnerByID PartnerID
instance (MonadDB m, MonadThrow m) => DBQuery m GetPartnerByID Partner where
  query (GetPartnerByID pid) = do
    runQuery_ . sqlSelect "partners" $ do
      mapM_ sqlResult $ partnerSelector
      sqlWhereEq "id" pid
    fetchOne fetchPartner

-- This must be only used for testing.
data InsertPartnerForTests = InsertPartnerForTests Partner
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m InsertPartnerForTests Bool where
  update (InsertPartnerForTests pt) = do
    runQuery01 . sqlInsert "partners" $ do
      sqlSet "id" . ptID $ pt
      sqlSet "name" . ptName $ pt
      sqlSet "default_partner" False -- @note one can't create a new default partner
      sqlSet "user_group_id" . ptUserGroupID $ pt
