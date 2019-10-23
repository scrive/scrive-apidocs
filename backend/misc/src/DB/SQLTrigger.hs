module DB.SQLTrigger
    (
      SQLTrigger(..)
    , defineTriggers
    )
    where

import Database.PostgreSQL.PQTypes

-- | Basic SQL trigger handling.
data SQLTrigger =
     SQLTrigger
     {
       sqlTrigFun :: RawSQL () -> RawSQL ()
     -- ^ The definition of the function to execute when fired
     , sqlTrigTbl :: RawSQL ()
     -- ^ The table that the trigger is coupled with
     , sqlTrigCol :: Maybe (RawSQL ())
     -- ^ If it should only fire on a certain column, specify the name of that
     -- column here
     }

triggerNameSuffix :: SQLTrigger -> RawSQL ()
triggerNameSuffix SQLTrigger {..} =
  sqlTrigTbl <> maybe "" (\colName -> "__" <> colName) sqlTrigCol

triggerDefName :: SQLTrigger -> RawSQL ()
triggerDefName t = "trgdef__" <> triggerNameSuffix t

triggerFunName :: SQLTrigger -> RawSQL ()
triggerFunName t = "trgfun__" <> triggerNameSuffix t

-- | Create the actual code defining the trigger and name it based on table
-- name, and column name if that is defined.
--
-- For now, only row-oriented triggers using `AFTER INSERT OR UPDATE` are
-- supported.
toTriggerDef :: SQLTrigger -> RawSQL ()
toTriggerDef t@SQLTrigger {..} =
  let mColumn = maybe "" (\colName -> "of" <+> colName) sqlTrigCol
  in  "create trigger"
        <+> (triggerDefName t)
        <+> "after insert or update"
        <+> mColumn
        <+> "on"
        <+> sqlTrigTbl
        <+> "for each row"
        <+> "execute procedure"
        <+> triggerFunName t
        <>  "();"

-- | Produce the code defining the function that a trigger hooks into.
toTriggerFun :: SQLTrigger -> RawSQL ()
toTriggerFun t@SQLTrigger {..} = sqlTrigFun . triggerFunName $ t

-- We need to drop and recreate triggers since no `create or replace` exists for
-- these.
dropTriggerIfExists :: SQLTrigger -> RawSQL ()
dropTriggerIfExists t@SQLTrigger {..} =
  "drop trigger if exists" <+> (triggerDefName t) <+> "on" <+> sqlTrigTbl <+> ";"

-- | Define a collection of triggers.
defineTriggers :: MonadDB m => [SQLTrigger] -> m ()
defineTriggers ts = do
  mapM_ (runQuery_ . dropTriggerIfExists) ts
  mapM_ (runQuery_ . toTriggerFun)        ts
  mapM_ (runQuery_ . toTriggerDef)        ts
