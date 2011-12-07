module SQL.Builder where

import Database.HDBC
import Data.List
import Data.Maybe

data WhereClause = WhereSimple String [SqlValue]
                 | WhereAnd WhereClause WhereClause
                 | WhereOr WhereClause WhereClause

stringFromWhere :: WhereClause -> (String, [SqlValue])
stringFromWhere (WhereSimple s vs) = (s, vs)
stringFromWhere (WhereAnd a b) = 
  let (s1, vs1) = stringFromWhere a
      (s2, vs2) = stringFromWhere b
  in ("(" ++ s1 ++ " AND " ++ s2 ++ ")", vs1 ++ vs2)
stringFromWhere (WhereOr  a b) = 
  let (s1, vs1) = stringFromWhere a
      (s2, vs2) = stringFromWhere b
  in ("(" ++ s1 ++ " OR  " ++ s2 ++ ")", vs1 ++ vs2)


data UpdateStatement = UpdateStatement { usTableName :: String
                                       , usSetFields :: [SetField]
                                       , usWhereClause :: Maybe WhereClause
                                       }
                       
stringFromUpdateStatement :: UpdateStatement -> (String, [SqlValue])
stringFromUpdateStatement (UpdateStatement{ usTableName, usSetFields, usWhereClause }) =
  let (sfs, sfvs) = unzip $ map stringFromSetField usSetFields
      (ws, wvs) = stringFromWhere $ fromMaybe (WhereSimple "1=1" []) usWhereClause
  in (" UPDATE " ++ usTableName ++ 
      " SET " ++ intercalate ", " sfs ++
      " WHERE " ++ ws,
      sfvs ++ wvs)

data SetField = SetField String SqlValue

stringFromSetField :: SetField -> (String, SqlValue)
stringFromSetField (SetField r v) = (r, v)

combineWhereAnd :: Maybe WhereClause -> Maybe WhereClause -> Maybe WhereClause
combineWhereAnd Nothing Nothing = Nothing
combineWhereAnd Nothing a = a
combineWhereAnd a Nothing = a
combineWhereAnd (Just a) (Just b) = Just $ WhereAnd a b
  
