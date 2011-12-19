module SQL.Builder where

import Database.HDBC
import Data.List
import Data.Maybe

data WhereClause = WhereSimple String [SqlValue]
                 | WhereAnd WhereClause WhereClause
                 | WhereOr WhereClause WhereClause
                 | WhereReason WhereClause String
                 deriving (Show)

stringFromWhere :: WhereClause -> (String, [SqlValue])
stringFromWhere (WhereReason c _) = stringFromWhere c
stringFromWhere (WhereSimple s vs) = (s, vs)
stringFromWhere (WhereAnd a b) = 
  let (s1, vs1) = stringFromWhere a
      (s2, vs2) = stringFromWhere b
  in ("(" ++ s1 ++ " AND " ++ s2 ++ ")", vs1 ++ vs2)
stringFromWhere (WhereOr  a b) = 
  let (s1, vs1) = stringFromWhere a
      (s2, vs2) = stringFromWhere b
  in ("(" ++ s1 ++ " OR  " ++ s2 ++ ")", vs1 ++ vs2)

data UpdateStatement = UpdateStatement { usTableName   :: String
                                       , usSetFields   :: [SetField]
                                       , usWhereClause :: Maybe WhereClause
                                       }
                       deriving (Show)
                       
data SelectStatement = SelectStatement { ssFields      :: [String]
                                       , ssFrom        :: [String]
                                       , ssWhereClause :: Maybe WhereClause
                                       }
                       deriving (Show)
                       
stringFromUpdateStatement :: UpdateStatement -> (String, [SqlValue])
stringFromUpdateStatement (UpdateStatement{ usTableName, usSetFields, usWhereClause }) =
  let (sfs, sfvs) = unzip $ map stringFromSetField usSetFields
      (ws, wvs) = stringFromWhere $ fromMaybe (WhereSimple "1=1" []) usWhereClause
  in (" UPDATE " ++ usTableName ++ 
      " SET " ++ intercalate ", " sfs ++
      " WHERE " ++ ws,
      sfvs ++ wvs)
     
stringFromSelectStatement :: SelectStatement -> (String, [SqlValue])
stringFromSelectStatement (SelectStatement { ssFields, ssFrom, ssWhereClause }) =
  let (ws, wvs) = stringFromWhere $ fromMaybe (WhereSimple "1=1" []) ssWhereClause
  in (" SELECT " ++ (intercalate ", " ssFields) ++
      " FROM   " ++ (intercalate ", " ssFrom  ) ++
      " WHERE  " ++ ws,
      wvs)

data SetField = SetField String SqlValue
              deriving (Show)

stringFromSetField :: SetField -> (String, SqlValue)
stringFromSetField (SetField r v) = (r, v)

combineWhereAnd :: Maybe WhereClause -> Maybe WhereClause -> Maybe WhereClause
combineWhereAnd Nothing Nothing = Nothing
combineWhereAnd Nothing a = a
combineWhereAnd a Nothing = a
combineWhereAnd (Just a) (Just b) = Just $ WhereAnd a b
  
reasonsFromUpdate :: UpdateStatement -> Maybe ReasonSelect
reasonsFromUpdate (UpdateStatement { usTableName , usWhereClause }) =
  case usWhereClause of
    Nothing -> Nothing
    Just wc -> transformToSelects usTableName wc 

data ReasonSelect = SimpleSelect SelectStatement String
                  | ReasonOr ReasonSelect ReasonSelect
                  | ReasonAnd ReasonSelect ReasonSelect
                  deriving (Show)
                                    
transformToSelects :: String -> WhereClause -> Maybe ReasonSelect
transformToSelects _ (WhereSimple _ _) = Nothing
transformToSelects t (WhereAnd a b) = 
  case (transformToSelects t a, transformToSelects t b) of
    (Nothing, Nothing) -> Nothing
    (Just ra, Nothing) -> Just ra
    (Nothing, Just rb) -> Just rb
    (Just ra, Just rb) -> Just $ ReasonAnd ra rb
transformToSelects t (WhereOr  a b) = 
  case (transformToSelects t a, transformToSelects t b) of
    (Nothing, Nothing) -> Nothing
    (Just ra, Nothing) -> Just ra
    (Nothing, Just rb) -> Just rb
    (Just ra, Just rb) -> Just $ ReasonOr ra rb
transformToSelects t (WhereReason a s) = Just $ SimpleSelect SelectStatement { ssFields = ["1"]
                                                                             , ssFrom   = [t]
                                                                             , ssWhereClause = Just a
                                                                             } s
{-This should work, though it need to go somewhere else.
instance DBQuery ReasonSelect [String] where
  dbQuery rs = recurse rs
    where 
      recurse (ReasonAnd a b) = do
        ra <- recurse a
        case ra of
          [] -> recurse b
          s -> return s
      recurse (ReasonOr a b) = do
        ra <- recurse a
        case ra of
          [] -> return []
          sa -> do
            rb <- recurse b
            return $ sa ++ rb
      recurse (SimpleSelect s reason) = do
        let (ss, vals) = stringFromSelectStatement s
        r <- run ss vals
        if r == 0
          then return [reason]
          else return []
-}
