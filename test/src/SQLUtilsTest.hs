
module SQLUtilsTest (sqlUtilsTests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)

import DB.Utils
import Database.HDBC
import Database.HDBC.Statement as HDBC
import Data.IORef
import DB.Classes
import Data.Typeable
import qualified Control.Exception as E

mkStatement :: [[SqlValue]] -> IO (Statement, IORef ([[SqlValue]],Bool))
mkStatement results = do
  r <- newIORef (results,False)
  return $ (Statement 
           { execute = error "execute not defined"
           , executeRaw = error "executeRaw not defined"
           , executeMany = error "executeMany not defined"
           , finish = atomicModifyIORef r $ \(xs,_fc) -> ((xs,True),())

           , fetchRow = atomicModifyIORef r $ \result ->
                          case result of
                            ([],_fc) -> (([],True),Nothing)
                            ((x:xs),finishCalled) -> ((xs,finishCalled),Just x)

           , getColumnNames = error "getColumnNames not defined"
           , HDBC.originalQuery = "original query"
           , describeResult = error "describeResult not defined"
           }, r)

data A = A Int String Double
       deriving (Eq, Ord, Show, Read, Typeable)

decodeA :: Int -> String -> Double -> Either DBException A
decodeA i s d = return (A i s d)

decodeANonZero :: Int -> String -> Double -> Either DBException A
decodeANonZero i s d = 
  if i == 0 then Left $ CannotParseRow "" "I do not like zeros"
  else return (A i s d)

sqlTestFetcherProperData :: Assertion
sqlTestFetcherProperData = do
  (s,k) <- mkStatement [ [SqlInt64 123, SqlString "abc", SqlDouble 1.23]]
  v <- fetchValues s decodeA
  assertEqual "result" [A 123 "abc" 1.23] v
  (r,fc) <- readIORef k
  assertEqual "finish called" True fc
  assertEqual "read all rows" True (null r)

sqlTestFetcherProperDataManyRows :: Assertion
sqlTestFetcherProperDataManyRows = do
  (s,k) <- mkStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 2, SqlString "b", SqlDouble 2.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- fetchValues s decodeA
  assertEqual "result" [ A 1 "a" 1.23
                       , A 2 "b" 2.23
                       , A 3 "c" 3.23
                       ] v
  (r,fc) <- readIORef k
  assertEqual "finish called" True fc
  assertEqual "read all rows" True (null r)

sqlTestFetcherRowTooShort :: Assertion
sqlTestFetcherRowTooShort = do
  (s,k) <- mkStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 2, SqlString "b"]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ fetchValues s decodeA
  assertEqual "row too short exception was thrown" True $
              case v of
                Left RowLengthMismatch{} -> True
                _ -> False

  (r,fc) <- readIORef k
  assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)

sqlTestFetcherRowTooLong :: Assertion
sqlTestFetcherRowTooLong = do
  (s,k) <- mkStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 2, SqlString "b", SqlDouble 1.23, SqlDouble 1.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ fetchValues s decodeA
  assertEqual "row too long exception was thrown" True $
              case v of
                Left RowLengthMismatch{} -> True
                _ -> False

  (r,fc) <- readIORef k
  assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)

sqlTestFetcherConvertError :: Assertion
sqlTestFetcherConvertError = do
  (s,k) <- mkStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlString "b", SqlString "b", SqlDouble 1.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ fetchValues s decodeA
  assertEqual "convert error was thrown" True $
              case v of
                Left CannotConvertSqlValue{} -> True
                _ -> False

  (r,fc) <- readIORef k
  assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)

sqlTestFetcherUserConvertError :: Assertion
sqlTestFetcherUserConvertError = do
  (s,k) <- mkStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 0, SqlString "b", SqlDouble 1.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ fetchValues s decodeANonZero
  assertEqual "user convert error was thrown" True $
              case v of
                Left CannotParseRow{} -> True
                _ -> False

  (r,fc) <- readIORef k
  assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)

sqlUtilsTests :: Test
sqlUtilsTests = testGroup "SQLUtils"
                [ testCase "fetcher proper data works"               $ sqlTestFetcherProperData
                , testCase "fetcher proper data works for many rows" $ sqlTestFetcherProperDataManyRows
                , testCase "fetcher row too short"                   $ sqlTestFetcherRowTooShort
                , testCase "fetcher row too long"                    $ sqlTestFetcherRowTooLong
                , testCase "fetcher cannot convert SqlValue"         $ sqlTestFetcherConvertError
                , testCase "fetcher cannot parse row"                $ sqlTestFetcherUserConvertError
                ]

