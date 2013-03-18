module SQLUtilsTest (sqlUtilsTests) where

import Control.Monad
import Control.Monad.IO.Class
import Test.Framework (Test, testGroup)
import Database.HDBC
import Database.HDBC.Statement as HDBC
import Data.IORef
import Data.Typeable
import qualified Control.Exception.Lifted as E

import DB
import TestingUtil
import TestKontra
import Control.Monad.State.Class

sqlUtilsTests :: TestEnvSt -> Test
sqlUtilsTests env = testGroup "SQLUtils" [
    testThat "fetcher proper data works"               env sqlTestFetcherProperData
  , testThat "fetcher proper data works for many rows" env sqlTestFetcherProperDataManyRows
  , testThat "fetcher row too short"                   env sqlTestFetcherRowTooShort
  , testThat "fetcher row too long"                    env sqlTestFetcherRowTooLong
  , testThat "fetcher cannot convert SqlValue"         env sqlTestFetcherConvertError
  , testThat "fetcher cannot parse row"                env sqlTestFetcherUserConvertError
  ]

runDeepTest :: DBT TestEnv () -> TestEnv ()
runDeepTest action = do
  nex <- getNexus
  runDBT nex (DBEnvSt Nothing [] Nothing) $ action

injectStatement :: [[SqlValue]] -> DBT TestEnv (IORef ([[SqlValue]], Bool))
injectStatement results = DBT $ do
  r <- liftIO $ newIORef (results, False)
  let st = Statement {
          execute = error "execute not defined"
        , executeRaw = error "executeRaw not defined"
        , executeMany = error "executeMany not defined"
        , finish = atomicModifyIORef r $ \(xs,_fc) -> ((xs,True),())
        , fetchRow = atomicModifyIORef r $ \result ->
            case result of
              ([], _) -> (([], True), Nothing)
              ((x:xs), finishCalled) -> ((xs, finishCalled), Just x)
        , getColumnNames = return []
        , HDBC.originalQuery = "original query"
        , describeResult = error "describeResult not defined"
        }
  modify (\s -> s { dbStatement = Just st })
  return r

data A = A Int String Double
       deriving (Eq, Ord, Show, Read, Typeable)

decodeA :: [A] -> Int -> String -> Double -> [A]
decodeA acc i s d = A i s d : acc

decodeANonZero :: [A] -> Int -> String -> Double -> [A]
decodeANonZero acc i s d = if i == 0
  then error "I do not like zeros"
  else A i s d : acc

sqlTestFetcherProperData :: TestEnv ()
sqlTestFetcherProperData = runDeepTest $ do
  k <- injectStatement [[SqlInt64 123, SqlString "abc", SqlDouble 1.23]]
  v <- kFold decodeA []
  assertEqual "result" [A 123 "abc" 1.23] v
  (r, fc) <- liftIO $ readIORef k
  assertEqual "finish called" True fc
  assertEqual "read all rows" True (null r)

sqlTestFetcherProperDataManyRows :: TestEnv ()
sqlTestFetcherProperDataManyRows = runDeepTest $ do
  k <- injectStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 2, SqlString "b", SqlDouble 2.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- reverse `liftM` kFold decodeA []
  assertEqual "result" [ A 1 "a" 1.23
                       , A 2 "b" 2.23
                       , A 3 "c" 3.23
                       ] v
  (r, fc) <- liftIO $ readIORef k
  assertEqual "finish called" True fc
  assertEqual "read all rows" True (null r)

sqlTestFetcherRowTooShort :: TestEnv ()
sqlTestFetcherRowTooShort = runDeepTest $ do
  k <- injectStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 2, SqlString "b"]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ kFold decodeA []
  assertEqual "row too short exception was thrown" True $
              case v of
                Left RowLengthMismatch{} -> True
                _ -> False

  (r, fc) <- liftIO $ readIORef k
  assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)

sqlTestFetcherRowTooLong :: TestEnv ()
sqlTestFetcherRowTooLong = runDeepTest $ do
  k <- injectStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 2, SqlString "b", SqlDouble 1.23, SqlDouble 1.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ kFold decodeA []
  assertEqual "row too long exception was thrown" True $
              case v of
                Left RowLengthMismatch{} -> True
                _ -> False

  (r, fc) <- liftIO $ readIORef k
  assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)

sqlTestFetcherConvertError :: TestEnv ()
sqlTestFetcherConvertError = runDeepTest $ do
  k <- injectStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlString "b", SqlString "b", SqlDouble 1.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ kFold decodeA []
  assertEqual "convert error was thrown" True $
              case v of
                Left CannotConvertSqlValue{} -> True
                _ -> False

  (r, fc) <- liftIO $ readIORef k
  assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)


sqlTestFetcherUserConvertError :: TestEnv ()
sqlTestFetcherUserConvertError = runDeepTest $ do
  k <- injectStatement [ [SqlInt64 1, SqlString "a", SqlDouble 1.23]
                       , [SqlInt64 0, SqlString "b", SqlDouble 1.23]
                       , [SqlInt64 3, SqlString "c", SqlDouble 3.23]
                       ]
  v <- E.try $ kFold decodeANonZero []
  assertEqual "user convert error was thrown" True $
              case v of
                Left E.ErrorCall{} -> True
                _ -> False

  (r, _fc) <- liftIO $ readIORef k
  --assertEqual "finish called even after exception" True fc
  assertEqual "read all rows" False (null r)
