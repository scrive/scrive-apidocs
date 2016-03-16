module Shake.TeamCity where

import Data.List
import Data.Maybe
import System.Exit

hUnitForTeamCity :: ExitCode -> String -> IO ()
hUnitForTeamCity c out = do
  allErrs <- mapM parseHUnit $ lines out
  let errs = unlines $ fmap fromJust $ filter isJust allErrs
  case c of
    ExitSuccess -> return ()
    ExitFailure _ -> error errs
  where parseHUnit :: String -> IO (Maybe String)
        parseHUnit s | "[Failed]" `isInfixOf` s = do
          putStrLn s
          putStrLn $ teamcityFailureMessage s
          return $ Just s
        parseHUnit s | otherwise = do
          putStrLn s
          return Nothing

teamcityFailureMessage :: String -> String
teamcityFailureMessage message =
  createTeamcityLine "message" [("text",message),("status","FAILURE")]

createTeamcityLine :: String -> [(String,String)] -> String
createTeamcityLine messageName attributes =
  "##teamcity[" ++ messageName ++
    concatMap (\(attrname,attrvalue) ->
           " " ++ attrname ++ "='" ++ escapeTeamcityString attrvalue ++ "'") attributes ++ "]"

escapeTeamcityString :: String -> String
escapeTeamcityString = concatMap escapeTeamcityChar
  where
    escapeTeamcityChar c = case c of
                             '\'' -> "|'"
                             '\n' -> "|n"
                             '\r' -> "|r"
                             '|' -> "||"
                             '[' -> "|["
                             ']' -> "|]"
                             _ | c<' ' -> "" -- just remove crap
                             _ -> [c]
