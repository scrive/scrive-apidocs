module Shake.TeamCity where

import Data.List
import Data.Maybe
import System.Exit

-- | Takes the output of running a HUnit test suite in the form of the
-- executable processes ExitCode and standard output as a String.
-- This function then "parses" this to annotate it with messages that TeamCity
-- can understand as per:
-- https://confluence.jetbrains.com/display/TCD8/Build+Script+Interaction+with+TeamCity
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

-- * TeamCity compatible output
-- See: https://confluence.jetbrains.com/display/TCD8/Build+Script+Interaction+with+TeamCity

teamcityFailureMessage :: String -> String
teamcityFailureMessage message =
  createTeamcityLine "message" [("text",message),("status","FAILURE")]

createTeamcityLine :: String -> [(String,String)] -> String
createTeamcityLine messageName attributes =
  "##teamcity[" ++ messageName ++
    concatMap (\(attrname,attrvalue) ->
                 " " ++ attrname ++ "='" ++ escapeTeamcityString attrvalue
                 ++ "'") attributes ++ "]"

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
