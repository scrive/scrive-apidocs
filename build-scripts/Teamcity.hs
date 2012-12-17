module Teamcity where

import System.Environment
import Data.List
import System.IO

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

createTeamcityLine :: String -> [(String,String)] -> String
createTeamcityLine messageName attributes =
  "##teamcity[" ++ messageName ++
    concatMap (\(attrname,attrvalue) ->
           " " ++ attrname ++ "='" ++ escapeTeamcityString attrvalue ++ "'") attributes ++ "]\n"



teamcityErrorMessage :: String -> String
teamcityErrorMessage message =
  createTeamcityLine "message" [("text",message),("status","ERROR")]

teamcityWarningMessage :: String -> String
teamcityWarningMessage message =
  createTeamcityLine "message" [("text",message),("status","WARNING")]

teamcityFailureMessage :: String -> String
teamcityFailureMessage message =
  createTeamcityLine "message" [("text",message),("status","FAILURE")]

teamcityNormalMessage :: String -> String
teamcityNormalMessage message =
  createTeamcityLine "message" [("text",message)]

teamcityCompilationStarted :: String -> String
teamcityCompilationStarted compiler =
  createTeamcityLine "compilationStarted" [("compiler",compiler)]

teamcityCompilationFinished :: String -> String
teamcityCompilationFinished compiler =
  createTeamcityLine "compilationFinished" [("compiler",compiler)]

teamcityBlockOpened :: String -> String
teamcityBlockOpened name =
  createTeamcityLine "blockOpened" [("name",name)]

teamcityBlockClosed :: String -> String
teamcityBlockClosed name =
  createTeamcityLine "blockClosed" [("name",name)]

hunit :: String -> String
hunit line =
  if "[Failed]" `isInfixOf` line
     then teamcityFailureMessage line
     else line

main = do
  mode <- getArgs
  case mode of
    ["hunit"] -> interact (unlines . map hunit . lines)
    _ -> do
      hPutStrLn stderr "Invalid argument"
