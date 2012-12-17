module Teamcity where

import System.Environment
import Data.List
import System.IO
import Data.Char

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
           " " ++ attrname ++ "='" ++ escapeTeamcityString attrvalue ++ "'") attributes ++ "]"



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

ghc :: Bool -> String -> (Bool,String)
ghc True line | "    " `isPrefixOf` line = (True, teamcityErrorMessage line)
ghc _ line = case elemIndices ':' line of
               [a,b,c] ->
                 let a1 = take a line
                     b1 = take (b-a-1) (drop (a+1) line)
                     c1 = take (c-b-1) (drop (b+1) line)
                 in if all isDigit b1 && all isDigit c1
                    then (True, teamcityErrorMessage line)
                    else (False, line)
               x -> (False, line)


main = do
  mode <- getArgs
  case mode of
    ["hunit"] -> interact (unlines . map hunit . lines)
    ["ghc"]   -> interact (unlines . snd . mapAccumL ghc False . lines)
    _ -> do
      hPutStrLn stderr "Invalid argument"
