-- | Command-line flags accepted by the main Shake script.

module Shake.Flags (ShakeFlag(..)
                   ,OptimisationLevel(..)
                   ,Pattern
                   ,EnableExecutableDynamic
                   ,shakeFlags) where

import Data.Bifunctor
import System.Console.GetOpt

type Pattern = String
type EnableExecutableDynamic = Bool

data OptimisationLevel = NoOptimisation
                       | DefaultOptimisation
                       | MaxOptimisation
  deriving Eq

optimisationLevelFromString :: String -> Either String OptimisationLevel
optimisationLevelFromString "0" = Right NoOptimisation
optimisationLevelFromString "1" = Right DefaultOptimisation
optimisationLevelFromString "2" = Right MaxOptimisation
optimisationLevelFromString l =
  Left $ "Unknown optimisation level: " ++ l ++ ", must be 0, 1, or 2."

data ShakeFlag = TransifexUser     String
               | TransifexPassword String
               | TransifexLang     String
               | SrcSubdir         FilePath
               | QuickBranch       String
               | NewBuild
               | OldBuild
               | OptimisationLevel OptimisationLevel
               | DisableExecutableDynamic
               | CreateDB
               | TestPattern       Pattern
  deriving Eq

shakeFlags :: [OptDescr (Either String ShakeFlag)]
shakeFlags =
  [ Option "" ["user"]     (reqArg TransifexUser "USER")     "User name"
  , Option "" ["password"] (reqArg TransifexPassword "PASS") "Password"
  , Option "" ["lang"]     (reqArg TransifexLang "LANG")     "Language"
  , Option ""
           ["src-subdir"]
           (reqArg SrcSubdir "DIR")
           "Source subdirectory (for 'hindent'/'stylish-haskell'/'hlint')"
  , Option ""
           ["quick-branch"]
           (reqArg QuickBranch "GIT_MAIN_BRANCH")
           "Git branch to compare for quick hlint/refactor/formatting"
  , Option "" ["new-build"] (noArg NewBuild) "Use 'v2-build' (default)."
  , Option "" ["old-build"] (noArg OldBuild) "Don't use 'v2-build'."
  , Option
    "O"
    ["enable-optimisation", "enable-optimization", "optimisation", "optimization"]
    (optArg (second OptimisationLevel . optimisationLevelFromString)
            (OptimisationLevel DefaultOptimisation)
            "NUM"
    )
    "Build the back end with optimisation enabled (use this in production)"
  , Option ""
           ["disable-executable-dynamic"]
           (noArg DisableExecutableDynamic)
           "Disable dynamic linking for the back end (use this in production)"
  , Option ""
           ["create-db"]
           (noArg CreateDB)
           "Use a new DB for tests. See 'help-env' for relevant env var settings."
  , Option "p"
           ["pattern"]
           (reqArg TestPattern "PATTERN")
           "Run only tests matching a pattern (for 'test'/'test-server')"
  ]
  where
    noArg flagVal = NoArg (Right flagVal)
    reqArg toFlag = ReqArg (Right . toFlag)
    optArg toFlag def = OptArg (maybe (Right def) toFlag)
