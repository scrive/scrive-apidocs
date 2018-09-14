-- | Command-line flags accepted by the main Shake script.

module Shake.Flags (ShakeFlag(..)
                   ,Pattern
                   ,shakeFlags) where

import System.Console.GetOpt

type Pattern = String

data ShakeFlag = TransifexUser     String
               | TransifexPassword String
               | TransifexLang     String
               | SrcSubdir         FilePath
               | NewBuild
               | OldBuild
               | EnableOptimisation
               | CreateDB
               | TestPattern       Pattern
  deriving Eq

shakeFlags :: [OptDescr (Either String ShakeFlag)]
shakeFlags =
  [ Option ""  ["user"]       (reqArg TransifexUser     "USER") "User name"
  , Option ""  ["password"]   (reqArg TransifexPassword "PASS") "Password"
  , Option ""  ["lang"]       (reqArg TransifexLang     "LANG") "Language"
  , Option ""  ["src-subdir"] (reqArg SrcSubdir         "DIR")
    "Source subdirectory (for 'hindent'/'stylish-haskell'/'hlint')"
  , Option ""  ["new-build"]  (noArg  NewBuild)
    "Use 'new-build' (default)."
  , Option ""  ["old-build"]  (noArg  OldBuild)
    "Don't use 'new-build'."
  , Option ""
    ["enable-optimisation", "enable-optimization"]
    (noArg EnableOptimisation)
    "Build the back end with optimisation enabled"
  , Option ""  ["create-db"]  (noArg  CreateDB)
    "Use a new DB for tests. See 'help-env' for relevant env var settings."
  , Option "p" ["pattern"]    (reqArg TestPattern       "PATTERN")
    "Run only tests matching a pattern (for 'test'/'test-server')"
  ]
  where
    noArg  flagVal     = NoArg  (Right flagVal)
    reqArg toFlag name = ReqArg (Right . toFlag) name
