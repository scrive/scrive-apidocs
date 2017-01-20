-- | Command-line flags accepted by the main Shake script.

module Shake.Flags (ShakeFlag(..)
                   ,shakeFlags) where

import System.Console.GetOpt

data ShakeFlag = TransifexUser     String
               | TransifexPassword String
               | TransifexLang     String
               | SrcSubdir         FilePath
               | NewBuild
  deriving Eq

shakeFlags :: [OptDescr (Either String ShakeFlag)]
shakeFlags =
  [ Option "" ["user"]       (reqArg TransifexUser     "USER") "User name"
  , Option "" ["password"]   (reqArg TransifexPassword "PASS") "Password"
  , Option "" ["lang"]       (reqArg TransifexLang     "LANG") "Language"
  , Option "" ["src-subdir"] (reqArg SrcSubdir         "DIR")
    "Source subdirectory (for hindent/stylish-haskell/hlint)"
  , Option "" ["new-build"] (noArg  NewBuild)                  "Use 'new-build'."
  ]
  where
    noArg  flagVal     = NoArg  (Right flagVal)
    reqArg toFlag name = ReqArg (Right . toFlag) name
