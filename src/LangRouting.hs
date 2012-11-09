module LangRouting
  ( allLangDirs
  , dirByLang
  ) where

import Happstack.StaticRouting (Route, choice, dir)
import User.Model

allLangDirs :: (Lang -> Route a) -> Route a
allLangDirs r = forAllTargetedLangs $ \l -> langDir l $ r l

dirByLang :: HasLang l => l -> String -> String -> Route a -> Route a
dirByLang lang swedishdir englishdir
  | getLang lang == LANG_SV = dir swedishdir
  | otherwise = dir englishdir

forAllTargetedLangs :: (Lang -> Route h) -> Route h
forAllTargetedLangs r = choice (map r allLangs)

langDir :: Lang -> Route a -> Route a
langDir = dir . codeFromLang . getLang
