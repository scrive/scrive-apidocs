module LocaleRouting
  ( allLocaleDirs
  , dirByLang
  ) where

import Happstack.StaticRouting (Route, choice, dir)
import User.Model

allLocaleDirs :: (Locale -> Route a) -> Route a
allLocaleDirs r = forAllTargetedLocales $ \l -> regionDir l $ langDir l $ r l

dirByLang :: HasLocale l => l -> String -> String -> Route a -> Route a
dirByLang locale swedishdir englishdir
  | getLang locale == LANG_SE = dir swedishdir
  | otherwise = dir englishdir

forAllTargetedLocales :: (Locale -> Route h) -> Route h
forAllTargetedLocales r = choice (map r targetedLocales)

regionDir :: Locale -> Route a -> Route a
regionDir = dir . codeFromRegion . getRegion

langDir :: Locale -> Route a -> Route a
langDir = dir . codeFromLang . getLang
