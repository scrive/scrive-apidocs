module LangRouting
  ( allLangDirs
  ) where

import Happstack.StaticRouting (Route, choice, dir)
import User.Model
import Kontra

allLangDirs :: Route (KontraPlus a) -> Route (KontraPlus a)
allLangDirs r = choice $ r : (map (langDir r) allLangs)

langDir :: Route (KontraPlus a) -> Lang -> Route (KontraPlus a)
langDir r l  = dir (codeFromLang l) $ fmap (\r' -> switchLang l >> r') r
