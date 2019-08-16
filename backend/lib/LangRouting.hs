module LangRouting
  ( allLangDirs
  ) where

import Happstack.StaticRouting (Route, choice, dir)
import qualified Data.Text as T

import Kontra
import User.Model

allLangDirs :: Route (Kontra a) -> Route (Kontra a)
allLangDirs r = choice $ r : (map (langDir r) allLangs)

langDir :: Route (Kontra a) -> Lang -> Route (Kontra a)
langDir r l  = dir (T.unpack $ codeFromLang l) $ fmap (\r' -> switchLang l >> r') r
