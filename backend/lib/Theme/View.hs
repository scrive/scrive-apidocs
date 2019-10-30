{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Theme.View
       (
          unjsonTheme
        , unjsonThemesList
       )
       where

import Data.Functor.Invariant
import Data.Unjson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8

import Theme.Model

unjsonTheme :: UnjsonDef Theme
unjsonTheme =
  objectOf
    $   pure Theme
    <*> field "id"   themeID   "Id of a theme (unique)"
    <*> field "name" themeName "Name of a theme"
    <*> fieldBy
          "logo"
          themeLogo
          "Logo of a theme"
          (invmap
            (\l -> B64.decodeLenient $ BSC8.pack $ drop 1 $ dropWhile ((/=) ',') l)
            (\l ->
              BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode l
            )
            unjsonDef
          )
    <*> field "brandColor"      themeBrandColor      "Color of brand"
    <*> field "brandTextColor"  themeBrandTextColor  "Color of brand text"
    <*> field "actionColor"     themeActionColor     "Color of action"
    <*> field "actionTextColor" themeActionTextColor "Color of action text"
    <*> field "actionSecondaryColor"
              themeActionSecondaryColor
              "Color of action (secondary)"
    <*> field "actionSecondaryTextColor"
              themeActionSecondaryTextColor
              "Color of action text (secondary)"
    <*> field "positiveColor"     themePositiveColor     "Positive color"
    <*> field "positiveTextColor" themePositiveTextColor "Positive text color"
    <*> field "negativeColor"     themeNegativeColor     "Negative color"
    <*> field "negativeTextColor" themeNegativeTextColor "Negative text color"
    <*> field "font"              themeFont              "Font"

unjsonThemesList :: UnjsonDef [Theme]
unjsonThemesList =
  objectOf $ fieldBy "themes" identity "List of themes" (arrayOf unjsonTheme)

instance Unjson Theme where
  unjsonDef = unjsonTheme
