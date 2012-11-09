{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.TemplatesLoader
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- This module also provides reading template files, some checking and template info
-- 'KontrakcjaTemplates' is alias for template group, but ma be changed in a future
-----------------------------------------------------------------------------
module Templates.TemplatesLoader
    ( KontrakcjaTemplates
    , KontrakcjaTemplate
    , KontrakcjaGlobalTemplates
    , renderTemplateMain
    , getTemplatesModTime
    , readGlobalTemplates
    , localizedVersion
    ) where

import Text.StringTemplate
import Text.StringTemplate.Classes
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Html (stringToHtmlString)
import System.Directory
import System.Time
import Templates.TemplatesFiles
import User.Lang
import Templates.TextTemplates
import Utils.Enum

{-Names of template files -}


type KontrakcjaTemplates = STGroup String
type KontrakcjaTemplate = StringTemplate String
type KontrakcjaGlobalTemplates = Map.Map Lang KontrakcjaTemplates

localizedVersion :: Lang -> KontrakcjaGlobalTemplates -> KontrakcjaTemplates
localizedVersion lang mtemplates = mtemplates ! lang

-- Fixme: Make this do only one read of all files !!
readGlobalTemplates :: (Functor m, MonadIO m) => m KontrakcjaGlobalTemplates
readGlobalTemplates =
  fmap Map.fromList $ forM allValues $ \lang -> do
    templates <- liftIO $ readTemplates lang
    return (lang,templates)

readTemplates :: Lang -> IO KontrakcjaTemplates
readTemplates lang = do
    ts <- mapM getTemplates templatesFilesPath
    texts <- getTextTemplates lang
    return $ groupStringTemplates $ fmap (\(n,v) -> (n,newSTMP v)) $ (concat ts) ++ texts

getTemplatesModTime :: IO ClockTime
getTemplatesModTime = do
    mtimes <- mapM getModificationTime templatesFilesPath
    mt <- getTextTemplatesMTime
    return $ maximum $ mt : mtimes

renderTemplateMain :: ToSElem a => KontrakcjaTemplates -> String -> [(String, a)]
                   -> (KontrakcjaTemplate -> KontrakcjaTemplate) -> String
renderTemplateMain ts name params f = case mt of
  Just t  -> render $ f (setManyAttrib params t)
  Nothing -> error  $ "No template named " ++ name
  where
    ts' = setEncoderGroup stringToHtmlString ts
    noescape = groupStringTemplates [("noescape", newSTMP "$it$" :: StringTemplate String)]
    mt = getStringTemplate name $ mergeSTGroups noescape ts'

{- For some reasons the SElem a is not of class ToSElem -}
instance (Stringable a) => ToSElem (SElem a) where
  toSElem (STR a) = (STR a)
  toSElem (BS a) = (BS a)
  toSElem (STSH a) = (STSH a)
  toSElem (SM a) = (SM $ fmap (toSElem) a)
  toSElem (LI a) = (LI $ fmap (toSElem) a)
  toSElem (SBLE a) = (SBLE $ convert a)
  toSElem (SNAT a) = (SNAT $ convert a)
  toSElem SNull = SNull

convert :: (Stringable a, Stringable b) => a -> b
convert = stFromString . stToString
