{-# OPTIONS_GHC -fno-warn-orphans #-}
module Templates ( getAllTemplates
                 , readGlobalTemplates
                 , localizedVersion
                 , KontrakcjaGlobalTemplates
                 , KontrakcjaTemplates
                 , getTemplatesModTime
                 , getTextTemplates
                 , getLocalTemplates
                 , renderLocalTemplate
                 , renderLocalTemplate_
                 , runTemplatesT
                 ) where

import Data.List (isSuffixOf)

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Base
import System.Time

import Utils.Default
import Utils.Enum
import User.Lang
import Crypto.RNG

import qualified Text.StringTemplates.Files as TF
import qualified Text.StringTemplates.TemplatesLoader as TL
import qualified Text.StringTemplates.TextTemplates as TT
import qualified Text.StringTemplates.Templates as T
import qualified Text.StringTemplates.Fields as F
import Text.StringTemplates.Utils (directoryFilesRecursive)

templateFilesDir :: FilePath
templateFilesDir = "templates"

textsDirectory :: FilePath
textsDirectory = "texts"

getAllTemplates :: IO [(String, String)]
getAllTemplates = do
  files <- directoryFilesRecursive templateFilesDir
  let templatesFiles = filter (".st" `isSuffixOf`) files
  concat `fmap` mapM TF.getTemplates templatesFiles

type KontrakcjaGlobalTemplates = TL.GlobalTemplates
type KontrakcjaTemplates = TL.Templates

readGlobalTemplates :: MonadIO m => m KontrakcjaGlobalTemplates
readGlobalTemplates = TL.readGlobalTemplates textsDirectory templateFilesDir $ map show allLangs

localizedVersion :: Lang -> KontrakcjaGlobalTemplates -> KontrakcjaTemplates
localizedVersion lang = TL.localizedVersion $ show lang

getTemplatesModTime :: IO ClockTime
getTemplatesModTime = TL.getTemplatesModTime textsDirectory templateFilesDir

getTextTemplates :: Lang -> IO [(String,String)]
getTextTemplates lang = TT.getTextTemplatesFor textsDirectory (show lang) otherLangs
    where otherLangs = map show (filter (/= lang) allValues :: [Lang])

instance (Functor m, Monad m) => T.TemplatesMonad (ReaderT KontrakcjaGlobalTemplates m) where
  getTemplates = T.getTextTemplatesByColumn $ show (defaultValue :: Lang)
  getTextTemplatesByColumn lang = do
      globaltemplates <- ask
      return $ TL.localizedVersion lang globaltemplates

getLocalTemplates :: T.TemplatesMonad m => Lang -> m TL.Templates
getLocalTemplates = T.getTextTemplatesByColumn . show

renderLocalTemplate :: (HasLang a, T.TemplatesMonad m) => a -> String -> F.Fields m () -> m String
renderLocalTemplate haslang name fields = do
  ts <- T.getTextTemplatesByColumn $ show $ getLang haslang
  T.renderHelper ts name fields

renderLocalTemplate_ :: (HasLang a, T.TemplatesMonad m) => a -> String -> m String
renderLocalTemplate_ haslang name = renderLocalTemplate haslang name $ return ()

instance CryptoRNG m => CryptoRNG (T.TemplatesT m) where
    getCryptoRNGState = T.TemplatesT $ ReaderT $ \_r -> getCryptoRNGState

deriving instance (MonadBase IO m) => MonadBase IO (T.TemplatesT m)                        

runTemplatesT :: (Functor m, Monad m) => (Lang, TL.GlobalTemplates) -> T.TemplatesT m a -> m a
runTemplatesT (lang, ts) action = runReaderT (T.unTT action) (show lang, ts)
