{-# OPTIONS_GHC -fno-warn-orphans #-}
module Templates ( getAllTemplates
                 , readGlobalTemplates
                 , localizedVersion
                 , KontrakcjaGlobalTemplates
                 , KontrakcjaTemplates
                 , getTemplatesModTime
                 , renderTextTemplate
                 , renderTextTemplate_
                 , renderLocalTemplate
                 , runTemplatesT
                 , templateName
                 ) where

import Control.Monad.Reader
import Data.Time.Clock
import Text.StringTemplates.Utils (directoryFilesRecursive)
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F
import qualified Text.StringTemplates.Files as TF
import qualified Text.StringTemplates.Templates as ST
import qualified Text.StringTemplates.TemplatesLoader as TL

import User.Lang

templateFilesDir :: FilePath
templateFilesDir = "templates"

textsDirectory :: FilePath
textsDirectory = "texts"

getAllTemplates :: IO [(Text, Text)]
getAllTemplates = do
  files <- directoryFilesRecursive templateFilesDir
  let templatesFiles = filter (".st" `isSuffixOf`) files
  templates <- mapM getTextTemplates templatesFiles
  return $ concat templates

getTextTemplates :: FilePath -> IO [(Text, Text)]
getTextTemplates templatesFile = do
  templates <- TF.getTemplates templatesFile
  return $ fmap packTuple templates
   where
    packTuple :: (String, String) -> (Text, Text)
    packTuple (t1, t2) = (T.pack t1, T.pack t2)

type KontrakcjaGlobalTemplates = TL.GlobalTemplates
type KontrakcjaTemplates = TL.Templates

readGlobalTemplates :: MonadIO m => m KontrakcjaGlobalTemplates
readGlobalTemplates =
  TL.readGlobalTemplates textsDirectory templateFilesDir
  (T.unpack $ codeFromLang LANG_EN)

localizedVersion :: Lang -> KontrakcjaGlobalTemplates -> KontrakcjaTemplates
localizedVersion lang = TL.localizedVersion $ T.unpack $ codeFromLang lang

getTemplatesModTime :: IO UTCTime
getTemplatesModTime = TL.getTemplatesModTime textsDirectory templateFilesDir

renderTextTemplate :: (Monad m, ST.TemplatesMonad m) => Text -> ST.Fields m () -> m Text
renderTextTemplate x m = fmap T.pack $ ST.renderTemplate (T.unpack x) m

renderTextTemplate_ :: (Monad m, ST.TemplatesMonad m) => Text -> m Text
renderTextTemplate_ x = fmap T.pack $ ST.renderTemplate_ (T.unpack x)

renderLocalTemplate :: (HasLang a, ST.TemplatesMonad m)
                    => a -> Text -> F.Fields m () -> m Text
renderLocalTemplate haslang name fields = do
  ts <- ST.getTextTemplatesByLanguage $ T.unpack $ codeFromLang $ getLang haslang
  fmap T.pack $ ST.renderHelper ts (T.unpack name) fields

runTemplatesT :: (Functor m, Monad m)
              => (Lang, TL.GlobalTemplates) -> ST.TemplatesT m a -> m a
runTemplatesT (lang, ts) action =
  runReaderT (ST.unTT action) (T.unpack $ codeFromLang lang, ts)

-- | Use 'templateName' to flag that a string literal is a template
-- name (for detect_old_templates)
templateName :: Text -> Text
templateName = id
