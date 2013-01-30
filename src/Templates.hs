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
                 ) where

import Control.Monad.Trans
import Control.Monad.Reader
import System.Time

import Utils.Default
import Utils.Enum
import User.Lang
import Crypto.RNG

import Text.StringTemplates.Files (getTemplates)
import qualified Text.StringTemplates.TemplatesLoader as TL
import qualified Text.StringTemplates.TextTemplates as TT
import qualified Text.StringTemplates.Templates as T
import qualified Text.StringTemplates.Fields as F

templateFiles :: [FilePath]
templateFiles =
    map ("templates/"++) [ "modals.st"
                         , "flash.st"
                         , "mails.st"
                         , "nicemail.st"
                         , "utils.st"
                         , "pages.st"
                         , "administration.st"
                         , "userpages/accountpage.st"
                         , "userpages/userotherpages.st"
                         , "docpages/doclist.st"
                         , "docpages/doctexts.st"
                         , "docpages/docviewutils.st"
                         , "docpages/docview.st"
                         , "docpages/docsignview.st"
                         , "docpages/docdesign.st"
                         , "docpages/docupload.st"
                         , "docpages/docseal.st"
                         , "docpages/attachmentpage.st"
                         , "docpages/paddeviceview.st"
                         , "docpages/verification.st"
                         , "apppages.st"
                         , "staticpages/priceplanpage.st"
                         , "staticpages/loginpage.st"
                         , "eleg.st"
                         , "oauth/dashboard.st"
                         , "oauth/accept.st"
                         , "javascript-langs.st"
                         , "evidencelog/htmllog.st"
                         , "evidencelog/evidenceOfIntent.st"
                         , "evidencelog/texts.st"
                         , "payments.st"
                         , "evidencelog/simplified.st"
                         ]

textsDirectory :: FilePath
textsDirectory = "texts"

getAllTemplates :: IO [(String, String)]
getAllTemplates = concat `fmap` mapM getTemplates templateFiles

type KontrakcjaGlobalTemplates = TL.GlobalTemplates
type KontrakcjaTemplates = TL.Templates

readGlobalTemplates :: MonadIO m => m KontrakcjaGlobalTemplates
readGlobalTemplates = TL.readGlobalTemplates textsDirectory templateFiles $ map show allLangs

localizedVersion :: Lang -> KontrakcjaGlobalTemplates -> KontrakcjaTemplates
localizedVersion lang = TL.columnizedVersion $ show lang

getTemplatesModTime :: IO ClockTime
getTemplatesModTime = TL.getTemplatesModTime textsDirectory templateFiles

getTextTemplates :: Lang -> IO [(String,String)]
getTextTemplates lang = TT.getTextTemplatesFor textsDirectory (show lang) otherLangs
    where otherLangs = map show (filter (/= lang) allValues :: [Lang])

instance (Functor m, Monad m) => T.TemplatesMonad (ReaderT KontrakcjaGlobalTemplates m) where
  getTemplates = T.getTextTemplatesByColumn $ show (defaultValue :: Lang)
  getTextTemplatesByColumn lang = do
      globaltemplates <- ask
      return $ TL.columnizedVersion lang globaltemplates

getLocalTemplates :: T.TemplatesMonad m => Lang -> m TL.Templates
getLocalTemplates = T.getTextTemplatesByColumn . show

renderLocalTemplate :: (HasLang a, T.TemplatesMonad m) => a -> String -> F.Fields m () -> m String
renderLocalTemplate haslang name fields = do
  ts <- getLocalTemplates $ getLang haslang
  T.renderHelper ts name fields

renderLocalTemplate_ :: (HasLang a, T.TemplatesMonad m) => a -> String -> m String
renderLocalTemplate_ haslang name = renderLocalTemplate haslang name $ return ()

instance CryptoRNG m => CryptoRNG (T.TemplatesT m) where
    getCryptoRNGState = T.TemplatesT $ ReaderT $ \_r -> getCryptoRNGState
