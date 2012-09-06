-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.Templates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- This is main templating module. It provides basic interface for
-- generation templates (RenderTemplate) with 'renderTemplate'
-- function. We also provide types for templates and few functions for
-- loading and testing
--
-- HOW TO USE TEMPLATING SYSTEM
--
-- 1) There is a folder called templates, All templates files (*.st) are there. Each files contains many templates
--    definitions, and there must be line starting with # between each two templates.
--    Template definition has form 'nameOfTemplate=bodyOfTemplate'.
--
-- 2) Template body is just String Template and you should be able to find more
--    info at http://www.haskell.org/haskellwiki/HStringTemplate
--
-- 3) All templates are in a global scope. Watch out for conflicting names.
--    On dev computers they are loaded on every request, so one can change them without stoping server.
--
-- 4) To generate a template in haskell call renderTemplate.
--       First param is a set of all templates. Usually you can get it from 'Context'.
--       Next is a name of template that You wan't to render.
--       Last one is some for of list of params.
--       As a result you get IO String.
--        If template will fail You will get error info inside. But this is only for syntax errors.
--        If You will forget a param there will be info in log, and template set param value to something empty.
--
--
-- FIELDS
--
-- Current policy is to use fields. You can find usage of
-- [(String,String)] as params and also composition of setAttribute
-- functions in a code.  This are old concepts and will be droped at
-- some point.
--
-- How to user fields:
--  - there is one function ('field') that sets one field
--  - fields form a monad so you can use do notation for setting many fields
--  - value of a field can be almoust everything (String, Int, Maybe, List, Map, types that are instances of Data and Typeable etc)
--  - IO wrapped values and fields can be also a values of a field.
--
-- Example
-- @
--       userView tempates user =
--         renderTemplate templates "userView" $ do
--             userFields
--
--       userFields user = do
--             field "name" $ username user
--             field "company" $ usercompany user
--             field "documents" $ map (documentFields) getUserDocumentsFromDB
--
--       documentFields document = do
--             field "id" $ documentid document
--             field "title" $ documenttitle document
--
-- @
--
--
-- Why we want to use fields
--      - They force reuse. We write documentFields, and reuse it every time we want to pass document info to template.
--      - Fields can be extended. If I want to have extended info about user I use 'userFields' to set basic info and
--        then add advanced fields
--      - No need to first bind from IO, then pass to template
--      - They support advanced structures like lists and maybe's
--
--
-- Some extra info:
--  In templates use maybe. You can use 'if' in template body to check for Nothing
--  Always change ByteString to String. We have a problems with encoding, so please watch for this.
-----------------------------------------------------------------------------
module Templates.Templates (
    Fields
  , runFields
  , TemplatesMonad(..)
  , renderTemplate
  , renderTemplate_
  , renderTemplateI
  , renderLocalTemplate
  , renderLocalTemplate_
  ) where

import Control.Monad.Trans.Maybe
import Text.StringTemplate.Base hiding (ToSElem, toSElem, render)

import Utils.Default
import User.Locale
import Templates.Fields
import Templates.TemplatesLoader
import Control.Monad.Reader
import Control.Monad.Identity

class (Functor m, Monad m) => TemplatesMonad m where
  getTemplates      :: m KontrakcjaTemplates
  getLocalTemplates :: Locale -> m KontrakcjaTemplates

instance TemplatesMonad m => TemplatesMonad (MaybeT m) where
  getTemplates = lift getTemplates
  getLocalTemplates = lift . getLocalTemplates

instance (Functor m, Monad m) => TemplatesMonad (ReaderT KontrakcjaGlobalTemplates m) where
  getTemplates      = getLocalTemplates defaultValue 
  getLocalTemplates locale = do
      globaltemplates <- ask
      return $ localizedVersion locale globaltemplates

renderTemplate :: TemplatesMonad m => String -> Fields m () -> m String
renderTemplate name fields = do
  ts <- getTemplates
  renderHelper ts name fields

renderTemplateI :: TemplatesMonad m => String -> Fields Identity () -> m String
renderTemplateI name fields = do
  ts <- getTemplates
  return $ renderTemplateMain ts name ([]::[(String, String)]) (setManyAttrib $ runIdentity $ runFields fields)

renderTemplate_ :: TemplatesMonad m => String -> m String
renderTemplate_ name = renderTemplate name $ return ()

renderLocalTemplate :: (HasLocale a, TemplatesMonad m) => a -> String -> Fields m () -> m String
renderLocalTemplate haslocale name fields = do
  ts <- getLocalTemplates $ getLocale haslocale
  renderHelper ts name fields

renderLocalTemplate_ :: (HasLocale a, TemplatesMonad m) => a -> String -> m String
renderLocalTemplate_ haslocale name = renderLocalTemplate haslocale name $ return ()

renderHelper :: Monad m => KontrakcjaTemplates -> String -> Fields m () -> m String
renderHelper ts name fields = do
  attrs <- runFields fields
  return $ renderTemplateMain ts name ([]::[(String, String)]) (setManyAttrib attrs)
