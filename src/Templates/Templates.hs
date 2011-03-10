{-# OPTIONS_GHC -Wall -XOverlappingInstances -XTypeSynonymInstances#-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.Templates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- This is main templating module. It provides basic interface for generation templates (RenderTemplate)
-- with 'renderTemplate' function. We also provide types for templates and few functions for loading and testing
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
--        If You will forget a param  there will be info in log, and template set param value to something empty.
--
--
-- FIELDS
--
-- Current policy is to use fields. You can find usage of [(String,String)] as params and also composition of setAttribute functions in a code.
-- This are old concepts and will be droped at some point.
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
module Templates.Templates
(RenderTemplate, readTemplates,renderTemplate,templateList,KontrakcjaTemplates,Templates.Templates.setAttribute, Fields, Field , field) where

import System.IO
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Char
import Templates.TemplatesLoader (readTemplates,renderTemplateMain,templateList,KontrakcjaTemplate,KontrakcjaTemplates)
import Text.StringTemplate.Base hiding (ToSElem,toSElem)
import Text.StringTemplate.Classes hiding (ToSElem,toSElem)
import qualified Text.StringTemplate.Classes as HST
import qualified Text.StringTemplate (setAttribute)
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS

{- Filling template with a given name using given attributes
   It never fail, just returns empty message and writes something in the logs
   Params - Templates (loaded from local files), Name of template, somethinmg that sets params value
-}

class RenderTemplate a where
  renderTemplate::KontrakcjaTemplates ->String -> a -> IO String 

{- Basic rendering interface 
   It allows to pass some string attributes to templates. Usefull when working with simple templates
 -}
instance RenderTemplate () where
   renderTemplate ts name () = renderTemplateMain ts name ([]::[(String,String)]) id
   
instance RenderTemplate [(String, String)] where
   renderTemplate ts name attrs = renderTemplateMain ts name attrs id

instance RenderTemplate [(String, [String])] where
   renderTemplate ts name attrs = renderTemplateMain ts name attrs id


{- More advanced schema allows to pass to template params for complex types.
   This can be done by passing as last param composition of setAttributeFunction
-}

instance RenderTemplate (KontrakcjaTemplate -> KontrakcjaTemplate) where
   renderTemplate ts name f = renderTemplateMain ts name ([]::[(String, String)]) f
   

{-Use this as (setAttributes name1 val1) . (setAttributes name2 val2) . (setAttributes name3 val3) -}
setAttribute :: (ToSElem a) => String -> a -> KontrakcjaTemplate -> KontrakcjaTemplate
setAttribute name value =  Text.StringTemplate.setAttribute name (toSElem value::SElem String)

type Fields =   State ([(String,IO (SElem String))]) ()

class Field a where 
  field::String ->  a -> Fields

instance (ToSElem a) => Field (IO a) where 
  field a b = do
             s <- get 
             put ((a,fmap toSElem b):s)


instance (ToSElem a) => Field a where 
  field a b = do
             s <- get 
             put ((a,return $ toSElem b):s)


instance Field (Fields) where 
  field a b =  do
               s <- get 
               let val = fmap toSElem $ sequence $ map packIO $ execState b [] 
               put ((a,val):s)

instance Field [Fields] where 
  field a fs =  do
               s <- get 
               let vals f = fmap Map.fromList $ sequence $ map packIO $ execState f [] 
               put ((a,fmap toSElem $ mapM vals fs):s)
               

instance RenderTemplate Fields where
   renderTemplate ts name fields = do
                                    attrs <- sequence $ map packIO $ execState fields [] 
                                    renderTemplateMain ts name ([]::[(String, String)]) (setManyAttrib attrs)
   

packIO::(a, IO b) -> IO (a,b)
packIO (name,comp)= do 
                     res <- comp
                     return (name,res)
                     
                     
                     

-- | Importan Util. We overide default serialisation to support serialisation of bytestrings .
-- | We use ByteString with UTF all the time but default is Latin-1 and we get strange chars 
-- | after rendering. !This will not always work with advanced structures.! So always convert to String.

class ToSElem a where
  toSElem:: (Stringable b) => a -> SElem b

instance (HST.ToSElem a) => ToSElem a where
  toSElem = HST.toSElem

instance ToSElem BS.ByteString where
  toSElem = HST.toSElem . BS.toString                     

instance ToSElem (Maybe BS.ByteString) where
  toSElem = HST.toSElem . fmap BS.toString           
  
instance ToSElem [BS.ByteString] where
  toSElem = toSElem . fmap BS.toString     
  
instance (HST.ToSElem a) => ToSElem [a] where
  toSElem [] = SNull
  toSElem l  = LI $ map HST.toSElem l 

instance (HST.ToSElem a) => ToSElem (Map.Map String a) where
  toSElem m = if (Map.null m)
                then SNull   
                else SM $ Map.map HST.toSElem m
  