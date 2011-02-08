{-# OPTIONS_GHC -Wall -XOverlappingInstances -XTypeSynonymInstances #-}
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
import Data.Map (fromList)
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
               let val = fmap (SM . fromList) $ sequence $ map packIO $ execState b [] 
               put ((a,val):s)



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
-- | after rendering

class (HST.ToSElem a) => ToSElem a where
  toSElem:: (Stringable b) => a -> SElem b

instance (HST.ToSElem a) => ToSElem a where
  toSElem = HST.toSElem

instance ToSElem BS.ByteString where
  toSElem = HST.toSElem . BS.toString                     