{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.Templates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- This is main templating module. Basic interface contains 
-- 'renderTemplate' and 'renderTemplateComplex' and 'setAttribute'.
-- 'renderTemplate'  takes template name and list of name<->string params maps
-- 'renderTemplateComplex' can handle passing advanced data structures to template
-- This module also provides reading template files, some checking and template info
-- 'KontrakcjaTemplates' is alias for template group, but ma be changed in a future
-----------------------------------------------------------------------------
module Templates.Templates
    (readTemplates,renderTemplate,renderTemplate',renderTemplateComplex,templateList,KontrakcjaTemplates,Templates.Templates.setAttribute) where

import Text.StringTemplate 
import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import System.Log.Logger

{-Names of template files -}
templateFiles::[String]
templateFiles = ["templates/landpages.st",
                 "templates/flash.st",
                 "templates/mails.st",
                 "templates/utils.st",
                 "templates/pages.st",
                 "templates/payments.st",
                 "templates/administration.st",
                 "templates/userpages.st",
                 "templates/docpages.st"]

type KontrakcjaTemplates =  STGroup String
type KontrakcjaTemplate = StringTemplate String
{- Filling template with a given name using given attributes
   It never fail, just returns empty message and writes something in the logs
   HStringTemplate fails with UTF8 bytestrings so basic interface supports strings and lists of strings
-}
renderTemplate::KontrakcjaTemplates ->String->[(String, String)] ->  IO String
renderTemplate ts name attrs = renderTemplateMain ts name attrs id

renderTemplate'::KontrakcjaTemplates ->String->[(String, [String])] ->  IO String
renderTemplate'  ts name attrs = renderTemplateMain ts name attrs id


{-This is special templating function . Use it carefull'y with setAttributes composition as last param
  Remember that setAttributes can work with maps, data structures and other not-string stuff.
  It should be used when template has some logic (usually iteration).See payments view for example
-}
renderTemplateComplex::KontrakcjaTemplates ->String->(KontrakcjaTemplate -> KontrakcjaTemplate) ->  IO String
renderTemplateComplex  ts name f = renderTemplateMain ts name ([]::[(String, String)]) f

{-Use this as (setAttributes name1 val1) . (setAttributes name2 val2) . (setAttributes name3 val3) -}
setAttribute :: (ToSElem a) => String -> a -> KontrakcjaTemplate -> KontrakcjaTemplate
setAttribute =  Text.StringTemplate.setAttribute

--This is avaible only for special cases
renderTemplateMain::(ToSElem a)=>KontrakcjaTemplates ->String->[(String, a)] -> (KontrakcjaTemplate-> KontrakcjaTemplate)->  IO String
renderTemplateMain ts name params f = do 
                               let mt =  getStringTemplate name ts
                               case mt of 
                                  Just t -> do
                                            let t'= f (setManyAttrib params  t)   
                                            let (e,p,st) = checkTemplateDeep t'
                                            when (not (null e) || not (null p) || not (null st)) $
                                                    errorM "Happstack.Server" $ "Template " ++ name ++ " problem with message " ++(show (e,p,st)) 
                                            return $ render t'
                                  Nothing -> do
                                              errorM "Happstack.Server" $ "No template named " ++ name 
                                              return ""                                   
readTemplates::IO KontrakcjaTemplates 
readTemplates =   do
                   ts <- sequence (map getTemplates templateFiles)
                   return $ groupStringTemplates (concat ts)


getTemplates::String -> IO [(String, StringTemplate String)]            
getTemplates fp= do
                 handle <- openFile fp ReadMode
                 hSetEncoding handle utf8
                 ts <- parseTemplates handle
                 hClose handle
                 return $ Prelude.map (fromJust) $ Prelude.filter isJust ts

parseTemplates::Handle-> IO [Maybe (String,StringTemplate String) ]
parseTemplates handle =  do
                     e <- hIsEOF handle
                     if (e) 
                      then return []
                      else liftM2 (:) (parseTemplate handle) (parseTemplates handle)

parseTemplate::Handle->IO (Maybe (String,StringTemplate String))                              
parseTemplate handle = do
                        ls <- parseLines handle
                        let (name,t) = break (==  '=') $ head ls 
                        if (null ls || null (name) || null t)
                         then return Nothing   
                         else do
                              let template = intercalate "\r\n" ((tail t): (tail ls)) 
                              return $ Just (filter isAlphaNum name,newSTMP template)
parseLines::Handle->IO [String]                                    
parseLines handle = do
                     l <- hGetLine handle
                     e <- hIsEOF handle
                     if (e || isPrefixOf ("#") l) 
                      then return []
                      else fmap ((:) l) (parseLines handle)

{- Template checker, printing info about params-}
templateList :: IO ()
templateList = do 
                ts <- fmap concat $ sequence (map getTemplates templateFiles)         
                let tsnames = map fst ts
                let tsgroup = groupStringTemplates ts
                sequence_ $ map (printTemplateData tsgroup) tsnames
                
printTemplateData::STGroup String -> String -> IO ()
printTemplateData tsgroup name =  do 
                                   let Just t = getStringTemplate name tsgroup
                                   let (e,p,st) = checkTemplateDeep t
                                   putStrLn $ name ++ ": " 
                                   if (not $ null e) 
                                      then putStrLn $ "PARSE ERROR " ++ (show e)
                                      else if (not $ null st)  
                                            then putStrLn $ "MISING SUBTEMPLATES " ++ (show st)
                                            else 
                                              do
                                               sequence_ $ map (putStrLn . ("    " ++ )) p
                                               putStrLn ""