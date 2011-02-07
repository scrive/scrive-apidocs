{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
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
(KontrakcjaTemplates, KontrakcjaTemplate, renderTemplateMain,templateList, readTemplates) where

import Text.StringTemplate 
import Text.StringTemplate.Base
import Text.StringTemplate.Classes
import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import System.Log.Logger
import Text.Html (stringToHtmlString)
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
                 "templates/docpages/doclist.st",
                 "templates/docpages/doctexts.st",
                 "templates/docpages/docview.st",
                 "templates/apppages.st"]

type KontrakcjaTemplates =  STGroup String
type KontrakcjaTemplate = StringTemplate String

--This is avaible only for special cases
renderTemplateMain::(ToSElem a)=>KontrakcjaTemplates ->String->[(String, a)] -> (KontrakcjaTemplate-> KontrakcjaTemplate)->  IO String
renderTemplateMain ts name params f = do 
                               let ts' = setEncoderGroup stringToHtmlString ts
                               let noescape = groupStringTemplates [("noescape", newSTMP "$it$" :: StringTemplate String)]
                               let mt =  getStringTemplate name $ mergeSTGroups noescape ts'
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



{- For some reasons we SElem a is not of class ToSElem -}
instance  (Stringable a) => ToSElem (SElem a) where
   toSElem (STR a) = (STR a)    
   toSElem (BS a) = (BS a)    
   toSElem (STSH a) = (STSH a)    
   toSElem (SM a) = (SM $ fmap (toSElem) a)       
   toSElem (LI a) = (LI $ fmap (toSElem) a)       
   toSElem (SBLE a ) = (SBLE $ convert a)       
   toSElem (SNAT a ) = (SNAT $ convert a)       
   toSElem SNull = SNull       


convert = stFromString . stToString