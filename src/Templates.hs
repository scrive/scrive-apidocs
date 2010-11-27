{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}
module Templates(renderTemplate,renderTemplate',wrapHTML) where

import Text.StringTemplate 
import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import System.Log.Logger

{-Names of template files -}
templateFiles::[String]
templateFiles = ["templates/landpages.st","templates/flash.st","templates/mails.st","templates/utils.st"]


{- Filling template with a given name using given attributes
   It never fail, just returns empty message and writes something in the logs
   In next version it will be shared, but for now it reads all files for every template
-}
renderTemplate::String->[(String, String)] ->  IO String
renderTemplate = renderTemplateMain

renderTemplate'::String->[(String, [String])] ->  IO String
renderTemplate'= renderTemplateMain

renderTemplateMain::(ToSElem a)=>String->[(String, a)] ->  IO String
renderTemplateMain name params = do 
                               ts <- sequence (map getTemplates templateFiles)
                               let ts' = groupStringTemplates (concat ts)
                               let mt =  getStringTemplate name ts'
                               case mt of 
                                  Just t -> return $ render (setManyAttrib params  t)
                                  Nothing -> do
                                              errorM "Happstack.Server" $ "Template problem with " ++ name 
                                              return $ ""
                               


getTemplates::String -> IO [(String, StringTemplate String)]            
getTemplates fp= do
                 handle <- openFile fp ReadMode
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
                              let template = concat ((tail t): (tail ls)) 
                              return $ Just (filter isAlphaNum name,newSTMP template)
parseLines::Handle->IO [String]                                    
parseLines handle = do
                     l <- hGetLine handle
                     e <- hIsEOF handle
                     if (e || isPrefixOf ("#") l) 
                      then return []
                      else fmap ((:) l) (parseLines handle)

{- Common templates - should be shared and it seams like a good place fo them -}

wrapHTML::String->IO String
wrapHTML body =  renderTemplate "wrapHTML" [("body",body)]
