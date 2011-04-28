{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
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
    , renderTemplateMain
    , templateList
    , readTemplates
    , getTemplatesModTime
    ) where

import Text.StringTemplate 
import Text.StringTemplate.Base
import Text.StringTemplate.Classes
import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import AppLogger as Log
import Text.Html (stringToHtmlString)
import System.Directory
import System.Time

{-Names of template files -}
templateFiles :: [String]
templateFiles = ["templates/landpages.st",
                 "templates/modals.st",
                 "templates/flash.st",
                 "templates/mails.st",
                 "templates/utils.st",
                 "templates/listutils/listutils.st",
                 "templates/pages.st",
                 "templates/payments.st",
                 "templates/administration.st",
                 "templates/userpages/accountpage.st",
                 "templates/userpages/subaccountslist.st",
                 "templates/userpages/friendpage.st",
                 "templates/userpages/securitypage.st",
                 "templates/userpages/userotherpages.st",                     
                 "templates/docpages/doclist.st",
                 "templates/docpages/doctemplatelist.st",
                 "templates/docpages/docofferslist.st",
                 "templates/docpages/doctexts.st",
                 "templates/docpages/docviewutils.st",
                 "templates/docpages/docviewforauthor.st",
                 "templates/docpages/docviewforsignatory.st",
                 "templates/docpages/docviewforviewer.st",
                 "templates/docpages/docdesign.st",
                 "templates/docpages/docupload.st",
                 "templates/docpages/doctemplates.st",
                 "templates/docpages/docsignatories.st",
                 "templates/docpages/docseal.st",
                 "templates/contacts/contacts.st",
                 "templates/apppages.st",
                 "templates/firstpage.st",
                 "templates/priceplanpage.st",
                 "templates/securitypage.st",
                 "templates/legalpage.st",
                 "templates/privacypolicypage.st",
                 "templates/termspage.st",
                 "templates/aboutpage.st",
                 "templates/partnerspage.st",
                 "templates/clientspage.st",
                 "templates/eleg.st" ]


type KontrakcjaTemplates = STGroup String
type KontrakcjaTemplate = StringTemplate String

--This is avaible only for special cases
renderTemplateMain :: (ToSElem a) => KontrakcjaTemplates -> String -> [(String, a)]
                   -> (KontrakcjaTemplate -> KontrakcjaTemplate) -> IO String
renderTemplateMain ts name params f = do 
    let ts' = setEncoderGroup stringToHtmlString ts
    let noescape = groupStringTemplates [("noescape", newSTMP "$it$" :: StringTemplate String)]
    let mt =  getStringTemplate name $ mergeSTGroups noescape ts'
    case mt of 
        Just t -> do
                let t'= f (setManyAttrib params  t)   
                let (e,p,st) = checkTemplateDeep t'
                when (not (null e) || not (null p) || not (null st)) $
                     Log.error $ "Template " ++ name ++ " problem with message " ++ (show (e,p,st)) 
                return $ render t'
        Nothing -> do
                Log.error $ "No template named " ++ name 
                return ""                                   

getTemplatesModTime :: IO ClockTime
getTemplatesModTime = do
    mtimes <- mapM getModificationTime templateFiles
    return (maximum mtimes)

readTemplates :: IO KontrakcjaTemplates 
readTemplates = do
    ts <- mapM getTemplates templateFiles
    return $ groupStringTemplates (concat ts)

getTemplates :: String -> IO [(String, StringTemplate String)]            
getTemplates fp = 
    withFile fp ReadMode $ \handle -> do
        hSetEncoding handle utf8
        ts <- parseTemplates handle
        return $ catMaybes ts

parseTemplates :: Handle -> IO [Maybe (String,StringTemplate String)]
parseTemplates handle = do
    e <- hIsEOF handle
    if (e) 
        then return []
        else liftM2 (:) (parseTemplate handle) (parseTemplates handle)

parseTemplate :: Handle -> IO (Maybe (String,StringTemplate String))                              
parseTemplate handle = do
    ls <- parseLines handle
    let (name,t) = break (==  '=') $ head ls 
    if (null ls || null (name) || null t)
        then return Nothing   
        else do
            let template = intercalate "\r\n" ((tail t): (tail ls)) 
            return $ Just (filter isAlphaNum name,newSTMP template)

parseLines :: Handle -> IO [String]                                    
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
                
printTemplateData :: STGroup String -> String -> IO ()
printTemplateData tsgroup name = do 
    let Just t = getStringTemplate name tsgroup
    let (e,p,st) = checkTemplateDeep t
    putStrLn $ name ++ ": " 
    if (not $ null e) 
        then putStrLn $ "PARSE ERROR " ++ (show e)
        else if (not $ null st)  
             then putStrLn $ "MISSING SUBTEMPLATES " ++ (show st)
             else do
                 sequence_ $ map (putStrLn . ("    " ++ )) p
                 putStrLn ""    



{- For some reasons the SElem a is not of class ToSElem -}
instance (Stringable a) => ToSElem (SElem a) where
   toSElem (STR a) = (STR a)    
   toSElem (BS a) = (BS a)    
   toSElem (STSH a) = (STSH a)    
   toSElem (SM a) = (SM $ fmap (toSElem) a)       
   toSElem (LI a) = (LI $ fmap (toSElem) a)       
   toSElem (SBLE a ) = (SBLE $ convert a)       
   toSElem (SNAT a ) = (SNAT $ convert a)       
   toSElem SNull = SNull       


convert :: (Stringable a, Stringable b) => a -> b
convert = stFromString . stToString
