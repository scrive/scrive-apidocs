-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.TemplatesFiles
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Names of all templates files with some utils
-----------------------------------------------------------------------------
module Templates.TemplatesFiles
    (   templatesFilesPath
      , templateFilePath
      , templateFiles
      , getTemplates
    ) where

import Data.Char
import Data.List
import Data.Maybe
import System.IO
{-Names of template files -}

templatesFilesPath :: [String]
templatesFilesPath = map templateFilePath templateFiles

templateFilePath :: String -> String
templateFilePath fn =  "templates/" ++ fn

templateFiles :: [String]
templateFiles = ["landpages.st",
                 "modals.st",
                 "flash.st",
                 "mails.st",
                 "nicemail.st",
                 "utils.st",
                 "listutils/listutils.st",
                 "pages.st",
                 "administration.st",
                 "userpages/accountpage.st",
                 "userpages/companypage.st",
                 "userpages/companyaccountslist.st",
                 "userpages/mailapipage.st",
                 "userpages/securitypage.st",
                 "userpages/usagestats.st",
                 "userpages/userotherpages.st",
                 "docpages/doclist.st",
                 "docpages/doctemplatelist.st",
                 "docpages/docofferslist.st",
                 "docpages/docattachmentlist.st",
                 "docpages/doclistutils.st",
                 "docpages/doctexts.st",
                 "docpages/docviewutils.st",
                 "docpages/docview.st",
                 "docpages/docsignview.st",
                 "docpages/docdesign.st",
                 "docpages/docupload.st",
                 "docpages/docsignatories.st",
                 "docpages/docorderslist.st",
                 "docpages/docseal.st",
                 "docpages/attachmentpage.st",
                 "docpages/rubbishbinlist.st",
                 "apppages.st",
                 "firstpage.st",
                 "staticpages/sitemap.st",
                 "staticpages/priceplanpage.st",
                 "staticpages/securitypage.st",
                 "staticpages/legalpage.st",
                 "staticpages/privacypolicypage.st",
                 "staticpages/termspage.st",
                 "staticpages/aboutpage.st",
                 "staticpages/partnerspage.st",
                 "staticpages/clientspage.st",
                 "staticpages/contactus.st",
                 "staticpages/api.st",
                 "staticpages/scrivebymail.st",
                 "eleg.st",
                 "api/embedded.st",
                 "api/services.st",
                 "javascript-langs.st",
                 "evidencelog/htmllog.st"]


getTemplates :: String -> IO [(String, String)]
getTemplates fp =
    withFile fp ReadMode $ \handle -> do
        hSetEncoding handle utf8
        parseTemplates handle


parseTemplates :: Handle -> IO [(String,String)]
parseTemplates handle = do
    e <- hIsEOF handle
    if (e)
        then return []
        else do
               t  <- parseTemplate handle
               ts <- parseTemplates handle
               return $ (maybeToList t) ++ ts

parseTemplate :: Handle -> IO (Maybe (String, String))
parseTemplate handle = do
    ls <- parseLines handle
    let (name,t) = break (==  '=') $ head ls
    if (null ls || null (name) || null t)
        then return Nothing
        else do
            let template = intercalate "\r\n" ((tail t): (tail ls))
            return $ Just (filter isAlphaNum name,template)

parseLines :: Handle -> IO [String]
parseLines handle = do
    l <- hGetLine handle
    e <- hIsEOF handle
    if (isPrefixOf ("#") l)
        then return []
        else if e
            then return [l]
            else fmap ((:) l) (parseLines handle)


