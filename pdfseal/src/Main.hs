{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where
import PdfModel
import System.Environment
import Data.Maybe
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BS


listPageRefIDSFromPages :: Document -> RefID -> [RefID]
listPageRefIDSFromPages document pagesrefid =
    let
       Just (Indir (Dict pages) _) = PdfModel.lookup pagesrefid document
       unKids (Ref r) = r
       list = case Prelude.lookup (BS.pack "Kids") pages of
                Just (Array kids) -> concatMap (listPageRefIDSFromPages document . unKids) kids
                Nothing -> [pagesrefid]
    in list

listPageRefIDs :: Document -> RefID -> [RefID]
listPageRefIDs document catalogrefid =
    let
        Just (Indir (Dict catalog) _) = PdfModel.lookup catalogrefid document
        Just (Ref pagesrefid) = Prelude.lookup (BS.pack "Pages") catalog
    in listPageRefIDSFromPages document pagesrefid

placeSealOnPageRefID :: RefID -> Document -> Document
placeSealOnPageRefID pagerefid document = 
    let
        Just (Indir pagedict pagestrem) = PdfModel.lookup pagerefid document
        newpagedict = pagedict `ext` [(BS.pack "GP",Boolean True)]
        newpage = (Indir newpagedict pagestrem)
        newdocument = setIndirF pagerefid newpage document
    in newdocument

placeSeals :: Document -> Document
placeSeals document =
    let
        firstBody = head (documentBodies document)
        trailer = bodyTrailer firstBody
        Just (Ref root) = Prelude.lookup (BS.pack "Root") trailer
        pages = listPageRefIDs document root
        newdocument = foldr placeSealOnPageRefID document pages
    in newdocument

process sourceFileName destinationFileName = do
    Just doc <- PdfModel.parseFile sourceFileName
    let doc2 = placeSeals doc
    putStrLn "Writting file..."
    writeFileX destinationFileName doc2
    return ()

main = do
    [sourceFileName,destinationFileName] <- getArgs
    process sourceFileName destinationFileName

