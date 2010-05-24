{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where
import PdfModel
import System.Environment
import Data.Maybe
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BS
import Control.Monad.State.Strict

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

placeSealOnPageRefID :: RefID -> RefID -> Document -> Document
placeSealOnPageRefID sealrefid pagerefid document = 
    let
        Just (Indir (Dict pagedict) pagestrem) = 
            PdfModel.lookup pagerefid document
        Just contentvalue = Prelude.lookup (BS.pack "Contents") pagedict
        contentlist = case contentvalue of
                        Ref{} -> [contentvalue]
                        Array arr -> arr
        -- FIXME: add "q " at the beginning and " Q " at the end
        newcontentarray = Array (contentlist ++ [Ref sealrefid])
        pagedict2 = filter (not . (==) (BS.pack "Contents") . fst) pagedict
        newpagedict = Dict pagedict2 `ext` [(BS.pack "Contents",newcontentarray)]
        newpage = (Indir newpagedict pagestrem)
        newdocument = setIndirF pagerefid newpage document
    in newdocument

placeSeals :: RefID -> Document -> Document
placeSeals sealrefid document =
    let
        firstBody = head (documentBodies document)
        trailer = bodyTrailer firstBody
        Just (Ref root) = Prelude.lookup (BS.pack "Root") trailer
        pages = listPageRefIDs document root
        newdocument = foldr (placeSealOnPageRefID sealrefid) document pages
    in newdocument

process sourceFileName destinationFileName = do
    Just doc <- PdfModel.parseFile sourceFileName
    Just seal <- PdfModel.parseFile "seal.pdf"
    let ([newcontentrefid],doc1) = 
            runState (importObjects seal [refid 8 0]) doc
    let doc2 = placeSeals newcontentrefid doc1
    putStrLn "Writting file..."
    writeFileX destinationFileName doc2
    return ()

main = do
    [sourceFileName,destinationFileName] <- getArgs
    process sourceFileName destinationFileName

