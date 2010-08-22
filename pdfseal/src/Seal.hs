{-# OPTIONS_GHC -fglasgow-exts #-}

module Seal where
import PdfModel
import System.Environment
import Data.Maybe
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.Char as Char
import Misc

data SealPerson = 
    SealPerson { sealPerson :: String
               , sealPersonSmall :: String
               }
    deriving (Eq,Ord,Show,Read)

data SealSpec = SealSpec 
    { sealInput :: String
    , sealOutput :: String
    , sealDocumentNumber :: Int
    , sealPersons :: [SealPerson]
    }
    deriving (Eq,Ord,Show,Read)

{-
BT
0.863 0.43 0.152 0.004 k
/SkrivaPaGS1 gs
/SkrivaPaT1_0 1 Tf
6.3716 0 0 6.4351 187.6509 19.251 Tm
( )Tj
5.9408 0 0 6 189.562 19.251 Tm
[(A)60(vt)60(al N)20(r 123456789101112131415161718)]TJ
25.152 0 Td
[(s)20(kriv)55(aP)70(\345 F\366)30(r)15(s)20(egl)-20(a)65(t)]TJ
ET
-}

--lastPageSealFileName = "files/seal.pdf"
--paginationSealFileName = "files/pagination.pdf"
sealFileName = "files/seal2.pdf"

listPageRefIDSFromPages :: Document -> RefID -> [RefID]
listPageRefIDSFromPages document pagesrefid =
    let
       Just (Indir (Dict pages) _) = PdfModel.lookup pagesrefid document
       unKids (Ref r) = r
       list = case Prelude.lookup (BS.pack "Kids") pages of
                Just (Array kids) -> concatMap (listPageRefIDSFromPages document . unKids) kids
                Nothing -> [pagesrefid]
    in list

listPageRefIDs :: Document -> [RefID]
listPageRefIDs document =
    let
        firstBody = head (documentBodies document)
        trailer = bodyTrailer firstBody
        Just (Ref root) = Prelude.lookup (BS.pack "Root") trailer
        Just (Indir (Dict catalog) _) = PdfModel.lookup root document
        Just (Ref pagesrefid) = Prelude.lookup (BS.pack "Pages") catalog
    in listPageRefIDSFromPages document pagesrefid

getResDict :: Document -> RefID -> DictData
getResDict doc pageid =
    let
        -- FIXME: refs or something
        Just (Indir (Dict pagedict) _) = PdfModel.lookup pageid doc
        resource = case Prelude.lookup (BS.pack "Resources") pagedict of
          Nothing -> error (show pagedict)
          Just (Dict value) -> value
          Just (Ref refid) -> case PdfModel.lookup refid doc of
                                Just (Indir (Dict value) _) -> value
                                _ -> error "2"
    in resource
        
mergeResourceBranch :: Document -> DictData -> DictData -> DictData
mergeResourceBranch document source1 source2 = source1 ++ source2
{-
    map merge source1
    where
      merge (key,Dict value) = 
          let
              Dict value2 = maybe (Dict []) id $ Prelude.lookup key source2
          in (key,Dict (value ++ value2))
      merge (key,Ref refid) = 
          case PdfModel.lookup refid document of
            Just (Indir value _) -> merge (key,value)
            _ -> merge (key,Dict [])
      merge (key,x) = (key,x)
-}

mergeResources :: Document -> DictData -> DictData -> DictData
mergeResources document source1 source2 =
    Map.toList map3
    where
      map1 = Map.fromList source1
      map2 = Map.fromList source2
      map3 = Map.unionWith unite map1 map2
      unite (Ref refid) x = 
          case PdfModel.lookup refid document of
            Just (Indir value _) -> unite value x
            _ -> x
      unite x (Ref refid) = 
          case PdfModel.lookup refid document of
            Just (Indir value _) -> unite x value
            _ -> x
      unite (Dict d1) (Dict d2) =
          Dict (mergeResourceBranch document d1 d2)
      unite x y = x

placeSealOnPageRefID :: RefID -> String -> RefID -> Document -> Document
placeSealOnPageRefID sealrefid sealtext pagerefid document = 
    let
        Just (Indir (Dict pagedict) pagestrem) = 
            PdfModel.lookup pagerefid document
        Just contentvalue = Prelude.lookup (BS.pack "Contents") pagedict
        contentlist = case contentvalue of
                        Ref{} -> [contentvalue]
                        Array arr -> arr

        ([q,qQ], docx) = flip runState document $ do
            q <- addStream (Dict []) $ BSL.pack "q "
            qQ <- addStream (Dict []) $ BSL.pack ( " Q " ++ rotationtext ++ " " ++ sealtext)
            return [q,qQ]

        sealpagecontents = contentsValueListFromPageID document sealrefid

        rotatekey = Prelude.lookup (BS.pack "Rotate") pagedict

        rotationtext = case rotatekey of -- 595 842
                         Just (Number 90) -> "0 1 -1 0 595 0 cm"
                         Just (Number 180) -> "-1 0 0 -1 595 842 cm"
                         Just (Number 270) -> "0 -1 1 0 0 842 cm"
                         _ -> ""

        sealresdict = getResDict document sealrefid
        pageresdict = getResDict document pagerefid
        newresdict = Dict $ mergeResources document sealresdict pageresdict
                   

        newcontentarray = Array ([Ref q] ++ contentlist ++ [Ref qQ] ++ map Ref sealpagecontents)
        skipkey x = BS.pack "Contents" == x || BS.pack "Resources" == x
        pagedict2 = filter (not . skipkey . fst) pagedict
        newpagedict = Dict pagedict2 `ext` [(BS.pack "Contents", newcontentarray)
                                           ,(BS.pack "Resources", newresdict)]
        newpage = (Indir newpagedict pagestrem)
        newdocument = setIndirF pagerefid newpage docx
    in newdocument

placeSeals :: RefID -> String -> RefID -> String -> Document -> Document
placeSeals sealrefid sealtext paginrefid pagintext document =
    let
        pages = listPageRefIDs document
        paginpages = init pages
        sealpage = last pages
        newdocument1 = foldr (placeSealOnPageRefID paginrefid pagintext) document paginpages
        newdocument = foldr (placeSealOnPageRefID sealrefid sealtext) newdocument1 [sealpage]
    in newdocument

{- 
process1 sourceFileName destinationFileName = do
    Just doc <- PdfModel.parseFile sourceFileName
    Just seal <- PdfModel.parseFile "seal.pdf"
    let ([newcontentrefid],doc1) = 
            runState (importObjects seal [refid 8 0]) doc
    let doc2 = placeSeals newcontentrefid doc1
    putStrLn "Writting file..."
    writeFileX destinationFileName doc2
    return ()
-}

contentsValueListFromPageID :: Document -> RefID -> [RefID]
contentsValueListFromPageID document pagerefid = 
    let
        Just (Indir (Dict pagedict) pagestrem) = 
            PdfModel.lookup pagerefid document
        Just contentvalue = Prelude.lookup (BS.pack "Contents") pagedict
        contentlist = case contentvalue of
                        Ref r -> [r]
                        Array arr -> map unRefID arr
        unRefID (Ref r) = r
    in contentlist

process (SealSpec 
    { sealInput
    , sealOutput
    , sealDocumentNumber
    , sealPersons}) = do
    Just doc <- PdfModel.parseFile sealInput
    --Just pagin <- PdfModel.parseFile paginationSealFileName
    --Just seal <- PdfModel.parseFile lastPageSealFileName 
    Just seal <- PdfModel.parseFile sealFileName 
    --let sealpage1 = head $ listPageRefIDs seal
    --let paginpage1 = head $ listPageRefIDs pagin
    let [paginpage1, sealpage1] = listPageRefIDs seal
    let sealNumberText = let s = show sealDocumentNumber
         in take (10-length s) "0000000000" ++ s 
    let ([newsealcontents,newpagincontents],doc1) = 
            runState (do
                          --[a] <- importObjects seal [sealpage1] 
                          --[b] <- importObjects pagin [paginpage1]
                          [a,b] <- importObjects seal [paginpage1,sealpage1]
                          return [b,a] ) doc
        pagintext = -- BS.pack $
                    "BT " ++
                    "0.863 0.43 0.152 0.004 k " ++
                    "/SkrivaPaGS1 gs " ++
                    "/SkrivaPaT1_0 1 Tf " ++
                    "5.9408 0 0 6 267 19.957 Tm " ++
                    "[(A)60(vt)60(al N)20(r " ++ sealNumberText ++ ")]TJ " ++
                    "5.9408 0 0 6 360 19.957 Tm " ++
                    "[(S)20(kriv)55(aP)70(\\345)]TJ " ++
                    "ET "
        sealtext = --BS.pack $ 
            movemtx ++
            "BT " ++
            "0.855 0.422 0.152 0.004 k " ++
            "/SkrivaPaGS1 gs " ++
            "/SkrivaPaT1_0 1 Tf " ++
            "8 0 0 8 62.707 71.735 Tm " ++
            "[(A) 60 (vt) 60 (al N) 20 (r ) 0 (" ++ sealNumberText ++ ")] TJ " ++
            "7.9999 0 0 8 62.9619 49.7622 Tm " ++
            "[(U)40(nd)10(e)20(r)38(teckna)65(t)5(:)]TJ " ++
            "15 TL " ++
            "1 0 0 1 137.4512 49.7856 Tm " ++
            concatMap seal1text sealPersons ++
            "ET "
        seal1text (SealPerson {sealPerson,sealPersonSmall})=
            "/SkrivaPaT1_0 10 Tf " ++
            "[(" ++ map unicodeToWinAnsi sealPerson ++ ")] TJ " ++
            "/SkrivaPaT1_0 6 Tf " ++
            "[(" ++ map unicodeToWinAnsi sealPersonSmall ++ ")] TJ " ++
            "T* "
        movemtx = if length sealPersons>2
                  then "1 0 0 1 0 " ++ show ((length sealPersons-2)*15) ++ " cm "
                  else ""


    let doc2 = placeSeals newsealcontents sealtext newpagincontents pagintext doc1
    writeFileX sealOutput doc2
    return ()


