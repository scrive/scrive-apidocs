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
import Graphics.PDF.Text

data Person = 
    Person { fullname :: String
           , company :: String
           , number :: String
           , email :: String
           }
    deriving (Eq,Ord,Show,Read)

data SealSpec = SealSpec 
    { input :: String
    , output :: String
    , documentNumber :: String
    , persons :: [Person]
    , history :: [HistEntry]
    , initials :: String
    }
    deriving (Eq,Ord,Show,Read)

data HistEntry = HistEntry
    { histdate :: String
    , histcomment :: String
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

sealFileName = "files/seal3.pdf"

listPageRefIDSFromPages :: Document -> RefID -> [RefID]
listPageRefIDSFromPages document pagesrefid =
    let
       Just (Indir (Dict pages) _) = PdfModel.lookup pagesrefid document
       unKids (Ref r) = r
       list = case Prelude.lookup (BS.pack "Kids") pages of
                Just (Array kids) -> concatMap (listPageRefIDSFromPages document . unKids) kids
                Nothing -> [pagesrefid]
    in list

addPageToDocument :: Value -> State Document RefID
addPageToDocument (Dict newpagevalue) = do 
  rec
    document <- get
    let
        firstBody = head (documentBodies document)
        trailer = bodyTrailer firstBody
        Just (Ref root) = Prelude.lookup (BS.pack "Root") trailer
        Just (Indir (Dict catalog) _) = PdfModel.lookup root document
        Just (Ref pagesrefid) = Prelude.lookup (BS.pack "Pages") catalog
        Just (Indir (Dict pages) _) = PdfModel.lookup pagesrefid document
        unKids (Ref r) = r
        newkids = case Prelude.lookup (BS.pack "Kids") pages of
                Just (Array kids) -> Array (kids ++ [Ref newpageid])
                Nothing -> error "this never happens"
        newcount = case Prelude.lookup (BS.pack "Count") pages of
                Just (Number count) -> count + 1
                Nothing -> error "this never happens either"
        skipkeys list = filter (\(a,b) -> a/=BS.pack "Count" && a/=BS.pack "Kids") list
        skipkeys2 list = filter (\(a,b) -> a/=BS.pack "Parent") list
        newpagesindir = Indir (Dict ([(BS.pack "Count", Number newcount),(BS.pack "Kids",newkids)] ++ skipkeys pages)) Nothing
    setIndir pagesrefid newpagesindir
    newpageid <- addObject (Dict (skipkeys2 newpagevalue ++ [(BS.pack "Parent", Ref pagesrefid)]))
  return newpageid

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
          Nothing -> []
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

        ([q,qQ,rr], docx) = flip runState document $ do
            q <- addStream (Dict []) $ BSL.pack "q "
            qQ <- addStream (Dict []) $ BSL.pack (" Q " ++ rotationtext ++ " ")
            rr <- addStream (Dict []) $ BSL.pack (sealtext)
            return [q,qQ,rr]

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
                   

        newcontentarray = Array ([Ref q] ++ contentlist ++ [Ref qQ] ++ map Ref sealpagecontents ++ [Ref rr])
        skipkey x = BS.pack "Contents" == x || BS.pack "Resources" == x
        pagedict2 = filter (not . skipkey . fst) pagedict
        newpagedict = Dict pagedict2 `ext` [(BS.pack "Contents", newcontentarray)
                                           ,(BS.pack "Resources", newresdict)]
        newpage = (Indir newpagedict pagestrem)
        newdocument = setIndirF pagerefid newpage docx
    in newdocument

placeSeals :: RefID -> String -> RefID -> String -> State Document ()
placeSeals sealrefid sealtext paginrefid pagintext = do
    pages <- gets listPageRefIDs
    let pagevalue = page_dict (Array []) (Array []) `ext` [entryna "MediaBox" [0,0,595,842]]
    -- should optimize pagintext into one stream
    modify $ \document -> foldr (placeSealOnPageRefID paginrefid pagintext) document pages
    lastpage <- addPageToDocument pagevalue
    modify $ \document -> foldr (placeSealOnPageRefID sealrefid sealtext) document [lastpage]


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

pagintext (SealSpec{documentNumber,initials}) = 
 "BT " ++
 "0.546 0.469 0.454 0.113 k " ++
 "/SkrivaPaHelvetica 1 Tf " ++
 "8 0 0 8 315.3584 10.8433 Tm " ++
 "(U)Tj " ++
 "6 0 0 6 321.1357 10.8433 Tm " ++
 "[(NDER)18(TECKNAT)111(: )]TJ " ++
 "8 0 0 8 368.6992 10.8433 Tm " ++
 "(" ++ initials ++ ")Tj " ++
 "-23.668 0 Td " ++
 "(D)Tj " ++
 "6 0 0 6 185.1362 10.8433 Tm " ++
 "(OK.NR. )Tj " ++
 "8 0 0 8 207.4722 10.8433 Tm " ++
 "(" ++ documentNumber ++ ")Tj " ++
 "ET "

signatorybox (Person {fullname,company,number,email}) = 
 let
    orgnroffset = textWidth (PDFFont Helvetica_Oblique 12) (toPDFString $ "Org.Nr. " ++ number)
    emailoffset = textWidth (PDFFont Helvetica_Oblique 12) (toPDFString email)
    rightmargin = 595 - 46.5522
 in
 "BT " ++
 "0.806 0.719 0.51 0.504 k " ++
 "/TT1 1 Tf " ++
 "12 0 0 12 46.5522 707.3906 Tm " ++
 "(" ++ map unicodeToWinAnsi fullname ++ ")Tj " ++
 "/TT0 1 Tf " ++
 "0 -1.2 TD " ++
 "(" ++ map unicodeToWinAnsi company ++ ")Tj " ++
 "/TT2 1 Tf " ++
 "12 0 0 12 " ++ show (rightmargin - orgnroffset) ++ " 707.3906 Tm " ++
 "(Org.Nr. " ++ map unicodeToWinAnsi number ++ ")Tj " ++
 "0.863 0.43 0.152 0.004 k " ++ 
 "12 0 0 12 " ++ show (rightmargin - emailoffset) ++ " 692.9906 Tm " ++
 "(" ++ map unicodeToWinAnsi email ++ ")Tj " ++
 "ET " ++ 
 "0.039 0.024 0.02 0 k " ++
 "566.479 678.209 -537.601 3.841 re " ++
 "f " ++
 "1 0 0 1 0 -47 cm "


logentry (HistEntry {histdate,histcomment}) = 
 "BT " ++
 "/TT0 1 Tf " ++
 "0.591 0.507 0.502 0.19 k " ++
 "12 0 0 12 54.1978 520.8887 Tm " ++
 "(" ++ map unicodeToWinAnsi histdate ++ ")Tj " ++
 "12 0 0 12 231.1978 520.8887 Tm " ++
 "(" ++ map unicodeToWinAnsi histcomment ++ ")Tj " ++
 "ET 1 0 0 1 0 -20 cm "


lastpage (SealSpec {documentNumber,persons,history}) = 
 "0.081 0.058 0.068 0 k " ++
 "/GS0 gs " ++
 "581.839 14.37 -567.36 813.12 re " ++
 "f " ++
 "0 0 0 0 k " ++
 "578.853 824.603 -561.385 -807.345 re " ++
 "f " ++
 "0.039 0.024 0.02 0 k " ++
 "566.479 731.97 -537.601 20.16 re " ++
 "f " ++
 "566.479 759.33 -537.601 54.72 re " ++
 "f " ++


 "BT " ++
 "/TT0 1 Tf " ++

 "0.806 0.719 0.51 0.504 k " ++
 "23 0 0 23 39.8198 787.9463 Tm " ++
 "(Certifikat)Tj " ++

 "0.546 0.469 0.454 0.113 k " ++
 "17 0 0 17 39.8198 766.9555 Tm " ++
 "[(Dok.nr)55(. " ++ map unicodeToWinAnsi documentNumber ++ ")]TJ " ++

 "0.806 0.719 0.51 0.504 k " ++
 "17 0 0 17 39.8198 736.3555 Tm " ++
 "(Undertecknat av)Tj " ++
 "ET " ++

 "BT " ++
 "0.785 0.699 0.48 0.535 k " ++
 "/T1_0 1 Tf " ++
 "0.01 Tc -0.01 Tw 28.2709 0 0 28.2709 386.4658 779.1211 Tm " ++
 "[(S)20(kriv)55(aP)70(\\345)]TJ " ++
 "ET " ++




 "q " ++
 "0 841.89 595.28 -841.89 re " ++
 "W n " ++
 "q " ++
 "0 g " ++
 "0 Tc 0 Tw /Fm0 Do " ++
 "Q " ++
 "Q " ++


 {- nice shadows between lines 
 "0.039 0.024 0.02 0 k " ++
 "566.479 678.209 -537.601 3.841 re " ++
 "f " ++
 "566.479 631.17 -537.601 3.84 re " ++
 "f " ++
 -}


 -- every signatory on its own line, repeated every 64 pixels down
 "q " ++ concatMap signatorybox persons ++
 "1 0 0 1 0 141 cm " ++ -- compensate for 3 signatures
 

 -- Datum and Handelse
 "0.039 0.024 0.02 0 k " ++
 "566.479 540.93 -537.601 20.16 re " ++
 "f " ++
 "566.479 566.85 -537.601 20.16 re " ++
 "f " ++

 "BT " ++
 "0.784 0.698 0.475 0.533 k " ++
 "/TT0 1 Tf " ++
 "0 Tc 0 Tw " ++
 "16 0 0 16 39.8198 571.4502 Tm " ++
 "[(T)37(idst\\344mplar)]TJ " ++
 "13 0 0 13 50.4092 546.3926 Tm " ++
 "(Datum)Tj " ++
 "13 0 0 13 231.1978 546.3926 Tm " ++
 "(H\\344ndelse)Tj " ++

 "0.039 0.024 0.02 0 k " ++
 "571.856 24.7 -548.354 64.55 re " ++
 "f " ++

 -- logentry
 concatMap logentry history ++

 "Q " ++

 "BT " ++
 "0.625 0.537 0.53 0.257 k " ++
 "/TT0 1 Tf " ++
 "12 0 0 12 39.8198 69.2334 Tm " ++
 "1.2 TL " ++
 "[(Detta certifikat \\344r utf\\344rdat av SkrivaP\\345 CPM )55(AB och styrker viktig information om)]TJ " ++
 "T* " ++ 
 "(avtalet, avtalsparter och avtalsprocessen.  )Tj " ++
 "0.546 0.469 0.454 0.113 k " ++
 "13 0 0 13 46.5522 34.5469 Tm " ++
 "(Sida 1 av 1)Tj " ++
 "ET " ++ rightcornerseal

 {-
 "q " ++
 "0 841.89 595.28 -841.89 re " ++
 "W n " ++
 "0.546 0.469 0.454 0.113 k " ++
 "660 264.78 -682 72 re " ++
 "f " ++
 "EMC " ++ 
 "Q" 
 -}

process (sealSpec@SealSpec 
    { input
    , output
    , documentNumber
    , persons}) = do
    Just doc <- PdfModel.parseFile input
    Just seal <- PdfModel.parseFile sealFileName 
    let [paginpage1, sealpage1] = listPageRefIDs seal
        pagintext1 = pagintext sealSpec
        sealtext = lastpage sealSpec

    let ((),outputdoc) = 
            flip runState doc $ do
              [newpagincontents,newsealcontents] <- importObjects seal [paginpage1,sealpage1]
              placeSeals newsealcontents sealtext newpagincontents pagintext1
    writeFileX output outputdoc
    return ()




-- -------------------------------------------------------------
{-





0.081 0.058 0.068 0 k
/GS0 gs
581.839 14.37 -567.36 813.12 re
f
0 0 0 0 k
578.853 824.603 -561.385 -807.345 re
f
0.039 0.024 0.02 0 k
566.479 566.85 -537.601 20.16 re
f
566.479 731.97 -537.601 20.16 re
f
566.479 759.33 -537.601 54.72 re
f


BT
0.806 0.719 0.51 0.504 k
/TT0 1 Tf
23 0 0 23 39.8198 787.9463 Tm
(Certifikat)Tj
17 0 0 17 39.8198 736.3555 Tm
(Undertecknat av)Tj
0.546 0.469 0.454 0.113 k
0 1.812 TD
[(Dok.nr)55(. 1)74(1)74(1)74(1)74(1)74(1)74(1)74(1)74(1232 )]TJ
0.785 0.699 0.48 0.535 k


/T1_0 1 Tf
0.01 Tc -0.01 Tw 28.2709 0 0 28.2709 386.4658 779.1211 Tm
[(s)20(kriv)55(aP)70(\345)]TJ
ET




q
0 841.89 595.28 -841.89 re
W n
q
0 g
0 Tc 0 Tw /Fm0 Do
Q
Q



0.039 0.024 0.02 0 k
566.479 678.209 -537.601 3.841 re
f
566.479 631.17 -537.601 3.84 re
f
566.479 540.93 -537.601 20.16 re
f


BT
0.784 0.698 0.475 0.533 k
/TT0 1 Tf
0 Tc 0 Tw 13 0 0 13 50.4092 546.3926 Tm
(Datum)Tj
ET


BT
0.806 0.719 0.51 0.504 k

/TT1 1 Tf
12 0 0 12 46.5522 707.3906 Tm
(Lukas Duczko)Tj
/TT0 1 Tf
0 -1.2 TD
(CEO, SkrivaP\345 Ltd)Tj
/TT1 1 Tf
-0.034 -2.717 Td
(Lukas Duczko)Tj
/TT0 1 Tf
T*
(CEO, SkrivaP\345 Ltd)Tj
/TT2 1 Tf
32.645 5.117 Td
[(Org.Nr)37(. 898765-8658)]TJ
0.863 0.43 0.152 0.004 k
-1.939 -1.2 Td
(lukas.duczko@gmail.com)Tj
0.806 0.719 0.51 0.504 k
1.939 -2.717 Td
[(Org.Nr)37(. 898765-8658)]TJ
0.863 0.43 0.152 0.004 k
-1.939 -1.2 Td
(lukas.duczko@gmail.com)Tj
0.806 0.719 0.51 0.504 k

/TT1 1 Tf
12 0 0 12 46.5522 707.3906 Tm
(Lukas Duczko)Tj
/TT0 1 Tf
T*
(CEO, SkrivaP\345 Ltd)Tj
/TT2 1 Tf
32.645 1.2 Td
[(Org.Nr)37(. 898765-8658)]TJ
0.863 0.43 0.152 0.004 k
-1.939 -1.2 Td
(lukas.duczko@gmail.com)Tj
ET



0.039 0.024 0.02 0 k
571.856 24.7 -548.354 64.55 re
f


BT
/TT0 1 Tf
12 0 0 12 54.1978 520.8887 Tm
(2010-06-24 02:29)Tj
0 -4.8 TD
(2010-06-24 02:28)Tj
0 -2.4 TD
(2010-06-24 02:28)Tj
0 -2.4 TD
(2010-06-24 02:28)Tj
0 -2.4 TD
(2010-06-24 02:28)Tj
0.625 0.537 0.53 0.257 k
14.75 12 Td
[(Name of signatory)74(,)]TJ
0.863 0.43 0.152 0.004 k
[( what has happened, \(IP: Nu)-1(mber\))]TJ
0 -1.2 TD
[(and if its very much the text continues. )18(The spacing is )]TJ
0 -1.2 TD
(simply one line.)Tj
0.625 0.537 0.53 0.257 k
0 -2.4 TD
[(Name of signatory)74(,)]TJ
0.863 0.43 0.152 0.004 k
[( what has happened, \(IP: Nu)-1(mber\))]TJ
0.625 0.537 0.53 0.257 k
0 -2.4 TD
[(Name of signatory)74(,)]TJ
0.863 0.43 0.152 0.004 k
[( what has happened, \(IP: Nu)-1(mber\))]TJ
0.625 0.537 0.53 0.257 k
0 -2.4 TD
[(Name of signatory)74(,)]TJ
0.863 0.43 0.152 0.004 k
[( what has happened, \(IP: Nu)-1(mber\))]TJ
0.625 0.537 0.53 0.257 k
0 -2.4 TD
[(Name of signatory)74(,)]TJ
0.863 0.43 0.152 0.004 k
[( what has happened, \(IP: Nu)-1(mber\))]TJ
ET

BT
0.784 0.698 0.475 0.533 k
13 0 0 13 231.1978 546.3926 Tm
(H\344ndelse)Tj
ET


q
0 841.89 595.28 -841.89 re
W n
0.546 0.469 0.454 0.113 k
660 264.78 -682 72 re
f
EMC 
Q



/OC /MC0 BDC 
0.081 0.058 0.068 0 k
/GS0 gs
581.839 14.37 -567.36 813.12 re
f
0 0 0 0 k
578.853 824.603 -561.385 -807.345 re
f
0.039 0.024 0.02 0 k
566.479 566.85 -537.601 20.16 re
f
566.479 731.97 -537.601 20.16 re
f
566.479 759.33 -537.601 54.72 re
f
BT
0.806 0.719 0.51 0.504 k
/TT0 1 Tf
23 0 0 23 39.8198 787.9463 Tm
(Certifikat)Tj
17 0 0 17 39.8198 736.3555 Tm
(Undertecknat av)Tj
0.546 0.469 0.454 0.113 k
0 1.812 TD
[(Dok.nr)55(. 1)74(1)74(1)74(1)74(1)74(1)74(1)74(1)74(1232 )]TJ
0.785 0.699 0.48 0.535 k
/T1_0 1 Tf
0.01 Tc -0.01 Tw 28.2709 0 0 28.2709 386.4658 779.1221 Tm
[(s)20(kriv)55(aP)70(\345)]TJ
ET
q
0 841.89 595.28 -841.89 re
W n
q
0 g
0 Tc 0 Tw /Fm0 Do
Q
Q
0.039 0.024 0.02 0 k
566.479 678.209 -537.601 3.841 re
f
566.479 631.17 -537.601 3.84 re
f
566.479 540.93 -537.601 20.16 re
f
BT
0.784 0.698 0.475 0.533 k
/TT0 1 Tf
0 Tc 0 Tw 13 0 0 13 50.4092 546.3926 Tm
(Datum)Tj
0.806 0.719 0.51 0.504 k
/TT1 1 Tf
12 0 0 12 46.5522 707.3906 Tm
(Lukas Duczko)Tj
/TT0 1 Tf
0 -1.2 TD
(CEO, SkrivaP\345 Ltd)Tj
/TT1 1 Tf
-0.034 -2.717 Td
(Lukas Duczko)Tj
/TT0 1 Tf
T*
(CEO, SkrivaP\345 Ltd)Tj
/TT2 1 Tf
32.645 5.117 Td
[(Org.Nr)37(. 898765-8658)]TJ
-1.939 -1.2 Td
(lukas.duczko@gmail.com)Tj
1.939 -2.717 Td
[(Org.Nr)37(. 898765-8658)]TJ
-1.939 -1.2 Td
(lukas.duczko@gmail.com)Tj
/TT1 1 Tf
-30.705 -2.797 Td
(Lukas Duczko)Tj
/TT0 1 Tf
T*
(CEO, SkrivaP\345 Ltd)Tj
/TT2 1 Tf
32.645 1.2 Td
[(Org.Nr)37(. 898765-8658)]TJ
-1.939 -1.2 Td
(lukas.duczko@gmail.com)Tj
ET
0.039 0.024 0.02 0 k
571.856 24.7 -548.354 64.55 re
f
BT
0.625 0.537 0.53 0.257 k
/TT0 1 Tf
12 0 0 12 39.8198 69.2334 Tm
1.2 TL 
[(Detta certifikat \344r utf\344rdat av SkrivaP\345 CPM )55(AB och styrker viktig information om)]TJ
T*
(avtalet, avtalsparter och avtalsprocessen.  )Tj
0.806 0.719 0.51 0.504 k
16 0 0 16 39.8198 571.4502 Tm
[(T)37(idst\344mplar)]TJ
0.546 0.469 0.454 0.113 k
13 0 0 13 46.5522 34.5469 Tm
(Sida 1 av 1)Tj
ET

-}

rightcornerseal = "q " ++
 "0 841.89 595.28 -841.89 re " ++
 "W n " ++
 "0 0 0 0 k " ++
 "q 1 0 0 1 490.9414 57.4893 cm " ++
 "0 0 m " ++
 "0 -23.452 19.227 -42.463 42.943 -42.463 c " ++
 "66.659 -42.463 85.886 -23.452 85.886 0 c " ++
 "85.886 23.451 66.659 42.462 42.943 42.462 c " ++
 "19.227 42.462 0 23.451 0 0 c " ++
 "f " ++
 "Q " ++
 "0.863 0.43 0.152 0.004 k " ++
 "q 1 0 0 1 533.8447 15.377 cm " ++
 "0 0 m " ++
 "-23.278 0 -42.148 18.868 -42.148 42.146 c " ++
 "-42.148 65.425 -23.278 84.29 0 84.29 c " ++
 "23.271 84.29 42.143 65.425 42.143 42.146 c " ++
 "42.143 18.868 23.271 0 0 0 c " ++
 "42.561 42.146 m " ++
 "45.04 43.386 l " ++
 "42.5 44.485 l " ++
 "44.901 45.863 l " ++
 "42.307 46.824 l " ++
 "44.631 48.327 l " ++
 "41.985 49.146 l " ++
 "44.223 50.774 l " ++
 "41.534 51.444 l " ++
 "43.683 53.201 l " ++
 "40.956 53.719 l " ++
 "43.307 55.854 l " ++
 "40.26 55.954 l " ++
 "42.199 57.931 l " ++
 "39.436 58.148 l " ++
 "41.268 60.23 l " ++
 "38.494 60.293 l " ++
 "40.208 62.474 l " ++
 "37.435 62.386 l " ++
 "39.029 64.653 l " ++
 "36.265 64.411 l " ++
 "37.729 66.768 l " ++
 "34.982 66.376 l " ++
 "36.313 68.806 l " ++
 "33.598 68.266 l " ++
 "34.791 70.764 l " ++
 "32.108 70.073 l " ++
 "33.163 72.635 l " ++
 "30.52 71.799 l " ++
 "31.434 74.415 l " ++
 "28.84 73.429 l " ++
 "29.604 76.094 l " ++
 "27.071 74.97 l " ++
 "27.691 77.67 l " ++
 "25.224 76.408 l " ++
 "25.694 79.139 l " ++
 "23.297 77.741 l " ++
 "23.613 80.495 l " ++
 "21.303 78.972 l " ++
 "21.467 81.737 l " ++
 "19.24 80.087 l " ++
 "19.256 82.855 l " ++
 "17.119 81.083 l " ++
 "16.98 83.854 l " ++
 "14.949 81.965 l " ++
 "14.661 84.723 l " ++
 "12.733 82.727 l " ++
 "12.29 85.464 l " ++
 "10.48 83.367 l " ++
 "9.886 86.072 l " ++
 "8.192 83.876 l " ++
 "7.45 86.548 l " ++
 "5.878 84.264 l " ++
 "4.991 86.886 l " ++
 "3.552 84.521 l " ++
 "2.521 87.095 l " ++
 "1.211 84.649 l " ++
 "0.041 87.162 l " ++
 "-1.131 84.649 l " ++
 "-2.44 87.095 l " ++
 "-3.472 84.521 l " ++
 "-4.914 86.886 l " ++
 "-5.801 84.264 l " ++
 "-7.37 86.548 l " ++
 "-8.112 83.876 l " ++
 "-9.805 86.072 l " ++
 "-10.398 83.367 l " ++
 "-12.208 85.464 l " ++
 "-12.65 82.727 l " ++
 "-14.578 84.723 l " ++
 "-14.869 81.965 l " ++
 "-16.897 83.854 l " ++
 "-17.042 81.083 l " ++
 "-19.173 82.855 l " ++
 "-19.16 80.087 l " ++
 "-21.384 81.737 l " ++
 "-21.223 78.972 l " ++
 "-23.533 80.495 l " ++
 "-23.217 77.741 l " ++
 "-25.61 79.139 l " ++
 "-25.144 76.408 l " ++
 "-27.611 77.67 l " ++
 "-26.992 74.97 l " ++
 "-29.523 76.094 l " ++
 "-28.757 73.429 l " ++
 "-31.352 74.415 l " ++
 "-30.439 71.799 l " ++
 "-33.08 72.635 l " ++
 "-32.023 70.073 l " ++
 "-34.708 70.764 l " ++
 "-33.515 68.266 l " ++
 "-36.232 68.806 l " ++
 "-34.899 66.376 l " ++
 "-37.648 66.768 l " ++
 "-36.183 64.414 l " ++
 "-38.946 64.653 l " ++
 "-37.357 62.386 l " ++
 "-40.125 62.474 l " ++
 "-38.415 60.293 l " ++
 "-41.186 60.23 l " ++
 "-39.353 58.148 l " ++
 "-42.119 57.931 l " ++
 "-40.177 55.954 l " ++
 "-43.036 55.719 l " ++
 "-40.876 53.719 l " ++
 "-43.601 53.201 l " ++
 "-41.451 51.444 l " ++
 "-44.14 50.774 l " ++
 "-41.899 49.146 l " ++
 "-44.551 48.327 l " ++
 "-42.222 46.824 l " ++
 "-44.821 45.863 l " ++
 "-42.417 44.485 l " ++
 "-44.96 43.386 l " ++
 "-42.48 42.146 l " ++
 "-44.96 40.907 l " ++
 "-42.417 39.804 l " ++
 "-44.821 38.43 l " ++
 "-42.222 37.467 l " ++
 "-44.551 35.96 l " ++
 "-41.899 35.147 l " ++
 "-44.14 33.519 l " ++
 "-41.451 32.847 l " ++
 "-43.601 31.092 l " ++
 "-40.876 30.578 l " ++
 "-42.924 28.708 l " ++
 "-40.177 28.339 l " ++
 "-42.181 26.236 l " ++
 "-39.353 26.145 l " ++
 "-41.186 24.063 l " ++
 "-38.415 24.001 l " ++
 "-40.125 21.819 l " ++
 "-37.357 21.906 l " ++
 "-38.946 19.64 l " ++
 "-36.183 19.878 l " ++
 "-37.648 17.525 l " ++
 "-34.899 17.916 l " ++
 "-36.232 15.484 l " ++
 "-33.515 16.029 l " ++
 "-34.708 13.529 l " ++
 "-32.023 14.22 l " ++
 "-33.08 11.655 l " ++
 "-30.439 12.496 l " ++
 "-31.352 9.876 l " ++
 "-28.757 10.861 l " ++
 "-29.523 8.198 l " ++
 "-26.992 9.323 l " ++
 "-27.611 6.622 l " ++
 "-25.144 7.884 l " ++
 "-25.61 5.151 l " ++
 "-23.217 6.548 l " ++
 "-23.533 3.798 l " ++
 "-21.223 5.321 l " ++
 "-21.384 2.555 l " ++
 "-19.16 4.206 l " ++
 "-19.173 1.438 l " ++
 "-17.042 3.21 l " ++
 "-16.897 0.438 l " ++
 "-14.869 2.325 l " ++
 "-14.578 -0.431 l " ++
 "-12.65 1.566 l " ++
 "-12.208 -1.171 l " ++
 "-10.398 0.926 l " ++
 "-9.805 -1.779 l " ++
 "-8.112 0.417 l " ++
 "-7.37 -2.258 l " ++
 "-5.801 0.029 l " ++
 "-4.914 -2.594 l " ++
 "-3.472 -0.229 l " ++
 "-2.44 -2.802 l " ++
 "-1.131 -0.356 l " ++
 "0.041 -2.869 l " ++
 "1.211 -0.356 l " ++
 "2.521 -2.802 l " ++
 "3.552 -0.229 l " ++
 "4.991 -2.594 l " ++
 "5.878 0.029 l " ++
 "7.45 -2.258 l " ++
 "8.192 0.417 l " ++
 "9.886 -1.779 l " ++
 "10.48 0.926 l " ++
 "12.29 -1.171 l " ++
 "12.733 1.566 l " ++
 "14.661 -0.431 l " ++
 "14.949 2.325 l " ++
 "16.98 0.438 l " ++
 "17.119 3.21 l " ++
 "19.256 1.438 l " ++
 "19.24 4.206 l " ++
 "21.467 2.555 l " ++
 "21.303 5.321 l " ++
 "23.613 3.798 l " ++
 "23.297 6.548 l " ++
 "25.694 5.151 l " ++
 "25.224 7.884 l " ++
 "27.691 6.622 l " ++
 "27.071 9.323 l " ++
 "29.604 8.198 l " ++
 "28.84 10.861 l " ++
 "31.434 9.876 l " ++
 "30.52 12.496 l " ++
 "33.163 11.655 l " ++
 "32.108 14.22 l " ++
 "34.791 13.529 l " ++
 "33.598 16.029 l " ++
 "36.313 15.484 l " ++
 "34.982 17.916 l " ++
 "37.729 17.525 l " ++
 "36.265 19.878 l " ++
 "39.029 19.64 l " ++
 "37.435 21.906 l " ++
 "40.208 21.819 l " ++
 "38.494 24.001 l " ++
 "41.268 24.063 l " ++
 "39.436 26.145 l " ++
 "42.604 25.997 l " ++
 "40.26 28.339 l " ++
 "43.007 28.708 l " ++
 "40.956 30.578 l " ++
 "43.683 31.092 l " ++
 "41.534 32.847 l " ++
 "44.223 33.519 l " ++
 "41.985 35.147 l " ++
 "44.631 35.96 l " ++
 "42.307 37.467 l " ++
 "44.901 38.43 l " ++
 "42.5 39.804 l " ++
 "45.04 40.907 l " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 533.8857 97.4873 cm " ++
 "0 0 m " ++
 "-22.039 0 -39.966 -17.926 -39.966 -39.964 c " ++
 "-39.966 -62.002 -22.039 -79.929 0 -79.929 c " ++
 "22.04 -79.929 39.964 -62.002 39.964 -39.964 c " ++
 "39.964 -17.926 22.04 0 0 0 c " ++
 "0 -80.589 m " ++
 "-22.403 -80.589 -40.626 -62.366 -40.626 -39.964 c " ++
 "-40.626 -17.562 -22.403 0.663 0 0.663 c " ++
 "22.401 0.663 40.629 -17.562 40.629 -39.964 c " ++
 "40.629 -62.366 22.401 -80.589 0 -80.589 c " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 533.8857 95.1152 cm " ++
 "0 0 m " ++
 "-20.732 0 -37.589 -16.863 -37.589 -37.592 c " ++
 "-37.589 -58.32 -20.732 -75.184 0 -75.184 c " ++
 "20.728 -75.184 37.594 -58.32 37.594 -37.592 c " ++
 "37.594 -16.863 20.728 0 0 0 c " ++
 "0 -76.658 m " ++
 "-21.543 -76.658 -39.064 -59.134 -39.064 -37.592 c " ++
 "-39.064 -16.05 -21.543 1.475 0 1.475 c " ++
 "21.541 1.475 39.063 -16.05 39.063 -37.592 c " ++
 "39.063 -59.134 21.541 -76.658 0 -76.658 c " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 549.2959 40.249 cm " ++
 "0 0 m " ++
 "-0.734 0.849 l " ++
 "-4.148 -2.191 -8.53 -4.201 -13.424 -4.632 c " ++
 "-14.076 -4.692 -14.73 -4.719 -15.377 -4.719 c " ++
 "-26.674 -4.719 -36.295 4.105 -37.287 15.375 c " ++
 "-37.355 16.141 -37.382 16.901 -37.369 17.655 c " ++
 "-37.898 17.661 -38.407 17.664 -38.891 17.653 c " ++
 "-38.906 16.856 -38.874 16.053 -38.8 15.24 c " ++
 "-38.287 9.34 -35.59 3.891 -31.213 -0.093 c " ++
 "-26.866 -4.056 -21.24 -6.239 -15.377 -6.239 c " ++
 "-14.69 -6.239 -13.985 -6.208 -13.292 -6.148 c " ++
 "-8.072 -5.69 -3.399 -3.55 0.243 -0.313 c " ++
 "0.238 -0.303 0.232 -0.296 0.223 -0.286 c " ++
 "0.161 -0.204 0.094 -0.113 0 0 c " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 512.373 61.9805 cm " ++
 "0 0 m " ++
 "1.915 9.281 9.726 16.627 19.64 17.502 c " ++
 "20.287 17.557 20.946 17.587 21.596 17.587 c " ++
 "32.89 17.587 42.517 8.762 43.505 -2.501 c " ++
 "43.914 -7.144 42.838 -11.58 40.679 -15.344 c " ++
 "41.025 -15.751 41.374 -16.165 41.711 -16.587 c " ++
 "44.21 -12.458 45.477 -7.532 45.021 -2.367 c " ++
 "44.502 3.531 41.808 8.977 37.432 12.964 c " ++
 "33.081 16.929 27.456 19.107 21.596 19.107 c " ++
 "20.899 19.107 20.2 19.079 19.503 19.017 c " ++
 "8.698 18.067 0.223 9.907 -1.603 -0.295 c " ++
 "-1.416 -0.261 -1.232 -0.221 -1.058 -0.186 c " ++
 "-0.737 -0.119 -0.373 -0.058 0 0 c " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 548.7559 52.6768 cm " ++
 "0 0 m " ++
 "-0.257 -0.001 -0.456 -0.206 -0.456 -0.458 c " ++
 "-0.453 -0.712 -0.244 -0.914 0.003 -0.911 c " ++
 "0.918 -0.903 l " ++
 "1.168 -0.903 1.374 -0.697 1.372 -0.446 c " ++
 "1.372 -0.194 1.162 0.009 0.91 0.009 c " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 518.0303 51.54 cm " ++
 "0 0 m " ++
 "0.25 0.001 0.452 0.207 0.446 0.462 c " ++
 "0.446 0.711 0.24 0.915 -0.01 0.911 c " ++
 "-0.923 0.904 l " ++
 "-1.175 0.903 -1.378 0.697 -1.378 0.446 c " ++
 "-1.378 0.192 -1.166 -0.01 -0.917 -0.009 c " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 554.0801 52.2598 cm " ++
 "0 0 m " ++
 "0 0.251 -0.204 0.455 -0.457 0.454 c " ++
 "-1.368 0.447 l " ++
 "-1.623 0.445 -1.823 0.238 -1.823 -0.013 c " ++
 "-1.82 -0.264 -1.618 -0.469 -1.361 -0.466 c " ++
 "-0.451 -0.46 l " ++
 "-0.201 -0.458 0.004 -0.251 0 0 c " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 514.0742 51.5107 cm " ++
 "0 0 m " ++
 "0.249 0.002 0.455 0.209 0.447 0.46 c " ++
 "0.445 0.711 0.243 0.911 -0.009 0.911 c " ++
 "-0.926 0.904 l " ++
 "-1.176 0.902 -1.375 0.698 -1.375 0.446 c " ++
 "-1.372 0.193 -1.17 -0.011 -0.915 -0.006 c " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 529.8945 51.6279 cm " ++
 "0 0 m " ++
 "0.252 0.001 0.452 0.208 0.452 0.459 c " ++
 "0.452 0.711 0.246 0.911 -0.006 0.911 c " ++
 "-0.919 0.904 l " ++
 "-1.173 0.904 -1.375 0.695 -1.372 0.443 c " ++
 "-1.372 0.192 -1.166 -0.011 -0.913 -0.006 c " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 525.9375 51.5996 cm " ++
 "0 0 m " ++
 "0.25 0 0.452 0.208 0.452 0.458 c " ++
 "0.449 0.71 0.247 0.911 -0.002 0.91 c " ++
 "-0.917 0.904 l " ++
 "-1.172 0.904 -1.372 0.694 -1.372 0.443 c " ++
 "-1.372 0.191 -1.166 -0.011 -0.911 -0.006 c " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 521.9844 51.5693 cm " ++
 "0 0 m " ++
 "0.252 0.002 0.455 0.209 0.452 0.458 c " ++
 "0.449 0.711 0.243 0.913 -0.009 0.91 c " ++
 "-0.923 0.904 l " ++
 "-1.176 0.903 -1.378 0.696 -1.372 0.444 c " ++
 "-1.372 0.193 -1.167 -0.01 -0.913 -0.008 c " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 537.8076 51.6855 cm " ++
 "0 0 m " ++
 "0.253 0 0.452 0.205 0.452 0.458 c " ++
 "0.449 0.712 0.243 0.912 -0.009 0.91 c " ++
 "-0.922 0.904 l " ++
 "-1.171 0.904 -1.375 0.697 -1.375 0.445 c " ++
 "-1.372 0.192 -1.166 -0.011 -0.913 -0.01 c " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 554.833 41.3301 cm " ++
 "0 0 m " ++
 "-0.937 2.269 -2.632 3.863 -4.198 5.819 c " ++
 "-5.537 7.415 -7.083 9.027 -8.804 10.543 c " ++
 "-8.715 10.627 -8.663 10.741 -8.666 10.87 c " ++
 "-8.666 11.125 -8.87 11.327 -9.124 11.325 c " ++
 "-9.712 11.322 l " ++
 "-9.828 11.416 -9.943 11.514 -10.062 11.609 c " ++
 "-11.688 12.96 -13.495 14.138 -15.366 15.14 c " ++
 "-13.785 16.774 -12.527 18.575 -11.25 20.188 c " ++
 "-10.332 21.467 -9.65 22.557 -8.958 23.843 c " ++
 "-8.675 24.593 -8.233 24.894 -8.219 26.423 c " ++
 "-8.302 26.741 -8.296 26.966 -8.606 27.293 c " ++
 "-8.91 27.622 -9.423 27.815 -9.567 27.788 c " ++
 "-10.122 27.838 -10.413 27.723 -10.596 27.651 c " ++
 "-11.001 27.54 -11.39 27.258 -11.762 27.045 c " ++
 "-14.135 25.007 -14.852 22.823 -15.813 21.233 c " ++
 "-16.696 19.648 -17.505 18.299 -17.995 17.431 c " ++
 "-18.159 17.156 -18.312 16.885 -18.471 16.615 c " ++
 "-20.757 17.573 -23.067 18.269 -25.235 18.684 c " ++
 "-30.22 19.738 -34.459 19.918 -36.533 19.807 c " ++
 "-38.69 19.604 -41.477 19.322 -43.197 18.978 c " ++
 "-45.018 18.589 -47.592 18.261 -48.951 17.455 c " ++
 "-45.968 18.131 -42.249 18.045 -38.328 18.131 c " ++
 "-35.643 18.19 -28.12 16.689 -21.328 14.116 c " ++
 "-21.132 13.996 -20.389 13.623 -20.195 13.46 c " ++
 "-20.591 12.688 -20.949 11.95 -21.272 11.234 c " ++
 "-21.902 11.229 l " ++
 "-22.154 11.229 -22.356 11.023 -22.354 10.771 c " ++
 "-22.351 10.519 -22.148 10.314 -21.899 10.316 c " ++
 "-21.682 10.322 l " ++
 "-23.032 7.173 -23.791 4.348 -24.609 0.656 c " ++
 "-22.465 4.85 -20.785 7.85 -18.497 11.745 c " ++
 "-18.44 11.72 -18.13 12.223 -18.073 12.2 c " ++
 "-16.949 11.684 -15.606 10.578 -14.593 9.897 c " ++
 "-12.53 8.505 -10.971 7.185 -9.616 5.781 c " ++
 "-6.877 2.935 -5.489 1.193 -5.521 1.232 c " ++
 "-5.511 1.227 -5.231 0.895 -4.384 -0.094 c " ++
 "-4.008 -0.527 -3.981 -0.676 -3.343 -1.265 c " ++
 "-3.005 -1.566 -2.899 -1.878 -2.453 -2.165 c " ++
 "-2.039 -2.273 l " ++
 "-1.709 -2.245 l " ++
 "-1.155 -2.264 -0.185 -2.03 0.114 -1.722 c " ++
 "0.416 -1.41 0.255 -0.919 y " ++
 "0.224 -0.559 0.098 -0.303 0 0 c " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 562.0801 41.0176 cm " ++
 "0 0 m " ++
 "-1.059 -2.278 l " ++
 "0.781 -3.988 l " ++
 "-1.713 -3.684 l " ++
 "-2.772 -5.963 l " ++
 "-3.249 -3.496 l " ++
 "-5.748 -3.196 l " ++
 "-3.554 -1.974 l " ++
 "-4.032 0.489 l " ++
 "-2.193 -1.219 l " ++
 "h " ++
 "f " ++
 "Q " ++
 "q 1 0 0 1 510.0029 41.6709 cm " ++
 "0 0 m " ++
 "-0.258 -2.5 l " ++
 "2.04 -3.513 l " ++
 "-0.413 -4.048 l " ++
 "-0.668 -6.543 l " ++
 "-1.933 -4.371 l " ++
 "-4.388 -4.901 l " ++
 "-2.714 -3.027 l " ++
 "-3.976 -0.858 l " ++
 "-1.676 -1.872 l " ++
 "h " ++
 "f " ++
 "Q " ++
 "Q " ++
 "BT " ++
 "0.863 0.43 0.152 0.004 k " ++
 "/GS1 gs " ++
 "/T1_0 1 Tf " ++
 "-2.3713 9.3285 -9.3285 -2.3713 508.2863 47.3001 Tm " ++
 "(u)Tj " ++
 "0.2736 9.6213 -9.6213 0.2736 506.4337 53.8986 Tm " ++
 "(n)Tj " ++
 "2.801 9.2086 -9.2086 2.801 506.5599 61.7092 Tm " ++
 "(d)Tj " ++
 "4.8763 8.2985 -8.2985 4.8763 508.8849 68.9897 Tm " ++
 "(e)Tj " ++
 "6.4776 7.1194 -7.1194 6.4776 511.419 73.4892 Tm " ++
 "(r)Tj " ++
 "7.8131 5.6213 -5.6213 7.8131 515.4361 77.9578 Tm " ++
 "(t)Tj " ++
 "8.7926 3.9158 -3.9158 8.7926 520.2061 81.3838 Tm " ++
 "(e)Tj " ++
 "9.4325 1.9161 -1.9161 9.4325 525.0447 83.6379 Tm " ++
 "(c)Tj " ++
 "9.62 -0.3163 0.3163 9.62 531.4432 84.9539 Tm " ++
 "(k)Tj " ++
 "9.222 -2.7567 2.7567 9.222 537.7826 84.878 Tm " ++
 "(n)Tj " ++
 "8.2553 -4.9492 4.9492 8.2553 545.2322 82.5567 Tm " ++
 "(a)Tj " ++
 "7.1596 -6.433 6.433 7.1596 549.7919 79.9156 Tm " ++
 "(t)Tj " ++
 "6.0576 -7.4799 7.4799 6.0576 554.1339 75.8589 Tm " ++
 "( )Tj " ++
 "4.8037 -8.3407 8.3407 4.8037 555.9721 73.8069 Tm " ++
 "(a)Tj " ++
 "3.0867 -9.1168 9.1168 3.0867 558.6974 69.233 Tm " ++
 "(v)Tj " ++
 "1.0679 -9.5658 9.5658 1.0679 560.6075 63.6613 Tm " ++
 "(t)Tj " ++
 "-0.7798 -9.5935 9.5935 -0.7798 561.2701 58.3725 Tm " ++
 "(a)Tj " ++
 "-2.6864 -9.2427 9.2427 -2.6864 560.7751 52.5012 Tm " ++
 "(l)Tj " ++
 "/GS0 gs " ++
 "6.6805 -7.9597 7.1581 6.0077 506.3887 37.4092 Tm " ++
 "( )Tj " ++
 "7.5268 -7.1647 6.4431 6.7688 508.6992 34.6982 Tm " ++
 "( )Tj " ++
 "8.2531 -6.3144 5.6785 7.4219 511.2881 32.2666 Tm " ++
 "( )Tj " ++
 "6.8037 -3.936 3.5396 6.1185 514.1973 30.0303 Tm " ++
 "(S)Tj " ++
 "7.3266 -2.8466 2.5599 6.5887 518.5654 27.5107 Tm " ++
 "(k)Tj " ++
 "7.696 -1.5981 1.4372 6.9209 524.2305 25.373 Tm " ++
 "(r)Tj " ++
 "7.8378 -0.5921 0.5325 7.0485 530.0693 24.2705 Tm " ++
 "(i)Tj " ++
 "7.8525 0.3467 -0.3118 7.0617 533.7451 23.9678 Tm " ++
 "(v)Tj " ++
 "7.7344 1.4007 -1.2596 6.9554 538.8574 24.2012 Tm " ++
 "(a)Tj " ++
 "7.423 2.5847 -2.3244 6.6754 544.3965 25.248 Tm " ++
 "(P)Tj " ++
 "6.982 3.6102 -3.2466 6.2789 549.2734 26.9512 Tm " ++
 "(\\345)Tj " ++
 "ET"
 

{-
0.591 0.507 0.502 0.19 k
/TT0 1 Tf
12 0 0 12 54.1978 520.8887 Tm
(2010-06-24 02:29 -0700)Tj
0 -4.8 TD
(2010-06-24 02:28 -0700)Tj
0 -2.4 TD
(2010-06-24 02:28 -0700)Tj
0 -2.4 TD
(2010-06-24 02:28 -0700)Tj
0 -2.4 TD
(2010-06-24 02:28 -0700)Tj
0.806 0.719 0.51 0.504 k
14.75 12 Td
[(Name of signatory)74(, what has happened, \(IP: Nu)-1(mber\))]TJ
0 -1.2 TD
[(and if its very much the text continues. )18(The spacing is )]TJ
0 -1.2 TD
(simply one line.)Tj
0 -2.4 TD
[(Name of signatory)74(, what has happened, \(IP: Nu)-1(mber\))]TJ
0 -2.4 TD
[(Name of signatory)74(, what has happened, \(IP: Nu)-1(mber\))]TJ
0 -2.4 TD
[(Name of signatory)74(, what has happened, \(IP: Nu)-1(mber\))]TJ
0 -2.4 TD
[(Name of signatory)74(, what has happened, \(IP: Nu)-1(mber\))]TJ
0.784 0.698 0.475 0.533 k
13 0 0 13 231.1978 546.3926 Tm
(H\344ndelse)Tj
ET
EMC 

-}

