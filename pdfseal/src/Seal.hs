{-# LANGUAGE DoRec #-}

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
import Data.List
import Data.Char
import Graphics.PDF.Text
import Debug.Trace
import Graphics.PDF
import SealSpec

winAnsiPostScriptEncode text = concatMap charEncode text
    where
      charEncode '(' = "\\("
      charEncode ')' = "\\)"
      charEncode x = [unicodeToWinAnsi x]

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

placeSealOnPageRefID :: RefID -> RefID -> (RefID,String) -> Document -> Document
placeSealOnPageRefID sealrefid sealmarkerformrefid (pagerefid,sealtext) document = 
    let
        Just (Indir (Dict pagedict) pagestrem) = 
            PdfModel.lookup pagerefid document
        Just contentvalue = {- trace (show pagedict) $ -} Prelude.lookup (BS.pack "Contents") pagedict
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
        newresdictcont1 = mergeResources document sealresdict pageresdict
        newxobjectdict = case Prelude.lookup (BS.pack "XObject") newresdictcont1 of
                           Just (Dict w) -> Dict (w ++ [(BS.pack "SealMarkerForm",Ref sealmarkerformrefid)]) 
                           Just (Ref r) -> case PdfModel.lookup r document of
                                             Just (Indir (Dict w) _) -> 
                                                   Dict (w ++ [(BS.pack "SealMarkerForm",Ref sealmarkerformrefid)])
                                             x -> error (show x)

                           Nothing -> Dict [(BS.pack "SealMarkerForm",Ref sealmarkerformrefid)]
                           x -> error (show x)
        newresdict = Dict ((BS.pack "XObject",newxobjectdict) : skipXObject newresdictcont1)
        skipXObject = filter (not . (== BS.pack "XObject") . fst)
                   

        newcontentarray = Array ([Ref q] ++ contentlist ++ [Ref qQ] ++ map Ref sealpagecontents ++ [Ref rr])
        skipkey x = BS.pack "Contents" == x || BS.pack "Resources" == x
        pagedict2 = filter (not . skipkey . fst) pagedict
        newpagedict = Dict pagedict2 `ext` [(BS.pack "Contents", newcontentarray)
                                           ,(BS.pack "Resources", newresdict)]
        newpage = (Indir newpagedict pagestrem)
        newdocument = setIndirF pagerefid newpage docx
    in newdocument

fieldstext fields = concatMap fieldtext fields
  where
    fieldtext Field{ SealSpec.value = val
                   , x
                   , y
                   , w
                   , h
                   } = "q 1 0 0 1 " ++ show (topagex x w) ++ " " ++ show (topagey y h) ++ " cm " ++
                       "BT /SkrivaPaHelvetica 10 Tf (" ++ winAnsiPostScriptEncode val ++ ") Tj ET Q "
    pagew = 595
    pageh = 842
    topagex x w = (fromIntegral x / fromIntegral w * fromIntegral pagew)
    topagey y h = (fromIntegral (h - y) / fromIntegral h * fromIntegral pageh) - 10 {- 10 is baseline -}

placeSeals :: [Field] -> RefID -> String -> RefID -> String -> RefID -> State Document ()
placeSeals fields sealrefid sealtext paginrefid pagintext sealmarkerformrefid = do
    pages <- gets listPageRefIDs
    let pagevalue = page_dict (Array []) (Array []) `ext` [entryna "MediaBox" [0,0,595,842]]
    -- should optimize pagintext into one stream
    let findFields pageno = filter (\x -> page x == pageno) fields
    let pagintext1 pageno = fieldstext (findFields pageno)++ 
                     pagintext ++ 
                     " q 0.2 0 0 0.2 " ++ show ((595 - 18) / 2) ++ " 14 cm /SealMarkerForm Do Q "

    modify $ \document -> foldr (placeSealOnPageRefID paginrefid sealmarkerformrefid) document 
                          [(page,pagintext1 pageno) | (page,pageno) <- zip pages [1..]]
    lastpage <- addPageToDocument pagevalue
    modify $ \document -> foldr (placeSealOnPageRefID sealrefid sealmarkerformrefid) document [(lastpage,sealtext)]


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
 let
    font = PDFFont Helvetica 8
    docnrwidth = textWidth font (toPDFString docnrtext)
    docnroffset = center - 20 - docnrwidth
    center = 595/2
    signedinitials = "Undertecknat: " ++ winAnsiPostScriptEncode initials 
    siwidth = textWidth font (toPDFString signedinitials)
    sioffset = center + 20
    docnrtext = "Dok.nr. " ++ documentNumber
    
 in
 "q 1 0 0 1 0 5 cm " ++
 "BT " ++
 "0.546 0.469 0.454 0.113 k " ++
 "/SkrivaPaHelvetica 1 Tf " ++
 "8 0 0 8 " ++ show sioffset ++ " 15 Tm " ++
 "[(" ++ signedinitials ++ ")]TJ " ++
 "8 0 0 8 " ++ show docnroffset ++ " 15 Tm " ++
 "(" ++ docnrtext ++ ")Tj " ++
 "ET " ++ 
 "0.863 0.43 0.152 0.004 K " ++
 "0.4 w " ++
 "60 18 m " ++ show (docnroffset-10) ++ " 18 l S " ++
 show (sioffset+siwidth+10) ++ " 18 m " ++ show (595 - 60) ++ " 18 l S " ++
 "Q "

signatorybox (Person {fullname,company,number,email}) = 
 let
    orgnrtext = if number=="" then "" else "Org.nr. " ++ number
    orgnroffset = textWidth (PDFFont Helvetica 10) (toPDFString orgnrtext)
    emailoffset = textWidth (PDFFont Helvetica_Oblique 10) (toPDFString email)
    rightmargin = 595 - 46.5522
 in
 "BT " ++
 "0.806 0.719 0.51 0.504 k " ++
 "/TT1 1 Tf " ++
 "10 0 0 10 46.5522 707.3906 Tm " ++
 "(" ++ winAnsiPostScriptEncode fullname ++ ")Tj " ++
 "/TT0 1 Tf " ++
 "10 0 0 10 46.5522 695.9906 Tm " ++
 "(" ++ winAnsiPostScriptEncode company ++ ")Tj " ++
 "10 0 0 10 " ++ show (rightmargin - orgnroffset) ++ " 707.3906 Tm " ++
 "(" ++ winAnsiPostScriptEncode orgnrtext ++ ")Tj " ++
 "/TT2 1 Tf " ++
 "10 0 0 10 " ++ show (rightmargin - emailoffset) ++ " 695.9906 Tm " ++
 "(" ++ winAnsiPostScriptEncode email ++ ")Tj " ++
 "ET " ++ 
 -- "0.039 0.024 0.02 0 k " ++
 -- "566.479 678.209 -537.601 3.841 re " ++
 -- "f " ++
 "1 0 0 1 0 -42 cm "

makeManyLines font width text = result
    where
    textSplit :: String -> [String]
    textSplit [] = []
    -- textSplit [x] = [[x]]
    textSplit (text) = let (b,a1) = break (==' ') text 
                           (s,a) = break (/=' ') a1
                       in (b ++ s) : textSplit a  
    textWithLength :: String -> (PDFFloat,String)
    textWithLength text = (textWidth font (toPDFString text),text)
    textSplitWithLength = map textWithLength (textSplit text)
    takeWhileLength :: PDFFloat -> String -> [(PDFFloat,String)] -> [String]
    takeWhileLength len text [] = [text]
    takeWhileLength len text all@((l,t):rest)
                    | len + l < width = takeWhileLength (len + l) (text ++ t) rest
                    | otherwise = text : takeWhileLength 0 "" all
    textOutLine text = "[(" ++ winAnsiPostScriptEncode text ++ ")]TJ T* "
    textLines = takeWhileLength 0 "" textSplitWithLength
    result = map textOutLine textLines 

logentry (HistEntry {histdate,histcomment}) = 
 let outlines = (makeManyLines (PDFFont Helvetica_Oblique 10) 300 histcomment) in
 "BT " ++
 "/TT2 1 Tf " ++
 "0.591 0.507 0.502 0.19 k " ++
 "10 0 0 10 46 520.8887 Tm " ++
 "(" ++ winAnsiPostScriptEncode histdate ++ ")Tj " ++
 "10 0 0 10 231 520.8887 Tm " ++
 "1.2 TL " ++
 concat outlines ++
 "ET 1 0 0 1 0 " ++ show ((-8) - length outlines * 12) ++ " cm "


lastpage (SealSpec {documentNumber,persons,history,hostpart}) = 
 "0.081 0.058 0.068 0 k " ++
 "/GS0 gs " ++
 "0.4 G " ++
 "581.839 14.37 -567.36 813.12 re " ++
 "S " ++
 -- "0 0 0 0 k " ++
 -- "578.853 824.603 -561.385 -807.345 re " ++
 -- "f " ++
 "0.039 0.024 0.02 0 k " ++
 "566.479 731.97 -537.601 20.16 re " ++
 "S " ++
 -- "566.479 759.33 -537.601 54.72 re " ++
 -- "f " ++


 "BT " ++
 "/TT0 1 Tf " ++

 "0.806 0.719 0.51 0.504 k " ++
 "21 0 0 21 39.8198 787.9463 Tm " ++
 "(Verifikat)Tj " ++

 "0.546 0.469 0.454 0.113 k " ++
 "12 0 0 12 39.8198 766.9555 Tm " ++
 "[(Dok.nr)55(. " ++ winAnsiPostScriptEncode documentNumber ++ ")]TJ " ++

 "0.806 0.719 0.51 0.504 k " ++
 "12 0 0 12 39.8198 736.8555 Tm " ++
 "(Parter)Tj " ++
 "ET " ++

 {-
 "q " ++
 "0 841.89 595.28 -841.89 re " ++
 "W n " ++
 "q " ++
 "0 g " ++
 "0 Tc 0 Tw /Fm0 Do " ++
 "Q " ++
 "Q " ++
 -}


 -- every signatory on its own line, repeated every 64 pixels down
 "q " ++ concatMap signatorybox persons ++
 "1 0 0 1 0 126 cm " ++ -- compensate for 3 signatures
 

 -- Datum and Handelse
 "0.4 G " ++
 "566.479 540.93 -537.601 20.16 re " ++
 "S " ++
 "566.479 566.85 -537.601 20.16 re " ++
 "S " ++

 "BT " ++
 "0.784 0.698 0.475 0.533 k " ++
 "/TT0 1 Tf " ++
 "0 Tc 0 Tw " ++
 "12 0 0 12 40 571.9502 Tm " ++
 "[(Registrerade händelser)]TJ " ++
 "11 0 0 11 40 546.3926 Tm " ++
 "(Datum)Tj " ++
 "11 0 0 11 225 546.3926 Tm " ++
 "(Händelser)Tj " ++
 "ET " ++ 

 -- logentry
 concatMap logentry history ++

 "Q " ++

 -- "0.039 0.024 0.02 0 k " ++
 "571.856 24.7 -548.354 64.55 re " ++
 "S " ++

 "BT " ++
 "0.625 0.537 0.53 0.257 k " ++
 "/TT0 1 Tf " ++
 "8 0 0 8 39.8198 74.2334 Tm " ++
 "1.2 TL " ++
 "[(Detta verifikat är utfärdat av SkrivaPå CM AB och styrker att dokument nummer " ++ documentNumber ++ 
 " har undertecknats)]TJ " ++
 "T* " ++
 "[(av parterna och är juridiskt bindande. Kursiverad information är säkert verifierad genom vår tjänst.)]TJ " ++
 "T* " ++
 "[(Kontrollera dokumentet mot vår databas genom följande länk: " ++ hostpart ++ "/d/" ++ documentNumber ++ ".)]TJ " ++
 "0.546 0.469 0.454 0.113 k " ++
 "10 0 0 10 46.5522 31.5469 Tm " ++
 "(Sida 1 av 1)Tj " ++
 "ET " ++ rightcornerseal2 

rightcornerseal2 = "q 1 0 0 1 491.839 14.37 cm /SealMarkerForm Do Q" 

pageToForm :: RefID -> State Document RefID
pageToForm refid = do
    Just (Indir (Dict page) _) <- gets $ PdfModel.lookup refid
    let contentsrefid = case Prelude.lookup (BS.pack "Contents") page of
                          Just (Array [Ref contentsrefid]) -> contentsrefid
                          Just (Ref contentsrefid) -> contentsrefid
    Just (Indir (Dict contentsdict) (Just streamdata)) <- gets $ PdfModel.lookup contentsrefid
    
    let changekeys (k,v)
            | k==BS.pack "MediaBox" = [(BS.pack "BBox",v)]
            | k==BS.pack "Group" = [(k,v)]
            | k==BS.pack "Resources" = [(k,v)]
            | True = []
    let value = concatMap changekeys page ++ [entry "Subtype" "Form"] ++ contentsdict
    addStream (Dict value) streamdata



process (sealSpec@SealSpec 
    { input
    , output
    , documentNumber
    , persons
    , fields 
    }) = do
    Just doc <- PdfModel.parseFile input
    Just seal <- PdfModel.parseFile sealFileName 
    Just sealmarker <- PdfModel.parseFile "files/sealmarker.pdf" 
    let [paginpage1, sealpage1] = listPageRefIDs seal
        [sealmarkerpage] = listPageRefIDs sealmarker 

    let ((),outputdoc) = 
            flip runState doc $ do
              [newpagincontents,newsealcontents] <- importObjects seal [paginpage1,sealpage1]
              [sealmarkerpage2] <- importObjects sealmarker [sealmarkerpage]
              sealmarkerform <- pageToForm sealmarkerpage2
              let pagintext1 = pagintext sealSpec
              let sealtext = lastpage sealSpec
              placeSeals fields newsealcontents sealtext newpagincontents pagintext1 sealmarkerform
    writeFileX output outputdoc
    return ()


-- this is cheating
-- FIXME: font encoding
winAnsiChars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~?€\201‚ƒ„…†‡ˆ‰Š‹Œ\215Ž\217\220‘’“”•–—˜™š›œ\235žŸ ¡¢£¤¥¦§¨©ª«¬?®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"

unicodeToWinAnsi x = 
    case findIndex (==x) winAnsiChars of
      Just i -> chr (i + 33)
      Nothing -> x
      
