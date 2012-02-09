{-# LANGUAGE DoRec #-}

module Seal where
import PdfModel
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Graphics.PDF.Text
import Graphics.PDF hiding (Box)
import SealSpec
import qualified Data.ByteString.Base64 as Base64

winAnsiPostScriptEncode :: String -> String
winAnsiPostScriptEncode text' = concatMap charEncode text'
    where
      charEncode '(' = "\\("
      charEncode ')' = "\\)"
      charEncode x = [unicodeToWinAnsi x]

sealFileName :: String
sealFileName = "files/seal3.pdf"

listPageRefIDSFromPages :: Document -> RefID -> [RefID]
listPageRefIDSFromPages document' pagesrefid =
    let
       pages = case PdfModel.lookup pagesrefid document' of
                 Just (Indir (Dict pages') _) -> pages'
                 x -> error ("lookup of " ++ show pagesrefid ++ " as /Page returned " ++ show x)
       unKids (Ref r) = r
       unKids x = error ("element of /Kids array is not Ref, it is " ++ show x)
       list = case Prelude.lookup (BS.pack "Kids") pages of
                Just (Array kids) -> concatMap (listPageRefIDSFromPages document' . unKids) kids
                Nothing -> [pagesrefid]
                x -> error ("/Kids key in " ++ show pagesrefid ++ " is not array of refs, it is " ++ show x)
    in list

addPageToDocument :: Value -> State Document RefID
addPageToDocument (Dict newpagevalue) = do 
  rec
    document <- get
    let
        firstBody = head (documentBodies document)
        trailer' = bodyTrailer firstBody
        root = case Prelude.lookup (BS.pack "Root") trailer' of
                 Just (Ref root') -> root'
                 x -> error ("/Root is wrong: " ++ show x)
        catalog = case PdfModel.lookup root document of
                    Just (Indir (Dict catalog') _) -> catalog'
                    x -> error ("lookup of " ++ show root ++ " returned " ++ show x)
        pagesrefid = case Prelude.lookup (BS.pack "Pages") catalog of
                       Just (Ref pagesrefid') -> pagesrefid'
                       x -> error ("lookup of /Pages in catalog returned " ++ show x)
        pages = case PdfModel.lookup pagesrefid document of
                  Just (Indir (Dict pages') _) -> pages'
                  x -> error ("lookup of root pages dict at " ++ show pagesrefid ++ " returned " ++ show x)
--      unKids (Ref r) = r
        newkids = case Prelude.lookup (BS.pack "Kids") pages of
                Just (Array kids) -> Array (kids ++ [Ref newpageid])
                x -> error ("lookup for /Kids returned " ++ show x)
        newcount = case Prelude.lookup (BS.pack "Count") pages of
                Just (Number count) -> count + 1
                x -> error ("lookup for /Count returned " ++ show x)
        skipkeys list = filter (\(a,_) -> a/=BS.pack "Count" && a/=BS.pack "Kids") list
        skipkeys2 list = filter (\(a,_) -> a/=BS.pack "Parent") list
        newpagesindir = Indir (Dict ([(BS.pack "Count", Number newcount),(BS.pack "Kids",newkids)] ++ skipkeys pages)) Nothing
    setIndir pagesrefid newpagesindir
    newpageid <- addObject (Dict (skipkeys2 newpagevalue ++ [(BS.pack "Parent", Ref pagesrefid)]))
  return newpageid
addPageToDocument _ = error "addPageToDocument only for Dict"

listPageRefIDs :: Document -> [RefID]
listPageRefIDs document' =
    let
        firstBody = head (documentBodies document')
        trailer' = bodyTrailer firstBody
        root = case Prelude.lookup (BS.pack "Root") trailer' of
                 Just (Ref root') -> root'
                 x -> error ("/Root is wrong: " ++ show x)
        catalog = case PdfModel.lookup root document' of
                    Just (Indir (Dict catalog') _) -> catalog'
                    x -> error ("lookup of /Catalog at " ++ show root ++ " in document returned " ++ show x)
        pagesrefid = case Prelude.lookup (BS.pack "Pages") catalog of
                       Just (Ref pagesrefid') -> pagesrefid'
                       x -> error ("lookup of /Pages in catalog at " ++ show root ++ " returned " ++ show x ++ " catalog is " ++ show catalog)
                       
    in listPageRefIDSFromPages document' pagesrefid

getResDict :: Document -> RefID -> DictData
getResDict doc pageid =
    let
        -- FIXME: refs or something
        Just (Indir (Dict pagedict) _) = PdfModel.lookup pageid doc
        resource = case Prelude.lookup (BS.pack "Resources") pagedict of
          Nothing -> case Prelude.lookup (BS.pack "Parent") pagedict of
                         Nothing -> []
                         Just (Ref parentrefid') -> getResDict doc parentrefid'
                         _ -> []
          Just (Dict value') -> value'
          Just (Ref refid') -> case PdfModel.lookup refid' doc of
                                Just (Indir (Dict value') _) -> value'
                                _ -> error "2"
          _ -> error "/Resources key is totally bogus"
    in resource
        
mergeResourceBranch :: Document -> DictData -> DictData -> DictData
mergeResourceBranch _document source1 source2 = source1 ++ source2
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
mergeResources document' source1 source2 =
    Map.toList map3
    where
      map1 = Map.fromList source1
      map2 = Map.fromList source2
      map3 = Map.unionWith unite map1 map2
      unite (Ref refid') x = 
          case PdfModel.lookup refid' document' of
            Just (Indir value' _) -> unite value' x
            _ -> x
      unite x (Ref refid') = 
          case PdfModel.lookup refid' document' of
            Just (Indir value' _) -> unite x value'
            _ -> x
      unite (Dict d1) (Dict d2) =
          Dict (mergeResourceBranch document' d1 d2)
      unite x _ = x

placeSealOnPageRefID :: RefID -> RefID -> (RefID,String) -> Document -> Document
placeSealOnPageRefID sealrefid sealmarkerformrefid (pagerefid,sealtext) document' = 
    let
        Just (Indir (Dict pagedict) pagestrem) = 
            PdfModel.lookup pagerefid document'
        Just contentvalue = {- trace (show pagedict) $ -} Prelude.lookup (BS.pack "Contents") pagedict
        contentlist = case contentvalue of
                        Ref{} -> [contentvalue]
                        Array arr -> arr
                        _ -> error "/Contents must be either ref or array of refs"

        ([q,qQ,rr], docx) = flip runState document' $ do
            q' <- addStream (Dict []) $ BSL.pack "q "
            qQ' <- addStream (Dict []) $ BSL.pack (" Q " ++ normalizeToA4 ++ " " ++ rotationtext ++ " % here\n")
            rr' <- addStream (Dict []) $ BSL.pack (sealtext)
            return [q',qQ',rr']

        sealpagecontents = contentsValueListFromPageID document' sealrefid

        rotatekey = Prelude.lookup (BS.pack "Rotate") pagedict
        ([cropbox_l, cropbox_b, cropbox_r, cropbox_t] {- :: Double -})
            = case (Prelude.lookup (BS.pack "CropBox") pagedict `mplus` Prelude.lookup (BS.pack "MediaBox") pagedict) of
                  Just (PdfModel.Array [PdfModel.Number l, PdfModel.Number b, PdfModel.Number r, PdfModel.Number t]) -> 
                       [l :: Double, b, r, t]
                  _ -> [0, 0, 595, 842]

        cropbox_w = cropbox_r - cropbox_l
        cropbox_h = cropbox_t - cropbox_b

        normalizeToA4 = show (Number (cropbox_w/595)) ++ " " ++
                        show (Number (0)) ++ " " ++
                        show (Number (0)) ++ " " ++
                        show (Number (cropbox_h/842)) ++ " " ++
                        show (Number (-cropbox_l)) ++ " " ++
                        show (Number (-cropbox_b)) ++ " cm"

        rotationtext = case rotatekey of -- 595 842
                         Just (Number 90) -> "0 1 -1 0 " ++ show (Number cropbox_w) ++ " 0 cm"
                         Just (Number 180) -> "-1 0 0 -1 " ++ show (Number cropbox_w) ++ " " ++ show (Number cropbox_h) ++ " cm"
                         Just (Number 270) -> "0 -1 1 0 0 " ++ show (Number cropbox_h) ++ " cm"
                         _ -> ""

        sealresdict = getResDict document' sealrefid
        pageresdict = getResDict document' pagerefid
        newresdictcont1 = mergeResources document' sealresdict pageresdict
        newxobjectdict = case Prelude.lookup (BS.pack "XObject") newresdictcont1 of
                           Just (Dict w) -> Dict (w ++ [(BS.pack "SealMarkerForm",Ref sealmarkerformrefid)]) 
                           Just (Ref r) -> case PdfModel.lookup r document' of
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

-- FIXME: here we still have font size problem. On the page it appears
-- as some pt size font. We need to translate that size into PDF pt
-- size. For now pretend we are using 10pt font.
--
-- For next generations: the coordinate space in PDF is in printer's
-- points and bottom left corner is (0,0). Grows right and
-- upwards. Fonts are also strange: the y=0 is on font baseline. Read
-- about this one, before you change what is below.
fieldstext :: Int -> Int -> [Field] -> String
fieldstext pagew pageh fields = concatMap fieldtext fields
  where
    fontBaseline = 8
    fieldtext Field{ SealSpec.value = val
                   , x
                   , y
                   , w
                   , h
                   } = "q 1 0 0 1 " ++ show (fromIntegral (x * pagew) / fromIntegral w :: Double) ++ " " ++ 
                       show (fromIntegral ((h - y) * pageh) / fromIntegral h - fontBaseline :: Double) ++ " cm " ++
                       "BT /SkrivaPaHelvetica 10 Tf (" ++ winAnsiPostScriptEncode val ++ ") Tj ET Q "
    fieldtext FieldJPG{ SealSpec.valueBase64 = val
                   , x
                   , y
                   , w
                   , h
                   , image_w, image_h
                   , internal_image_w, internal_image_h
                   } = "q " ++ intercalate " " [  show (fromIntegral (image_w * pagew) / fromIntegral w :: Double) 
                                               , "0"
                                               , "0"
                                               ,  show (fromIntegral (image_h * pageh) / fromIntegral h :: Double)
                                               ,  show (fromIntegral (x * pagew) / fromIntegral w :: Double)
                                               ,  show (fromIntegral ((h - y - image_h) * pageh) / fromIntegral h :: Double)
                                               ] ++ " cm " ++
                       "BI /BPC 8 /CS /RGB /F /DCT " ++
                       "/H " ++ show internal_image_h ++ " /W " ++ show internal_image_w ++ " ID\n" ++ 
                       BS.unpack (Base64.decodeLenient (BS.pack val)) ++ "\nEI Q "


placeSeals :: [Field] -> RefID -> [String] -> RefID -> String -> RefID -> State Document ()
placeSeals fields sealrefid sealtexts paginrefid pagintext' sealmarkerformrefid = do
    pages <- gets listPageRefIDs
    let pagew = 595
        pageh = 842
    let pagevalue = page_dict (Array []) (Array []) `ext` [entryna "MediaBox" [0,0,pagew,pageh]]
    -- should optimize pagintext' into one stream
    let findFields pageno = filter (\x -> page x == pageno) fields
    let pagintext1 pageno = fieldstext pagew pageh (findFields pageno)++ 
                     pagintext' ++ 
                     " q 0.2 0 0 0.2 " ++ show (fromIntegral (pagew - 18) / 2 :: Double) ++ " 14 cm /SealMarkerForm Do Q "

    modify $ \document' -> foldr (placeSealOnPageRefID paginrefid sealmarkerformrefid) document'
                          [(page,pagintext1 pageno) | (page,pageno) <- zip pages [1..]]
    flip mapM_ sealtexts $ \sealtext -> do
        lastpage' <- addPageToDocument pagevalue
        modify $ \document' -> foldr (placeSealOnPageRefID sealrefid sealmarkerformrefid) document' [(lastpage',sealtext)]


contentsValueListFromPageID :: Document -> RefID -> [RefID]
contentsValueListFromPageID document' pagerefid = 
    let
        Just (Indir (Dict pagedict) _) = 
            PdfModel.lookup pagerefid document'
        Just contentvalue = Prelude.lookup (BS.pack "Contents") pagedict
        contentlist = case contentvalue of
                        Ref r -> [r]
                        Array arr -> map unRefID arr
                        _ -> error "/Contents must be ref or an array of refs"
        unRefID (Ref r) = r
        unRefID _ = error "unRefID not on ref"
    in contentlist

data Box = Box Int String

boxToString :: Box -> String
boxToString (Box height content) = " q " ++ content ++ " Q " ++ "1 0 0 1 0 " ++ show (-height) ++ " cm "

groupBoxesUpToHeight :: Int -> [Box] -> [[Box]]
groupBoxesUpToHeight height boxes = helper boxes
    where
        worker _ currentBoxes [] = (currentBoxes,[])
        worker currentHeight currentBoxes rest@((x@(Box h _)):xs) 
                                               | currentHeight + h < height = worker (currentHeight + h) (x:currentBoxes) xs
                                               | otherwise = (currentBoxes, rest)
        helper boxes' = case worker 0 [] boxes' of
                            (cb, []) -> [reverse cb]
                            (cb, rest) -> reverse cb : helper rest

pagintext :: SealSpec -> String
pagintext (SealSpec{documentNumber,initials,staticTexts }) = 
 let
    font = PDFFont Helvetica 8
    docnrwidth = textWidth font (toPDFString docnrtext)
    docnroffset = center - 20 - docnrwidth
    center = 595/2
    signedinitials = (signedText staticTexts) ++": " ++ winAnsiPostScriptEncode initials 
    siwidth = textWidth font (toPDFString signedinitials)
    sioffset = center + 20
    docnrtext = (docPrefix staticTexts) ++ " "++ documentNumber
    
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
 show (sioffset+siwidth+10) ++ " 18 m " ++ show (595 - 60 :: Int) ++ " 18 l S " ++
 "Q "

makeManyLines :: PDFFont -> PDFFloat -> String -> [String]
makeManyLines font width text' = result
    where
    textSplit :: String -> [String]
    textSplit [] = []
    -- textSplit [x] = [[x]]
    textSplit (text'') = let (b,a1) = break (==' ') text''
                             (s,a) = break (/=' ') a1
                         in (b ++ s) : textSplit a  
    textWithLength :: String -> (PDFFloat,String)
    textWithLength text'' = (textWidth font (toPDFString text''),text'')
    textSplitWithLength = map textWithLength (textSplit text')
    takeWhileLength :: PDFFloat -> String -> [(PDFFloat,String)] -> [String]
    takeWhileLength _   text'' [] = [text'']
    takeWhileLength len text'' all'@((l,t):rest)
                    | len + l < width = takeWhileLength (len + l) (text'' ++ t) rest
                    | otherwise = text'' : takeWhileLength 0 "" all'
    textOutLine text'' = "[(" ++ winAnsiPostScriptEncode text'' ++ ")]TJ T* "
    textLines = takeWhileLength 0 "" textSplitWithLength
    result = map textOutLine textLines 




verificationTextBox :: SealingTexts -> Box
verificationTextBox staticTexts = Box 22 $
    "BT " ++
    "/TT0 1 Tf " ++
    "0.806 0.719 0.51 0.504 k " ++
    "21 0 0 21 39.8198 787.9463 Tm " ++
    "(" ++ winAnsiPostScriptEncode (verificationTitle staticTexts) ++ ") Tj " ++
    "ET "

documentNumberTextBox :: SealingTexts -> String -> Box
documentNumberTextBox staticTexts documentNumber = Box 30 $
    "1 0 0 1 0 22 cm " ++
    "BT " ++
    "/TT0 1 Tf " ++
    "0.546 0.469 0.454 0.113 k " ++
    "12 0 0 12 39.8198 766.9555 Tm " ++
    "[(" ++ docPrefix staticTexts ++ ") 55 ( " ++ winAnsiPostScriptEncode documentNumber ++ ")]TJ " ++
    "ET "

partnerTextBox :: SealingTexts -> Box
partnerTextBox staticTexts = Box 30 $
    "1 0 0 1 0 22 cm " ++
    "1 0 0 1 0 30 cm " ++
    "0.039 0.024 0.02 0 k " ++
    "566.479 731.97 -537.601 20.16 re " ++
    "S " ++
    "BT " ++
    "/TT0 1 Tf " ++
    "0.806 0.719 0.51 0.504 k " ++
    "12 0 0 12 39.8198 736.8555 Tm " ++
    "("++ winAnsiPostScriptEncode (partnerText staticTexts) ++ ")Tj " ++
    "ET "

secretaryBox :: SealingTexts -> Box
secretaryBox staticTexts = Box 27 $
            "1 0 0 1 0 17 cm " ++
            "1 0 0 1 0 30 cm " ++
            "BT " ++
            "/TT0 1 Tf " ++
            "0.806 0.719 0.51 0.504 k " ++
            "12 0 0 12 39.8198 736.8555 Tm " ++
            "(" ++ secretaryText staticTexts ++ ")Tj " ++
            "ET "

signatoryBox :: SealingTexts -> Person -> Box
signatoryBox sealingTexts (Person {fullname,company,companynumber,email}) = 
 let
    orgnrtext = if companynumber=="" then "" else (orgNumberText sealingTexts) ++ " " ++ companynumber
    orgnroffset = textWidth (PDFFont Helvetica 10) (toPDFString orgnrtext)
    emailoffset = textWidth (PDFFont Helvetica_Oblique 10) (toPDFString email)
    rightmargin = 595 - 46.5522
 in Box 42 $
    "1 0 0 1 0 22 cm " ++
    "1 0 0 1 0 26 cm " ++
    "1 0 0 1 0 30 cm " ++
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
    "ET "


handlingBox :: SealingTexts -> Box
handlingBox staticTexts = Box 26 $
    "1 0 0 1 0 22 cm " ++
    "1 0 0 1 0 26 cm " ++
    "1 0 0 1 0 100 cm " ++
    "1 0 0 1 0 30 cm " ++
    "1 0 0 1 0 30 cm " ++
    "0.4 G " ++
    "566.479 566.85 -537.601 20.16 re " ++
    "S " ++
    "BT " ++
    "0.784 0.698 0.475 0.533 k " ++
    "/TT0 1 Tf " ++
    "0 Tc 0 Tw " ++
    "12 0 0 12 40 571.9502 Tm " ++
    "[("++ eventsText staticTexts ++")]TJ " ++
    "ET "

dateAndHistoryBox :: SealingTexts -> Box
dateAndHistoryBox staticTexts = Box 26 $
    "1 0 0 1 0 22 cm " ++
    "1 0 0 1 0 26 cm " ++
    "1 0 0 1 0 26 cm " ++
    "1 0 0 1 0 100 cm " ++
    "1 0 0 1 0 30 cm " ++
    "1 0 0 1 0 30 cm " ++
    "0.4 G " ++
    "566.479 540.93 -537.601 20.16 re " ++
    "S " ++
    "BT " ++
    "0.784 0.698 0.475 0.533 k " ++
    "/TT0 1 Tf " ++
    "11 0 0 11 40 546.3926 Tm " ++
    "("++ dateText staticTexts ++")Tj " ++
    "11 0 0 11 225 546.3926 Tm " ++
    "("++ historyText staticTexts ++")Tj " ++
    "ET "

logEntryBox :: HistEntry -> Box
logEntryBox (HistEntry {histdate,histcomment}) = 
    let outlines = (makeManyLines (PDFFont Helvetica_Oblique 10) 300 histcomment)
    in
        Box (8 + length outlines * 12) $ 
                "1 0 0 1 0 30 cm " ++
                "1 0 0 1 0 30 cm " ++
                "1 0 0 1 0 22 cm " ++
                "1 0 0 1 0 26 cm " ++
                "1 0 0 1 0 26 cm " ++
                "1 0 0 1 0 26 cm " ++
                "1 0 0 1 0 100 cm " ++
                "BT " ++
                "/TT2 1 Tf " ++
                "0.591 0.507 0.502 0.19 k " ++
                "10 0 0 10 46 520.8887 Tm " ++
                "(" ++ winAnsiPostScriptEncode histdate ++ ")Tj " ++
                "10 0 0 10 231 520.8887 Tm " ++
                "1.2 TL " ++
                concat outlines ++
                "ET "

verificationPagesContents :: SealSpec -> [String]
verificationPagesContents (SealSpec {documentNumber,persons,secretaries,history,staticTexts}) = 
    let boxes = [verificationTextBox staticTexts] ++
                [documentNumberTextBox staticTexts documentNumber] ++
                [partnerTextBox staticTexts] ++

                -- every signatory on its own line, repeated every 64 pixels down
                map (signatoryBox staticTexts) persons ++

                (case secretaries of
                     [] -> []
                     _ -> [secretaryBox staticTexts] ++
                          map (signatoryBox staticTexts) secretaries
                ) ++
 
                -- Datum and Handelse
                [handlingBox staticTexts] ++
                [dateAndHistoryBox staticTexts] ++

                -- logentry
                map (logEntryBox) history 
        groupedBoxes = groupBoxesUpToHeight 650 boxes
        groupedBoxesNumbered = zip groupedBoxes [1::Int ..]
    in flip map groupedBoxesNumbered $ \(thisPageBoxes, thisNumber) ->   

    "/GS0 gs " ++
    "0.4 G " ++

    -- Frame around whole page
    "0.081 0.058 0.068 0 k " ++
    "581.839 14.37 -567.36 813.12 re " ++
    "S " ++

    "q " ++
    concatMap boxToString thisPageBoxes ++
    "Q " ++

    -- "0.039 0.024 0.02 0 k " ++
    "571.856 24.7 -548.354 64.55 re " ++
    "S " ++

    "BT " ++
    "0.625 0.537 0.53 0.257 k " ++
    "/TT0 1 Tf " ++
    "8 0 0 8 39.8198 74.2334 Tm " ++
    "1.2 TL " ++
    intercalate "T* " (map (\t -> "[(" ++ t ++ ")]TJ ") (verificationFooter staticTexts)) ++
    "0.546 0.469 0.454 0.113 k " ++
    "10 0 0 10 46.5522 31.5469 Tm " ++
    "(" ++ show thisNumber ++ "/" ++ show (length groupedBoxesNumbered) ++ ")Tj " ++
    "ET " ++ rightcornerseal2 

-- To emulate a near perfect circle of radius r with cubic Bézier
-- curves, draw four curves such that one of the line segments
-- connecting the starting point with the nearest control point, or
-- the ending point with its nearest control point, is vertical, while
-- the other one is horizontal. The length l of each such segment
-- equals r multiplied by kappa. kappa is 0.5522847498 in our case we
-- need: 45 + 25 = 70, 45 - 25 = 20
rightcornerseal2 :: String
rightcornerseal2 = "q 1 0 0 1 491.839 14.37 cm " ++
                   "1 g 1 G " ++
                   "0 45 m " ++
                   "0  20 20 0  45 0  c " ++
                   "70 0  90 20 90 45 c " ++
                   "90 70 70 90 45 90 c " ++
                   "20 90 0  70 0  45 c " ++
                   "f " ++
                   "/SealMarkerForm Do Q" 

pageToForm :: RefID -> State Document RefID
pageToForm refid' = do
    Just (Indir (Dict page) _) <- gets $ PdfModel.lookup refid'
    let contentsrefid = case Prelude.lookup (BS.pack "Contents") page of
                          Just (Array [Ref contentsrefid']) -> contentsrefid'
                          Just (Ref contentsrefid') -> contentsrefid'
                          _ -> error "/Contents must be a ref or an array of refs"
    Just (Indir (Dict contentsdict) (Just streamdata)) <- gets $ PdfModel.lookup contentsrefid
    
    let changekeys (k,v)
            | k==BS.pack "MediaBox" = [(BS.pack "BBox",v)]
            | k==BS.pack "Group" = [(k,v)]
            | k==BS.pack "Resources" = [(k,v)]
            | True = []
    let value' = concatMap changekeys page ++ [entry "Subtype" "Form"] ++ contentsdict
    addStream (Dict value') streamdata

addFileStream :: SealAttachment -> State Document (Value, RefID)
addFileStream SealAttachment{fileBase64Content, fileName} = do
   let content = BSL.fromChunks [Base64.decodeLenient (BS.pack fileBase64Content)]
   sid <- addStream (Dict [(BS.pack "Type", Name $ BS.pack "EmbeddedFile")]) $ content
   sid2 <- addObject (Dict [ (BS.pack "Type", Name $ BS.pack "Filespec")
                           , (BS.pack "F", string fileName)
                           , (BS.pack "EF", Dict [
                                   (BS.pack "F", Ref sid)
                                   ])
                           ])

   return (string fileName, sid2)

attachFiles :: [SealAttachment] -> State Document ()
attachFiles [] = return () -- do nothing
attachFiles sealAttachments = do
    -- two step process:
    -- 1. create appriopriate streams
    -- 2. link them into name tree
    --
    -- Embedded files names tree is in: /Root -> /Names ->
    -- /EmbeddedFiles If /Names is inside, we want to preserve
    -- content.  We want to kill what is inside /EmbeddedFiles, too
    -- much trouble to try to merge this together.
    --
    -- We probably should add /Limits here as this seems required

    pairArray <- mapM addFileStream sealAttachments
    let plainArray1 = sortBy (comparing fst) pairArray
        plainArray = concatMap (\(a,b) -> [a,Ref b]) plainArray1
        lowLimit = fst (head plainArray1)
        highLimit = fst (last plainArray1)

    embeddedFilesNamesTree <- addObject (Dict [ (BS.pack "Names", Array plainArray)
                                              , (BS.pack "Limits", Array [lowLimit, highLimit])
                                              ])

    embeddedFilesNamesTreeRoot <- addObject (Dict [ (BS.pack "Kids", Array [Ref embeddedFilesNamesTree])])

    document <- get
    let
        firstBody = head (documentBodies document)
        trailer' = bodyTrailer firstBody
        root = case Prelude.lookup (BS.pack "Root") trailer' of
                 Just (Ref root') -> root'
                 x -> error ("/Root is wrong: " ++ show x)
        catalog = case PdfModel.lookup root document of
                    Just (Indir (Dict catalog') _) -> catalog'
                    x -> error ("lookup of " ++ show root ++ " returned " ++ show x)
        names = case Prelude.lookup (BS.pack "Names") catalog of
                    Just (Dict names') -> names'
                    Just (Ref namesrefid) -> case PdfModel.lookup namesrefid document of
                                               Just (Indir (Dict names') _) -> names'
                                               x -> error ("lookup of " ++ show namesrefid ++ " returned " ++ show x)
                    Nothing -> []
                    x -> error ("lookup of " ++ show root ++ " returned " ++ show x)

        skipEmbeddedFiles = filter (not . (== BS.pack "EmbeddedFiles") . fst)
        skipNames = filter (not . (== BS.pack "Names") . fst)

        names2 = Dict ((BS.pack "EmbeddedFiles", Ref embeddedFilesNamesTreeRoot) : skipEmbeddedFiles names)
        catalog2 = Dict ((BS.pack "Names", names2) : skipNames catalog)

    setObject root catalog2


process :: SealSpec -> IO ()
process (sealSpec@SealSpec 
    { input
    , output
    , fields 
    , attachments
    }) = do
    mdoc <- PdfModel.parseFile input
    doc <- maybe (error $ "Cannot parse input PDF " ++ input) return mdoc
    mseal <- PdfModel.parseFile sealFileName 
    seal <- maybe (error $ "Cannot parse seal PDF " ++ sealFileName) 
            return mseal
    msealmarker <- PdfModel.parseFile "files/sealmarker.pdf" 
    sealmarker <- maybe (error $ "Cannot parse marker PDF " ++ "files/sealmarker.pdf")
                  return msealmarker

    let [paginpage1, sealpage1] = listPageRefIDs seal
        [sealmarkerpage] = listPageRefIDs sealmarker 

    let ((),outputdoc) = 
            flip runState doc $ do
              [newpagincontents,newsealcontents] <- importObjects seal [paginpage1,sealpage1]
              [sealmarkerpage2] <- importObjects sealmarker [sealmarkerpage]
              sealmarkerform <- pageToForm sealmarkerpage2
              let pagintext1 = pagintext sealSpec
              let sealtexts = verificationPagesContents sealSpec
              placeSeals fields newsealcontents sealtexts newpagincontents pagintext1 sealmarkerform
              when (not (null attachments)) $
                   attachFiles attachments
    putStrLn $ "Writing file " ++ output
    writeFileX output outputdoc
    return ()
