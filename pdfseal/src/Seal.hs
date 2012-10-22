{-# LANGUAGE DoRec #-}

module Seal where
import PdfModel hiding(trace)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Graphics.PDF.Text
import Graphics.PDF (PDFFloat)
import SealSpec
import qualified Data.ByteString.Base64 as Base64

buildList :: State [a] () -> [a]
buildList actions = reverse (execState actions [])

lm :: a -> State [a] ()
lm e = modify (\x -> e : x)

lms :: [a] -> State [a] ()
lms es = mapM_ lm es

printableWidth :: Int
printableWidth = 567

printableHeight :: Int
printableHeight = 673

printableMargin :: Int
printableMargin = 23

frameInnerPadding :: Int
frameInnerPadding = 16

cardWidth :: Int
cardWidth = (printableWidth - 4*frameInnerPadding - 2*printableMargin) `div` 2

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

getResDict :: RefID -> State Document DictData
getResDict pageid = do
  value' <- getInheritedPageKey "Resources" pageid
  case value' of
    Just (Dict dt) -> return dt
    _ -> do
      -- /Resources key is not dict, should not happen in normal PDFs
      return []

-- Keys 'Resources', 'MediaBox' and 'CropBox' in PDF are inherited
-- from /Page and /Pages via /Parent key. Use this method to fins
-- proper key. This inheritance is old concept and abandoned, other
-- similar keys like ArtBox, BleedBox and TrimBox are not inherited in
-- PDF.
getInheritedPageKey :: String -> RefID -> State Document (Maybe Value)
getInheritedPageKey key pageid = do
  Just (Indir (Dict pagedict) _) <- gets $ PdfModel.lookup pageid
  case Prelude.lookup (BS.pack key) pagedict of
    Nothing -> do
      case Prelude.lookup (BS.pack "Parent") pagedict of
        Nothing -> do
          return Nothing
        Just (Ref parentrefid') -> do
          getInheritedPageKey key parentrefid'
        _ -> do
          -- /Parent key is bogus, but we ignore it
          return Nothing
    Just (Ref refid') -> do
      m <- gets $ PdfModel.lookup refid'
      case m of
        Just (Indir value' _) -> do
          return (Just value')
        _ -> do
          -- refered object does not exist in PDF, should not happen,
          -- but according to spec this is same as null
          return Nothing
    Just value' -> do
      return (Just value')

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
          -- at this point we merge resources from two PDF pages d1
          -- and d2 are maps so simple concat will produce duplicates
          -- luckyly we control names in d2 and have chosen them to
          -- not conflict with anything in this world
          Dict (d1 ++ d2)
      unite x _ = x

placeContentOnPage :: DictData -> RefID -> (Int -> Int -> String) -> State Document ()
placeContentOnPage newres pagerefid sealtext = do
  Just (Indir (Dict pagedict) pagestrem) <- gets $
            PdfModel.lookup pagerefid
  let mcontentvalue = Prelude.lookup (BS.pack "Contents") pagedict
  let contentlist = case mcontentvalue of
                        Just (contentvalue@Ref{}) -> [contentvalue]
                        Just (Array arr) -> arr
                        x -> error $ "/Contents must be either ref or array of refs but found " ++ show x

  [cropbox_l, cropbox_b, cropbox_r, cropbox_t] <- do
     mcropBox <- getInheritedPageKey "CropBox" pagerefid
     case mcropBox of
       Just (Array [Number l, Number b, Number r, Number t]) ->
         return [l :: Double, b, r, t]
       _ -> do
         mmediaBox <- getInheritedPageKey "MediaBox" pagerefid
         case mmediaBox of
           Just (Array [Number l, Number b, Number r, Number t]) ->
             return [l :: Double, b, r, t]
           _ -> return [0, 0, 595, 842]

  let cropbox_w = cropbox_r - cropbox_l
      cropbox_h = cropbox_t - cropbox_b

  let offsetToZero = [1,0,0,1,-cropbox_l,-cropbox_b]
  let rotateKey = Prelude.lookup (BS.pack "Rotate") pagedict
  let rotateToZero = case rotateKey of
                       Just (Number 90)  -> [ 0,  1, -1,  0, cropbox_w,         0]
                       Just (Number 180) -> [-1,  0,  0, -1, cropbox_w, cropbox_h]
                       Just (Number 270) -> [ 0, -1,  1,  0,         0, cropbox_h]
                       _                 -> [ 1,  0,  0,  1,         0,         0]

  q <- addStream (Dict []) $ BSL.pack "q "
  qQ <- addStream (Dict []) $ BSL.pack $ unlines [ " Q "
                                                 , unwords (map show offsetToZero) ++ " cm"
                                                 , unwords (map show rotateToZero) ++ " cm "
                                                 ]

  rr <- addStream (Dict []) $ BSL.pack (sealtext (floor cropbox_w) (floor cropbox_h))

  pageresdict <- getResDict pagerefid
  document' <- get

  -- create new /Resources dictionary that has all resources from page
  -- merged with all resources from our seal template files and merged
  -- with seal marker resource on top of that
  let newresdict = Dict $ mergeResources document' pageresdict newres

  -- new content array should have following structure:
  -- - save state
  -- - render old contents as it were
  -- - restore state
  -- - compensate for possible cropbox offset from (0,0)
  -- - compensate for /Rotation attribute
  -- - render whatever was specified to render
  let newcontentarray = Array ([Ref q] ++ contentlist ++ [Ref qQ] ++ [Ref rr])
      skipkey x = BS.pack "Contents" == x || BS.pack "Resources" == x
      pagedict2 = filter (not . skipkey . fst) pagedict
      newpagedict = Dict pagedict2 `ext` [(BS.pack "Contents", newcontentarray)
                                           ,(BS.pack "Resources", newresdict)]
      newpage = (Indir newpagedict pagestrem)
  -- finally set new page in place of the old one
  modify (setIndirF pagerefid newpage)

placeFieldsOnPage :: (RefID,String) -> Document -> Document
placeFieldsOnPage (pagerefid,sealtext) document' =
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

        pageresdict = evalState (getResDict pagerefid) document'


        newcontentarray = Array ([Ref q] ++ contentlist ++ [Ref qQ] ++ [Ref rr])
        skipkey x = BS.pack "Contents" == x || BS.pack "Resources" == x
        pagedict2 = filter (not . skipkey . fst) pagedict
        newpagedict = Dict pagedict2 `ext` [(BS.pack "Contents", newcontentarray)
                                           ,(BS.pack "Resources", Dict pageresdict)]
        newpage = (Indir newpagedict pagestrem)
        newdocument = setIndirF pagerefid newpage docx
    in newdocument

tellMatrix :: (MonadWriter String m) => Double -> Double -> Double -> Double -> Double -> Double -> m ()
tellMatrix a b c d e f =
      tell $ (concat $ intersperse " " $ map (show . Number) [a::Double,b,c,d,e,f]) ++ " cm\n"

-- FIXME: here we still have font size problem. On the page it appears
-- as some pt size font. We need to translate that size into PDF pt
-- size. For now pretend we are using 10pt font.
--
-- For next generations: the coordinate space in PDF is in printer's
-- points and bottom left corner is (0,0). Grows right and
-- upwards. Fonts are also strange: the y=0 is on font baseline. Read
-- about this one, before you change what is below.
--
-- Image x, y, image_w and image_h are in screen pixels, anchored in
-- upper left corner.  The internal_image_w and internal_image_h are
-- in image pixels. Usually these are equal to image_w and image_h,
-- but we have the machinery to scale should it be needed.
commandsFromFields :: Int -> Int -> [(Field,[RefID])] -> String
commandsFromFields pagew pageh fields = concatMap commandsFromField fields
  where
    fontBaseline = 0.8
    commandsFromField ( Field{ SealSpec.value = val
                             , x
                             , y
                             , fontSize
                             }
                      , _) = execWriter $ do
                               tell "q\n"
                               tellMatrix 1 0 0 1
                                      (x * fromIntegral pagew)

                                      (((1 - y) * fromIntegral pageh) - fontBaseline * fs)
                               tell "BT\n"
                               tell $ "/SkrivaPaHelvetica " ++ show (Number fs) ++ " Tf\n"
                               tell $ "(" ++ winAnsiPostScriptEncode val ++ ") Tj\n"
                               tell "ET\n"
                               tell "Q\n"
       where
          fs = if fontSize>0
               then fontSize * fromIntegral pagew
               else 10

    commandsFromField (FieldJPG{ SealSpec.valueBase64 = val },_) | null val = ""
    commandsFromField (FieldJPG{ x
                               , y
                               , image_w, image_h
                               }
                      ,[imgid]) = execWriter $ do
                         tell "q\n"
                         tellMatrix (image_w * fromIntegral pagew)
                                    0 0
                                    (image_h * fromIntegral pageh)
                                    (x * fromIntegral pagew)
                                    ((1 - y - image_h) * fromIntegral pageh)
                         tell $ "/SkrivaPa_" ++ show (objno imgid) ++ "_" ++ show (gener imgid) ++ " Do\n"
                         tell "Q\n"
    commandsFromField _ = "" -- ignoring broken field specification

makeXObjectImageIfNeeded :: Field -> State Document (Field,[RefID])
makeXObjectImageIfNeeded field@(FieldJPG{ SealSpec.valueBase64 = val
                                       , internal_image_w
                                       , internal_image_h
                                       , keyColor
                                       }) = do
  let dict' = dict ([ entry "Subtype" "Image"
                    , entry "ColorSpace" "DeviceRGB"
                    , entry "Filter" "DCTDecode"
                    , entryn "BitsPerComponent" (8::Int)
                    , entryn "Width" internal_image_w
                    , entryn "Height" internal_image_h
                    ] ++ (case keyColor of
                           Nothing -> []
                           Just (r,g,b) -> [ entryna "Mask" [ max (r-10) 0, min (r+10) 255
                                                            , max (g-10) 0, min (g+10) 255
                                                            , max (b-10) 0, min (b+10) 255
                                                            ]]))
  let streamData = BSL.fromChunks [Base64.decodeLenient (BS.pack val)]
  refid' <- addStream dict' streamData
  return (field,[refid'])
makeXObjectImageIfNeeded field = do
  return (field,[])

placeFieldsAndPagina :: SealSpec -> [Field] -> RefID -> RefID -> State Document ()
placeFieldsAndPagina sealSpec fields paginrefid sealmarkerformrefid = do
    pages <- gets listPageRefIDs

    fieldsWithIndirectObjs <- mapM makeXObjectImageIfNeeded fields

    let findFields pageno = filter (\(field,_) -> page field == pageno) fieldsWithIndirectObjs
    let contentCommands pageno = \pagew pageh ->
           commandsFromFields pagew pageh (findFields pageno) ++
           paginCommands pagew sealSpec

    paginresdict <- getResDict paginrefid

    document' <- get

    let makeEntry rd = entry ("SkrivaPa_" ++ show (objno rd) ++ "_" ++ show (gener rd)) rd
    let newres = mergeResources document' paginresdict
                        [ entry "XObject" $ dict (entry "SealMarkerForm" sealmarkerformrefid
                                                  : (map makeEntry . concatMap snd) fieldsWithIndirectObjs)
                        ]
    mapM_ (uncurry $ placeContentOnPage newres)
            [(page,contentCommands pageno) | (page,pageno) <- zip pages [1..]]

appendVerificationPages :: RefID -> [String] -> RefID -> State Document ()
appendVerificationPages sealrefid sealtexts sealmarkerformrefid = do

    sealresdict <- getResDict sealrefid

    document' <- get

    let newres = mergeResources document' sealresdict
                        ([(BS.pack "XObject", Dict [(BS.pack "SealMarkerForm",Ref sealmarkerformrefid)])])

    flip mapM_ sealtexts $ \sealtext -> do
      let pagew = 595
          pageh = 842
      lastpage' <- addPageToDocument (page_dict (Array []) (Array []) `ext` [entryna "MediaBox" [0::Int,0,pagew,pageh]])
      placeContentOnPage newres lastpage' (\_ _ -> sealtext)

placeFields :: [Field] -> RefID -> State Document ()
placeFields fields paginrefid = do
    pages <- gets listPageRefIDs

    fieldsWithIndirectObjs <- mapM makeXObjectImageIfNeeded fields

    let findFields pageno = filter (\(field,_) -> page field == pageno) fieldsWithIndirectObjs

    let contentCommands pageno = \pagew pageh ->
           commandsFromFields pagew pageh (findFields pageno)

    paginresdict <- getResDict paginrefid
    document' <- get

    let makeEntry rd = entry ("SkrivaPa_" ++ show (objno rd) ++ "_" ++ show (gener rd)) rd
    let newres = mergeResources document' paginresdict
                        [ entry "XObject" $ dict $ (map makeEntry . concatMap snd) fieldsWithIndirectObjs
                        ]
    mapM_ (uncurry $ placeContentOnPage newres)
            [(page,contentCommands pageno) | (page,pageno) <- zip pages [1..]]

contentsValueListFromPageID :: RefID -> State Document [RefID]
contentsValueListFromPageID pagerefid = do
  Just (Indir (Dict pagedict) _) <- gets $
            PdfModel.lookup pagerefid
  let mcontentvalue = Prelude.lookup (BS.pack "Contents") pagedict
  let contentlist = case mcontentvalue of
                        Just (Ref r) -> [r]
                        Just (Array arr) -> map unRefID arr
                        x -> error $ "/Contents must be ref or an array of refs, found " ++ show x
      unRefID (Ref r) = r
      unRefID _ = error "unRefID not on ref"
  return contentlist


data Box = Box
  { boxWidth    :: Int         -- ^ box width in pt
  , boxHeight   :: Int         -- ^ box height in pt
  , boxCommands :: String      -- ^ commands for the box, does not safe the state
  }


boxHCat :: Int -> [Box] -> Box
boxHCat sep boxes = boxDrawDebugRectAround $
  Box { boxHeight = maximum (map boxHeight boxes)
      , boxWidth  = sum (map boxWidth boxes)
      , boxCommands = concatMap boxF boxes
      }
  where boxF box = " q " ++ boxCommands box ++ " Q 1 0 0 1 " ++ show (boxWidth box + sep) ++ " 0 cm "

boxVCat :: Int -> [Box] -> Box
boxVCat sep boxes = boxDrawDebugRectAround $
  Box { boxHeight = sum (map boxHeight boxes)
      , boxWidth  = maximum (map boxWidth boxes)
      , boxCommands = concatMap boxF boxes
      }
  where boxF box = " q " ++ boxCommands box ++ " Q 1 0 0 1 0 " ++ show (-boxHeight box - sep) ++ " cm "


boxHCenter :: Int -> Box -> Box
boxHCenter width box =
  box { boxWidth = width
      , boxCommands = " 1 0 0 1 " ++ show ((width - boxWidth box) `div` 2) ++ " 0 cm\n" ++ boxCommands box
      }

boxVCenter :: Int -> Box -> Box
boxVCenter height box =
  box { boxHeight = height
      , boxCommands = " 1 0 0 1 0 " ++ show ((height - boxHeight box) `div` 2) ++ " cm\n" ++ boxCommands box
      }


boxEnlarge :: Int -> Int -> Int -> Int -> Box -> Box
boxEnlarge left bottom right top  box =
  Box { boxWidth = boxWidth box + left + right
      , boxHeight = boxHeight box + bottom + top
      , boxCommands = " 1 0 0 1 " ++ show left ++ " " ++ show (-top) ++ " cm " ++ boxCommands box
      }

boxStrokeColor :: Float -> Float -> Float -> Float -> Box -> Box
boxStrokeColor c m y k box =
  box { boxCommands = show c ++ " " ++ show m ++ " " ++ show y ++ " " ++ show k ++ " K " ++ boxCommands box }

boxFillColor :: Float -> Float -> Float -> Float -> Box -> Box
boxFillColor c m y k box =
  box { boxCommands = show c ++ " " ++ show m ++ " " ++ show y ++ " " ++ show k ++ " k " ++ boxCommands box }

boxDrawDebugRectAround :: Box -> Box
boxDrawDebugRectAround box = box
{-
  box { boxCommands = " q [1 5] 0 d 0 g 0 G 0 0 " ++ show (boxWidth box) ++ " " ++ show (-boxHeight box) ++ " re S Q " ++ boxCommands box
      }
-}

boxDrawFrame :: Box -> Box
boxDrawFrame box =
  box { boxCommands = " q 0 0 " ++ show (boxWidth box) ++ " " ++ show (-boxHeight box) ++ " re S Q " ++ boxCommands box
      }

boxDrawBottomLine :: Box -> Box
boxDrawBottomLine box =
  box { boxCommands = boxCommands box ++ 
                      " q 0 " ++ show (-boxHeight box) ++ " m " ++ 
                      show (boxWidth box) ++ " " ++ show (-boxHeight box) ++ " l S Q "
      }

setFrameColor :: Box -> Box
setFrameColor = boxStrokeColor 0 0 0 0.333

setDarkTextColor :: Box -> Box
setDarkTextColor = boxFillColor 0.806 0.719 0.51 0.504

setLightTextColor :: Box -> Box
setLightTextColor = boxFillColor 0.597 0.512 0.508 0.201

groupBoxesUpToHeight :: Int -> [(Bool,Bool,Box)] -> [[(Bool,Box)]]
groupBoxesUpToHeight height boxes = helper boxes
    where
        worker _ currentBoxes [] = (currentBoxes,[])
        worker currentHeight currentBoxes rest@((goesWithNext, needsFrame, x@(Box _ h _)):xs)
                                               | currentHeight + h < height || goesWithNext
                                                 = worker (currentHeight + h) ((needsFrame,x):currentBoxes) xs
                                               | otherwise = (currentBoxes, rest)
        helper boxes' = case worker 0 [] boxes' of
                            (cb, []) -> [reverse cb]
                            (cb, rest) -> reverse cb : helper rest

paginCommands :: Int -> SealSpec -> String
paginCommands pageWidth (SealSpec{documentNumber,initials,staticTexts }) =
 let
    font = PDFFont Helvetica 8
    docnrwidth = textWidth font (toPDFString docnrtext)
    docnroffset = center - 20 - docnrwidth
    center = fromIntegral pageWidth/2
    signedinitials = signedText staticTexts ++ ": " ++ winAnsiPostScriptEncode initials
    siwidth = textWidth font (toPDFString signedinitials)
    sioffset = center + 20
    docnrtext = docPrefix staticTexts ++ " " ++ documentNumber

 in unlines [ "q"                                              -- safe state
            , "0.546 0.469 0.454 0.113 k"                      -- set graish color
            , "BT"                                             -- begin text
            ,   "/SkrivaPaHelvetica 8 Tf"                        -- choose 8pt font
            ,   "1 0 0 1 " ++ show docnroffset ++ " 20 Tm"       -- move to beginning of document number text
            ,   "(" ++ docnrtext ++ ")Tj"                        -- write document number
            ,   "1 0 0 1 " ++ show sioffset ++ " 20 Tm"          -- move to the beginning of initials text
            , "[(" ++ signedinitials ++ ")] TJ"                -- write initials
            , "ET"                                             -- end of text
            , "0.863 0.43 0.152 0.004 K"                       -- choose blueish color
            , "0.4 w"                                          -- line is 0.4pt wide
            , "60 23 m " ++ show (docnroffset-10) ++ " 23 l S" -- draw left line
            , show (sioffset+siwidth+10) ++ " 23 m " ++ show (pageWidth - 60) ++ " 23 l S" -- draw right line
                   -- seal form is 90pt x 90 pt, so 0.2*90 is 18
                   -- line at 23 minus half of 18 is 14
            , "0.2 0 0 0.2 " ++ show (Number ((fromIntegral pageWidth - 18) / 2)) ++ " 14 cm" -- position for drawing seal
            , "/SealMarkerForm Do"                             -- draw the seal in the middle between lines
            , "Q"                                              -- restore state
            ]


-- | This function takes font and maximal width of lines. Then it
-- splits text on space characters so that neither line crosses width
-- limit unless there is a sole word that is too long to split.
splitLinesOfLength :: PDFFont -> Int -> String -> [String]
splitLinesOfLength font width text' = result
    where
    widthBreak :: String -> String -> (String,String)
    widthBreak "" r = (r,"")
    widthBreak (t:ts) r = if (textWidth font (toPDFString $ r ++ [t]) < fromIntegral width )
                            then widthBreak ts (r ++ [t])
                            else (r,t:ts)
    textSplit :: String -> [String]
    textSplit [] = []
    textSplit (text'') = let (b,a1) = break (==' ') text''
                             (s,a) = break (/=' ') a1
                             h = b ++ s
                         in if (textWidth font (toPDFString h) < fromIntegral width )
                               then h : textSplit a
                               else let
                                      (t,r) =   widthBreak h ""
                                    in t : (textSplit (r ++ a))
    textWithLength :: String -> (PDFFloat,String)
    textWithLength text'' = (textWidth font (toPDFString text''),text'')
    textSplitWithLength = map textWithLength (textSplit text')
    takeWhileLength :: PDFFloat -> String -> [(PDFFloat,String)] -> [String]
    takeWhileLength _   text'' [] = [text'']
    takeWhileLength len text'' all'@((l,t):rest)
                    | len + l < fromIntegral width = takeWhileLength (len + l) (text'' ++ t) rest
                    | otherwise = text'' : takeWhileLength 0 "" all'
    result = takeWhileLength 0 "" textSplitWithLength
    
makeManyLines :: PDFFont -> Int -> String -> [String]
makeManyLines font width text' = result
  where
    textOutLine text'' = "[(" ++ winAnsiPostScriptEncode text'' ++ ")] TJ T* "
    result = map textOutLine (splitLinesOfLength font width text')


makeLeftTextBox :: PDFFont -> Int -> String -> Box
makeLeftTextBox font@(PDFFont name' size) width text' = result
  where
    result = Box { boxWidth = width
                 , boxHeight = 12 * size * length lines' `div` 10
                 , boxCommands = commands
                 }
    lines' = splitLinesOfLength font width text'
    textOutLine text'' = "[(" ++ winAnsiPostScriptEncode text'' ++ ")] TJ T*\n"
    commands = " BT\n" ++
               " " ++ show (Number $ 1.2 * fromIntegral size) ++ " TL\n" ++
               " /" ++ resourceFontName ++ " " ++ show size ++ " Tf\n" ++
               " 1 0 0 1 0 " ++  show (-size) ++ " Tm\n" ++ -- FIXME: where to start this really?
               concatMap textOutLine lines' ++
               " ET\n"
    resourceFontName =
      case name' of
        Helvetica -> "TT0"
        Helvetica_Bold -> "TT1"
        Helvetica_Oblique -> "TT2"
        _ -> error $ "Font " ++ show name' ++ " not available, add to resources dictionary"

verificationTextBox :: SealingTexts -> Box
verificationTextBox staticTexts =
  setDarkTextColor $
  makeLeftTextBox (PDFFont Helvetica 21) 500
                  (verificationTitle staticTexts)

documentNumberTextBox :: SealingTexts -> String -> Box
documentNumberTextBox staticTexts documentNumber =
  setLightTextColor $
  makeLeftTextBox (PDFFont Helvetica 12) 500
                  (docPrefix staticTexts ++ " " ++ documentNumber)


partnerTextBox :: SealingTexts -> Box
partnerTextBox staticTexts =
  setDarkTextColor $
  boxEnlarge 5 12 0 23 $
  makeLeftTextBox (PDFFont Helvetica 12) 200
                    (partnerText staticTexts)

secretaryBox :: SealingTexts -> Box
secretaryBox staticTexts =
  setDarkTextColor $
  boxEnlarge 5 12 0 23 $
  makeLeftTextBox (PDFFont Helvetica 12) 200
                    (secretaryText staticTexts)

documentTextBox :: SealingTexts -> Box
documentTextBox staticTexts =
  setDarkTextColor $
  boxEnlarge 5 12 0 23 $
  makeLeftTextBox (PDFFont Helvetica 12) 200
                    (documentText staticTexts)

signatoryBox :: SealingTexts -> Person -> Box
signatoryBox sealingTexts (Person {fullname,personalnumber,company,companynumber,email,fields}) =
  boxVCat 0 $ buildList $ do
    lm (makeLeftTextBox (PDFFont Helvetica_Bold 10) width fullname)
    lm (makeLeftTextBox (PDFFont Helvetica 10) width company)
    lm (Box 0 10 "")
    when (not (null personalnumber)) $
         lm (makeLeftTextBox (PDFFont Helvetica 10) width $ personalNumberText sealingTexts ++ " " ++ personalnumber)
    when (not (null companynumber)) $
         lm (makeLeftTextBox (PDFFont Helvetica 10) width $ orgNumberText sealingTexts ++ " " ++ companynumber)
    lm (makeLeftTextBox (PDFFont Helvetica_Oblique 10) width email)
    forM_ fields $ \field ->
      case field of
        FieldJPG{ SealSpec.valueBase64 = val
                , internal_image_w, internal_image_h
                , includeInSummary = True
                } -> let halfWidth, halfHeight :: Int
                         halfWidth = cardWidth `div` 2
                         halfHeight = (halfWidth * internal_image_h `div` internal_image_w)
                    in lm $ boxEnlarge 0 6 0 6 $
                        setFrameColor $
                        boxDrawBottomLine $
                        Box halfWidth halfHeight $ execWriter $ do
                         tell "q\n"
                         tellMatrix (fromIntegral halfWidth) 0 0 (fromIntegral halfHeight) 0 (-fromIntegral halfHeight)
                         tell "BI\n"        -- begin image
                         tell "/BPC 8\n"    -- 8 bits per pixel
                         tell "/CS /RGB\n"  -- color space is RGB
                         tell "/F /DCT\n"   -- filter is DCT, that means JPEG
                         tell $ "/H " ++ show internal_image_h ++ "\n"  -- height is pixels
                         tell $ "/W " ++ show internal_image_w ++ "\n"  -- width in pixels
                         tell "ID "         -- image data follow
                         tell $ BS.unpack (Base64.decodeLenient (BS.pack val)) ++ "\n"
                         tell "EI\n"        -- end image
                         tell "Q\n"
        _ -> return ()
  where
    width = cardWidth


fileBox :: SealingTexts -> FileDesc -> Box
fileBox _sealingTexts (FileDesc {fileTitle,fileRole,filePagesText,fileAttachedBy}) =
  boxVCat 0 $ buildList $ do
    lm (makeLeftTextBox (PDFFont Helvetica_Bold 10) cardWidth fileTitle)
    lm (makeLeftTextBox (PDFFont Helvetica 10) cardWidth fileRole)
    lm (Box 0 10 "")
    lm (makeLeftTextBox (PDFFont Helvetica 10) cardWidth filePagesText)
    lm (makeLeftTextBox (PDFFont Helvetica_Oblique 10) cardWidth fileAttachedBy)


handlingBox :: SealingTexts -> Box
handlingBox staticTexts =
  setDarkTextColor $
  boxEnlarge 5 12 0 23 $
  makeLeftTextBox (PDFFont Helvetica 12) 300
                    (eventsText staticTexts)


makeHistoryEntryBox :: HistEntry -> Box
makeHistoryEntryBox (HistEntry {histdate,histcomment,histaddress}) =
  setLightTextColor $
  boxHCat 0 [ boxEnlarge frameInnerPadding 5 frameInnerPadding 5 $
              boxVCat 0 $ buildList $ do
                lm (makeLeftTextBox (PDFFont Helvetica_Oblique 10) (140) histdate)
                when (not (null histaddress))$
                     lm (makeLeftTextBox (PDFFont Helvetica_Oblique 8) (140) histaddress)
            , boxEnlarge frameInnerPadding 5 frameInnerPadding 5 $
              makeLeftTextBox (PDFFont Helvetica_Oblique 10) (cardWidth*2 - 140) histcomment
            ]


boxHCat2 :: [Box] -> [Box]
boxHCat2 [] = []
boxHCat2 [box] = [setLightTextColor . setFrameColor . boxDrawFrame . boxEnlarge 16 11 16 11 $ box]
boxHCat2 (box1:box2:boxes) = (boxHCat 0 [ (setLightTextColor . setFrameColor . boxDrawFrame . boxEnlarge 16 11 16 11) $
                                                            box1 { boxHeight = maximum [boxHeight box1, boxHeight box2] }
                                        , (setLightTextColor . setFrameColor . boxDrawFrame . boxEnlarge 16 11 16 11) $
                                                            box2 { boxHeight = maximum [boxHeight box1, boxHeight box2] }
                                        ]) : boxHCat2 boxes

boxP :: [Box] -> [Box]
boxP = boxHCat2

verificationPagesContents :: SealSpec -> [String]
verificationPagesContents (SealSpec {documentNumber,persons,secretaries,history,staticTexts,filesList}) =
  map pageContent groupedBoxesNumbered
    where
      histExample = makeHistoryEntryBox (HistEntry "" "" "")

      boxes = buildList $ do
                  lm (True,False,Box 0 30 "")
                  lm (True,False,verificationTextBox staticTexts)
                  lm (True,False,documentNumberTextBox staticTexts documentNumber)
                  lm (True,False,Box 0 20 "")

                  lm (True,False,documentTextBox staticTexts)
                  lms (map (\x -> (False,False,x)) $ boxP $ map (fileBox staticTexts) filesList)

                  lm (True,False,partnerTextBox staticTexts)
                  lms (map (\x -> (False,False,x)) $ boxP $ map (signatoryBox staticTexts) persons)

                  when (not (null secretaries)) $ do
                     lm (True,False,secretaryBox staticTexts)
                     lms (map (\x -> (False,False,x)) $ boxP $ map (signatoryBox staticTexts) secretaries)

                  lm (True,False,handlingBox staticTexts)
                  lms (map (\x -> (False, True, makeHistoryEntryBox x)) history)

      groupedBoxes = groupBoxesUpToHeight printableHeight boxes
      groupedBoxesNumbered = zip groupedBoxes [1::Int ..]
      boxize boxlist = if (fst (head boxlist))
                       then (setLightTextColor . setFrameColor . boxDrawFrame) $ boxVCat 0 $ map snd boxlist
                       else boxVCat 0 $ map snd boxlist
      drawFramesAsNeeded pb =
        map boxize $ groupBy (\x y -> fst x == fst y) pb

      pageContent (pageBoxes,pageNumber) =
          execWriter $ do
            tell "/GS0 gs "

            -- Frame around whole page
            tell "0 0 0 0.333 K "
            tell "581.839 14.37 -567.36 813.12 re "
            tell "S\n"

            tell "q\n"
            tell "1 0 0 1 15 808 cm\n"
            tell "0 g 0 G\n"
            tell $ boxCommands (boxEnlarge printableMargin 0 0 0 $ boxVCat 0 $ drawFramesAsNeeded pageBoxes)
            tell "Q\n"

            tell "q\n"

            let footerBox = boxEnlarge printableMargin 0 0 0 $
                                (setLightTextColor . setFrameColor . boxDrawFrame . boxEnlarge 16 11 16 11) $
                                boxEnlarge 0 0 90 0 $
                                boxVCat 5
                                          [ makeLeftTextBox (PDFFont Helvetica 8) (boxWidth histExample - 90 - 32)
                                                              (verificationFooter staticTexts)
                                          , makeLeftTextBox (PDFFont Helvetica 8) (boxWidth histExample - 90 - 32)
                                                              (show pageNumber ++ "/" ++ show (length groupedBoxesNumbered))
                                          ]
            tell $ "1 0 0 1 15 " ++ show (15 + boxHeight footerBox + printableMargin) ++ " cm\n"
            tell $ boxCommands footerBox
            tell "Q\n"
            tell (rightcornerseal (printableWidth - printableMargin - 66)
                  (15 + printableMargin + ((boxHeight footerBox - 90) `div` 2)))

-- To emulate a near perfect circle of radius r with cubic Bézier
-- curves, draw four curves such that one of the line segments
-- connecting the starting point with the nearest control point, or
-- the ending point with its nearest control point, is vertical, while
-- the other one is horizontal. The length l of each such segment
-- equals r multiplied by kappa. kappa is 0.5522847498 in our case we
-- need: 45 + 25 = 70, 45 - 25 = 20
rightcornerseal :: Int -> Int -> String
rightcornerseal x y = "q 1 0 0 1 " ++ show x ++ " " ++ show y ++ "  cm " ++
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

    document' <- get
    let
        firstBody = head (documentBodies document')
        trailer' = bodyTrailer firstBody
        root = case Prelude.lookup (BS.pack "Root") trailer' of
                 Just (Ref root') -> root'
                 x -> error ("/Root is wrong: " ++ show x)
        catalog = case PdfModel.lookup root document' of
                    Just (Indir (Dict catalog') _) -> catalog'
                    x -> error ("lookup of " ++ show root ++ " returned " ++ show x)
        names = case Prelude.lookup (BS.pack "Names") catalog of
                    Just (Dict names') -> names'
                    Just (Ref namesrefid) -> case PdfModel.lookup namesrefid document' of
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
    , persons
    , attachments
    , secretaries
    }) = do
    let fields' = concatMap fields persons ++ concatMap fields secretaries
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

    let outputdoc = flip execState doc $ do
              [newpagincontents,newsealcontents] <- importObjects seal [paginpage1,sealpage1]
              [sealmarkerpage2] <- importObjects sealmarker [sealmarkerpage]
              sealmarkerform <- pageToForm sealmarkerpage2
              let sealtexts = verificationPagesContents sealSpec
              placeFieldsAndPagina sealSpec fields' newpagincontents sealmarkerform
              appendVerificationPages newsealcontents sealtexts sealmarkerform
              when (not (null attachments)) $
                   attachFiles attachments
    putStrLn $ "Writing file " ++ output
    writeFileX output outputdoc
    return ()

preprocess :: PreSealSpec -> IO ()
preprocess (PreSealSpec{ pssInput, pssOutput, pssFields }) = do
    mdoc <- PdfModel.parseFile pssInput
    doc <- maybe (error $ "Cannot parse input PDF " ++ pssInput) return mdoc

    mseal <- PdfModel.parseFile sealFileName
    seal <- maybe (error $ "Cannot parse seal PDF " ++ sealFileName)
            return mseal

    let outputdoc = flip execState doc $ do
              let [paginpage1, _sealpage1] = listPageRefIDs seal
              [newpagincontents] <- importObjects seal [paginpage1]
              placeFields pssFields newpagincontents

    putStrLn $ "Writing file " ++ pssOutput
    writeFileX pssOutput outputdoc
    return ()
