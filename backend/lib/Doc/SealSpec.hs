module Doc.SealSpec where

import Data.Int
import qualified Data.ByteString.Char8 as BS

data Person =
    Person { fullname            :: String
           , company             :: String
           , personalnumber      :: String
           , companynumber       :: String
           , email               :: String
           , phone               :: String
           , fullnameverified    :: Bool
           , companyverified     :: Bool
           , numberverified      :: Bool
           , emailverified       :: Bool
           , phoneverified       :: Bool
           , fields              :: [Field]
           , signtime            :: String
           , signedAtText        :: String
           , personalNumberText  :: String
           , companyNumberText   :: String
           , highlightedImages   :: [HighlightedImage]
           , identifiedNameText  :: String
           , nameFromText        :: String
           }
    deriving (Eq,Ord,Show,Read)

-- | Field coordinates are in screen coordinate space. That means:
--
-- * upper left corner is (0,0)
-- * units are pixels
-- * (x,y) are coordinates of upper left corner of a field.
--
-- It is for pdfseal program to recalculate pixel coordinates into
-- correct PDF pt coordinates. pdfseal will use pixel wise (w,h) to
-- translate (x,y).  pdfseal is also responsible to take into account
-- PDF's way of baseline calculation for font used.
data Field
  = Field
    { value            :: String -- ^ text to put into a field
    , x                :: Double -- ^ left coordinate of field in (0,0)-(1,1)
    , y                :: Double -- ^ upper coordinate of field in (0,0)-(1,1)
    , page             :: Int32  -- ^ on which page should the field be placed
    , fontSize         :: Double -- ^ font size divided by width
    , greyed           :: Bool   -- ^ field should be grayed, (to mark temporary values)
    , includeInSummary :: Bool   -- ^ add this field to report at the very end of document
    }
  | FieldJPG
    { valueBinary      :: BS.ByteString -- ^ binary content of image to put into a field
    , x                :: Double -- ^ left coordinate of field in (0,0)-(1,1)
    , y                :: Double -- ^ upper coordinate of field in  (0,0)-(1,1)
    , page             :: Int32  -- ^ on which page should the field be placed
    , image_w          :: Double -- ^ image width in (0,0)-(1,1)
    , image_h          :: Double -- ^ image height in (0,0)-(1,1)
    , includeInSummary :: Bool   -- ^ add this field to report at the very end of document
    , onlyForSummary   :: Bool   -- ^ if image is supposed to be included only in summary x,y, page will be inored
    , keyColor         :: Maybe (Int,Int,Int) -- ^ transparent color in the form of RGB tripple (0-255)
    }
    deriving (Eq, Ord, Show, Read)

-- | Image is a transparent layer that was put on pdf page during signing to
--   highlight important information to signatory. It should cover whole page

data HighlightedImage = HighlightedImage {
    hiPage             :: Int32  -- ^ on which page should the image be placed
  , hiImage            :: BS.ByteString -- ^ binary content of image
  } deriving (Eq, Ord, Show, Read)

-- | An attachment that will be put into a PDF. Attachments are put in
-- order.  File name should be without any directory parts. File
-- content as base64 encoded string.
data SealAttachment = SealAttachment
  { fileName     :: String        -- ^ how should attached file be named
  , mimeType     :: Maybe String  -- ^ optional "subtype" specification, like "text/plain"
  , fileContent  :: BS.ByteString -- ^ binary content of the file
  }
    deriving (Eq,Ord,Show,Read)

data SealSpec = SealSpec
    { input          :: String
    , output         :: String
    , documentNumberText :: String
    , persons        :: [Person]
    , secretaries    :: [Person]
    , initiator      :: Maybe Person
    , initialsText   :: String
    , hostpart       :: String
    , staticTexts    :: SealingTexts
    , attachments    :: [SealAttachment]
    , filesList      :: [FileDesc]
    , disableFooter  :: Bool
    , extendedFlattening :: Bool
    }
    deriving (Eq,Ord,Show,Read)

data FileDesc = FileDesc
    { fileTitle      :: String
    , fileRole       :: String
    , filePagesText  :: String
    , fileAttachedBy :: String
    , fileSealedOn   :: Maybe String
    , fileAttachedToSealedFileText :: Maybe String
    , fileInput      :: Maybe String
    }
    deriving (Eq,Ord,Show,Read)

data PreSealSpec = PreSealSpec
    { pssInput  :: String
    , pssOutput :: String
    , pssFields :: [Field]
    , pssExtendedFlattening :: Bool
    }
    deriving (Eq,Ord,Show,Read)

{- |  Static (almost) text for sealing document.
      !!!! IMPORTANT Templates for sealing depends on read instance of this class
      If You change this structure sealing WILL fail, unless changes are made to docseal.st
-}
data SealingTexts = SealingTexts
  { verificationTitle  :: String -- Big title at last page
  , partnerText        :: String -- Header for partner list
  , initiatorText      :: String -- Header for initiator
  , documentText       :: String -- Header for documents list
  , verificationPageDescription :: String -- Long summary near the end of verification page
  , hiddenAttachmentText :: String -- "Concealed Attachment"
  , onePageText        :: String -- "1 page"
  }
  deriving (Eq,Ord,Show,Read)
