module Doc.SealSpec where

import Data.Int
import qualified Data.ByteString.Char8 as BS

data Person =
    Person { fullname            :: Text
           , company             :: Text
           , personalnumber      :: Text
           , companynumber       :: Text
           , email               :: Text
           , phone               :: Text
           , fullnameverified    :: Bool
           , companyverified     :: Bool
           , numberverified      :: Bool
           , emailverified       :: Bool
           , phoneverified       :: Bool
           , fields              :: [Field]
           , signtime            :: Text
           , signedAtText        :: Text
           , personalNumberText  :: Text
           , companyNumberText   :: Text
           , highlightedImages   :: [HighlightedImage]
           , identifiedNameText  :: Text
           , nameFromText        :: Text
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
    { value            :: Text -- ^ text to put into a field
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
  { fileName     :: Text        -- ^ how should attached file be named
  , mimeType     :: Maybe Text  -- ^ optional "subtype" specification, like "text/plain"
  , fileContent  :: BS.ByteString -- ^ binary content of the file
  }
    deriving (Eq,Ord,Show,Read)

data SealSpec = SealSpec
    { input          :: Text
    , output         :: Text
    , documentNumberText :: Text
    , persons        :: [Person]
    , secretaries    :: [Person]
    , initiator      :: Maybe Person
    , initialsText   :: Text
    , hostpart       :: Text
    , staticTexts    :: SealingTexts
    , attachments    :: [SealAttachment]
    , filesList      :: [FileDesc]
    , disableFooter  :: Bool
    }
    deriving (Eq,Ord,Show,Read)

data FileDesc = FileDesc
    { fileTitle      :: Text
    , fileRole       :: Text
    , filePagesText  :: Text
    , fileAttachedBy :: Text
    , fileSealedOn   :: Maybe Text
    , fileAttachedToSealedFileText :: Maybe Text
    , fileInput      :: Maybe Text
    }
    deriving (Eq,Ord,Show,Read)

data PreSealSpec = PreSealSpec
    { pssInput  :: Text
    , pssOutput :: Text
    , pssFields :: [Field]
    }
    deriving (Eq,Ord,Show,Read)

{- |  Static (almost) text for sealing document.
      !!!! IMPORTANT Templates for sealing depends on read instance of this class
      If You change this structure sealing WILL fail, unless changes are made to docseal.st
-}
data SealingTexts = SealingTexts
  { verificationTitle  :: Text -- Big title at last page
  , partnerText        :: Text -- Header for partner list
  , initiatorText      :: Text -- Header for initiator
  , documentText       :: Text -- Header for documents list
  , verificationPageDescription :: Text -- Long summary near the end of verification page
  , hiddenAttachmentText :: Text -- "Concealed Attachment"
  , onePageText        :: Text -- "1 page"
  }
  deriving (Eq,Ord,Show,Read)
