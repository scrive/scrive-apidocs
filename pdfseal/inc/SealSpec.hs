
module SealSpec where

data Person =
    Person { fullname         :: String
           , company          :: String
           , personalnumber   :: String
           , companynumber    :: String
           , email            :: String
           , fullnameverified :: Bool
           , companyverified  :: Bool
           , numberverified   :: Bool
           , emailverified    :: Bool
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
data Field =
    Field { value :: String -- ^ text to put into a field
          , x     :: Int    -- ^ left coordinate of field
          , y     :: Int    -- ^ upper coordinate of field in screen coordinate space
          , page  :: Int    -- ^ on which page should the field be placed
          , w     :: Int    -- ^ page width in pixels
          , h     :: Int    -- ^ page height in pixels
          }
    | FieldJPG
      { valueBase64      :: String -- ^ binary content of image to put into a field
      , x                :: Int    -- ^ left coordinate of field
      , y                :: Int    -- ^ upper coordinate of field in screen coordinate space
      , page             :: Int    -- ^ on which page should the field be placed
      , w                :: Int    -- ^ page width in pixels
      , h                :: Int    -- ^ page height in pixels
      , image_w          :: Int    -- ^ image width in pixels as appears on te screen
      , image_h          :: Int    -- ^ image height in pixels as appears on te screen
      , internal_image_w :: Int    -- ^ pixels horizontal of image, if it was not scaled this is same as image_w
      , internal_image_h :: Int    -- ^ pixels vertical of image, if it was not scaled this is same as image_h
      }
    deriving (Eq, Ord, Show, Read)

-- | An attachment that will be put into a PDF. Attachments are put in
-- order.  File name should be without any directory parts. File
-- content as base64 encoded string.
data SealAttachment = SealAttachment
  { fileName          :: String -- ^ how should attached file be named
  , fileBase64Content :: String -- ^ base64 binary content of the file
  }
    deriving (Eq,Ord,Show,Read)

data SealSpec = SealSpec
    { input          :: String
    , output         :: String
    , documentNumber :: String
    , persons        :: [Person]
    , secretaries    :: [Person]
    , history        :: [HistEntry]
    , initials       :: String
    , hostpart       :: String
    , fields         :: [Field]
    , staticTexts    :: SealingTexts
    , attachments    :: [SealAttachment]
    }
    deriving (Eq,Ord,Show,Read)

data PreSealSpec = PreSealSpec
    { pssInput  :: String
    , pssOutput :: String
    , pssFields :: [Field]
    }
    deriving (Eq,Ord,Show,Read)

data HistEntry = HistEntry
    { histdate    :: String
    , histcomment :: String
    , histaddress :: String
    }
    deriving (Eq,Ord,Show,Read)


{- |  Static (almoust) text for sealing document.
      !!!! IMPORTANT Templates for sealing depends on read instance of this class
      If You change this structure sealing WILL fail, unless changes are made to docseal.st
-}
data SealingTexts = SealingTexts
  { verificationTitle  :: String -- Big title at last page
  , docPrefix          :: String -- ex. Doc. nr (last page and all footers)
  , signedText         :: String -- ex. Underteknat (all footers)
  , partnerText        :: String -- Header for partner list
  , secretaryText      :: String -- Header for secretary list
  , orgNumberText      :: String -- Info about partner subtext
  , eventsText         :: String -- history table preheader
  , dateText           :: String -- history table date header
  , historyText        :: String -- history table event header
  , verificationFooter :: String -- Long text all the end saying that doc was verified
  }
  deriving (Eq,Ord,Show,Read)
