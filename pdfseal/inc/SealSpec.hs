
module SealSpec where

import qualified Text.JSON.Gen as J

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
           , fields           :: [Field]
           }
    deriving (Eq,Ord,Show,Read)

instance J.ToJSValue Person where
  toJSValue person = J.runJSONGen $ do
    J.value "fullname" $ fullname person
    J.value "company" $ company person
    J.value "personalnumber" $ personalnumber person
    J.value "companynumber" $ companynumber person
    J.value "email" $ email person
    J.value "fullnameverified" $ fullnameverified person
    J.value "companyverified" $ companyverified person
    J.value "numberverified" $ numberverified person
    J.value "emailverified" $ emailverified person
    J.value "fields" $ map J.toJSValue $ fields person

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
    , page             :: Int    -- ^ on which page should the field be placed
    , fontSize         :: Double -- ^ font size divided by width
    , greyed           :: Bool   -- ^ field should be grayed, (to mark temporary values)
    , includeInSummary :: Bool   -- ^ add this field to report at the very end of document
    }
  | FieldJPG
    { valueBase64      :: String -- ^ binary content of image to put into a field
    , x                :: Double -- ^ left coordinate of field in (0,0)-(1,1)
    , y                :: Double -- ^ upper coordinate of field in  (0,0)-(1,1)
    , page             :: Int    -- ^ on which page should the field be placed
    , image_w          :: Double -- ^ image width in (0,0)-(1,1)
    , image_h          :: Double -- ^ image height in (0,0)-(1,1)
    , internal_image_w :: Int    -- ^ pixels horizontal of image in pixels
    , internal_image_h :: Int    -- ^ pixels vertical of image in pixels
    , includeInSummary :: Bool   -- ^ add this field to report at the very end of document
    , onlyForSummary   :: Bool   -- ^ if image is supposed to be included only in summary x,y, page will be inored
    , keyColor         :: Maybe (Int,Int,Int) -- ^ transparent color in the form of RGB tripple (0-255)
    }
    deriving (Eq, Ord, Show, Read)

instance J.ToJSValue Field where
  toJSValue Field{..} = J.runJSONGen $ do
    J.value "value" value
    J.value "x" x
    J.value "y" y
    J.value "page" page
    J.value "fontSize" fontSize
    J.value "greyed" greyed
    J.value "includeInSummary" includeInSummary
  toJSValue FieldJPG{..} = J.runJSONGen $ do
    J.value "valueBase64" valueBase64
    J.value "x" x
    J.value "y" y
    J.value "page" page
    J.value "image_w" image_w
    J.value "image_h" image_h
    J.value "internal_image_w" internal_image_w
    J.value "internal_image_h" internal_image_h
    J.value "includeInSummary" includeInSummary
    J.value "onlyForSummary" onlyForSummary
    J.value "keyColor" $ case keyColor of
                           Just (r,g,b) -> Just [r,g,b]
                           Nothing -> Nothing

-- | An attachment that will be put into a PDF. Attachments are put in
-- order.  File name should be without any directory parts. File
-- content as base64 encoded string.
data SealAttachment = SealAttachment
  { fileName          :: String       -- ^ how should attached file be named
  , mimeType          :: Maybe String -- ^ optional "subtype" specification, like "text/plain"
  , fileBase64Content :: String       -- ^ base64 binary content of the file
  }
    deriving (Eq,Ord,Show,Read)

instance J.ToJSValue SealAttachment where
  toJSValue SealAttachment{..} = J.runJSONGen $ do
   J.value "fileName" fileName
   J.value "mimeType" mimeType
   J.value "fileBase64Content" fileBase64Content

data SealSpec = SealSpec
    { input          :: String
    , output         :: String
    , documentNumber :: String
    , persons        :: [Person]
    , secretaries    :: [Person]
    , history        :: [HistEntry]
    , initials       :: String
    , hostpart       :: String
    , staticTexts    :: SealingTexts
    , attachments    :: [SealAttachment]
    , filesList      :: [FileDesc]
    }
    deriving (Eq,Ord,Show,Read)

instance J.ToJSValue SealSpec where
  toJSValue SealSpec{..} = J.runJSONGen $ do
    J.value "input" input
    J.value "output" output
    J.value "documentNumber" documentNumber
    J.value "persons" persons
    J.value "secretaries" secretaries
    J.value "history" history
    J.value "initials" initials
    J.value "hostpart" hostpart
    --J.value staticTexts    :: SealingTexts
    J.value "attachments" attachments
    J.value "filesList" filesList


data FileDesc = FileDesc
    { fileTitle      :: String
    , fileRole       :: String
    , filePagesText  :: String
    , fileAttachedBy :: String
    }
    deriving (Eq,Ord,Show,Read)

instance J.ToJSValue FileDesc where
  toJSValue FileDesc{..} = J.runJSONGen $ do
    J.value "title" fileTitle
    J.value "role" fileRole
    J.value "pagesText" filePagesText
    J.value "attachedBy" fileAttachedBy

data PreSealSpec = PreSealSpec
    { pssInput  :: String
    , pssOutput :: String
    , pssFields :: [Field]
    }
    deriving (Eq,Ord,Show,Read)

instance J.ToJSValue PreSealSpec where
  toJSValue PreSealSpec{..} = J.runJSONGen $ do
    J.value "input" pssInput
    J.value "output" pssOutput
    J.value "fields" $ map J.toJSValue $ pssFields
    J.value "preseal" True

data HistEntry = HistEntry
    { histdate    :: String
    , histcomment :: String
    , histaddress :: String
    }
    deriving (Eq,Ord,Show,Read)

instance J.ToJSValue HistEntry where
  toJSValue HistEntry{..} = J.runJSONGen $ do
    J.value "date" histdate
    J.value "comment" histcomment
    J.value "address" histaddress


{- |  Static (almost) text for sealing document.
      !!!! IMPORTANT Templates for sealing depends on read instance of this class
      If You change this structure sealing WILL fail, unless changes are made to docseal.st
-}
data SealingTexts = SealingTexts
  { verificationTitle  :: String -- Big title at last page
  , docPrefix          :: String -- ex. Doc. nr (last page and all footers)
  , signedText         :: String -- ex. Underteknat (all footers)
  , partnerText        :: String -- Header for partner list
  , secretaryText      :: String -- Header for secretary list
  , documentText       :: String -- Header for documents list
  , orgNumberText      :: String -- Info about partner subtext
  , personalNumberText :: String -- Info about partner subtext
  , eventsText         :: String -- history table preheader
  , dateText           :: String -- history table date header
  , historyText        :: String -- history table event header
  , verificationFooter :: String -- Long text all the end saying that doc was verified
  }
  deriving (Eq,Ord,Show,Read)
