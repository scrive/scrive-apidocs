module Doc.SealSpec where

import Control.Applicative
import Data.Functor.Invariant
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Unjson as Unjson

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
           }
    deriving (Eq,Ord,Show,Read)

unjsonPerson :: Unjson.UnjsonDef Person
unjsonPerson = Unjson.objectOf $ pure Person
    <*> Unjson.field "fullname"
        fullname
        ""
    <*> Unjson.field "company"
        company
        ""
    <*> Unjson.field "personalnumber"
        personalnumber
        ""
    <*> Unjson.field "companynumber"
        companynumber
        ""
    <*> Unjson.field "email"
        email
        ""
    <*> Unjson.field "phone"
        phone
        ""
    <*> Unjson.field "fullnameverified"
        fullnameverified
        ""
    <*> Unjson.field "companyverified"
        companyverified
        ""
    <*> Unjson.field "numberverified"
        numberverified
        ""
    <*> Unjson.field "emailverified"
        emailverified
        ""
    <*> Unjson.field "phoneverified"
        phoneverified
        ""
    <*> Unjson.fieldBy "fields"
        fields
        ""
        (Unjson.arrayOf unjsonField)
    <*> Unjson.field "signtime"
        signtime
        ""
    <*> Unjson.field "signedAtText"
        signedAtText
        ""
    <*> Unjson.field "personalNumberText"
        personalNumberText
        ""
    <*> Unjson.field "companyNumberText"
        companyNumberText
        ""

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
    { valueBinary      :: BS.ByteString -- ^ binary content of image to put into a field
    , x                :: Double -- ^ left coordinate of field in (0,0)-(1,1)
    , y                :: Double -- ^ upper coordinate of field in  (0,0)-(1,1)
    , page             :: Int    -- ^ on which page should the field be placed
    , image_w          :: Double -- ^ image width in (0,0)-(1,1)
    , image_h          :: Double -- ^ image height in (0,0)-(1,1)
    , includeInSummary :: Bool   -- ^ add this field to report at the very end of document
    , onlyForSummary   :: Bool   -- ^ if image is supposed to be included only in summary x,y, page will be inored
    , keyColor         :: Maybe (Int,Int,Int) -- ^ transparent color in the form of RGB tripple (0-255)
    }
    deriving (Eq, Ord, Show, Read)

isField :: Field -> Bool
isField (Field {}) = True
isField _ = False

isFieldJPG :: Field -> Bool
isFieldJPG (FieldJPG {}) = True
isFieldJPG _ = False


unjsonField :: Unjson.UnjsonDef Field
unjsonField = Unjson.unionOf
              [ (isField,
                 pure Field
                 <*> Unjson.field "value" value ""
                 <*> Unjson.field "x" x ""
                 <*> Unjson.field "y" y ""
                 <*> Unjson.field "page" page ""
                 <*> Unjson.field "fontSize" fontSize ""
                 <*> Unjson.field "greyed" greyed ""
                 <*> Unjson.field "includeInSummary" includeInSummary "")
              , (isFieldJPG,
                 pure FieldJPG
                 <*> Unjson.fieldBy "valueBase64" valueBinary ""
                     (invmap (B64.decodeLenient . BS.pack . Text.unpack) (Text.pack . BS.unpack . B64.encode) Unjson.unjsonDef)
                 <*> Unjson.field "x" x ""
                 <*> Unjson.field "y" y ""
                 <*> Unjson.field "page" page ""
                 <*> Unjson.field "image_w" image_w ""
                 <*> Unjson.field "image_h" image_h ""
                 <*> Unjson.field "includeInSummary" includeInSummary ""
                 <*> Unjson.field "onlyForSummary" onlyForSummary ""
                 <*> Unjson.fieldOpt "keyColor" keyColor ""
              )]


-- | An attachment that will be put into a PDF. Attachments are put in
-- order.  File name should be without any directory parts. File
-- content as base64 encoded string.
data SealAttachment = SealAttachment
  { fileName     :: String        -- ^ how should attached file be named
  , mimeType     :: Maybe String  -- ^ optional "subtype" specification, like "text/plain"
  , fileContent  :: BS.ByteString -- ^ binary content of the file
  }
    deriving (Eq,Ord,Show,Read)

unjsonSealAttachment :: Unjson.UnjsonDef SealAttachment
unjsonSealAttachment = Unjson.objectOf $ pure SealAttachment
   <*> Unjson.field "fileName" fileName ""
   <*> Unjson.fieldOpt "mimeType" mimeType ""
   <*> Unjson.fieldBy "fileBase64Content" fileContent ""
         (invmap (B64.decodeLenient . BS.pack . Text.unpack) (Text.pack . BS.unpack . B64.encode) Unjson.unjsonDef)


data SealSpec = SealSpec
    { input          :: String
    , output         :: String
    , documentNumberText :: String
    , persons        :: [Person]
    , secretaries    :: [Person]
    , initiator      :: Maybe Person
    , history        :: [HistEntry]
    , initialsText   :: String
    , hostpart       :: String
    , staticTexts    :: SealingTexts
    , attachments    :: [SealAttachment]
    , filesList      :: [FileDesc]
    }
    deriving (Eq,Ord,Show,Read)

unjsonSealSpec :: Unjson.UnjsonDef SealSpec
unjsonSealSpec = Unjson.objectOf $ pure SealSpec
    <*> Unjson.field "input" input ""
    <*> Unjson.field "output" output ""
    <*> Unjson.field "documentNumberText" documentNumberText ""
    <*> Unjson.fieldBy "persons" persons "" (Unjson.arrayOf unjsonPerson)
    <*> Unjson.fieldBy "secretaries" secretaries "" (Unjson.arrayOf unjsonPerson)
    <*> Unjson.fieldOptBy "initiator" initiator "" unjsonPerson
    <*> Unjson.fieldBy "history" history "" (Unjson.arrayOf unjsonHistEntry)
    <*> Unjson.field "initialsText" initialsText ""
    <*> Unjson.field "hostpart" hostpart ""
    <*> Unjson.fieldBy "staticTexts" staticTexts "" unjsonSealingTexts
    <*> Unjson.fieldBy "attachments" attachments "" (Unjson.arrayOf unjsonSealAttachment)
    <*> Unjson.fieldBy "filesList" filesList "" (Unjson.arrayOf unjsonFileDesc)


data FileDesc = FileDesc
    { fileTitle      :: String
    , fileRole       :: String
    , filePagesText  :: String
    , fileAttachedBy :: String
    , fileInput      :: Maybe String
    }
    deriving (Eq,Ord,Show,Read)

unjsonFileDesc :: Unjson.UnjsonDef FileDesc
unjsonFileDesc = Unjson.objectOf $ pure FileDesc
    <*> Unjson.field "title" fileTitle ""
    <*> Unjson.field "role" fileRole ""
    <*> Unjson.field "pagesText" filePagesText ""
    <*> Unjson.field "attachedBy" fileAttachedBy ""
    <*> Unjson.fieldOpt "input" fileInput ""

data PreSealSpec = PreSealSpec
    { pssInput  :: String
    , pssOutput :: String
    , pssFields :: [Field]
    }
    deriving (Eq,Ord,Show,Read)

unjsonPreSealSpec :: Unjson.UnjsonDef PreSealSpec
unjsonPreSealSpec = Unjson.objectOf $ pure PreSealSpec
    <*> Unjson.field "input" pssInput ""
    <*> Unjson.field "output" pssOutput ""
    <*> Unjson.fieldBy "fields" pssFields "" (Unjson.arrayOf unjsonField)
    <*  Unjson.field "preseal" (const True) ""

data HistEntry = HistEntry
    { histdate    :: String
    , histcomment :: String
    , histaddress :: String
    }
    deriving (Eq,Ord,Show,Read)

unjsonHistEntry :: Unjson.UnjsonDef HistEntry
unjsonHistEntry = Unjson.objectOf $ pure HistEntry
    <*> Unjson.field "date" histdate ""
    <*> Unjson.field "comment" histcomment ""
    <*> Unjson.field "address" histaddress ""


{- |  Static (almost) text for sealing document.
      !!!! IMPORTANT Templates for sealing depends on read instance of this class
      If You change this structure sealing WILL fail, unless changes are made to docseal.st
-}
data SealingTexts = SealingTexts
  { verificationTitle  :: String -- Big title at last page
  , partnerText        :: String -- Header for partner list
  , initiatorText      :: String -- Header for initiator
  , documentText       :: String -- Header for documents list
  , eventsText         :: String -- history table preheader
  , dateText           :: String -- history table date header
  , historyText        :: String -- history table event header
  , verificationFooter :: String -- Long text all the end saying that doc was verified
  , hiddenAttachmentText :: String -- "Concealed Attachment"
  , onePageText        :: String -- "1 page"
  }
  deriving (Eq,Ord,Show,Read)

unjsonSealingTexts :: Unjson.UnjsonDef SealingTexts
unjsonSealingTexts = Unjson.objectOf $ pure SealingTexts
   <*> Unjson.field "verificationTitle" verificationTitle ""
   <*> Unjson.field "partnerText" partnerText ""
   <*> Unjson.field "initiatorText" initiatorText ""
   <*> Unjson.field "documentText" documentText ""
   <*> Unjson.field "eventsText" eventsText ""
   <*> Unjson.field "dateText" dateText ""
   <*> Unjson.field "historyText" historyText ""
   <*> Unjson.field "verificationFooter" verificationFooter ""
   <*> Unjson.field "hiddenAttachmentText" hiddenAttachmentText ""
   <*> Unjson.field "onePageText" onePageText ""
