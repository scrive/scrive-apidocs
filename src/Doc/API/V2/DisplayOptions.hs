module Doc.API.V2.DisplayOptions (
  documentDisplayOptions
, applyDisplayOptionsToDocument
) where

import Control.Applicative
import Data.Unjson
import Doc.DocStateData
import KontraPrelude

data DocumentDisplayOptions = DocumentDisplayOptions {
    showHeader :: Bool
  , showPdfDownload :: Bool
  , showReject :: Bool
  , showFooter :: Bool
}

documentDisplayOptions :: Document -> DocumentDisplayOptions
documentDisplayOptions doc = DocumentDisplayOptions {
      showHeader = documentshowheader doc
    , showPdfDownload = documentshowpdfdownload doc
    , showReject = documentshowrejectoption doc
    , showFooter = documentshowfooter doc
  }

applyDisplayOptionsToDocument :: DocumentDisplayOptions -> Document -> Document
applyDisplayOptionsToDocument displayOptions doc = doc {
      documentshowheader = showHeader displayOptions
    , documentshowpdfdownload = showPdfDownload displayOptions
    , documentshowrejectoption = showReject displayOptions
    , documentshowfooter = showFooter displayOptions
  }

instance Unjson DocumentDisplayOptions where
  unjsonDef = objectOf $ pure DocumentDisplayOptions
    <*> field "show_header" showHeader "Show header while signing"
    <*> field "show_pdf_download" showPdfDownload "Show download option while signing"
    <*> field "show_reject_option" showReject "Show signatories option to reject document"
    <*> field "show_footer" showFooter "Show footer while signing"
