module Doc.API.V2.JSON.DisplayOptions (
  documentDisplayOptions
, applyDisplayOptionsToDocument
, unjsonDocumentDisplayOptions
) where

import Control.Applicative
import Data.Unjson

import Doc.DocStateData
import KontraPrelude

data DocumentDisplayOptions = DocumentDisplayOptions {
    showHeader :: Bool
  , showPdfDownload :: Bool
  , showReject :: Bool
  , allowRejectReason :: Bool
  , showFooter :: Bool
  , isReceipt :: Bool
}

documentDisplayOptions :: Document -> DocumentDisplayOptions
documentDisplayOptions doc = DocumentDisplayOptions {
      showHeader = documentshowheader doc
    , showPdfDownload = documentshowpdfdownload doc
    , showReject = documentshowrejectoption doc
    , allowRejectReason = documentallowrejectreason doc
    , showFooter = documentshowfooter doc
    , isReceipt = documentisreceipt doc
  }

applyDisplayOptionsToDocument :: DocumentDisplayOptions -> Document -> Document
applyDisplayOptionsToDocument displayOptions doc = doc {
      documentshowheader = showHeader displayOptions
    , documentshowpdfdownload = showPdfDownload displayOptions
    , documentshowrejectoption = showReject displayOptions
    , documentallowrejectreason = allowRejectReason displayOptions
    , documentshowfooter = showFooter displayOptions
    , documentisreceipt = isReceipt displayOptions
  }

unjsonDocumentDisplayOptions :: UnjsonDef DocumentDisplayOptions
unjsonDocumentDisplayOptions = objectOf $ pure DocumentDisplayOptions
    <*> field "show_header" showHeader "Show header while signing"
    <*> field "show_pdf_download" showPdfDownload "Show download option while signing"
    <*> field "show_reject_option" showReject "Show signatories option to reject document"
    <*> field "allow_reject_reason" allowRejectReason "Show textarea for typing in the reason for rejection"
    <*> field "show_footer" showFooter "Show footer while signing"
    <*> field "document_is_receipt" isReceipt "Act as if document is receipt"
