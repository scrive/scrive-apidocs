{-# LANGUAGE ExtendedDefaultRules #-}
module Doc.DocView (
    pageCreateFromTemplate
  , documentInfoFields
  , mailDocumentAwaitingForAuthor
  , mailDocumentClosed
  , mailDocumentRejected
  , mailDocumentRemind
  , mailInvitation
  , mailForwardSigned
  , pageDocumentDesign
  , pageDocumentView
  , pageDocumentSignView
  , pageDocumentSignForPadView
  , pageDocumentPadList
  , pageDocumentPadListLogin
  , pagePostSignview
  , pageDocumentToStartList
  , pageDocumentToStartLogin
  , pageDocumentToStartView
  , gtVerificationPage
  ) where

import Control.Applicative ((<$>))
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView (standardPageFields, companyUIForPage, renderFromBody)
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import Kontra
import User.Model


pageCreateFromTemplate :: TemplatesMonad m => m String
pageCreateFromTemplate = renderTemplate_ "createFromTemplatePage"

pageDocumentDesign :: Kontrakcja m
                   => Context
                   -> Document
                   -> AnalyticsData
                   -> m String
pageDocumentDesign ctx document ad = do
     mcompany <- companyUIForPage
     renderTemplate "pageDocumentDesign" $ do
         F.value "documentid" $ show $ documentid document
         standardPageFields ctx mcompany ad

pageDocumentView :: TemplatesMonad m
                    => Document
                    -> Maybe SignatoryLink
                    -> Bool
                    -> m String
pageDocumentView document msiglink authorcompanyadmin =
  renderTemplate "pageDocumentView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ fmap (show . signatorylinkid) msiglink
      F.value "authorcompanyadmin" $ authorcompanyadmin


pageDocumentSignView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentSignView ctx document siglink ad = do
  mcompany <- companyUIForPage
  renderTemplate "pageDocumentSignView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "usestandardheaders" $ (isJust $ maybesignatory siglink) && (maybesignatory siglink) == (userid <$> ctxmaybeuser ctx)
      standardPageFields ctx mcompany ad


pageDocumentSignForPadView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentSignForPadView ctx document siglink ad = do
  mcompany <- companyUIForPage
  renderTemplate "pageDocumentSignPadView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      standardPageFields ctx mcompany ad


pageDocumentPadList:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pageDocumentPadList ctx ad = do
  mcompany <- companyUIForPage
  renderTemplate "pagePadListView" $ do
      standardPageFields ctx mcompany ad

pageDocumentPadListLogin:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pageDocumentPadListLogin ctx ad = do
  renderTemplate "padLogin" $ do
      standardPageFields ctx Nothing ad


pagePostSignview :: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pagePostSignview ctx ad = do
  mcompany <- companyUIForPage
  renderTemplate "pagePostSignview" $ do
      standardPageFields ctx mcompany ad


{- To start, list + login + show view -}

pageDocumentToStartList :: Kontrakcja m
                           => Context
                           -> AnalyticsData
                           -> m String
pageDocumentToStartList ctx ad = do
  mcompany <- companyUIForPage
  renderTemplate "pageToStartListView" $ do
    standardPageFields ctx mcompany ad

pageDocumentToStartLogin:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pageDocumentToStartLogin ctx ad = do
  renderTemplate "toStartLogin" $ do
    standardPageFields ctx Nothing ad

pageDocumentToStartView :: Kontrakcja m
                    => Context
                    -> Document
                    -> AnalyticsData
                    -> m String
pageDocumentToStartView ctx document ad = do
  mcompany <- companyUIForPage
  renderTemplate "pageToStartDocumentView" $ do
    F.value "documentid" $ show $ documentid document
    F.value "documenttitle" $ documenttitle document
    standardPageFields ctx mcompany ad


-- | Basic info about document , name, id ,author
documentInfoFields :: Monad m => Document -> Fields m ()
documentInfoFields  document  = do
  F.value "documenttitle" $ documenttitle document
  F.value "title" $ documenttitle document
  F.value "name" $ documenttitle document
  F.value "id" $ show $ documentid document
  F.value "documentid" $ show $ documentid document
  F.value "template" $  isTemplate document
  F.value "hasanyattachments" $ not (null $ (documentauthorattachments document)) || not (null (concatMap signatoryattachments $ documentsignatorylinks document))
  documentStatusFields document

-- | Fields indication what is a document status
documentStatusFields :: Monad m => Document -> Fields m ()
documentStatusFields document = do
  F.value "preparation" $ documentstatus document == Preparation
  F.value "pending" $ documentstatus document == Pending
  F.value "cancel" $ documentstatus document == Canceled
  F.value "timedout" $ documentstatus document == Timedout
  F.value "rejected" $ documentstatus document == Rejected
  F.value "signed" $ documentstatus document == Closed
  -- awaitingauthor is used in old view in old template
  -- remove it when old view is removed
  -- currently it means: is the next turn for author to sign?
  F.value "awaitingauthor" $ canAuthorSignNow document

-- Page for GT verification
gtVerificationPage :: Kontrakcja m => m Response
gtVerificationPage = renderFromBody =<< renderTemplate_ "gtVerificationPage"
