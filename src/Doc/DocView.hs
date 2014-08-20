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
  , gtVerificationPage
  ) where

import AppView (kontrakcja, standardPageFields, brandingFields, companyUIForPage, renderFromBody)
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import Kontra
import Text.StringTemplates.Templates
import User.Model
import Control.Applicative ((<$>))
import Data.Maybe
import qualified Text.StringTemplates.Fields as F
import Analytics.Include
import Happstack.Server.SimpleHTTP
import BrandedDomain.BrandedDomain

pageCreateFromTemplate :: TemplatesMonad m => m String
pageCreateFromTemplate = renderTemplate_ "createFromTemplatePage"

pageDocumentDesign :: Kontrakcja m
                   => Context
                   -> Document
                   -> AnalyticsData
                   -> m String
pageDocumentDesign ctx document ad = do
     let  mbd = ctxbrandeddomain ctx
     mcompany <- companyUIForPage
     renderTemplate "pageDocumentDesign" $ do
         F.value "documentid" $ show $ documentid document
         standardPageFields ctx kontrakcja ad
         brandingFields mbd mcompany

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
  let  mbd = ctxbrandeddomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pageDocumentSignView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "usestandardheaders" $ (isJust $ maybesignatory siglink) && (maybesignatory siglink) == (userid <$> ctxmaybeuser ctx)
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany


pageDocumentSignForPadView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentSignForPadView ctx document siglink ad = do
  let  mbd = ctxbrandeddomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pageDocumentSignPadView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany


pageDocumentPadList:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pageDocumentPadList ctx ad = do
  let  mbd = ctxbrandeddomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pagePadListView" $ do
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany

pageDocumentPadListLogin:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pageDocumentPadListLogin ctx ad = do
  let  mbd = ctxbrandeddomain ctx
  renderTemplate "padLogin" $ do
      standardPageFields ctx kontrakcja ad
      F.value "servicelinkcolour" $ bdservicelinkcolour <$> mbd
      F.value "textscolour" $ bdexternaltextcolour <$> mbd
      F.value "background" $ bdbackgroundcolorexternal <$> mbd
      brandingFields mbd Nothing



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
  F.value "cancel" $ (documentstatus document == Canceled
      && (all (not . isJust . signatorylinkelegdatamismatchmessage) $ documentsignatorylinks document))
  F.value "timedout" $ documentstatus document == Timedout
  F.value "rejected" $ documentstatus document == Rejected
  F.value "signed" $ documentstatus document == Closed
  -- awaitingauthor is used in old view in old template
  -- remove it when old view is removed
  -- currently it means: is the next turn for author to sign?
  F.value "awaitingauthor" $ canAuthorSignNow document
  F.value "datamismatch" $ (documentstatus document == Canceled
      && (any (isJust . signatorylinkelegdatamismatchmessage) $ documentsignatorylinks document))

-- Page for GT verification
gtVerificationPage :: Kontrakcja m => m Response
gtVerificationPage = renderFromBody kontrakcja =<< renderTemplate_ "gtVerificationPage"
