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
  , pageDocumentIdentifyView
  , pageDocumentPadList
  , pageDocumentPadListLogin
  , pageDocumentToStartList
  , pageDocumentToStartLogin
  , pageDocumentToStartView
  , gtVerificationPage
  ) where

import Data.Unjson
import Happstack.Server.SimpleHTTP
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView (companyUIForPage, entryPointFields, simpleHtmlResonseClrFlash, standardPageFields)
import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Company.Model
import DB
import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Document
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import EID.Nets.Config
import File.FileID
import Kontra
import KontraLink
import KontraPrelude
import User.Model
import User.Utils
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

pageCreateFromTemplate :: TemplatesMonad m => Context -> m String
pageCreateFromTemplate ctx = renderTemplate "createFromTemplatePage" $ entryPointFields ctx

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
                    => Context
                    -> Document
                    -> Maybe SignatoryLink
                    -> Bool
                    -> m String
pageDocumentView ctx document msiglink authorcompanyadmin =
  renderTemplate "pageDocumentView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ fmap (show . signatorylinkid) msiglink
      F.value "authorcompanyadmin" $ authorcompanyadmin
      entryPointFields ctx

pageDocumentSignView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentSignView ctx document siglink ad = do

  -- Sign view needs some author details and information if company allows saving a safety copy.
  let authorid = $fromJust $ getAuthorSigLink document >>= maybesignatory
  auser <- fmap $fromJust $ dbQuery $ GetUserByIDIncludeDeleted authorid
  acompany <- getCompanyForUser auser
  acompanyui <- dbQuery $ GetCompanyUI (companyid acompany)
  let loggedAsSignatory = (isJust $ maybesignatory siglink) && (maybesignatory siglink) == (userid <$> getContextUser ctx);
  let loggedAsAuthor = (Just authorid == (userid <$> getContextUser ctx));
  let docjson = unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True }) (unjsonDocument (documentAccessForSlid (signatorylinkid siglink) document)) document
      mainfile = fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document)
      lockUrl = ctxDomainUrl ctx ++ "/img/preview_locked.png"
  renderTemplate "pageDocumentSignView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "loggedinsignatory" $ loggedAsSignatory
      F.value "loggedinauthor"   $ loggedAsAuthor
      F.value "allowsavesafetycopy" $ companyallowsavesafetycopy $ companyinfo acompany
      F.value "authorFullname" $ getFullName auser
      F.value "authorPhone" $ getMobile auser
      F.value "previewLink" $  case signatorylinkauthenticationtoviewmethod siglink of
          StandardAuthenticationToView -> show $ LinkDocumentPreview (documentid document) (Just siglink) mainfile 600
          _ -> lockUrl
      F.value "b64documentdata" $ B64.encode $ docjson
      standardPageFields ctx (Just acompanyui) ad -- Branding for signview depends only on authors company

pageDocumentIdentifyView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentIdentifyView ctx document siglink ad = do
  let authorid = $fromJust $ getAuthorSigLink document >>= maybesignatory
  auser <- fmap $fromJust $ dbQuery $ GetUserByIDIncludeDeleted authorid
  acompany <- getCompanyForUser auser
  acompanyui <- dbQuery $ GetCompanyUI (companyid acompany)
  let lockUrl = ctxDomainUrl ctx ++ "/img/preview_locked.png"

  renderTemplate "pageDocumentIdentifyView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "netsIdentifyUrl" $ netsIdentifyUrl <$> ctxnetsconfig ctx
      F.value "netsMerchantIdentifier" $ netsMerchantIdentifier <$> ctxnetsconfig ctx
      F.value "netsTrustedDomain" $ netsTrustedDomain <$> ctxnetsconfig ctx
      F.value "previewLink" lockUrl
      standardPageFields ctx (Just acompanyui) ad -- Branding for signview depends only on authors company

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
gtVerificationPage =  do
  ctx <- getContext
  ad <- getAnalyticsData
  if( bdMainDomain $ ctxbrandeddomain ctx)
  then do
    content <- renderTemplate "gtVerificationPage" $ do
      standardPageFields ctx Nothing ad
    simpleHtmlResonseClrFlash content
  else respond404

