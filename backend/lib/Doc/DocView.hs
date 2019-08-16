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
  , afterForwardPage
  ) where

import Data.Unjson
import Happstack.Server.SimpleHTTP
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView
  ( entryPointFields, simpleHtmlResponse, standardPageFields
  , userGroupUIForPage )

import BrandedDomain.BrandedDomain
import DB
import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Document
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocUtils
import Doc.DocViewMail
import EID.Nets.Config
import File.FileID
import Kontra
import KontraLink
import Templates (renderTextTemplate)
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

pageCreateFromTemplate :: TemplatesMonad m => Context -> m Text
pageCreateFromTemplate ctx =
  renderTextTemplate "createFromTemplatePage" $
    entryPointFields ctx

pageDocumentDesign :: Kontrakcja m
                   => Context
                   -> Document
                   -> AnalyticsData
                   -> m Text
pageDocumentDesign ctx document ad = do
     mugui <- userGroupUIForPage
     renderTextTemplate "pageDocumentDesign" $ do
         F.value "documentid" $ show $ documentid document
         standardPageFields ctx mugui ad

pageDocumentView :: TemplatesMonad m
                    => Context
                    -> Document
                    -> Maybe SignatoryLink
                    -> Bool
                    -> m Text
pageDocumentView ctx document msiglink authorcompanyadmin =
  renderTextTemplate "pageDocumentView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ fmap (show . signatorylinkid) msiglink
      F.value "authorcompanyadmin" $ authorcompanyadmin
      entryPointFields ctx

pageDocumentSignView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m Text
pageDocumentSignView ctx document siglink ad = do

  -- Sign view needs some author details and information if company allows saving a safety copy.
  let authorid = case getAuthorSigLink document of
        Nothing -> unexpectedError "Impossible happened: cannot find author signatory link"
        Just authorSigLink -> case maybesignatory authorSigLink of
          Nothing -> unexpectedError "Impossible happened: this document was not saved to an account"
          Just authorid' -> authorid'
  authoruser <- fmap fromJust $ dbQuery $ GetUserByIDIncludeDeleted authorid
  authorugwp <- dbQuery . UserGroupGetWithParentsByUserID . userid $ authoruser
  let loggedAsSignatory = (isJust $ maybesignatory siglink) && (maybesignatory siglink) == (userid <$> getContextUser ctx);
  let loggedAsAuthor = (Just authorid == (userid <$> getContextUser ctx));
  let docjson = unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True }) (unjsonDocument (documentAccessForSlid (signatorylinkid siglink) document)) document
      mainfile = fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document)
  renderTextTemplate "pageDocumentSignView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "loggedinsignatory" $ loggedAsSignatory
      F.value "loggedinauthor"   $ loggedAsAuthor
      F.value "authorFullname" $ getFullName authoruser
      F.value "authorPhone" $ getMobile authoruser
      F.value "previewLink" $  case signatorylinkauthenticationtoviewmethod siglink of
        StandardAuthenticationToView -> show $ LinkDocumentPreview (documentid document) (Just (siglink, Nothing)) mainfile 600
        _ -> show LinkPreviewLockedImage
      F.value "b64documentdata" $ B64.encode $ docjson
      F.value "legaltext" $ get ugsLegalText . ugwpSettings $ authorugwp
      standardPageFields ctx (Just (get ugID $ ugwpUG authorugwp, get ugUI $ ugwpUG authorugwp)) ad -- Branding for signview depends only on authors company

pageDocumentIdentifyView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m Text
pageDocumentIdentifyView ctx document siglink ad = do
  let authorid = fromJust $ getAuthorSigLink document >>= maybesignatory
  auser <- fmap fromJust $ dbQuery $ GetUserByIDIncludeDeleted authorid
  authorug <- dbQuery . UserGroupGetByUserID . userid $ auser

  renderTextTemplate "pageDocumentIdentifyView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "netsIdentifyUrl" $ netsIdentifyUrl <$> get ctxnetsconfig ctx
      F.value "netsMerchantIdentifier" $ netsMerchantIdentifier <$> get ctxnetsconfig ctx
      F.value "netsTrustedDomain" $ netsTrustedDomain <$> get ctxnetsconfig ctx
      F.value "previewLink" $ show LinkPreviewLockedImage
      standardPageFields ctx (Just (get ugID authorug, get ugUI authorug)) ad -- Branding for signview depends only on authors company

pageDocumentPadList:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m Text
pageDocumentPadList ctx ad = do
  mugui <- userGroupUIForPage
  renderTextTemplate "pagePadListView" $ do
      standardPageFields ctx mugui ad

pageDocumentPadListLogin:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m Text
pageDocumentPadListLogin ctx ad = do
  renderTextTemplate "padLogin" $ do
      standardPageFields ctx Nothing ad

{- To start, list + login + show view -}

pageDocumentToStartList :: Kontrakcja m
                           => Context
                           -> AnalyticsData
                           -> m Text
pageDocumentToStartList ctx ad = do
  mugui <- userGroupUIForPage
  renderTextTemplate "pageToStartListView" $ do
    standardPageFields ctx mugui ad

pageDocumentToStartLogin:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m Text
pageDocumentToStartLogin ctx ad = do
  renderTextTemplate "toStartLogin" $ do
    standardPageFields ctx Nothing ad

pageDocumentToStartView :: Kontrakcja m
                    => Context
                    -> Document
                    -> AnalyticsData
                    -> m Text
pageDocumentToStartView ctx document ad = do
  mugui <- userGroupUIForPage
  renderTextTemplate "pageToStartDocumentView" $ do
    F.value "documentid" $ show $ documentid document
    F.value "documenttitle" $ documenttitle document
    standardPageFields ctx mugui ad


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
  if (get (bdMainDomain . ctxbrandeddomain) ctx)
  then do
    content <- renderTextTemplate "gtVerificationPage" $ do
      standardPageFields ctx Nothing ad
    simpleHtmlResponse content
  else respond404

afterForwardPage :: Kontrakcja m => DocumentID -> m Text
afterForwardPage did = do
  ctx <- getContext
  ad <- getAnalyticsData
  renderTextTemplate "afterForwardPage" $ do
      F.value "documentid" $ show $ did
      standardPageFields ctx Nothing ad
