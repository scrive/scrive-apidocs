module Archive.View 
       (
         flashMessageAttachmentArchiveDone,
         flashMessageSignableArchiveDone,
         flashMessageTemplateArchiveDone,
         pageAttachmentList,
         pageContractsList,
         pageOffersList,
         pageOrdersList,
         pageRubbishBinList,
         pageTemplatesList,
         pagePadDeviceArchive,
         docForListJSON
       )
       where

import Doc.DocProcess
import Doc.DocStateData
import FlashMessage
import KontraLink
import Templates.Templates
import User.Model

import Control.Applicative
import Data.Maybe

import MinutesTime
import Control.Logic
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Control.Monad.Reader
import Text.JSON
import Data.List (intercalate)

import Doc.DocView
import Doc.DocUtils
import PadQueue.Model
import Text.JSON.Gen as J


flashMessageSignableArchiveDone :: TemplatesMonad m => DocumentType -> m FlashMessage
flashMessageSignableArchiveDone doctype = do
  toFlashMsg OperationDone <$> renderTextForProcess doctype processflashmessagearchivedone

flashMessageTemplateArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageTemplateArchiveDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageTemplateArchiveDone" ()

flashMessageAttachmentArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageAttachmentArchiveDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageAttachmentArchiveDone" ()

pageContractsList :: TemplatesMonad m => User -> m String
pageContractsList = pageList' "pageContractsList" LinkContracts 

pageTemplatesList :: TemplatesMonad m => User -> m String
pageTemplatesList = pageList' "pageTemplatesList" LinkTemplates 

pageAttachmentList :: TemplatesMonad m =>  User -> m String
pageAttachmentList = pageList' "pageAttachmentList" LinkAttachments 

pageOffersList :: TemplatesMonad m => User -> m String
pageOffersList = pageList' "pageOffersList" LinkOffers

pageOrdersList :: TemplatesMonad m => User -> m String
pageOrdersList = pageList' "pageOrdersList" LinkOrders 

pageRubbishBinList :: TemplatesMonad m => User ->  m String
pageRubbishBinList = pageList' "pageRubbishBinList" LinkRubbishBin 

pagePadDeviceArchive :: TemplatesMonad m => User ->  m String
pagePadDeviceArchive = pageList' "pagePadDeviceArchive" LinkPadDeviceArchive
    

{- |
    Helper function for list pages
-}
pageList' :: TemplatesMonad m
          => String
          -> KontraLink
          -> User
          -> m String
pageList' templatename currentlink user =
  renderTemplateFM templatename $ do
    field "canReallyDeleteDocs" $ useriscompanyadmin user || isNothing (usercompany user)
    field "currentlink" $ show $ currentlink
    field "linkdoclist" $ show $ LinkContracts 
    field "documentactive" $ (LinkContracts == currentlink)
    field "linkofferlist" $ show $ LinkOffers 
    field "offeractive" $ (LinkOffers == currentlink)
    field "linkorderlist" $ show $ LinkOrders
    field "orderactive" $ (LinkOrders == currentlink)
    field "linktemplatelist" $ show $ LinkTemplates
    field "templateactive" $ (LinkTemplates == currentlink)
    field "linkattachmentlist" $ show $ LinkAttachments 
    field "attachmentactive" $ (LinkAttachments == currentlink)
    field "linkrubbishbinlist" $ show $ LinkRubbishBin
    field "rubbishbinactive" $ (LinkRubbishBin == currentlink)
    field "linkpaddevicearchive" $ show LinkPadDeviceArchive 
    field "paddevicearchiveactive" $ (LinkPadDeviceArchive == currentlink)

docForListJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> User -> PadQueue ->  Document -> m JSValue
docForListJSON tl crtime user padqueue doc = do
  let link = case getSigLinkFor doc user of
        Just sl | not $ isAuthor sl -> LinkSignDoc doc sl
        _                           -> LinkIssueDoc $ documentid doc
      sigFilter sl =   isSignatory sl && (documentstatus doc /= Preparation)
  runJSONGenT $ do
    J.object "fields" $ docFieldsListForJSON tl crtime padqueue doc
    J.objects "subfields" $ map (signatoryFieldsListForJSON tl crtime padqueue doc) (filter sigFilter (documentsignatorylinks doc))
    J.value "link" $ show link

docFieldsListForJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime ->  PadQueue -> Document -> JSONGenT m ()
docFieldsListForJSON tl crtime padqueue doc = do
    J.value "id" $ show $ documentid doc
    J.value "title" $ documenttitle doc
    J.value "status" $ show $ documentStatusClass doc
    J.value "party" $ intercalate ", " $ map getSmartName $ getSignatoryPartnerLinks doc
    J.value "partner" $ intercalate ", " $ map getSmartName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "partnercomp" $ intercalate ", " $ map getCompanyName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "author" $ intercalate ", " $ map getSmartName $ filter isAuthor $ (documentsignatorylinks doc)
    J.value "time" $ showDateAbbrev tl crtime (documentmtime doc)
    J.value "process" =<< lift (renderTextForProcess doc processname)
    J.value "type" =<< lift renderDocType
    J.value "anyinvitationundelivered" $ show $ anyInvitationUndelivered  doc && Pending == documentstatus doc
    J.value "shared" $ show $ documentsharing doc == Shared
    J.value "file" $ fromMaybe "" $ show <$> (listToMaybe $ (documentsealedfiles doc) ++ (documentfiles doc))
    J.value "inpadqueue" $ "true" <| (fmap fst padqueue == Just (documentid doc)) |> "false"
  where
    renderDocType = do
      pn <- renderTextForProcess doc processname
      case documenttype doc of
        Attachment -> renderTemplateM "docListAttachmentLabel" ()
        AttachmentTemplate -> renderTemplateM "docListAttachmentLabel" ()
        Template _ -> renderTemplateFM "docListTemplateLabel" $ field "processname" pn
        Signable _ -> return pn

signatoryFieldsListForJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> PadQueue -> Document -> SignatoryLink -> JSONGenT m ()
signatoryFieldsListForJSON tl crtime padqueue doc sl = do
    J.value "status" $ show $ signatoryStatusClass doc sl
    J.value "name" $ getSmartName sl
    J.value "time" $ fromMaybe "" $ (showDateAbbrev tl crtime) <$> (sign `mplus` reject `mplus` seen `mplus` open)
    J.value "invitationundelivered" $ show $ isUndelivered sl && Pending == documentstatus doc
    J.value "inpadqueue" $ "true" <| (fmap fst padqueue == Just (documentid doc)) && (fmap snd padqueue == Just (signatorylinkid sl)) |> "false"
    where
        sign = signtime <$> maybesigninfo sl
        seen = signtime <$> maybesigninfo sl
        reject = case documentrejectioninfo doc of
          Just (rt, slid, _) | slid == signatorylinkid sl -> Just rt
          _                                               -> Nothing
        open = maybereadinvite sl

