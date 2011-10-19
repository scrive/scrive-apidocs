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
         pageTemplatesList         
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

{- |
    Helper function for list pages
-}
pageList' :: TemplatesMonad m
          => String
          -> KontraLink
          -> User
          -> m String
pageList' templatename currentlink user  =
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
